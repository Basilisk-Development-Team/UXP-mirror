/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
// Copyright (c) 2006-2008 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "base/waitable_event.h"

#include "base/condition_variable.h"
#include "base/lock.h"
#include "base/message_loop.h"

// -----------------------------------------------------------------------------
// A WaitableEvent on POSIX is implemented as a wait-list. Currently we don't
// support cross-process events (where one process can signal an event which
// others are waiting on). Because of this, we can avoid having one thread per
// listener in several cases.
//
// The WaitableEvent maintains a list of waiters, protected by a lock. Each
// waiter is either an async wait, in which case we have a Task and the
// MessageLoop to run it on, or a blocking wait, in which case we have the
// condition variable to signal.
//
// Waiting involves grabbing the lock and adding oneself to the wait list. Async
// waits can be canceled, which means grabbing the lock and removing oneself
// from the list.
//
// Waiting on multiple events is handled by adding a single, synchronous wait to
// the wait-list of many events. An event passes a pointer to itself when
// firing a waiter and so we can store that pointer to find out which event
// triggered.
// -----------------------------------------------------------------------------

namespace base {

// -----------------------------------------------------------------------------
// This is just an abstract base class for waking the two types of waiters
// -----------------------------------------------------------------------------
WaitableEvent::WaitableEvent(bool manual_reset, bool initially_signaled)
    : kernel_(new WaitableEventKernel(manual_reset, initially_signaled)) {
}

WaitableEvent::~WaitableEvent() {
}

void WaitableEvent::Reset() {
  AutoLock locked(kernel_->lock_);
  kernel_->signaled_ = false;
}

void WaitableEvent::Signal() {
  AutoLock locked(kernel_->lock_);

  if (kernel_->signaled_)
    return;

  if (kernel_->manual_reset_) {
    SignalAll();
    kernel_->signaled_ = true;
  } else {
    // In the case of auto reset, if no waiters were woken, we remain
    // signaled.
    if (!SignalOne())
      kernel_->signaled_ = true;
  }
}

bool WaitableEvent::IsSignaled() {
  AutoLock locked(kernel_->lock_);

  const bool result = kernel_->signaled_;
  if (result && !kernel_->manual_reset_)
    kernel_->signaled_ = false;
  return result;
}

// -----------------------------------------------------------------------------
// Synchronous waits

// -----------------------------------------------------------------------------
// This is an synchronous waiter. The thread is waiting on the given condition
// variable and the fired flag in this object.
// -----------------------------------------------------------------------------
class SyncWaiter : public WaitableEvent::Waiter {
 public:
  SyncWaiter(ConditionVariable* cv, Lock* lock)
      : fired_(false),
        cv_(cv),
        lock_(lock),
        signaling_event_(NULL) {
  }

  bool Fire(WaitableEvent *signaling_event) {
    AutoLock locked(*lock_);

    if (fired_) {
      return false;
    }

    fired_ = true;
    signaling_event_ = signaling_event;

    cv_->Broadcast();

    // SyncWaiters are stack allocated on the stack of the blocking thread.
    return true;
  }

  WaitableEvent* signaled_event() const {
    return signaling_event_;
  }

  // ---------------------------------------------------------------------------
  // These waiters are always stack allocated and don't delete themselves. Thus
  // there's no problem and the ABA tag is the same as the object pointer.
  // ---------------------------------------------------------------------------
  bool Compare(void* tag) {
    return this == tag;
  }

  // ---------------------------------------------------------------------------
  // Called with lock held.
  // ---------------------------------------------------------------------------
  bool fired() const {
    return fired_;
  }

  // ---------------------------------------------------------------------------
  // During a TimedWait, we need a way to make sure that an auto-reset
  // WaitableEvent doesn't think that this event has been signaled between
  // unlocking it and removing it from the wait-list. Called with lock held.
  // ---------------------------------------------------------------------------
  void Disable() {
    fired_ = true;
  }

 private:
  bool fired_;
  ConditionVariable *const cv_;
  Lock *const lock_;
  WaitableEvent* signaling_event_;  // The WaitableEvent which woke us
};

bool WaitableEvent::TimedWait(const TimeDelta& max_time) {
  const TimeTicks end_time(TimeTicks::Now() + max_time);
  const bool finite_time = max_time.ToInternalValue() >= 0;

  kernel_->lock_.Acquire();
    if (kernel_->signaled_) {
      if (!kernel_->manual_reset_) {
        // In this case we were signaled when we had no waiters. Now that
        // someone has waited upon us, we can automatically reset.
        kernel_->signaled_ = false;
      }

      kernel_->lock_.Release();
      return true;
    }

    Lock lock;
    lock.Acquire();
    ConditionVariable cv(&lock);
    SyncWaiter sw(&cv, &lock);

    Enqueue(&sw);
  kernel_->lock_.Release();
  // We are violating locking order here by holding the SyncWaiter lock but not
  // the WaitableEvent lock. However, this is safe because we don't lock @lock_
  // again before unlocking it.

  for (;;) {
    const TimeTicks current_time(TimeTicks::Now());

    if (sw.fired() || (finite_time && current_time >= end_time)) {
      const bool return_value = sw.fired();

      // We can't acquire @lock_ before releasing @lock (because of locking
      // order), however, inbetween the two a signal could be fired and @sw
      // would accept it, however we will still return false, so the signal
      // would be lost on an auto-reset WaitableEvent. Thus we call Disable
      // which makes sw::Fire return false.
      sw.Disable();
      lock.Release();

      kernel_->lock_.Acquire();
        kernel_->Dequeue(&sw, &sw);
      kernel_->lock_.Release();

      return return_value;
    }

    if (finite_time) {
      const TimeDelta max_wait(end_time - current_time);
      cv.TimedWait(max_wait);
    } else {
      cv.Wait();
    }
  }
}

bool WaitableEvent::Wait() {
  return TimedWait(TimeDelta::FromSeconds(-1));
}

// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
// Synchronous waiting on multiple objects.

static bool  // StrictWeakOrdering
cmp_fst_addr(const std::pair<WaitableEvent*, unsigned> &a,
             const std::pair<WaitableEvent*, unsigned> &b) {
  return a.first < b.first;
}

// static
size_t WaitableEvent::WaitMany(WaitableEvent** raw_waitables,
                               size_t count) {
  DCHECK(count) << "Cannot wait on no events";

  // We need to acquire the locks in a globally consistent order. Thus we sort
  // the array of waitables by address. We actually sort a pairs so that we can
  // map back to the original index values later.
  std::vector<std::pair<WaitableEvent*, size_t> > waitables;
  waitables.reserve(count);
  for (size_t i = 0; i < count; ++i)
    waitables.push_back(std::make_pair(raw_waitables[i], i));

  DCHECK_EQ(count, waitables.size());

  sort(waitables.begin(), waitables.end(), cmp_fst_addr);

  // The set of waitables must be distinct. Since we have just sorted by
  // address, we can check this cheaply by comparing pairs of consecutive
  // elements.
  for (size_t i = 0; i < waitables.size() - 1; ++i) {
    DCHECK(waitables[i].first != waitables[i+1].first);
  }

  Lock lock;
  ConditionVariable cv(&lock);
  SyncWaiter sw(&cv, &lock);

  const size_t r = EnqueueMany(&waitables[0], count, &sw);
  if (r) {
    // One of the events is already signaled. The SyncWaiter has not been
    // enqueued anywhere. EnqueueMany returns the count of remaining waitables
    // when the signaled one was seen, so the index of the signaled event is
    // @count - @r.
    return waitables[count - r].second;
  }

  // At this point, we hold the locks on all the WaitableEvents and we have
  // enqueued our waiter in them all.
  lock.Acquire();
    // Release the WaitableEvent locks in the reverse order
    for (size_t i = 0; i < count; ++i) {
      waitables[count - (1 + i)].first->kernel_->lock_.Release();
    }

    for (;;) {
      if (sw.fired())
        break;

      cv.Wait();
    }
  lock.Release();

  // The address of the WaitableEvent which fired is stored in the SyncWaiter.
  WaitableEvent *const signaled_event = sw.signaled_event();
  // This will store the index of the raw_waitables which fired.
  size_t signaled_index = 0;

  // Take the locks of each WaitableEvent in turn (except the signaled one) and
  // remove our SyncWaiter from the wait-list
  for (size_t i = 0; i < count; ++i) {
    if (raw_waitables[i] != signaled_event) {
      raw_waitables[i]->kernel_->lock_.Acquire();
        // There's no possible ABA issue with the address of the SyncWaiter here
        // because it lives on the stack. Thus the tag value is just the pointer
        // value again.
        raw_waitables[i]->kernel_->Dequeue(&sw, &sw);
      raw_waitables[i]->kernel_->lock_.Release();
    } else {
      signaled_index = i;
    }
  }

  return signaled_index;
}

// -----------------------------------------------------------------------------
// If return value == 0:
//   The locks of the WaitableEvents have been taken in order and the Waiter has
//   been enqueued in the wait-list of each. None of the WaitableEvents are
//   currently signaled
// else:
//   None of the WaitableEvent locks are held. The Waiter has not been enqueued
//   in any of them and the return value is the index of the first WaitableEvent
//   which was signaled, from the end of the array.
// -----------------------------------------------------------------------------
// static
size_t WaitableEvent::EnqueueMany
    (std::pair<WaitableEvent*, size_t>* waitables,
     size_t count, Waiter* waiter) {
  if (!count)
    return 0;

  waitables[0].first->kernel_->lock_.Acquire();
    if (waitables[0].first->kernel_->signaled_) {
      if (!waitables[0].first->kernel_->manual_reset_)
        waitables[0].first->kernel_->signaled_ = false;
      waitables[0].first->kernel_->lock_.Release();
      return count;
    }

    const size_t r = EnqueueMany(waitables + 1, count - 1, waiter);
    if (r) {
      waitables[0].first->kernel_->lock_.Release();
    } else {
      waitables[0].first->Enqueue(waiter);
    }

    return r;
}

// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
// Private functions...

// -----------------------------------------------------------------------------
// Wake all waiting waiters. Called with lock held.
// -----------------------------------------------------------------------------
bool WaitableEvent::SignalAll() {
  bool signaled_at_least_one = false;

  for (std::list<Waiter*>::iterator
       i = kernel_->waiters_.begin(); i != kernel_->waiters_.end(); ++i) {
    if ((*i)->Fire(this))
      signaled_at_least_one = true;
  }

  kernel_->waiters_.clear();
  return signaled_at_least_one;
}

// ---------------------------------------------------------------------------
// Try to wake a single waiter. Return true if one was woken. Called with lock
// held.
// ---------------------------------------------------------------------------
bool WaitableEvent::SignalOne() {
  for (;;) {
    if (kernel_->waiters_.empty())
      return false;

    const bool r = (*kernel_->waiters_.begin())->Fire(this);
    kernel_->waiters_.pop_front();
    if (r)
      return true;
  }
}

// -----------------------------------------------------------------------------
// Add a waiter to the list of those waiting. Called with lock held.
// -----------------------------------------------------------------------------
void WaitableEvent::Enqueue(Waiter* waiter) {
  kernel_->waiters_.push_back(waiter);
}

// -----------------------------------------------------------------------------
// Remove a waiter from the list of those waiting. Return true if the waiter was
// actually removed. Called with lock held.
// -----------------------------------------------------------------------------
bool WaitableEvent::WaitableEventKernel::Dequeue(Waiter* waiter, void* tag) {
  for (std::list<Waiter*>::iterator
       i = waiters_.begin(); i != waiters_.end(); ++i) {
    if (*i == waiter && (*i)->Compare(tag)) {
      waiters_.erase(i);
      return true;
    }
  }

  return false;
}

// -----------------------------------------------------------------------------

}  // namespace base
