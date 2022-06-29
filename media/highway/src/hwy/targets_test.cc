// Copyright 2020 Google LLC
// SPDX-License-Identifier: Apache-2.0
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#include "hwy/targets.h"

#include "hwy/tests/test_util-inl.h"

namespace fake {

#define DECLARE_FUNCTION(TGT)                                                \
  namespace N_##TGT {                                                        \
    /* Function argument is just to ensure/demonstrate they are possible. */ \
    uint32_t FakeFunction(int) { return HWY_##TGT; }                         \
  }

DECLARE_FUNCTION(AVX3_DL)
DECLARE_FUNCTION(AVX3)
DECLARE_FUNCTION(AVX2)
DECLARE_FUNCTION(SSE4)
DECLARE_FUNCTION(SSSE3)
DECLARE_FUNCTION(NEON)
DECLARE_FUNCTION(SVE)
DECLARE_FUNCTION(SVE2)
DECLARE_FUNCTION(PPC8)
DECLARE_FUNCTION(WASM)
DECLARE_FUNCTION(RVV)
DECLARE_FUNCTION(SCALAR)
DECLARE_FUNCTION(EMU128)

HWY_EXPORT(FakeFunction);

void CallFunctionForTarget(uint32_t target, int line) {
  if ((HWY_TARGETS & target) == 0) return;
  hwy::SetSupportedTargetsForTest(target);

  // Call Update() first to make &HWY_DYNAMIC_DISPATCH() return
  // the pointer to the already cached function.
  hwy::GetChosenTarget().Update();

  EXPECT_EQ(target, HWY_DYNAMIC_DISPATCH(FakeFunction)(42)) << line;

  // Calling DeInit() will test that the initializer function
  // also calls the right function.
  hwy::GetChosenTarget().DeInit();

  EXPECT_EQ(target, HWY_DYNAMIC_DISPATCH(FakeFunction)(42)) << line;

  // Second call uses the cached value from the previous call.
  EXPECT_EQ(target, HWY_DYNAMIC_DISPATCH(FakeFunction)(42)) << line;
}

void CheckFakeFunction() {
  // When adding a target, also add to DECLARE_FUNCTION above.
  CallFunctionForTarget(HWY_AVX3_DL, __LINE__);
  CallFunctionForTarget(HWY_AVX3, __LINE__);
  CallFunctionForTarget(HWY_AVX2, __LINE__);
  CallFunctionForTarget(HWY_SSE4, __LINE__);
  CallFunctionForTarget(HWY_SSSE3, __LINE__);
  CallFunctionForTarget(HWY_NEON, __LINE__);
  CallFunctionForTarget(HWY_SVE, __LINE__);
  CallFunctionForTarget(HWY_SVE2, __LINE__);
  CallFunctionForTarget(HWY_PPC8, __LINE__);
  CallFunctionForTarget(HWY_WASM, __LINE__);
  CallFunctionForTarget(HWY_RVV, __LINE__);
  // The tables only have space for either HWY_SCALAR or HWY_EMU128; the former
  // is opt-in only.
#if defined(HWY_COMPILE_ONLY_SCALAR)
  CallFunctionForTarget(HWY_SCALAR, __LINE__);
#else
  CallFunctionForTarget(HWY_EMU128, __LINE__);
#endif
}

}  // namespace fake

namespace hwy {

class HwyTargetsTest : public testing::Test {
 protected:
  void TearDown() override {
    SetSupportedTargetsForTest(0);
    DisableTargets(0);  // Reset the mask.
  }
};

// Test that the order in the HWY_EXPORT static array matches the expected
// value of the target bits. This is only checked for the targets that are
// enabled in the current compilation.
TEST_F(HwyTargetsTest, ChosenTargetOrderTest) { fake::CheckFakeFunction(); }

TEST_F(HwyTargetsTest, DisabledTargetsTest) {
  DisableTargets(~0u);
#if HWY_ARCH_X86
  // Check that the baseline can't be disabled.
  HWY_ASSERT(HWY_ENABLED_BASELINE == SupportedTargets());
#else
  // TODO(janwas): update when targets.cc changes
  HWY_ASSERT(HWY_TARGETS == SupportedTargets());
#endif

  DisableTargets(0);  // Reset the mask.
  uint32_t current_targets = SupportedTargets();
  if ((current_targets & ~static_cast<uint32_t>(HWY_ENABLED_BASELINE)) == 0) {
    // We can't test anything else if the only compiled target is the baseline.
    return;
  }
  // Get the lowest bit in the mask (the best target) and disable that one.
  uint32_t best_target = current_targets & (~current_targets + 1);
  // The lowest target shouldn't be one in the baseline.
  HWY_ASSERT((best_target & ~static_cast<uint32_t>(HWY_ENABLED_BASELINE)) != 0);
  DisableTargets(best_target);

  // Check that the other targets are still enabled.
  HWY_ASSERT((best_target ^ current_targets) == SupportedTargets());
  DisableTargets(0);  // Reset the mask.
}

}  // namespace hwy
