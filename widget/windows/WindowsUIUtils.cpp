/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#include <stdlib.h>
#include <windows.h>
#include <winsdkver.h>
#include "mozwrlbase.h"
#include "nsServiceManagerUtils.h"

#include "WindowsUIUtils.h"

#include "nsIObserverService.h"
#include "nsIBaseWindow.h"
#include "nsIDocShell.h"
#include "nsIAppShellService.h"
#include "nsAppShellCID.h"
#include "nsIXULWindow.h"
#include "mozilla/Services.h"
#include "mozilla/WindowsVersion.h"
#include "nsString.h"
#include "nsIWidget.h"

/* mingw currently doesn't support windows.ui.viewmanagement.h, so we disable it until it's fixed. */
#ifndef __MINGW32__

#include <windows.ui.viewmanagement.h>
#include <windows.media.h>
#include <windows.foundation.h>
#include <windows.storage.streams.h>
#include <systemmediatransportcontrolsinterop.h>

#pragma comment(lib, "runtimeobject.lib")

using namespace mozilla;
using namespace ABI::Windows::UI;
using namespace ABI::Windows::UI::ViewManagement;
using namespace ABI::Windows::Media;
using namespace ABI::Windows::Storage::Streams;
using namespace Microsoft::WRL;
using namespace Microsoft::WRL::Wrappers;
using namespace ABI::Windows::Foundation;

/* All of this is win10 stuff and we're compiling against win81 headers
 * for now, so we may need to do some legwork: */
#if WINVER_MAXVER < 0x0A00
namespace ABI {
  namespace Windows {
    namespace UI {
      namespace ViewManagement {
        enum UserInteractionMode {
          UserInteractionMode_Mouse = 0,
          UserInteractionMode_Touch = 1
        };
      }
    }
  }
}

#endif

#ifndef RuntimeClass_Windows_UI_ViewManagement_UIViewSettings
#define RuntimeClass_Windows_UI_ViewManagement_UIViewSettings L"Windows.UI.ViewManagement.UIViewSettings"
#endif

#ifndef RuntimeClass_Windows_Media_SystemMediaTransportControls
#define RuntimeClass_Windows_Media_SystemMediaTransportControls L"Windows.Media.SystemMediaTransportControls"
#endif

#ifndef RuntimeClass_Windows_Foundation_Uri
#define RuntimeClass_Windows_Foundation_Uri L"Windows.Foundation.Uri"
#endif

#ifndef RuntimeClass_Windows_Storage_Streams_RandomAccessStreamReference
#define RuntimeClass_Windows_Storage_Streams_RandomAccessStreamReference L"Windows.Storage.Streams.RandomAccessStreamReference"
#endif

// Some SDK configurations only forward-declare this interface unless newer
// NTDDI guards are enabled. Provide a local fallback definition.
#ifndef __ISystemMediaTransportControlsInterop_INTERFACE_DEFINED__
MIDL_INTERFACE("ddb0472d-c911-4a1f-86d9-dc3d71a95f5a")
ISystemMediaTransportControlsInterop : public IInspectable
{
public:
  virtual HRESULT STDMETHODCALLTYPE GetForWindow(
      HWND appWindow,
      REFIID riid,
      void** mediaTransportControl) = 0;
};
#endif

#if WINVER_MAXVER < 0x0A00
namespace ABI {
  namespace Windows {
    namespace UI {
      namespace ViewManagement {
        interface IUIViewSettings;
        MIDL_INTERFACE("C63657F6-8850-470D-88F8-455E16EA2C26")
          IUIViewSettings : public IInspectable
          {
            public:
              virtual HRESULT STDMETHODCALLTYPE get_UserInteractionMode(UserInteractionMode *value) = 0;
          };

        extern const __declspec(selectany) IID & IID_IUIViewSettings = __uuidof(IUIViewSettings);
      }
    }
  }
}
#endif

#ifndef IUIViewSettingsInterop

typedef interface IUIViewSettingsInterop IUIViewSettingsInterop;

MIDL_INTERFACE("3694dbf9-8f68-44be-8ff5-195c98ede8a6")
IUIViewSettingsInterop : public IInspectable
{
public:
  virtual HRESULT STDMETHODCALLTYPE GetForWindow(HWND hwnd, REFIID riid, void **ppv) = 0;
};
#endif

#endif

#ifndef __MINGW32__
namespace {
  ComPtr<ISystemMediaTransportControls> gSMTC;
  EventRegistrationToken gSMTCButtonPressedToken;

  void
  NotifySMTCControlCommand(const char16_t* aCommand)
  {
    if (!aCommand) {
      return;
    }
    nsCOMPtr<nsIObserverService> observerService = mozilla::services::GetObserverService();
    if (observerService) {
      observerService->NotifyObservers(nullptr, "smtc-control-command", aCommand);
    }
  }

  nsresult
  GetHiddenWindowHWND(HWND* aHwnd)
  {
    if (!aHwnd) {
      return NS_ERROR_INVALID_ARG;
    }
    *aHwnd = nullptr;

    nsCOMPtr<nsIAppShellService> appShell(do_GetService(NS_APPSHELLSERVICE_CONTRACTID));
    if (!appShell) {
      return NS_ERROR_FAILURE;
    }

    nsCOMPtr<nsIXULWindow> hiddenWindow;
    nsresult rv = appShell->GetHiddenWindow(getter_AddRefs(hiddenWindow));
    NS_ENSURE_SUCCESS(rv, rv);
    NS_ENSURE_TRUE(hiddenWindow, NS_ERROR_FAILURE);

    nsCOMPtr<nsIDocShell> docShell;
    rv = hiddenWindow->GetDocShell(getter_AddRefs(docShell));
    NS_ENSURE_SUCCESS(rv, rv);
    NS_ENSURE_TRUE(docShell, NS_ERROR_FAILURE);

    nsCOMPtr<nsIBaseWindow> baseWindow(do_QueryInterface(docShell));
    NS_ENSURE_TRUE(baseWindow, NS_ERROR_FAILURE);

    nsCOMPtr<nsIWidget> widget;
    baseWindow->GetMainWidget(getter_AddRefs(widget));
    NS_ENSURE_TRUE(widget, NS_ERROR_FAILURE);

    *aHwnd = reinterpret_cast<HWND>(widget->GetNativeData(NS_NATIVE_WINDOW));
    return *aHwnd ? NS_OK : NS_ERROR_FAILURE;
  }

  nsresult
  GetSMTCTargetHWND(HWND* aHwnd)
  {
    if (!aHwnd) {
      return NS_ERROR_INVALID_ARG;
    }
    *aHwnd = nullptr;

    // Prefer an interactive browser window for SMTC interop.
    HWND hwnd = ::GetForegroundWindow();
    if (!hwnd) {
      hwnd = ::GetActiveWindow();
    }
    if (hwnd) {
      *aHwnd = hwnd;
      return NS_OK;
    }

    // Fall back to hidden window if no foreground window is available.
    return GetHiddenWindowHWND(aHwnd);
  }

  nsresult
  EnsureSMTC()
  {
    if (gSMTC) {
      return NS_OK;
    }
    if (!IsWin10OrLater()) {
      return NS_OK;
    }

    HWND hwnd = nullptr;
    nsresult rv = GetSMTCTargetHWND(&hwnd);
    NS_ENSURE_SUCCESS(rv, rv);

    ComPtr<ISystemMediaTransportControlsInterop> interop;
    HRESULT hr = GetActivationFactory(
      HStringReference(RuntimeClass_Windows_Media_SystemMediaTransportControls).Get(),
      &interop);
    if (FAILED(hr)) {
      return NS_ERROR_NOT_AVAILABLE;
    }

    hr = interop->GetForWindow(hwnd, IID_PPV_ARGS(&gSMTC));
    if (FAILED(hr)) {
      return NS_ERROR_NOT_AVAILABLE;
    }

    gSMTC->put_IsEnabled(TRUE);
    gSMTC->put_IsPlayEnabled(TRUE);
    gSMTC->put_IsPauseEnabled(TRUE);
    gSMTC->put_IsStopEnabled(TRUE);
    gSMTC->put_IsNextEnabled(TRUE);
    gSMTC->put_IsPreviousEnabled(TRUE);

    auto buttonHandler = Callback<
      __FITypedEventHandler_2_Windows__CMedia__CSystemMediaTransportControls_Windows__CMedia__CSystemMediaTransportControlsButtonPressedEventArgs>(
      [](ABI::Windows::Media::ISystemMediaTransportControls*,
         ABI::Windows::Media::ISystemMediaTransportControlsButtonPressedEventArgs* aArgs) -> HRESULT {
        if (!aArgs) {
          return S_OK;
        }

        SystemMediaTransportControlsButton button;
        if (FAILED(aArgs->get_Button(&button))) {
          return S_OK;
        }

        switch (button) {
          case SystemMediaTransportControlsButton_Play:
            NotifySMTCControlCommand(u"play");
            break;
          case SystemMediaTransportControlsButton_Pause:
            NotifySMTCControlCommand(u"pause");
            break;
          case SystemMediaTransportControlsButton_Stop:
            NotifySMTCControlCommand(u"stop");
            break;
          default:
            break;
        }
        return S_OK;
      });

    if (buttonHandler) {
      gSMTC->add_ButtonPressed(buttonHandler.Get(), &gSMTCButtonPressedToken);
    }

    return NS_OK;
  }
}
#endif

WindowsUIUtils::WindowsUIUtils() :
  mInTabletMode(eTabletModeUnknown)
{
}

WindowsUIUtils::~WindowsUIUtils()
{
}

/*
 * Implement the nsISupports methods...
 */
NS_IMPL_ISUPPORTS(WindowsUIUtils,
                  nsIWindowsUIUtils)

NS_IMETHODIMP
WindowsUIUtils::GetInTabletMode(bool* aResult)
{
  if (mInTabletMode == eTabletModeUnknown) {
    UpdateTabletModeState();
  }
  *aResult = mInTabletMode == eTabletModeOn;
  return NS_OK;
}

NS_IMETHODIMP
WindowsUIUtils::UpdateTabletModeState()
{
#ifndef __MINGW32__
  if (!IsWin10OrLater()) {
    return NS_OK;
  }

  nsCOMPtr<nsIAppShellService> appShell(do_GetService(NS_APPSHELLSERVICE_CONTRACTID));
  nsCOMPtr<nsIXULWindow> hiddenWindow;

  nsresult rv = appShell->GetHiddenWindow(getter_AddRefs(hiddenWindow));
  if (NS_FAILED(rv)) {
    return rv;
  }

  nsCOMPtr<nsIDocShell> docShell;
  rv = hiddenWindow->GetDocShell(getter_AddRefs(docShell));
  if (NS_FAILED(rv) || !docShell) {
    return rv;
  }

  nsCOMPtr<nsIBaseWindow> baseWindow(do_QueryInterface(docShell));

  if (!baseWindow)
    return NS_ERROR_FAILURE;

  nsCOMPtr<nsIWidget> widget;
  baseWindow->GetMainWidget(getter_AddRefs(widget));

  if (!widget)
    return NS_ERROR_FAILURE;

  HWND winPtr = (HWND)widget->GetNativeData(NS_NATIVE_WINDOW);
  ComPtr<IUIViewSettingsInterop> uiViewSettingsInterop;

  HRESULT hr = GetActivationFactory(
      HStringReference(RuntimeClass_Windows_UI_ViewManagement_UIViewSettings).Get(),
      &uiViewSettingsInterop);
  if (SUCCEEDED(hr)) {
    ComPtr<IUIViewSettings> uiViewSettings;
    hr = uiViewSettingsInterop->GetForWindow(winPtr, IID_PPV_ARGS(&uiViewSettings));
    if (SUCCEEDED(hr)) {
      UserInteractionMode mode;
      hr = uiViewSettings->get_UserInteractionMode(&mode);
      if (SUCCEEDED(hr)) {
        TabletModeState oldTabletModeState = mInTabletMode;
        mInTabletMode = (mode == UserInteractionMode_Touch) ? eTabletModeOn : eTabletModeOff;
        if (mInTabletMode != oldTabletModeState) {
          nsCOMPtr<nsIObserverService> observerService =
            mozilla::services::GetObserverService();
          NS_NAMED_LITERAL_STRING(tabletMode, "tablet-mode");
          NS_NAMED_LITERAL_STRING(normalMode, "normal-mode");
          observerService->NotifyObservers(nullptr, "tablet-mode-change",
            ((mInTabletMode == eTabletModeOn) ? tabletMode.get() : normalMode.get()));
        }
      }
    }
  }
#endif

  return NS_OK;
}

NS_IMETHODIMP
WindowsUIUtils::SetSMTCPlaybackState(bool aPlaying)
{
#ifndef __MINGW32__
  if (!IsWin10OrLater()) {
    return NS_OK;
  }

  nsresult rv = EnsureSMTC();
  if (NS_FAILED(rv)) {
    return NS_OK;
  }
  if (!gSMTC) {
    return NS_OK;
  }

  if (aPlaying) {
    gSMTC->put_IsEnabled(TRUE);
    gSMTC->put_PlaybackStatus(MediaPlaybackStatus_Playing);
  } else {
    gSMTC->put_IsEnabled(TRUE);
    gSMTC->put_PlaybackStatus(MediaPlaybackStatus_Paused);
  }
#endif
  return NS_OK;
}

NS_IMETHODIMP
WindowsUIUtils::SetSMTCMetadata(const nsAString& aTitle,
                                const nsAString& aArtist,
                                const nsAString& aAlbum)
{
#ifndef __MINGW32__
  if (!IsWin10OrLater()) {
    return NS_OK;
  }

  nsresult rv = EnsureSMTC();
  if (NS_FAILED(rv)) {
    return NS_OK;
  }
  if (!gSMTC) {
    return NS_OK;
  }

  ComPtr<ISystemMediaTransportControlsDisplayUpdater> updater;
  HRESULT hr = gSMTC->get_DisplayUpdater(&updater);
  if (FAILED(hr) || !updater) {
    return NS_OK;
  }

  updater->put_Type(MediaPlaybackType_Music);

  ComPtr<IMusicDisplayProperties> music;
  hr = updater->get_MusicProperties(&music);
  if (FAILED(hr) || !music) {
    return NS_OK;
  }

  nsAutoString titleString(aTitle);
  nsAutoString artistString(aArtist);
  nsAutoString albumString(aAlbum);

  HString title;
  HString artist;
  HString album;
  title.Set(titleString.get(), static_cast<UINT32>(titleString.Length()));
  artist.Set(artistString.get(), static_cast<UINT32>(artistString.Length()));
  album.Set(albumString.get(), static_cast<UINT32>(albumString.Length()));

  music->put_Title(title.Get());
  music->put_Artist(artist.Get());
  music->put_AlbumArtist(album.Get());

  updater->Update();
#endif
  return NS_OK;
}

NS_IMETHODIMP
WindowsUIUtils::SetSMTCThumbnailURL(const nsAString& aThumbnailURL)
{
#ifndef __MINGW32__
  if (!IsWin10OrLater()) {
    return NS_OK;
  }

  nsresult rv = EnsureSMTC();
  if (NS_FAILED(rv)) {
    return NS_OK;
  }
  if (!gSMTC || aThumbnailURL.IsEmpty()) {
    return NS_OK;
  }

  ComPtr<ISystemMediaTransportControlsDisplayUpdater> updater;
  HRESULT hr = gSMTC->get_DisplayUpdater(&updater);
  if (FAILED(hr) || !updater) {
    return NS_OK;
  }

  ComPtr<IUriRuntimeClassFactory> uriFactory;
  hr = GetActivationFactory(HStringReference(RuntimeClass_Windows_Foundation_Uri).Get(), &uriFactory);
  if (FAILED(hr) || !uriFactory) {
    return NS_OK;
  }

  nsAutoString urlString(aThumbnailURL);
  HString url;
  url.Set(urlString.get(), static_cast<UINT32>(urlString.Length()));

  ComPtr<IUriRuntimeClass> uri;
  hr = uriFactory->CreateUri(url.Get(), &uri);
  if (FAILED(hr) || !uri) {
    return NS_OK;
  }

  ComPtr<IRandomAccessStreamReferenceStatics> streamRefStatics;
  hr = GetActivationFactory(
      HStringReference(RuntimeClass_Windows_Storage_Streams_RandomAccessStreamReference).Get(),
      &streamRefStatics);
  if (FAILED(hr) || !streamRefStatics) {
    return NS_OK;
  }

  ComPtr<IRandomAccessStreamReference> thumbnail;
  hr = streamRefStatics->CreateFromUri(uri.Get(), &thumbnail);
  if (FAILED(hr) || !thumbnail) {
    return NS_OK;
  }

  updater->put_Thumbnail(thumbnail.Get());
  updater->Update();
#endif
  return NS_OK;
}

NS_IMETHODIMP
WindowsUIUtils::ClearSMTCMetadata()
{
#ifndef __MINGW32__
  if (!IsWin10OrLater()) {
    return NS_OK;
  }

  nsresult rv = EnsureSMTC();
  if (NS_FAILED(rv)) {
    return NS_OK;
  }
  if (!gSMTC) {
    return NS_OK;
  }

  ComPtr<ISystemMediaTransportControlsDisplayUpdater> updater;
  HRESULT hr = gSMTC->get_DisplayUpdater(&updater);
  if (SUCCEEDED(hr) && updater) {
    updater->ClearAll();
    updater->Update();
  }

  gSMTC->put_PlaybackStatus(MediaPlaybackStatus_Stopped);
  gSMTC->put_IsEnabled(FALSE);
#endif
  return NS_OK;
}

