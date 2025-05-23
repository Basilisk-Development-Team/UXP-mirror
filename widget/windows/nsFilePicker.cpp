/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*-
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#include "nsFilePicker.h"

#include <shlobj.h>
#include <shlwapi.h>
#include <cderr.h>

#include "mozilla/mscom/EnsureMTA.h"
#include "mozilla/Preferences.h"
#include "mozilla/UniquePtr.h"
#include "mozilla/WindowsVersion.h"
#include "nsReadableUtils.h"
#include "nsNetUtil.h"
#include "nsWindow.h"
#include "nsILoadContext.h"
#include "nsIServiceManager.h"
#include "nsIURL.h"
#include "nsIStringBundle.h"
#include "nsEnumeratorUtils.h"
#include "nsCRT.h"
#include "nsString.h"
#include "nsToolkit.h"
#include "WinUtils.h"
#include "nsPIDOMWindow.h"
#include "GeckoProfiler.h"

using mozilla::IsWin8OrLater;
using mozilla::IsWin10OrLater;
using mozilla::MakeUnique;
using mozilla::mscom::EnsureMTA;
using mozilla::Preferences;
using mozilla::UniquePtr;
using namespace mozilla::widget;

UniquePtr<char16_t[], nsFilePicker::FreeDeleter>
    nsFilePicker::sLastUsedUnicodeDirectory;

char nsFilePicker::mLastUsedDirectory[MAX_PATH+1] = { 0 };

static const wchar_t kDialogPtrProp[] = L"DialogPtrProperty";
static const DWORD kDialogTimerID = 9999;


#define MAX_EXTENSION_LENGTH 10
#define FILE_BUFFER_SIZE     4096 

typedef DWORD FILEOPENDIALOGOPTIONS;

///////////////////////////////////////////////////////////////////////////////
// Helper classes

// Manages matching SuppressBlurEvents calls on the parent widget.
class AutoSuppressEvents
{
public:
  explicit AutoSuppressEvents(nsIWidget* aWidget) :
    mWindow(static_cast<nsWindow *>(aWidget)) {
    SuppressWidgetEvents(true);
  }

  ~AutoSuppressEvents() {
    SuppressWidgetEvents(false);
  }
private:
  void SuppressWidgetEvents(bool aFlag) {
    if (mWindow) {
      mWindow->SuppressBlurEvents(aFlag);
    }
  }
  RefPtr<nsWindow> mWindow;
};

// Manages NS_NATIVE_TMP_WINDOW child windows. NS_NATIVE_TMP_WINDOWs are
// temporary child windows of mParentWidget created to address RTL issues
// in picker dialogs. We are responsible for destroying these.
class AutoDestroyTmpWindow
{
public:
  explicit AutoDestroyTmpWindow(HWND aTmpWnd) :
    mWnd(aTmpWnd) {
  }

  ~AutoDestroyTmpWindow() {
    if (mWnd)
      DestroyWindow(mWnd);
  }
  
  inline HWND get() const { return mWnd; }
private:
  HWND mWnd;
};

// Manages matching PickerOpen/PickerClosed calls on the parent widget.
class AutoWidgetPickerState
{
public:
  explicit AutoWidgetPickerState(nsIWidget* aWidget) :
    mWindow(static_cast<nsWindow *>(aWidget)) {
    PickerState(true);
  }

  ~AutoWidgetPickerState() {
    PickerState(false);
  }
private:
  void PickerState(bool aFlag) {
    if (mWindow) {
      if (aFlag)
        mWindow->PickerOpen();
      else
        mWindow->PickerClosed();
    }
  }
  RefPtr<nsWindow> mWindow;
};

///////////////////////////////////////////////////////////////////////////////
// nsIFilePicker

nsFilePicker::nsFilePicker()
    : mSelectedType(1) {}

NS_IMPL_ISUPPORTS(nsFilePicker, nsIFilePicker)

NS_IMETHODIMP nsFilePicker::Init(mozIDOMWindowProxy *aParent, const nsAString& aTitle, int16_t aMode,
                                 bool aRequireInteraction)
{
  nsCOMPtr<nsPIDOMWindowOuter> window = do_QueryInterface(aParent);
  nsIDocShell* docShell = window ? window->GetDocShell() : nullptr;  
  mLoadContext = do_QueryInterface(docShell);
  mRequireInteraction = aRequireInteraction;
  
  return nsBaseFilePicker::Init(aParent, aTitle, aMode);
}

/*
 * Folder picker invocation
 */

/*
 * Show a folder picker.
 *
 * @param aInitialDir   The initial directory, the last used directory will be
 *                      used if left blank.
 * @return true if a file was selected successfully.
*/
bool
nsFilePicker::ShowFolderPicker(const nsString& aInitialDir)
{
  if (!IsWin8OrLater()) {
    // Some Windows 7 users are experiencing a race condition when some dlls
    // that are loaded by the file picker cause a crash while attempting to shut
    // down the COM multithreaded apartment. By instantiating EnsureMTA, we hold
    // an additional reference to the MTA that should prevent this race, since
    // the MTA will remain alive until shutdown.
    EnsureMTA ensureMTA;
  }

  RefPtr<IFileOpenDialog> dialog;
  if (FAILED(CoCreateInstance(CLSID_FileOpenDialog,
                              nullptr,
                              CLSCTX_INPROC_SERVER,
                              IID_IFileOpenDialog,
                              getter_AddRefs(dialog)))) {
    return false;
  }

  // options
  FILEOPENDIALOGOPTIONS fos = FOS_PICKFOLDERS;
  // Require interaction if the folder picker is triggered by an element that
  // is potentially unsafe to use the default value in.
  // Win 10+ only, because this dialog flag is broken in earlier versions.
  if (IsWin10OrLater() && mRequireInteraction) {
    fos |= FOS_OKBUTTONNEEDSINTERACTION;
  }

  HRESULT hr = dialog->SetOptions(fos);
  if (FAILED(hr)) {
    return false;
  }
 
  // initial strings
  hr = dialog->SetTitle(mTitle.get());
  if (FAILED(hr)) {
    return false;
  }

  if (!mOkButtonLabel.IsEmpty()) {
    hr = dialog->SetOkButtonLabel(mOkButtonLabel.get());
    if (FAILED(hr)) {
      return false;
    }
  }

  if (!aInitialDir.IsEmpty()) {
    RefPtr<IShellItem> folder;
    if (SUCCEEDED(
          WinUtils::SHCreateItemFromParsingName(aInitialDir.get(), nullptr,
                                                IID_IShellItem,
                                                getter_AddRefs(folder)))) {
      hr = dialog->SetFolder(folder);
      if (FAILED(hr)) {
        return false;
      }
    }
  }

  AutoDestroyTmpWindow adtw((HWND)(mParentWidget.get() ?
    mParentWidget->GetNativeData(NS_NATIVE_TMP_WINDOW) : nullptr));
 
  // display
  RefPtr<IShellItem> item;
  if (FAILED(dialog->Show(adtw.get())) ||
      FAILED(dialog->GetResult(getter_AddRefs(item))) ||
      !item) {
    return false;
  }

  // results

  // If the user chose a Win7 Library, resolve to the library's
  // default save folder.
  RefPtr<IShellItem> folderPath;
  RefPtr<IShellLibrary> shellLib;
  if (FAILED(CoCreateInstance(CLSID_ShellLibrary,
                              nullptr,
                              CLSCTX_INPROC_SERVER,
                              IID_IShellLibrary,
                              getter_AddRefs(shellLib)))) {
    return false;
  }

  if (shellLib &&
      SUCCEEDED(shellLib->LoadLibraryFromItem(item, STGM_READ)) &&
      SUCCEEDED(shellLib->GetDefaultSaveFolder(DSFT_DETECT, IID_IShellItem,
                                               getter_AddRefs(folderPath)))) {
    item.swap(folderPath);
  }

  // get the folder's file system path
  return WinUtils::GetShellItemPath(item, mUnicodeFile);
}

/*
 * File open and save picker invocation
 */

/*
 * Show a file picker.
 *
 * @param aInitialDir   The initial directory, the last used directory will be
 *                      used if left blank.
 * @return true if a file was selected successfully.
*/
bool
nsFilePicker::ShowFilePicker(const nsString& aInitialDir)
{
  PROFILER_LABEL_FUNC(js::ProfileEntry::Category::OTHER);

  if (!IsWin8OrLater()) {
    // Some Windows 7 users are experiencing a race condition when some dlls
    // that are loaded by the file picker cause a crash while attempting to shut
    // down the COM multithreaded apartment. By instantiating EnsureMTA, we hold
    // an additional reference to the MTA that should prevent this race, since
    // the MTA will remain alive until shutdown.
    EnsureMTA ensureMTA;
  }

  RefPtr<IFileDialog> dialog;
  if (mMode != modeSave) {
    if (FAILED(CoCreateInstance(CLSID_FileOpenDialog,
                                nullptr,
                                CLSCTX_INPROC_SERVER,
                                IID_IFileOpenDialog,
                                getter_AddRefs(dialog)))) {
      return false;
    }
  } else {
    if (FAILED(CoCreateInstance(CLSID_FileSaveDialog,
                                nullptr,
                                CLSCTX_INPROC_SERVER,
                                IID_IFileSaveDialog,
                                getter_AddRefs(dialog)))) {
      return false;
    }
  }

  // options

  FILEOPENDIALOGOPTIONS fos = 0;
  fos |= FOS_SHAREAWARE | FOS_OVERWRITEPROMPT |
         FOS_FORCEFILESYSTEM;
  
  // Require interaction if the file picker is triggered by an element that
  // is potentially unsafe to use the default value in.
  // Win 10+ only, because this dialog flag is broken in earlier versions.
  if (IsWin10OrLater() && mRequireInteraction) {
    fos |= FOS_OKBUTTONNEEDSINTERACTION;
  }

  // Handle add to recent docs settings
  if (IsPrivacyModeEnabled() || !mAddToRecentDocs) {
    fos |= FOS_DONTADDTORECENT;
  }

  // mode specification
  switch(mMode) {
    case modeOpenMultiple:
      fos |= FOS_ALLOWMULTISELECT;
      [[fallthrough]];

    case modeOpen: {
      fos |= FOS_FILEMUSTEXIST;
      int32_t followLinks = Preferences::GetInt("widget.windows.follow_shortcuts_on_file_open", 2);
      switch (followLinks) {
        case 1:
          break;
        case 2:
          // No special handling at the moment for `auto`. Fallthrough to `never`:
          [[fallthrough]];
        default:
          fos |= FOS_NODEREFERENCELINKS;
      }
      break;
    }

    case modeSave:
      fos |= FOS_NOREADONLYRETURN;
      // Don't follow shortcuts when saving a shortcut, this can be used
      // to trick users (BZ bug 271732). _Do_ follow shortcuts when not saving a
      // shortcut (BZ bug 283730).
      if (IsDefaultPathLink())
        fos |= FOS_NODEREFERENCELINKS;
      break;
  }

  HRESULT hr = dialog->SetOptions(fos);
  if (FAILED(hr)) {
    return false;
  }

  // initial strings

  // title
  hr = dialog->SetTitle(mTitle.get());
  if (FAILED(hr)) {
    return false;
  }

  // default filename
  if (!mDefaultFilename.IsEmpty()) {
    // Prevent the shell from expanding environment variables by removing
    // the % characters that are used to delimit them.
    nsAutoString sanitizedFilename(mDefaultFilename);
    sanitizedFilename.ReplaceChar('%', '_');

    hr = dialog->SetFileName(sanitizedFilename.get());
    if (FAILED(hr)) {
      return false;
    }
  }
  
  NS_NAMED_LITERAL_STRING(htmExt, "html");

  // default extension to append to new files
  if (!mDefaultExtension.IsEmpty()) {
    // We don't want environment variables expanded in the extension either.
    nsAutoString sanitizedExtension(mDefaultExtension);
    sanitizedExtension.ReplaceChar('%', '_');

    hr = dialog->SetDefaultExtension(sanitizedExtension.get());
    if (FAILED(hr)) {
      return false;
    }
  } else if (IsDefaultPathHtml()) {
    hr = dialog->SetDefaultExtension(htmExt.get());
    if (FAILED(hr)) {
      return false;
    }
  }

  // initial location
  if (!aInitialDir.IsEmpty()) {
    RefPtr<IShellItem> folder;
    if (SUCCEEDED(
          WinUtils::SHCreateItemFromParsingName(aInitialDir.get(), nullptr,
                                                IID_IShellItem,
                                                getter_AddRefs(folder)))) {
      hr = dialog->SetFolder(folder);
      if (FAILED(hr)) {
        return false;
      }
    }
  }

  // filter types and the default index
  if (!mComFilterList.IsEmpty()) {
    hr = dialog->SetFileTypes(mComFilterList.Length(), mComFilterList.get());
    if (FAILED(hr)) {
      return false;
    }

    hr = dialog->SetFileTypeIndex(mSelectedType);
    if (FAILED(hr)) {
      return false;
    }
  }

  // display

  {
    AutoDestroyTmpWindow adtw((HWND)(mParentWidget.get() ?
      mParentWidget->GetNativeData(NS_NATIVE_TMP_WINDOW) : nullptr));
    AutoWidgetPickerState awps(mParentWidget);

    if (FAILED(dialog->Show(adtw.get()))) {
      return false;
    }
  }

  // results

  // Remember what filter type the user selected
  UINT filterIdxResult;
  if (SUCCEEDED(dialog->GetFileTypeIndex(&filterIdxResult))) {
    mSelectedType = (int16_t)filterIdxResult;
  }

  // single selection
  if (mMode != modeOpenMultiple) {
    RefPtr<IShellItem> item;
    if (FAILED(dialog->GetResult(getter_AddRefs(item))) || !item)
      return false;
    return WinUtils::GetShellItemPath(item, mUnicodeFile);
  }

  // multiple selection
  RefPtr<IFileOpenDialog> openDlg;
  dialog->QueryInterface(IID_IFileOpenDialog, getter_AddRefs(openDlg));
  if (!openDlg) {
    // should not happen
    return false;
  }

  RefPtr<IShellItemArray> items;
  if (FAILED(openDlg->GetResults(getter_AddRefs(items))) || !items) {
    return false;
  }

  DWORD count = 0;
  items->GetCount(&count);
  for (unsigned int idx = 0; idx < count; idx++) {
    RefPtr<IShellItem> item;
    nsAutoString str;
    if (SUCCEEDED(items->GetItemAt(idx, getter_AddRefs(item)))) {
      if (!WinUtils::GetShellItemPath(item, str))
        continue;
      nsCOMPtr<nsIFile> file;
      if (NS_SUCCEEDED(NS_NewLocalFile(str, false, getter_AddRefs(file)))) {
        mFiles.AppendObject(file);
      }
    }
  }
  return true;
}

///////////////////////////////////////////////////////////////////////////////
// nsIFilePicker impl.

NS_IMETHODIMP
nsFilePicker::ShowW(int16_t *aReturnVal)
{
  NS_ENSURE_ARG_POINTER(aReturnVal);

  *aReturnVal = returnCancel;

  AutoSuppressEvents supress(mParentWidget);

  nsAutoString initialDir;
  if (mDisplayDirectory)
    mDisplayDirectory->GetPath(initialDir);

  // If no display directory, re-use the last one.
  if(initialDir.IsEmpty()) {
    // Allocate copy of last used dir.
    initialDir = sLastUsedUnicodeDirectory.get();
  }

  // Clear previous file selections
  mUnicodeFile.Truncate();
  mFiles.Clear();

  // On Win10, the picker doesn't support per-monitor DPI, so we open it
  // with our context set temporarily to system-dpi-aware
  WinUtils::AutoSystemDpiAware dpiAwareness;

  bool result = false;
  if (mMode == modeGetFolder) {
    result = ShowFolderPicker(initialDir);
  } else {
    result = ShowFilePicker(initialDir);
  }

  // exit, and return returnCancel in aReturnVal
  if (!result)
    return NS_OK;

  RememberLastUsedDirectory();

  int16_t retValue = returnOK;
  if (mMode == modeSave) {
    // Windows does not return resultReplace, we must check if file
    // already exists.
    nsCOMPtr<nsIFile> file;
    nsresult rv = NS_NewLocalFile(mUnicodeFile, false, getter_AddRefs(file));

    bool flag = false;
    if (NS_SUCCEEDED(rv) && NS_SUCCEEDED(file->Exists(&flag)) && flag) {
      retValue = returnReplace;
    }
  }

  *aReturnVal = retValue;
  return NS_OK;
}

NS_IMETHODIMP
nsFilePicker::Show(int16_t *aReturnVal)
{
  return ShowW(aReturnVal);
}

NS_IMETHODIMP
nsFilePicker::GetFile(nsIFile **aFile)
{
  NS_ENSURE_ARG_POINTER(aFile);
  *aFile = nullptr;

  if (mUnicodeFile.IsEmpty())
      return NS_OK;

  nsCOMPtr<nsIFile> file;
  nsresult rv = NS_NewLocalFile(mUnicodeFile, false, getter_AddRefs(file));
  if (NS_FAILED(rv)) {
    return rv;
  }

  file.forget(aFile);
  return NS_OK;
}

NS_IMETHODIMP
nsFilePicker::GetFileURL(nsIURI **aFileURL)
{
  *aFileURL = nullptr;
  nsCOMPtr<nsIFile> file;
  nsresult rv = GetFile(getter_AddRefs(file));
  if (!file)
    return rv;

  return NS_NewFileURI(aFileURL, file);
}

NS_IMETHODIMP
nsFilePicker::GetFiles(nsISimpleEnumerator **aFiles)
{
  NS_ENSURE_ARG_POINTER(aFiles);
  return NS_NewArrayEnumerator(aFiles, mFiles);
}

// Get the file + path
NS_IMETHODIMP
nsBaseWinFilePicker::SetDefaultString(const nsAString& aString)
{
  mDefaultFilePath = aString;

  // First, make sure the file name is not too long.
  int32_t nameLength;
  int32_t nameIndex = mDefaultFilePath.RFind("\\");
  if (nameIndex == kNotFound)
    nameIndex = 0;
  else
    nameIndex ++;
  nameLength = mDefaultFilePath.Length() - nameIndex;
  mDefaultFilename.Assign(Substring(mDefaultFilePath, nameIndex));
  
  if (nameLength > MAX_PATH) {
    int32_t extIndex = mDefaultFilePath.RFind(".");
    if (extIndex == kNotFound)
      extIndex = mDefaultFilePath.Length();

    // Let's try to shave the needed characters from the name part.
    int32_t charsToRemove = nameLength - MAX_PATH;
    if (extIndex - nameIndex >= charsToRemove) {
      mDefaultFilePath.Cut(extIndex - charsToRemove, charsToRemove);
    }
  }

  // Then, we need to replace illegal characters. At this stage, we cannot
  // replace the backslash as the string might represent a file path.
  mDefaultFilePath.ReplaceChar(FILE_ILLEGAL_CHARACTERS, '-');
  mDefaultFilename.ReplaceChar(FILE_ILLEGAL_CHARACTERS, '-');

  return NS_OK;
}

NS_IMETHODIMP
nsBaseWinFilePicker::GetDefaultString(nsAString& aString)
{
  return NS_ERROR_FAILURE;
}

// The default extension to use for files
NS_IMETHODIMP
nsBaseWinFilePicker::GetDefaultExtension(nsAString& aExtension)
{
  aExtension = mDefaultExtension;
  return NS_OK;
}

NS_IMETHODIMP
nsBaseWinFilePicker::SetDefaultExtension(const nsAString& aExtension)
{
  mDefaultExtension = aExtension;
  return NS_OK;
}

// Set the filter index
NS_IMETHODIMP
nsFilePicker::GetFilterIndex(int32_t *aFilterIndex)
{
  // Windows' filter index is 1-based, we use a 0-based system.
  *aFilterIndex = mSelectedType - 1;
  return NS_OK;
}

NS_IMETHODIMP
nsFilePicker::SetFilterIndex(int32_t aFilterIndex)
{
  // Windows' filter index is 1-based, we use a 0-based system.
  mSelectedType = aFilterIndex + 1;
  return NS_OK;
}

void
nsFilePicker::InitNative(nsIWidget *aParent,
                         const nsAString& aTitle)
{
  mParentWidget = aParent;
  mTitle.Assign(aTitle);
}

NS_IMETHODIMP
nsFilePicker::AppendFilter(const nsAString& aTitle, const nsAString& aFilter)
{
  nsAutoString sanitizedFilter(aFilter);
  sanitizedFilter.ReplaceChar('%', '_');
  mComFilterList.Append(aTitle, sanitizedFilter);
  return NS_OK;
}

void
nsFilePicker::RememberLastUsedDirectory()
{
  if (IsPrivacyModeEnabled()) {
    // Don't remember the directory if private browsing was in effect
    return;
  }

  nsCOMPtr<nsIFile> file;
  if (NS_FAILED(NS_NewLocalFile(mUnicodeFile, false, getter_AddRefs(file)))) {
    NS_WARNING("RememberLastUsedDirectory failed to init file path.");
    return;
  }

  nsCOMPtr<nsIFile> dir;
  nsAutoString newDir;
  if (NS_FAILED(file->GetParent(getter_AddRefs(dir))) ||
      !(mDisplayDirectory = do_QueryInterface(dir)) ||
      NS_FAILED(mDisplayDirectory->GetPath(newDir)) ||
      newDir.IsEmpty()) {
    NS_WARNING("RememberLastUsedDirectory failed to get parent directory.");
    return;
  }

  sLastUsedUnicodeDirectory.reset(ToNewUnicode(newDir));
}

bool
nsFilePicker::IsPrivacyModeEnabled()
{
  return mLoadContext && mLoadContext->UsePrivateBrowsing();
}

bool
nsFilePicker::IsDefaultPathLink()
{
  NS_ConvertUTF16toUTF8 ext(mDefaultFilePath);
  ext.Trim(" .", false, true); // watch out for trailing space and dots
  ToLowerCase(ext);
  if (StringEndsWith(ext, NS_LITERAL_CSTRING(".lnk")) ||
      StringEndsWith(ext, NS_LITERAL_CSTRING(".pif")) ||
      StringEndsWith(ext, NS_LITERAL_CSTRING(".url")))
    return true;
  return false;
}

bool
nsFilePicker::IsDefaultPathHtml()
{
  int32_t extIndex = mDefaultFilePath.RFind(".");
  if (extIndex >= 0) {
    nsAutoString ext;
    mDefaultFilePath.Right(ext, mDefaultFilePath.Length() - extIndex);
    if (ext.LowerCaseEqualsLiteral(".htm")  ||
        ext.LowerCaseEqualsLiteral(".html") ||
        ext.LowerCaseEqualsLiteral(".shtml"))
      return true;
  }
  return false;
}

void
nsFilePicker::ComDlgFilterSpec::Append(const nsAString& aTitle, const nsAString& aFilter)
{
  COMDLG_FILTERSPEC* pSpecForward = mSpecList.AppendElement();
  if (!pSpecForward) {
    NS_WARNING("mSpecList realloc failed.");
    return;
  }
  memset(pSpecForward, 0, sizeof(*pSpecForward));
  nsString* pStr = mStrings.AppendElement(aTitle);
  if (!pStr) {
    NS_WARNING("mStrings.AppendElement failed.");
    return;
  }
  pSpecForward->pszName = pStr->get();
  pStr = mStrings.AppendElement(aFilter);
  if (!pStr) {
    NS_WARNING("mStrings.AppendElement failed.");
    return;
  }
  if (aFilter.EqualsLiteral("..apps"))
    pStr->AssignLiteral("*.exe;*.com");
  else {
    pStr->StripWhitespace();
    if (pStr->EqualsLiteral("*"))
      pStr->AppendLiteral(".*");
  }
  pSpecForward->pszSpec = pStr->get();
}
