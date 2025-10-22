#ifndef SETTINGS_DIALOG_H
#define SETTINGS_DIALOG_H

#include <wx/dialog.h>
#include <wx/textctrl.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/spinctrl.h>
#include <wx/button.h>
#include <wx/notebook.h>
#include "PythonBridge.h"

class SettingsDialog : public wxDialog
{
public:
   SettingsDialog(wxWindow *parent, PythonBridge *bridge);
   
   // Get settings
   wxString GetApiKey() const { return mApiKey; }
   wxString GetPythonPath() const { return mPythonPath; }
   bool GetAutoUpdate() const { return mAutoUpdate; }
   bool GetShowNotifications() const { return mShowNotifications; }
   
private:
   void CreateControls();
   void LoadSettings();
   void SaveSettings();
   
   void OnTestConnection(wxCommandEvent &evt);
   void OnBrowsePython(wxCommandEvent &evt);
   void OnSave(wxCommandEvent &evt);
   void OnCancel(wxCommandEvent &evt);
   void OnRestoreDefaults(wxCommandEvent &evt);
   
   // UI Controls
   wxNotebook *mNotebook;
   
   // API Tab
   wxTextCtrl *mApiKeyCtrl;
   wxButton *mTestButton;
   wxStaticText *mConnectionStatus;
   
   // Paths Tab
   wxTextCtrl *mPythonPathCtrl;
   wxButton *mBrowseButton;
   wxTextCtrl *mScriptPathCtrl;
   wxButton *mBrowseScriptButton;
   
   // Defaults Tab
   wxChoice *mDefaultVoiceChoice;
   wxSpinCtrlDouble *mDefaultStability;
   wxSpinCtrlDouble *mDefaultSimilarity;
   wxSpinCtrl *mDefaultMusicDuration;
   wxSpinCtrl *mDefaultSfxDuration;
   
   // General Tab
   wxCheckBox *mAutoUpdateCheck;
   wxCheckBox *mShowNotificationsCheck;
   wxCheckBox *mShowProgressCheck;
   wxCheckBox *mKeepTempFilesCheck;
   wxChoice *mOutputFormatChoice;
   
   // Data
   PythonBridge *mBridge;
   wxString mApiKey;
   wxString mPythonPath;
   wxString mScriptPath;
   bool mAutoUpdate;
   bool mShowNotifications;
   
   wxDECLARE_EVENT_TABLE();
};

#endif // SETTINGS_DIALOG_H
