#ifndef VOICE_ISOLATION_DIALOG_H
#define VOICE_ISOLATION_DIALOG_H

#include <wx/dialog.h>
#include <wx/button.h>
#include <wx/gauge.h>
#include <wx/stattext.h>
#include "PythonBridge.h"

class VoiceIsolationDialog : public wxDialog
{
public:
   VoiceIsolationDialog(wxWindow *parent, PythonBridge *bridge);
   
   wxString GetInputPath() const { return mInputPath; }
   wxString GetOutputPath() const { return mOutputPath; }
   
private:
   void CreateControls();
   void OnIsolate(wxCommandEvent &evt);
   void OnCancel(wxCommandEvent &evt);
   
   wxStaticText *mInfoLabel;
   wxGauge *mProgressBar;
   wxButton *mIsolateButton;
   wxButton *mCancelButton;
   
   PythonBridge *mBridge;
   wxString mInputPath;
   wxString mOutputPath;
   
   wxDECLARE_EVENT_TABLE();
};

#endif // VOICE_ISOLATION_DIALOG_H
