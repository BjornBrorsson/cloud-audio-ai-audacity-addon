#ifndef TRANSCRIPTION_DIALOG_H
#define TRANSCRIPTION_DIALOG_H

#include <wx/dialog.h>
#include <wx/choice.h>
#include <wx/textctrl.h>
#include <wx/button.h>
#include <wx/gauge.h>
#include "PythonBridge.h"

class TranscriptionDialog : public wxDialog
{
public:
   TranscriptionDialog(wxWindow *parent, PythonBridge *bridge);
   
   wxString GetInputPath() const { return mInputPath; }
   wxString GetLanguage() const { return mLanguage; }
   wxString GetTranscriptionText() const { return mTranscriptionText; }
   
private:
   void CreateControls();
   void OnTranscribe(wxCommandEvent &evt);
   void OnCancel(wxCommandEvent &evt);
   void LoadLanguages();
   
   wxChoice *mLanguageChoice;
   wxTextCtrl *mResultCtrl;
   wxGauge *mProgressBar;
   wxButton *mTranscribeButton;
   wxButton *mExportButton;
   wxButton *mCancelButton;
   
   PythonBridge *mBridge;
   wxString mInputPath;
   wxString mLanguage;
   wxString mTranscriptionText;
   
   wxDECLARE_EVENT_TABLE();
};

#endif // TRANSCRIPTION_DIALOG_H
