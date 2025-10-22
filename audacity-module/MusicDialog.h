#ifndef MUSIC_DIALOG_H
#define MUSIC_DIALOG_H

#include <wx/dialog.h>
#include <wx/textctrl.h>
#include <wx/spinctrl.h>
#include <wx/button.h>
#include <wx/gauge.h>
#include <wx/listbox.h>
#include "PythonBridge.h"

class MusicDialog : public wxDialog
{
public:
   MusicDialog(wxWindow *parent, PythonBridge *bridge);
   
   wxString GetPrompt() const { return mPrompt; }
   int GetDuration() const { return mDuration; }
   wxString GetOutputPath() const { return mOutputPath; }
   
private:
   void CreateControls();
   void OnExampleSelect(wxCommandEvent &evt);
   void OnGenerate(wxCommandEvent &evt);
   void OnCancel(wxCommandEvent &evt);
   void OnPromptChange(wxCommandEvent &evt);
   void LoadExamples();
   
   // UI Controls
   wxTextCtrl *mPromptCtrl;
   wxListBox *mExamplesList;
   wxSpinCtrl *mDurationCtrl;
   wxGauge *mProgressBar;
   wxButton *mGenerateButton;
   wxButton *mCancelButton;
   
   // Data
   PythonBridge *mBridge;
   wxString mPrompt;
   int mDuration;
   wxString mOutputPath;
   wxArrayString mExamples;
   
   wxDECLARE_EVENT_TABLE();
};

#endif // MUSIC_DIALOG_H
