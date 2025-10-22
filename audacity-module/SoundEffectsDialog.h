#ifndef SOUND_EFFECTS_DIALOG_H
#define SOUND_EFFECTS_DIALOG_H

#include <wx/dialog.h>
#include <wx/textctrl.h>
#include <wx/spinctrl.h>
#include <wx/button.h>
#include <wx/gauge.h>
#include <wx/listbox.h>
#include "PythonBridge.h"

class SoundEffectsDialog : public wxDialog
{
public:
   SoundEffectsDialog(wxWindow *parent, PythonBridge *bridge);
   
   wxString GetPrompt() const { return mPrompt; }
   int GetDuration() const { return mDuration; }
   wxString GetOutputPath() const { return mOutputPath; }
   
private:
   void CreateControls();
   void OnExampleSelect(wxCommandEvent &evt);
   void OnGenerate(wxCommandEvent &evt);
   void OnCancel(wxCommandEvent &evt);
   void LoadExamples();
   
   wxTextCtrl *mPromptCtrl;
   wxListBox *mExamplesList;
   wxSpinCtrl *mDurationCtrl;
   wxGauge *mProgressBar;
   wxButton *mGenerateButton;
   wxButton *mCancelButton;
   
   PythonBridge *mBridge;
   wxString mPrompt;
   int mDuration;
   wxString mOutputPath;
   wxArrayString mExamples;
   
   wxDECLARE_EVENT_TABLE();
};

#endif // SOUND_EFFECTS_DIALOG_H
