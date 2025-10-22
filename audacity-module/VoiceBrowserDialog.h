#ifndef VOICE_BROWSER_DIALOG_H
#define VOICE_BROWSER_DIALOG_H

#include <wx/dialog.h>
#include <wx/listctrl.h>
#include <wx/textctrl.h>
#include <wx/choice.h>
#include <wx/button.h>
#include <wx/stattext.h>
#include <wx/panel.h>
#include "PythonBridge.h"

class VoiceBrowserDialog : public wxDialog
{
public:
   VoiceBrowserDialog(wxWindow *parent, PythonBridge *bridge);
   
   wxString GetSelectedVoiceId() const { return mSelectedVoiceId; }
   wxString GetSelectedVoiceName() const { return mSelectedVoiceName; }
   bool HasSelection() const { return !mSelectedVoiceId.IsEmpty(); }
   
private:
   void CreateControls();
   void LoadVoices();
   void PopulateList();
   void FilterVoices();
   
   void OnCategoryChange(wxCommandEvent &evt);
   void OnSearchChange(wxCommandEvent &evt);
   void OnVoiceSelected(wxListEvent &evt);
   void OnVoiceActivated(wxListEvent &evt);
   void OnPreviewClick(wxCommandEvent &evt);
   void OnSelectClick(wxCommandEvent &evt);
   void OnCancelClick(wxCommandEvent &evt);
   
   // UI Controls
   wxChoice *mCategoryChoice;
   wxTextCtrl *mSearchCtrl;
   wxListCtrl *mVoiceList;
   wxStaticText *mDescriptionLabel;
   wxButton *mPreviewButton;
   wxButton *mSelectButton;
   wxButton *mCancelButton;
   
   // Data
   PythonBridge *mBridge;
   std::vector<PythonBridge::Voice> mAllVoices;
   std::vector<PythonBridge::Voice> mFilteredVoices;
   wxString mSelectedVoiceId;
   wxString mSelectedVoiceName;
   wxString mCurrentCategory;
   
   wxDECLARE_EVENT_TABLE();
};

#endif // VOICE_BROWSER_DIALOG_H
