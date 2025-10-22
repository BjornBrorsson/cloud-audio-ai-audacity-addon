#ifndef TTS_DIALOG_H
#define TTS_DIALOG_H

#include <wx/dialog.h>
#include <wx/textctrl.h>
#include <wx/choice.h>
#include <wx/button.h>
#include <wx/slider.h>
#include <wx/stattext.h>
#include <wx/gauge.h>
#include "PythonBridge.h"

class TtsDialog : public wxDialog
{
public:
   TtsDialog(wxWindow *parent, PythonBridge *bridge);
   
   // Get user inputs
   wxString GetText() const { return mText; }
   wxString GetVoice() const { return mVoiceId; }
   float GetStability() const { return mStability; }
   float GetSimilarity() const { return mSimilarity; }
   wxString GetOutputPath() const { return mOutputPath; }
   
private:
   void CreateControls();
   void OnVoiceBrowse(wxCommandEvent &evt);
   void OnPreview(wxCommandEvent &evt);
   void OnGenerate(wxCommandEvent &evt);
   void OnCancel(wxCommandEvent &evt);
   void OnTextChange(wxCommandEvent &evt);
   void UpdateCharacterCount();
   
   // UI Controls
   wxTextCtrl *mTextCtrl;
   wxChoice *mVoiceChoice;
   wxButton *mBrowseButton;
   wxSlider *mStabilitySlider;
   wxSlider *mSimilaritySlider;
   wxStaticText *mCharCountLabel;
   wxGauge *mProgressBar;
   wxButton *mPreviewButton;
   wxButton *mGenerateButton;
   wxButton *mCancelButton;
   
   // Data
   PythonBridge *mBridge;
   wxString mText;
   wxString mVoiceId;
   float mStability;
   float mSimilarity;
   wxString mOutputPath;
   std::vector<PythonBridge::Voice> mVoices;
   
   wxDECLARE_EVENT_TABLE();
};

#endif // TTS_DIALOG_H
