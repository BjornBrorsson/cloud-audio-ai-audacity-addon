#include "TtsDialog.h"
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/valnum.h>
#include <wx/filedlg.h>

wxBEGIN_EVENT_TABLE(TtsDialog, wxDialog)
   EVT_BUTTON(wxID_ANY, TtsDialog::OnVoiceBrowse)
   EVT_BUTTON(wxID_PREVIEW, TtsDialog::OnPreview)
   EVT_BUTTON(wxID_OK, TtsDialog::OnGenerate)
   EVT_BUTTON(wxID_CANCEL, TtsDialog::OnCancel)
   EVT_TEXT(wxID_ANY, TtsDialog::OnTextChange)
wxEND_EVENT_TABLE()

TtsDialog::TtsDialog(wxWindow *parent, PythonBridge *bridge)
   : wxDialog(parent, wxID_ANY, wxT("AI Text-to-Speech Generator"),
              wxDefaultPosition, wxSize(600, 500),
              wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
   , mBridge(bridge)
   , mStability(0.5f)
   , mSimilarity(0.75f)
{
   CreateControls();
   
   // Load available voices
   mVoices = mBridge->GetAvailableVoices();
   for (const auto &voice : mVoices)
   {
      mVoiceChoice->Append(voice.name);
   }
   
   if (!mVoices.empty())
   {
      mVoiceChoice->SetSelection(0);
      mVoiceId = mVoices[0].id;
   }
   
   UpdateCharacterCount();
}

void TtsDialog::CreateControls()
{
   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);
   
   // Text input section
   wxStaticBoxSizer *textBox = new wxStaticBoxSizer(wxVERTICAL, this, wxT("Text to Speak"));
   
   mTextCtrl = new wxTextCtrl(this, wxID_ANY, wxEmptyString,
                              wxDefaultPosition, wxSize(-1, 150),
                              wxTE_MULTILINE | wxTE_WORDWRAP);
   mTextCtrl->SetHint(wxT("Enter the text you want to convert to speech..."));
   textBox->Add(mTextCtrl, 1, wxEXPAND | wxALL, 5);
   
   mCharCountLabel = new wxStaticText(this, wxID_ANY, wxT("Characters: 0 / 5000"));
   textBox->Add(mCharCountLabel, 0, wxALIGN_RIGHT | wxALL, 5);
   
   mainSizer->Add(textBox, 1, wxEXPAND | wxALL, 10);
   
   // Voice selection section
   wxStaticBoxSizer *voiceBox = new wxStaticBoxSizer(wxHORIZONTAL, this, wxT("Voice Selection"));
   
   wxStaticText *voiceLabel = new wxStaticText(this, wxID_ANY, wxT("Voice:"));
   voiceBox->Add(voiceLabel, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
   
   mVoiceChoice = new wxChoice(this, wxID_ANY);
   voiceBox->Add(mVoiceChoice, 1, wxEXPAND | wxALL, 5);
   
   mBrowseButton = new wxButton(this, wxID_ANY, wxT("Browse Voices..."));
   voiceBox->Add(mBrowseButton, 0, wxALL, 5);
   
   mainSizer->Add(voiceBox, 0, wxEXPAND | wxLEFT | wxRIGHT | wxBOTTOM, 10);
   
   // Voice settings section
   wxStaticBoxSizer *settingsBox = new wxStaticBoxSizer(wxVERTICAL, this, wxT("Voice Settings"));
   
   // Stability slider
   wxBoxSizer *stabilitySizer = new wxBoxSizer(wxHORIZONTAL);
   wxStaticText *stabilityLabel = new wxStaticText(this, wxID_ANY, wxT("Stability:"));
   stabilitySizer->Add(stabilityLabel, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
   
   mStabilitySlider = new wxSlider(this, wxID_ANY, 50, 0, 100,
                                   wxDefaultPosition, wxDefaultSize,
                                   wxSL_HORIZONTAL | wxSL_LABELS);
   mStabilitySlider->SetToolTip(wxT("Higher = more consistent, Lower = more expressive"));
   stabilitySizer->Add(mStabilitySlider, 1, wxEXPAND | wxALL, 5);
   
   settingsBox->Add(stabilitySizer, 0, wxEXPAND);
   
   // Similarity slider
   wxBoxSizer *similaritySizer = new wxBoxSizer(wxHORIZONTAL);
   wxStaticText *similarityLabel = new wxStaticText(this, wxID_ANY, wxT("Similarity:"));
   similaritySizer->Add(similarityLabel, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
   
   mSimilaritySlider = new wxSlider(this, wxID_ANY, 75, 0, 100,
                                    wxDefaultPosition, wxDefaultSize,
                                    wxSL_HORIZONTAL | wxSL_LABELS);
   mSimilaritySlider->SetToolTip(wxT("Higher = closer to original voice"));
   similaritySizer->Add(mSimilaritySlider, 1, wxEXPAND | wxALL, 5);
   
   settingsBox->Add(similaritySizer, 0, wxEXPAND);
   
   mainSizer->Add(settingsBox, 0, wxEXPAND | wxLEFT | wxRIGHT | wxBOTTOM, 10);
   
   // Progress bar
   mProgressBar = new wxGauge(this, wxID_ANY, 100);
   mProgressBar->SetValue(0);
   mProgressBar->Show(false);
   mainSizer->Add(mProgressBar, 0, wxEXPAND | wxLEFT | wxRIGHT | wxBOTTOM, 10);
   
   // Buttons
   wxBoxSizer *buttonSizer = new wxBoxSizer(wxHORIZONTAL);
   
   mPreviewButton = new wxButton(this, wxID_PREVIEW, wxT("Preview"));
   mPreviewButton->SetToolTip(wxT("Generate a short preview"));
   buttonSizer->Add(mPreviewButton, 0, wxALL, 5);
   
   buttonSizer->AddStretchSpacer();
   
   mGenerateButton = new wxButton(this, wxID_OK, wxT("Generate"));
   mGenerateButton->SetDefault();
   buttonSizer->Add(mGenerateButton, 0, wxALL, 5);
   
   mCancelButton = new wxButton(this, wxID_CANCEL, wxT("Cancel"));
   buttonSizer->Add(mCancelButton, 0, wxALL, 5);
   
   mainSizer->Add(buttonSizer, 0, wxEXPAND | wxALL, 10);
   
   SetSizer(mainSizer);
   Centre();
}

void TtsDialog::OnTextChange(wxCommandEvent &evt)
{
   UpdateCharacterCount();
}

void TtsDialog::UpdateCharacterCount()
{
   mText = mTextCtrl->GetValue();
   int charCount = mText.Length();
   
   mCharCountLabel->SetLabelText(
      wxString::Format(wxT("Characters: %d / 5000"), charCount)
   );
   
   // Enable/disable generate based on text length
   bool valid = charCount > 0 && charCount <= 5000;
   mGenerateButton->Enable(valid);
   mPreviewButton->Enable(valid);
   
   if (charCount > 5000)
   {
      mCharCountLabel->SetForegroundColour(*wxRED);
   }
   else
   {
      mCharCountLabel->SetForegroundColour(wxSystemSettings::GetColour(wxSYS_COLOUR_WINDOWTEXT));
   }
}

void TtsDialog::OnVoiceBrowse(wxCommandEvent &evt)
{
   // TODO: Open voice browser dialog
   wxMessageBox(wxT("Voice browser coming soon!"), wxT("Info"), wxOK | wxICON_INFORMATION);
}

void TtsDialog::OnPreview(wxCommandEvent &evt)
{
   // Generate preview (first 100 characters)
   wxString previewText = mText.Left(100);
   if (mText.Length() > 100)
      previewText += wxT("...");
   
   wxMessageBox(
      wxString::Format(wxT("Preview generation:\n\n%s"), previewText),
      wxT("Preview"),
      wxOK | wxICON_INFORMATION
   );
   
   // TODO: Actual preview implementation
}

void TtsDialog::OnGenerate(wxCommandEvent &evt)
{
   // Validate inputs
   if (mText.IsEmpty())
   {
      wxMessageBox(wxT("Please enter some text to convert."),
                   wxT("Error"), wxOK | wxICON_ERROR);
      return;
   }
   
   if (mVoiceChoice->GetSelection() == wxNOT_FOUND)
   {
      wxMessageBox(wxT("Please select a voice."),
                   wxT("Error"), wxOK | wxICON_ERROR);
      return;
   }
   
   // Get values
   int voiceIdx = mVoiceChoice->GetSelection();
   mVoiceId = mVoices[voiceIdx].id;
   mStability = mStabilitySlider->GetValue() / 100.0f;
   mSimilarity = mSimilaritySlider->GetValue() / 100.0f;
   
   // Generate temp output path
   wxFileName tempFile;
   tempFile.AssignTempFileName(wxT("audacity_tts"));
   tempFile.SetExt(wxT("wav"));
   mOutputPath = tempFile.GetFullPath();
   
   // Show progress
   mProgressBar->Show();
   mProgressBar->SetValue(0);
   mGenerateButton->Enable(false);
   mCancelButton->Enable(false);
   Layout();
   
   // Call Python backend
   PythonBridge::TtsParams params;
   params.text = mText;
   params.voice = mVoiceId;
   params.outputPath = mOutputPath;
   params.stability = mStability;
   params.similarity = mSimilarity;
   
   bool success = mBridge->GenerateTTS(params, [this](int progress) {
      mProgressBar->SetValue(progress);
      wxYield(); // Keep UI responsive
   });
   
   mProgressBar->Hide();
   mGenerateButton->Enable(true);
   mCancelButton->Enable(true);
   
   if (success)
   {
      EndModal(wxID_OK);
   }
   else
   {
      wxMessageBox(wxT("Failed to generate speech. Check log for details."),
                   wxT("Error"), wxOK | wxICON_ERROR);
   }
}

void TtsDialog::OnCancel(wxCommandEvent &evt)
{
   EndModal(wxID_CANCEL);
}
