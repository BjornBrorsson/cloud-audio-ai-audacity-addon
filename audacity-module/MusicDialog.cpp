#include "MusicDialog.h"
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/filename.h>

wxBEGIN_EVENT_TABLE(MusicDialog, wxDialog)
   EVT_LISTBOX(wxID_ANY, MusicDialog::OnExampleSelect)
   EVT_BUTTON(wxID_OK, MusicDialog::OnGenerate)
   EVT_BUTTON(wxID_CANCEL, MusicDialog::OnCancel)
   EVT_TEXT(wxID_ANY, MusicDialog::OnPromptChange)
wxEND_EVENT_TABLE()

MusicDialog::MusicDialog(wxWindow *parent, PythonBridge *bridge)
   : wxDialog(parent, wxID_ANY, wxT("AI Music Generator"),
              wxDefaultPosition, wxSize(700, 550),
              wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
   , mBridge(bridge)
   , mDuration(30)
{
   CreateControls();
   LoadExamples();
}

void MusicDialog::CreateControls()
{
   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);
   
   // Info text
   wxStaticText *infoText = new wxStaticText(this, wxID_ANY,
      wxT("Describe the music you want to generate. Be specific about mood, genre, instruments, and style."));
   infoText->Wrap(650);
   mainSizer->Add(infoText, 0, wxALL, 10);
   
   // Prompt section
   wxStaticBoxSizer *promptBox = new wxStaticBoxSizer(wxVERTICAL, this, wxT("Music Description"));
   
   mPromptCtrl = new wxTextCtrl(this, wxID_ANY, wxEmptyString,
                                wxDefaultPosition, wxSize(-1, 100),
                                wxTE_MULTILINE | wxTE_WORDWRAP);
   mPromptCtrl->SetHint(wxT("e.g., Upbeat electronic dance music with synthesizers and a strong beat"));
   promptBox->Add(mPromptCtrl, 1, wxEXPAND | wxALL, 5);
   
   mainSizer->Add(promptBox, 1, wxEXPAND | wxLEFT | wxRIGHT | wxBOTTOM, 10);
   
   // Examples section
   wxStaticBoxSizer *examplesBox = new wxStaticBoxSizer(wxVERTICAL, this, wxT("Example Prompts"));
   
   mExamplesList = new wxListBox(this, wxID_ANY);
   examplesBox->Add(mExamplesList, 1, wxEXPAND | wxALL, 5);
   
   wxStaticText *exampleHint = new wxStaticText(this, wxID_ANY,
      wxT("Click an example to use it as a starting point"));
   examplesBox->Add(exampleHint, 0, wxALL, 5);
   
   mainSizer->Add(examplesBox, 1, wxEXPAND | wxLEFT | wxRIGHT | wxBOTTOM, 10);
   
   // Duration section
   wxStaticBoxSizer *durationBox = new wxStaticBoxSizer(wxHORIZONTAL, this, wxT("Duration"));
   
   wxStaticText *durationLabel = new wxStaticText(this, wxID_ANY, wxT("Seconds:"));
   durationBox->Add(durationLabel, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
   
   mDurationCtrl = new wxSpinCtrl(this, wxID_ANY, wxEmptyString,
                                  wxDefaultPosition, wxDefaultSize,
                                  wxSP_ARROW_KEYS, 5, 180, 30);
   durationBox->Add(mDurationCtrl, 0, wxALL, 5);
   
   wxStaticText *durationHint = new wxStaticText(this, wxID_ANY,
      wxT("(5-180 seconds, longer = more expensive)"));
   durationBox->Add(durationHint, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
   
   mainSizer->Add(durationBox, 0, wxEXPAND | wxLEFT | wxRIGHT | wxBOTTOM, 10);
   
   // Progress bar
   mProgressBar = new wxGauge(this, wxID_ANY, 100);
   mProgressBar->SetValue(0);
   mProgressBar->Show(false);
   mainSizer->Add(mProgressBar, 0, wxEXPAND | wxLEFT | wxRIGHT | wxBOTTOM, 10);
   
   // Buttons
   wxBoxSizer *buttonSizer = new wxBoxSizer(wxHORIZONTAL);
   buttonSizer->AddStretchSpacer();
   
   mGenerateButton = new wxButton(this, wxID_OK, wxT("Generate Music"));
   mGenerateButton->SetDefault();
   mGenerateButton->Enable(false);
   buttonSizer->Add(mGenerateButton, 0, wxALL, 5);
   
   mCancelButton = new wxButton(this, wxID_CANCEL, wxT("Cancel"));
   buttonSizer->Add(mCancelButton, 0, wxALL, 5);
   
   mainSizer->Add(buttonSizer, 0, wxEXPAND | wxALL, 10);
   
   SetSizer(mainSizer);
   Centre();
}

void MusicDialog::LoadExamples()
{
   mExamples.Add(wxT("Upbeat electronic dance music with synthesizers and a strong beat"));
   mExamples.Add(wxT("Calm acoustic guitar melody with gentle piano accompaniment"));
   mExamples.Add(wxT("Epic orchestral music with strings and brass, dramatic and powerful"));
   mExamples.Add(wxT("Jazz trio with saxophone, piano, and upright bass, smooth and sophisticated"));
   mExamples.Add(wxT("Lo-fi hip hop beats for studying, relaxing atmosphere with vinyl crackle"));
   mExamples.Add(wxT("Rock music with electric guitar riffs and driving drums, energetic"));
   mExamples.Add(wxT("Ambient atmospheric soundscape with ethereal pads and subtle textures"));
   mExamples.Add(wxT("Classical piano solo, romantic and emotional, in the style of Chopin"));
   mExamples.Add(wxT("Funk music with slap bass, horns, and rhythm guitar, groovy and upbeat"));
   mExamples.Add(wxT("Celtic folk music with fiddle and acoustic instruments, lively dance tune"));
   
   mExamplesList->Set(mExamples);
}

void MusicDialog::OnExampleSelect(wxCommandEvent &evt)
{
   int sel = mExamplesList->GetSelection();
   if (sel != wxNOT_FOUND)
   {
      mPromptCtrl->SetValue(mExamples[sel]);
   }
}

void MusicDialog::OnPromptChange(wxCommandEvent &evt)
{
   mPrompt = mPromptCtrl->GetValue();
   mGenerateButton->Enable(!mPrompt.IsEmpty());
}

void MusicDialog::OnGenerate(wxCommandEvent &evt)
{
   mPrompt = mPromptCtrl->GetValue();
   mDuration = mDurationCtrl->GetValue();
   
   if (mPrompt.IsEmpty())
   {
      wxMessageBox(wxT("Please describe the music you want to generate."),
                   wxT("Error"), wxOK | wxICON_ERROR);
      return;
   }
   
   // Generate temp output path
   wxFileName tempFile;
   tempFile.AssignTempFileName(wxT("audacity_music"));
   tempFile.SetExt(wxT("wav"));
   mOutputPath = tempFile.GetFullPath();
   
   // Show progress
   mProgressBar->Show();
   mProgressBar->SetValue(0);
   mGenerateButton->Enable(false);
   Layout();
   
   // Call Python backend
   PythonBridge::MusicParams params;
   params.prompt = mPrompt;
   params.duration = mDuration;
   params.outputPath = mOutputPath;
   
   bool success = mBridge->GenerateMusic(params, [this](int progress) {
      mProgressBar->SetValue(progress);
      wxYield();
   });
   
   mProgressBar->Hide();
   mGenerateButton->Enable(true);
   
   if (success)
   {
      EndModal(wxID_OK);
   }
   else
   {
      wxMessageBox(wxT("Failed to generate music. Check log for details."),
                   wxT("Error"), wxOK | wxICON_ERROR);
   }
}

void MusicDialog::OnCancel(wxCommandEvent &evt)
{
   EndModal(wxID_CANCEL);
}
