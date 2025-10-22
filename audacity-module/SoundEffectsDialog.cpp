#include "SoundEffectsDialog.h"
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/filename.h>

wxBEGIN_EVENT_TABLE(SoundEffectsDialog, wxDialog)
   EVT_LISTBOX(wxID_ANY, SoundEffectsDialog::OnExampleSelect)
   EVT_BUTTON(wxID_OK, SoundEffectsDialog::OnGenerate)
   EVT_BUTTON(wxID_CANCEL, SoundEffectsDialog::OnCancel)
wxEND_EVENT_TABLE()

SoundEffectsDialog::SoundEffectsDialog(wxWindow *parent, PythonBridge *bridge)
   : wxDialog(parent, wxID_ANY, wxT("AI Sound Effects Generator"),
              wxDefaultPosition, wxSize(650, 500),
              wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
   , mBridge(bridge)
   , mDuration(5)
{
   CreateControls();
   LoadExamples();
}

void SoundEffectsDialog::CreateControls()
{
   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);
   
   // Info text
   wxStaticText *infoText = new wxStaticText(this, wxID_ANY,
      wxT("Describe the sound effect you want to generate. Be specific and detailed."));
   infoText->Wrap(600);
   mainSizer->Add(infoText, 0, wxALL, 10);
   
   // Prompt section
   wxStaticBoxSizer *promptBox = new wxStaticBoxSizer(wxVERTICAL, this, wxT("Sound Description"));
   
   mPromptCtrl = new wxTextCtrl(this, wxID_ANY, wxEmptyString,
                                wxDefaultPosition, wxSize(-1, 80),
                                wxTE_MULTILINE | wxTE_WORDWRAP);
   mPromptCtrl->SetHint(wxT("e.g., Thunder and lightning during a storm"));
   promptBox->Add(mPromptCtrl, 1, wxEXPAND | wxALL, 5);
   
   mainSizer->Add(promptBox, 0, wxEXPAND | wxLEFT | wxRIGHT | wxBOTTOM, 10);
   
   // Examples section
   wxStaticBoxSizer *examplesBox = new wxStaticBoxSizer(wxVERTICAL, this, wxT("Example Prompts"));
   
   mExamplesList = new wxListBox(this, wxID_ANY);
   examplesBox->Add(mExamplesList, 1, wxEXPAND | wxALL, 5);
   
   wxStaticText *exampleHint = new wxStaticText(this, wxID_ANY,
      wxT("Double-click an example to use it"));
   examplesBox->Add(exampleHint, 0, wxALL, 5);
   
   mainSizer->Add(examplesBox, 1, wxEXPAND | wxLEFT | wxRIGHT | wxBOTTOM, 10);
   
   // Duration section
   wxBoxSizer *durationSizer = new wxBoxSizer(wxHORIZONTAL);
   
   wxStaticText *durationLabel = new wxStaticText(this, wxID_ANY, wxT("Duration:"));
   durationSizer->Add(durationLabel, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
   
   mDurationCtrl = new wxSpinCtrl(this, wxID_ANY, wxEmptyString,
                                  wxDefaultPosition, wxDefaultSize,
                                  wxSP_ARROW_KEYS, 1, 22, 5);
   mDurationCtrl->SetToolTip(wxT("Sound effect length in seconds (1-22)"));
   durationSizer->Add(mDurationCtrl, 0, wxALL, 5);
   
   wxStaticText *secondsLabel = new wxStaticText(this, wxID_ANY, wxT("seconds"));
   durationSizer->Add(secondsLabel, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
   
   mainSizer->Add(durationSizer, 0, wxLEFT | wxRIGHT | wxBOTTOM, 10);
   
   // Progress bar
   mProgressBar = new wxGauge(this, wxID_ANY, 100);
   mProgressBar->SetValue(0);
   mProgressBar->Show(false);
   mainSizer->Add(mProgressBar, 0, wxEXPAND | wxLEFT | wxRIGHT | wxBOTTOM, 10);
   
   // Buttons
   wxBoxSizer *buttonSizer = new wxBoxSizer(wxHORIZONTAL);
   buttonSizer->AddStretchSpacer();
   
   mGenerateButton = new wxButton(this, wxID_OK, wxT("Generate Sound"));
   mGenerateButton->SetDefault();
   mGenerateButton->Enable(false);
   buttonSizer->Add(mGenerateButton, 0, wxALL, 5);
   
   mCancelButton = new wxButton(this, wxID_CANCEL, wxT("Cancel"));
   buttonSizer->Add(mCancelButton, 0, wxALL, 5);
   
   mainSizer->Add(buttonSizer, 0, wxEXPAND | wxALL, 10);
   
   SetSizer(mainSizer);
   Centre();
}

void SoundEffectsDialog::LoadExamples()
{
   // Nature sounds
   mExamples.Add(wxT("Thunder and lightning during a heavy storm"));
   mExamples.Add(wxT("Ocean waves crashing on a rocky shore"));
   mExamples.Add(wxT("Birds chirping in a forest at dawn"));
   mExamples.Add(wxT("Heavy rain falling on a tin roof"));
   mExamples.Add(wxT("Wind blowing through trees"));
   
   // Urban/mechanical sounds
   mExamples.Add(wxT("Busy city street with traffic and car horns"));
   mExamples.Add(wxT("Train passing by on railroad tracks"));
   mExamples.Add(wxT("Old wooden door creaking open slowly"));
   mExamples.Add(wxT("Glass breaking and shattering"));
   mExamples.Add(wxT("Footsteps walking on gravel path"));
   
   // Fantasy/Sci-fi sounds
   mExamples.Add(wxT("Magical spell being cast with sparkles"));
   mExamples.Add(wxT("Spaceship engine powering up"));
   mExamples.Add(wxT("Sword being drawn from metal scabbard"));
   mExamples.Add(wxT("Portal opening with energy swirl"));
   mExamples.Add(wxT("Robot transformation mechanical sounds"));
   
   // UI/Game sounds
   mExamples.Add(wxT("Smooth button click sound"));
   mExamples.Add(wxT("Success notification chime"));
   mExamples.Add(wxT("Error buzzer sound"));
   mExamples.Add(wxT("Coin collection pickup sound"));
   mExamples.Add(wxT("Level up achievement fanfare"));
   
   mExamplesList->Set(mExamples);
}

void SoundEffectsDialog::OnExampleSelect(wxCommandEvent &evt)
{
   int sel = mExamplesList->GetSelection();
   if (sel != wxNOT_FOUND)
   {
      mPromptCtrl->SetValue(mExamples[sel]);
      mGenerateButton->Enable(true);
   }
}

void SoundEffectsDialog::OnGenerate(wxCommandEvent &evt)
{
   mPrompt = mPromptCtrl->GetValue();
   mDuration = mDurationCtrl->GetValue();
   
   if (mPrompt.IsEmpty())
   {
      wxMessageBox(wxT("Please describe the sound effect you want."),
                   wxT("Error"), wxOK | wxICON_ERROR);
      return;
   }
   
   // Generate temp output path
   wxFileName tempFile;
   tempFile.AssignTempFileName(wxT("audacity_sfx"));
   tempFile.SetExt(wxT("wav"));
   mOutputPath = tempFile.GetFullPath();
   
   // Show progress
   mProgressBar->Show();
   mProgressBar->SetValue(0);
   mGenerateButton->Enable(false);
   Layout();
   
   // Call Python backend
   PythonBridge::SfxParams params;
   params.prompt = mPrompt;
   params.duration = mDuration;
   params.outputPath = mOutputPath;
   
   bool success = mBridge->GenerateSoundEffect(params, [this](int progress) {
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
      wxMessageBox(wxT("Failed to generate sound effect. Check log for details."),
                   wxT("Error"), wxOK | wxICON_ERROR);
   }
}

void SoundEffectsDialog::OnCancel(wxCommandEvent &evt)
{
   EndModal(wxID_CANCEL);
}
