#include "VoiceIsolationDialog.h"
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/filename.h>

wxBEGIN_EVENT_TABLE(VoiceIsolationDialog, wxDialog)
   EVT_BUTTON(wxID_OK, VoiceIsolationDialog::OnIsolate)
   EVT_BUTTON(wxID_CANCEL, VoiceIsolationDialog::OnCancel)
wxEND_EVENT_TABLE()

VoiceIsolationDialog::VoiceIsolationDialog(wxWindow *parent, PythonBridge *bridge)
   : wxDialog(parent, wxID_ANY, wxT("AI Voice Isolation"),
              wxDefaultPosition, wxSize(500, 300),
              wxDEFAULT_DIALOG_STYLE)
   , mBridge(bridge)
{
   CreateControls();
}

void VoiceIsolationDialog::CreateControls()
{
   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);
   
   // Info section
   wxStaticBoxSizer *infoBox = new wxStaticBoxSizer(wxVERTICAL, this, wxT("Voice Isolation"));
   
   wxStaticText *description = new wxStaticText(this, wxID_ANY,
      wxT("This effect will isolate the voice from your selected audio, removing background noise, music, and other sounds.\n\n")
      wxT("The original audio will be replaced with the isolated voice."));
   description->Wrap(450);
   infoBox->Add(description, 0, wxALL, 10);
   
   mInfoLabel = new wxStaticText(this, wxID_ANY, wxT("Ready to process selected audio."));
   infoBox->Add(mInfoLabel, 0, wxALL, 10);
   
   mainSizer->Add(infoBox, 0, wxEXPAND | wxALL, 10);
   
   // Quality info
   wxStaticBoxSizer *qualityBox = new wxStaticBoxSizer(wxVERTICAL, this, wxT("What to Expect"));
   
   wxString qualityText = 
      wxT("✓ Removes background music\n")
      wxT("✓ Eliminates noise and static\n")
      wxT("✓ Preserves voice quality\n")
      wxT("✓ Works with any language\n\n")
      wxT("Best results with:\n")
      wxT("• Clear speech recordings\n")
      wxT("• Interviews and podcasts\n")
      wxT("• Video dialogue");
   
   wxStaticText *qualityLabel = new wxStaticText(this, wxID_ANY, qualityText);
   qualityBox->Add(qualityLabel, 0, wxALL, 10);
   
   mainSizer->Add(qualityBox, 1, wxEXPAND | wxLEFT | wxRIGHT | wxBOTTOM, 10);
   
   // Progress bar
   mProgressBar = new wxGauge(this, wxID_ANY, 100);
   mProgressBar->SetValue(0);
   mProgressBar->Show(false);
   mainSizer->Add(mProgressBar, 0, wxEXPAND | wxLEFT | wxRIGHT | wxBOTTOM, 10);
   
   // Buttons
   wxBoxSizer *buttonSizer = new wxBoxSizer(wxHORIZONTAL);
   buttonSizer->AddStretchSpacer();
   
   mIsolateButton = new wxButton(this, wxID_OK, wxT("Isolate Voice"));
   mIsolateButton->SetDefault();
   buttonSizer->Add(mIsolateButton, 0, wxALL, 5);
   
   mCancelButton = new wxButton(this, wxID_CANCEL, wxT("Cancel"));
   buttonSizer->Add(mCancelButton, 0, wxALL, 5);
   
   mainSizer->Add(buttonSizer, 0, wxEXPAND | wxALL, 10);
   
   SetSizer(mainSizer);
   Centre();
}

void VoiceIsolationDialog::OnIsolate(wxCommandEvent &evt)
{
   // In a real implementation, we would get the selected audio from Audacity
   // For now, we'll create temp files
   
   wxFileName inputTemp;
   inputTemp.AssignTempFileName(wxT("audacity_input"));
   inputTemp.SetExt(wxT("wav"));
   mInputPath = inputTemp.GetFullPath();
   
   wxFileName outputTemp;
   outputTemp.AssignTempFileName(wxT("audacity_isolated"));
   outputTemp.SetExt(wxT("wav"));
   mOutputPath = outputTemp.GetFullPath();
   
   // TODO: Export selected audio to mInputPath
   // For now, we'll just show the process
   
   // Show progress
   mInfoLabel->SetLabelText(wxT("Processing audio..."));
   mProgressBar->Show();
   mProgressBar->SetValue(0);
   mIsolateButton->Enable(false);
   Layout();
   
   // Call Python backend
   PythonBridge::IsolationParams params;
   params.inputPath = mInputPath;
   params.outputPath = mOutputPath;
   
   bool success = mBridge->IsolateVoice(params, [this](int progress) {
      mProgressBar->SetValue(progress);
      mInfoLabel->SetLabelText(wxString::Format(wxT("Processing... %d%%"), progress));
      wxYield();
   });
   
   mProgressBar->Hide();
   mIsolateButton->Enable(true);
   
   if (success)
   {
      mInfoLabel->SetLabelText(wxT("Voice isolation complete!"));
      EndModal(wxID_OK);
   }
   else
   {
      mInfoLabel->SetLabelText(wxT("Failed to isolate voice."));
      wxMessageBox(wxT("Failed to isolate voice. Check log for details."),
                   wxT("Error"), wxOK | wxICON_ERROR);
   }
}

void VoiceIsolationDialog::OnCancel(wxCommandEvent &evt)
{
   EndModal(wxID_CANCEL);
}
