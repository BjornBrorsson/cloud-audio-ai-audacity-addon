#include "TranscriptionDialog.h"
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/filename.h>
#include <wx/filedlg.h>
#include <wx/clipbrd.h>

wxBEGIN_EVENT_TABLE(TranscriptionDialog, wxDialog)
   EVT_BUTTON(wxID_OK, TranscriptionDialog::OnTranscribe)
   EVT_BUTTON(wxID_CANCEL, TranscriptionDialog::OnCancel)
wxEND_EVENT_TABLE()

TranscriptionDialog::TranscriptionDialog(wxWindow *parent, PythonBridge *bridge)
   : wxDialog(parent, wxID_ANY, wxT("AI Audio Transcription"),
              wxDefaultPosition, wxSize(700, 550),
              wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
   , mBridge(bridge)
   , mLanguage(wxT("auto"))
{
   CreateControls();
   LoadLanguages();
}

void TranscriptionDialog::CreateControls()
{
   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);
   
   // Info text
   wxStaticText *infoText = new wxStaticText(this, wxID_ANY,
      wxT("Transcribe your selected audio to text using AI. Supports 99 languages with automatic detection."));
   infoText->Wrap(650);
   mainSizer->Add(infoText, 0, wxALL, 10);
   
   // Language selection
   wxStaticBoxSizer *langBox = new wxStaticBoxSizer(wxHORIZONTAL, this, wxT("Language"));
   
   wxStaticText *langLabel = new wxStaticText(this, wxID_ANY, wxT("Spoken language:"));
   langBox->Add(langLabel, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
   
   mLanguageChoice = new wxChoice(this, wxID_ANY);
   langBox->Add(mLanguageChoice, 1, wxEXPAND | wxALL, 5);
   
   mainSizer->Add(langBox, 0, wxEXPAND | wxLEFT | wxRIGHT | wxBOTTOM, 10);
   
   // Progress bar
   mProgressBar = new wxGauge(this, wxID_ANY, 100);
   mProgressBar->SetValue(0);
   mProgressBar->Show(false);
   mainSizer->Add(mProgressBar, 0, wxEXPAND | wxLEFT | wxRIGHT | wxBOTTOM, 10);
   
   // Results section
   wxStaticBoxSizer *resultsBox = new wxStaticBoxSizer(wxVERTICAL, this, wxT("Transcription Result"));
   
   mResultCtrl = new wxTextCtrl(this, wxID_ANY, wxEmptyString,
                                wxDefaultPosition, wxSize(-1, 300),
                                wxTE_MULTILINE | wxTE_WORDWRAP | wxTE_READONLY);
   mResultCtrl->SetHint(wxT("Transcribed text will appear here..."));
   resultsBox->Add(mResultCtrl, 1, wxEXPAND | wxALL, 5);
   
   // Result buttons
   wxBoxSizer *resultButtonSizer = new wxBoxSizer(wxHORIZONTAL);
   
   wxButton *copyButton = new wxButton(this, wxID_COPY, wxT("Copy to Clipboard"));
   copyButton->Bind(wxEVT_BUTTON, [this](wxCommandEvent&) {
      if (wxTheClipboard->Open())
      {
         wxTheClipboard->SetData(new wxTextDataObject(mResultCtrl->GetValue()));
         wxTheClipboard->Close();
         wxMessageBox(wxT("Copied to clipboard!"), wxT("Success"), wxOK | wxICON_INFORMATION);
      }
   });
   resultButtonSizer->Add(copyButton, 0, wxALL, 5);
   
   mExportButton = new wxButton(this, wxID_SAVE, wxT("Export to File..."));
   mExportButton->Bind(wxEVT_BUTTON, [this](wxCommandEvent&) {
      wxFileDialog saveDialog(this, wxT("Save Transcription"),
                             wxEmptyString, wxT("transcription.txt"),
                             wxT("Text files (*.txt)|*.txt|All files (*.*)|*.*"),
                             wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
      
      if (saveDialog.ShowModal() == wxID_OK)
      {
         wxString path = saveDialog.GetPath();
         wxFile file(path, wxFile::write);
         if (file.IsOpened())
         {
            file.Write(mResultCtrl->GetValue());
            file.Close();
            wxMessageBox(wxT("Transcription saved!"), wxT("Success"), wxOK | wxICON_INFORMATION);
         }
      }
   });
   mExportButton->Enable(false);
   resultButtonSizer->Add(mExportButton, 0, wxALL, 5);
   
   resultsBox->Add(resultButtonSizer, 0, wxALIGN_LEFT | wxALL, 5);
   
   mainSizer->Add(resultsBox, 1, wxEXPAND | wxLEFT | wxRIGHT | wxBOTTOM, 10);
   
   // Main buttons
   wxBoxSizer *buttonSizer = new wxBoxSizer(wxHORIZONTAL);
   buttonSizer->AddStretchSpacer();
   
   mTranscribeButton = new wxButton(this, wxID_OK, wxT("Transcribe Audio"));
   mTranscribeButton->SetDefault();
   buttonSizer->Add(mTranscribeButton, 0, wxALL, 5);
   
   mCancelButton = new wxButton(this, wxID_CANCEL, wxT("Close"));
   buttonSizer->Add(mCancelButton, 0, wxALL, 5);
   
   mainSizer->Add(buttonSizer, 0, wxEXPAND | wxALL, 10);
   
   SetSizer(mainSizer);
   Centre();
}

void TranscriptionDialog::LoadLanguages()
{
   mLanguageChoice->Append(wxT("Auto-detect"));
   mLanguageChoice->Append(wxT("English"));
   mLanguageChoice->Append(wxT("Spanish"));
   mLanguageChoice->Append(wxT("French"));
   mLanguageChoice->Append(wxT("German"));
   mLanguageChoice->Append(wxT("Italian"));
   mLanguageChoice->Append(wxT("Portuguese"));
   mLanguageChoice->Append(wxT("Chinese"));
   mLanguageChoice->Append(wxT("Japanese"));
   mLanguageChoice->Append(wxT("Korean"));
   mLanguageChoice->Append(wxT("Russian"));
   mLanguageChoice->Append(wxT("Arabic"));
   mLanguageChoice->Append(wxT("Hindi"));
   
   mLanguageChoice->SetSelection(0);
}

void TranscriptionDialog::OnTranscribe(wxCommandEvent &evt)
{
   int langIdx = mLanguageChoice->GetSelection();
   if (langIdx == 0)
      mLanguage = wxT("auto");
   else
      mLanguage = mLanguageChoice->GetString(langIdx).Lower();
   
   wxFileName inputTemp;
   inputTemp.AssignTempFileName(wxT("audacity_transcribe"));
   inputTemp.SetExt(wxT("wav"));
   mInputPath = inputTemp.GetFullPath();
   
   // Show progress
   mProgressBar->Show();
   mProgressBar->SetValue(0);
   mTranscribeButton->Enable(false);
   Layout();
   
   // Call Python backend
   PythonBridge::TranscriptionParams params;
   params.inputPath = mInputPath;
   params.language = mLanguage;
   
   PythonBridge::TranscriptionResult result;
   bool success = mBridge->Transcribe(params, result, [this](int progress) {
      mProgressBar->SetValue(progress);
      wxYield();
   });
   
   mProgressBar->Hide();
   mTranscribeButton->Enable(true);
   
   if (success)
   {
      mTranscriptionText = result.text;
      mResultCtrl->SetValue(mTranscriptionText);
      mExportButton->Enable(true);
   }
   else
   {
      wxMessageBox(wxT("Failed to transcribe audio. Check log for details."),
                   wxT("Error"), wxOK | wxICON_ERROR);
   }
}

void TranscriptionDialog::OnCancel(wxCommandEvent &evt)
{
   EndModal(wxID_CANCEL);
}
