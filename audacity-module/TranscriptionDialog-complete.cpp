// Continuation of TranscriptionDialog.cpp
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
   mLanguageChoice->Append(wxT("Auto-detect"), (void*)nullptr);
   mLanguageChoice->Append(wxT("English"), (void*)nullptr);
   mLanguageChoice->Append(wxT("Spanish"), (void*)nullptr);
   mLanguageChoice->Append(wxT("French"), (void*)nullptr);
   mLanguageChoice->Append(wxT("German"), (void*)nullptr);
   mLanguageChoice->Append(wxT("Italian"), (void*)nullptr);
   mLanguageChoice->Append(wxT("Portuguese"), (void*)nullptr);
   mLanguageChoice->Append(wxT("Chinese"), (void*)nullptr);
   mLanguageChoice->Append(wxT("Japanese"), (void*)nullptr);
   mLanguageChoice->Append(wxT("Korean"), (void*)nullptr);
   
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
