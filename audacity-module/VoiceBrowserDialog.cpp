#include "VoiceBrowserDialog.h"
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <algorithm>

wxBEGIN_EVENT_TABLE(VoiceBrowserDialog, wxDialog)
   EVT_CHOICE(wxID_ANY, VoiceBrowserDialog::OnCategoryChange)
   EVT_TEXT(wxID_ANY, VoiceBrowserDialog::OnSearchChange)
   EVT_LIST_ITEM_SELECTED(wxID_ANY, VoiceBrowserDialog::OnVoiceSelected)
   EVT_LIST_ITEM_ACTIVATED(wxID_ANY, VoiceBrowserDialog::OnVoiceActivated)
   EVT_BUTTON(wxID_PREVIEW, VoiceBrowserDialog::OnPreviewClick)
   EVT_BUTTON(wxID_OK, VoiceBrowserDialog::OnSelectClick)
   EVT_BUTTON(wxID_CANCEL, VoiceBrowserDialog::OnCancelClick)
wxEND_EVENT_TABLE()

VoiceBrowserDialog::VoiceBrowserDialog(wxWindow *parent, PythonBridge *bridge)
   : wxDialog(parent, wxID_ANY, wxT("Voice Browser"),
              wxDefaultPosition, wxSize(800, 600),
              wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
   , mBridge(bridge)
   , mCurrentCategory(wxT("All"))
{
   CreateControls();
   LoadVoices();
   PopulateList();
}

void VoiceBrowserDialog::CreateControls()
{
   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);
   
   // Filter section
   wxBoxSizer *filterSizer = new wxBoxSizer(wxHORIZONTAL);
   
   // Category filter
   wxStaticText *categoryLabel = new wxStaticText(this, wxID_ANY, wxT("Category:"));
   filterSizer->Add(categoryLabel, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
   
   mCategoryChoice = new wxChoice(this, wxID_ANY);
   mCategoryChoice->Append(wxT("All"));
   mCategoryChoice->Append(wxT("Premade"));
   mCategoryChoice->Append(wxT("Cloned"));
   mCategoryChoice->Append(wxT("Professional"));
   mCategoryChoice->Append(wxT("Custom"));
   mCategoryChoice->SetSelection(0);
   filterSizer->Add(mCategoryChoice, 0, wxALL, 5);
   
   filterSizer->AddSpacer(20);
   
   // Search box
   wxStaticText *searchLabel = new wxStaticText(this, wxID_ANY, wxT("Search:"));
   filterSizer->Add(searchLabel, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
   
   mSearchCtrl = new wxTextCtrl(this, wxID_ANY, wxEmptyString, 
                                wxDefaultPosition, wxSize(200, -1));
   mSearchCtrl->SetHint(wxT("Type to search voices..."));
   filterSizer->Add(mSearchCtrl, 1, wxEXPAND | wxALL, 5);
   
   mainSizer->Add(filterSizer, 0, wxEXPAND | wxALL, 10);
   
   // Voice list
   wxStaticBoxSizer *listBox = new wxStaticBoxSizer(wxVERTICAL, this, wxT("Available Voices"));
   
   mVoiceList = new wxListCtrl(this, wxID_ANY, wxDefaultPosition, wxDefaultSize,
                               wxLC_REPORT | wxLC_SINGLE_SEL | wxBORDER_SUNKEN);
   
   // Add columns
   mVoiceList->AppendColumn(wxT("Name"), wxLIST_FORMAT_LEFT, 200);
   mVoiceList->AppendColumn(wxT("Category"), wxLIST_FORMAT_LEFT, 120);
   mVoiceList->AppendColumn(wxT("Description"), wxLIST_FORMAT_LEFT, 400);
   
   listBox->Add(mVoiceList, 1, wxEXPAND | wxALL, 5);
   
   mainSizer->Add(listBox, 1, wxEXPAND | wxLEFT | wxRIGHT | wxBOTTOM, 10);
   
   // Description section
   wxStaticBoxSizer *descBox = new wxStaticBoxSizer(wxVERTICAL, this, wxT("Voice Details"));
   
   mDescriptionLabel = new wxStaticText(this, wxID_ANY, 
      wxT("Select a voice to see details..."));
   mDescriptionLabel->Wrap(750);
   descBox->Add(mDescriptionLabel, 0, wxALL, 10);
   
   mainSizer->Add(descBox, 0, wxEXPAND | wxLEFT | wxRIGHT | wxBOTTOM, 10);
   
   // Buttons
   wxBoxSizer *buttonSizer = new wxBoxSizer(wxHORIZONTAL);
   
   mPreviewButton = new wxButton(this, wxID_PREVIEW, wxT("Preview Voice"));
   mPreviewButton->Enable(false);
   buttonSizer->Add(mPreviewButton, 0, wxALL, 5);
   
   buttonSizer->AddStretchSpacer();
   
   mSelectButton = new wxButton(this, wxID_OK, wxT("Select Voice"));
   mSelectButton->SetDefault();
   mSelectButton->Enable(false);
   buttonSizer->Add(mSelectButton, 0, wxALL, 5);
   
   mCancelButton = new wxButton(this, wxID_CANCEL, wxT("Cancel"));
   buttonSizer->Add(mCancelButton, 0, wxALL, 5);
   
   mainSizer->Add(buttonSizer, 0, wxEXPAND | wxALL, 10);
   
   SetSizer(mainSizer);
   Centre();
}

void VoiceBrowserDialog::LoadVoices()
{
   mAllVoices = mBridge->GetAvailableVoices();
   mFilteredVoices = mAllVoices;
}

void VoiceBrowserDialog::PopulateList()
{
   mVoiceList->DeleteAllItems();
   
   for (size_t i = 0; i < mFilteredVoices.size(); i++)
   {
      const auto &voice = mFilteredVoices[i];
      
      long index = mVoiceList->InsertItem(i, voice.name);
      mVoiceList->SetItem(index, 1, voice.category);
      mVoiceList->SetItem(index, 2, voice.description);
      mVoiceList->SetItemData(index, i);
   }
   
   // Update count
   wxString title = wxString::Format(wxT("Available Voices (%zu)"), mFilteredVoices.size());
   wxStaticBox *box = wxStaticCast(mVoiceList->GetParent(), wxStaticBoxSizer)->GetStaticBox();
   box->SetLabel(title);
}

void VoiceBrowserDialog::FilterVoices()
{
   mFilteredVoices.clear();
   
   wxString searchTerm = mSearchCtrl->GetValue().Lower();
   
   for (const auto &voice : mAllVoices)
   {
      // Category filter
      if (mCurrentCategory != wxT("All") && voice.category != mCurrentCategory)
         continue;
      
      // Search filter
      if (!searchTerm.IsEmpty())
      {
         wxString name = voice.name.Lower();
         wxString desc = voice.description.Lower();
         
         if (!name.Contains(searchTerm) && !desc.Contains(searchTerm))
            continue;
      }
      
      mFilteredVoices.push_back(voice);
   }
   
   PopulateList();
}

void VoiceBrowserDialog::OnCategoryChange(wxCommandEvent &evt)
{
   mCurrentCategory = mCategoryChoice->GetStringSelection();
   FilterVoices();
}

void VoiceBrowserDialog::OnSearchChange(wxCommandEvent &evt)
{
   FilterVoices();
}

void VoiceBrowserDialog::OnVoiceSelected(wxListEvent &evt)
{
   long index = evt.GetIndex();
   size_t voiceIdx = mVoiceList->GetItemData(index);
   
   if (voiceIdx < mFilteredVoices.size())
   {
      const auto &voice = mFilteredVoices[voiceIdx];
      mSelectedVoiceId = voice.id;
      mSelectedVoiceName = voice.name;
      
      // Update description
      wxString desc = wxString::Format(
         wxT("Name: %s\n")
         wxT("Category: %s\n")
         wxT("ID: %s\n\n")
         wxT("Description: %s"),
         voice.name, voice.category, voice.id, voice.description
      );
      mDescriptionLabel->SetLabelText(desc);
      mDescriptionLabel->Wrap(750);
      
      mPreviewButton->Enable(true);
      mSelectButton->Enable(true);
   }
}

void VoiceBrowserDialog::OnVoiceActivated(wxListEvent &evt)
{
   // Double-click to select
   OnVoiceSelected(evt);
   EndModal(wxID_OK);
}

void VoiceBrowserDialog::OnPreviewClick(wxCommandEvent &evt)
{
   if (mSelectedVoiceId.IsEmpty())
      return;
   
   wxMessageBox(
      wxString::Format(wxT("Preview voice: %s\n\nID: %s"), 
                      mSelectedVoiceName, mSelectedVoiceId),
      wxT("Voice Preview"),
      wxOK | wxICON_INFORMATION
   );
   
   // TODO: Generate and play a short sample
   // "This is a preview of the selected voice."
}

void VoiceBrowserDialog::OnSelectClick(wxCommandEvent &evt)
{
   if (mSelectedVoiceId.IsEmpty())
   {
      wxMessageBox(wxT("Please select a voice first."),
                   wxT("No Selection"), wxOK | wxICON_WARNING);
      return;
   }
   
   EndModal(wxID_OK);
}

void VoiceBrowserDialog::OnCancelClick(wxCommandEvent &evt)
{
   EndModal(wxID_CANCEL);
}
