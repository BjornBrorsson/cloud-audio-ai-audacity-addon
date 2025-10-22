#include "AudioPreviewDialog.h"
#include <wx/sizer.h>
#include <wx/statbox.h>

wxBEGIN_EVENT_TABLE(AudioPreviewDialog, wxDialog)
   EVT_BUTTON(wxID_ANY, AudioPreviewDialog::OnPlay)
   EVT_TIMER(wxID_ANY, AudioPreviewDialog::OnTimer)
wxEND_EVENT_TABLE()

AudioPreviewDialog::AudioPreviewDialog(wxWindow *parent, const wxString &audioPath)
   : wxDialog(parent, wxID_ANY, wxT("Audio Preview"),
              wxDefaultPosition, wxSize(500, 300))
   , mAudioPath(audioPath)
   , mUserChoice(RESULT_CANCEL)
   , mTimer(this)
   , mIsPlaying(false)
   , mCurrentTime(0.0)
   , mTotalDuration(10.0)
{
   CreateControls();
}

void AudioPreviewDialog::CreateControls()
{
   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);
   
   // Info
   wxStaticText *info = new wxStaticText(this, wxID_ANY,
      wxT("Preview your generated audio before adding it to the track."));
   info->Wrap(450);
   mainSizer->Add(info, 0, wxALL, 10);
   
   // Playback controls
   wxStaticBoxSizer *playbackBox = new wxStaticBoxSizer(wxVERTICAL, this, wxT("Playback"));
   
   wxBoxSizer *buttonSizer = new wxBoxSizer(wxHORIZONTAL);
   
   mPlayButton = new wxButton(this, wxID_ANY, wxT("▶ Play"));
   mPlayButton->Bind(wxEVT_BUTTON, &AudioPreviewDialog::OnPlay, this);
   buttonSizer->Add(mPlayButton, 0, wxALL, 5);
   
   mStopButton = new wxButton(this, wxID_ANY, wxT("■ Stop"));
   mStopButton->Enable(false);
   mStopButton->Bind(wxEVT_BUTTON, &AudioPreviewDialog::OnStop, this);
   buttonSizer->Add(mStopButton, 0, wxALL, 5);
   
   playbackBox->Add(buttonSizer, 0, wxALIGN_CENTER | wxALL, 5);
   
   // Time display
   wxBoxSizer *timeSizer = new wxBoxSizer(wxHORIZONTAL);
   mTimeLabel = new wxStaticText(this, wxID_ANY, wxT("00:00"));
   timeSizer->Add(mTimeLabel, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
   
   timeSizer->Add(new wxStaticText(this, wxID_ANY, wxT(" / ")), 
                 0, wxALIGN_CENTER_VERTICAL);
   
   mDurationLabel = new wxStaticText(this, wxID_ANY, wxT("00:10"));
   timeSizer->Add(mDurationLabel, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
   
   playbackBox->Add(timeSizer, 0, wxALIGN_CENTER | wxALL, 5);
   
   // Volume control
   wxBoxSizer *volumeSizer = new wxBoxSizer(wxHORIZONTAL);
   volumeSizer->Add(new wxStaticText(this, wxID_ANY, wxT("Volume:")),
                   0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
   
   mVolumeSlider = new wxSlider(this, wxID_ANY, 80, 0, 100,
                                wxDefaultPosition, wxSize(200, -1));
   mVolumeSlider->Bind(wxEVT_SLIDER, &AudioPreviewDialog::OnVolumeChange, this);
   volumeSizer->Add(mVolumeSlider, 1, wxEXPAND | wxALL, 5);
   
   playbackBox->Add(volumeSizer, 0, wxEXPAND | wxALL, 5);
   
   mainSizer->Add(playbackBox, 1, wxEXPAND | wxALL, 10);
   
   // Action buttons
   wxStaticBoxSizer *actionBox = new wxStaticBoxSizer(wxVERTICAL, this, wxT("What would you like to do?"));
   
   wxBoxSizer *actionButtonSizer = new wxBoxSizer(wxHORIZONTAL);
   
   mCommitButton = new wxButton(this, wxID_ANY, wxT("✓ Add to Track"));
   mCommitButton->SetDefault();
   mCommitButton->Bind(wxEVT_BUTTON, &AudioPreviewDialog::OnCommit, this);
   actionButtonSizer->Add(mCommitButton, 1, wxEXPAND | wxALL, 5);
   
   mRegenerateButton = new wxButton(this, wxID_ANY, wxT("↻ Regenerate"));
   mRegenerateButton->Bind(wxEVT_BUTTON, &AudioPreviewDialog::OnRegenerate, this);
   actionButtonSizer->Add(mRegenerateButton, 1, wxEXPAND | wxALL, 5);
   
   mCancelButton = new wxButton(this, wxID_CANCEL, wxT("✗ Cancel"));
   mCancelButton->Bind(wxEVT_BUTTON, &AudioPreviewDialog::OnCancel, this);
   actionButtonSizer->Add(mCancelButton, 1, wxEXPAND | wxALL, 5);
   
   actionBox->Add(actionButtonSizer, 0, wxEXPAND | wxALL, 5);
   
   mainSizer->Add(actionBox, 0, wxEXPAND | wxALL, 10);
   
   SetSizer(mainSizer);
   Centre();
}

void AudioPreviewDialog::OnPlay(wxCommandEvent &evt)
{
   if (!mIsPlaying)
   {
      // TODO: Actually play audio using wxSound or other method
      mIsPlaying = true;
      mPlayButton->SetLabel(wxT("⏸ Pause"));
      mStopButton->Enable(true);
      mTimer.Start(100); // Update every 100ms
      
      wxLogMessage("Playing audio: %s", mAudioPath);
   }
   else
   {
      mIsPlaying = false;
      mPlayButton->SetLabel(wxT("▶ Play"));
      mTimer.Stop();
   }
}

void AudioPreviewDialog::OnStop(wxCommandEvent &evt)
{
   mIsPlaying = false;
   mPlayButton->SetLabel(wxT("▶ Play"));
   mStopButton->Enable(false);
   mTimer.Stop();
   mCurrentTime = 0.0;
   mTimeLabel->SetLabel(wxT("00:00"));
}

void AudioPreviewDialog::OnCommit(wxCommandEvent &evt)
{
   OnStop(evt);
   mUserChoice = RESULT_COMMIT;
   EndModal(wxID_OK);
}

void AudioPreviewDialog::OnRegenerate(wxCommandEvent &evt)
{
   OnStop(evt);
   mUserChoice = RESULT_REGENERATE;
   EndModal(wxID_OK);
}

void AudioPreviewDialog::OnCancel(wxCommandEvent &evt)
{
   OnStop(evt);
   mUserChoice = RESULT_CANCEL;
   EndModal(wxID_CANCEL);
}

void AudioPreviewDialog::OnVolumeChange(wxCommandEvent &evt)
{
   int volume = mVolumeSlider->GetValue();
   // TODO: Update playback volume
   wxLogMessage("Volume: %d%%", volume);
}

void AudioPreviewDialog::OnTimer(wxTimerEvent &evt)
{
   if (mIsPlaying)
   {
      mCurrentTime += 0.1;
      if (mCurrentTime >= mTotalDuration)
      {
         mCurrentTime = 0.0;
         OnStop(wxCommandEvent());
      }
      
      int minutes = (int)mCurrentTime / 60;
      int seconds = (int)mCurrentTime % 60;
      mTimeLabel->SetLabel(wxString::Format(wxT("%02d:%02d"), minutes, seconds));
   }
}
