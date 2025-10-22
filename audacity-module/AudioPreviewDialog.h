#ifndef AUDIO_PREVIEW_DIALOG_H
#define AUDIO_PREVIEW_DIALOG_H

#include <wx/dialog.h>
#include <wx/button.h>
#include <wx/slider.h>
#include <wx/stattext.h>
#include <wx/timer.h>

class AudioPreviewDialog : public wxDialog
{
public:
   AudioPreviewDialog(wxWindow *parent, const wxString &audioPath);
   
   enum Result {
      RESULT_COMMIT,
      RESULT_REGENERATE,
      RESULT_CANCEL
   };
   
   Result GetUserChoice() const { return mUserChoice; }
   
private:
   void CreateControls();
   void OnPlay(wxCommandEvent &evt);
   void OnStop(wxCommandEvent &evt);
   void OnCommit(wxCommandEvent &evt);
   void OnRegenerate(wxCommandEvent &evt);
   void OnCancel(wxCommandEvent &evt);
   void OnVolumeChange(wxCommandEvent &evt);
   void OnTimer(wxTimerEvent &evt);
   
   wxButton *mPlayButton;
   wxButton *mStopButton;
   wxSlider *mVolumeSlider;
   wxStaticText *mTimeLabel;
   wxStaticText *mDurationLabel;
   wxButton *mCommitButton;
   wxButton *mRegenerateButton;
   wxButton *mCancelButton;
   
   wxString mAudioPath;
   Result mUserChoice;
   wxTimer mTimer;
   bool mIsPlaying;
   double mCurrentTime;
   double mTotalDuration;
   
   wxDECLARE_EVENT_TABLE();
};

#endif // AUDIO_PREVIEW_DIALOG_H
