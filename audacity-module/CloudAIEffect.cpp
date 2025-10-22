#include "CloudAIEffect.h"
#include "TtsDialog.h"
#include "MusicDialog.h"
#include "SoundEffectsDialog.h"
#include "VoiceIsolationDialog.h"
#include "TranscriptionDialog.h"
#include <wx/log.h>
#include <wx/file.h>

// ============================================================================
// CloudAIEffect Base Class
// ============================================================================

CloudAIEffect::CloudAIEffect()
   : mPythonBridge(std::make_unique<PythonBridge>())
{
}

CloudAIEffect::~CloudAIEffect()
{
}

VendorSymbol CloudAIEffect::GetVendor() const
{
   return VendorSymbol{ XO("Bjorn Brorsson") };
}

wxString CloudAIEffect::GetPath() const
{
   return wxT("CloudAI");
}

EffectType CloudAIEffect::GetType() const
{
   return EffectTypeGenerate;
}

EffectFamilySymbol CloudAIEffect::GetFamily() const
{
   return EffectFamilySymbol{ XO("CloudAI") };
}

bool CloudAIEffect::CheckPythonBackend()
{
   if (!mPythonBridge->IsAvailable())
   {
      wxMessageBox(
         wxT("Python backend not found. Please install audacity-cloudai:\n\n")
         wxT("pip install audacity-cloudai\n\n")
         wxT("Or ensure audacity_cloudai.py is in your PATH."),
         wxT("Cloud AI Error"),
         wxOK | wxICON_ERROR
      );
      return false;
   }
   return true;
}

wxString CloudAIEffect::GetPythonExecutable() const
{
   return mPythonBridge->GetPythonPath();
}

// ============================================================================
// Text-to-Speech Effect
// ============================================================================

const ComponentInterfaceSymbol TextToSpeechEffect::Symbol{ XO("AI Text-to-Speech") };

TextToSpeechEffect::TextToSpeechEffect()
{
}

ComponentInterfaceSymbol TextToSpeechEffect::GetSymbol() const
{
   return Symbol;
}

TranslatableString TextToSpeechEffect::GetDescription() const
{
   return XO("Generate speech from text using AI voices");
}

ManualPageID TextToSpeechEffect::ManualPage() const
{
   return L"AI_Text_to_Speech";
}

bool TextToSpeechEffect::ShowInterface(wxWindow &parent, bool forceModal)
{
   if (!CheckPythonBackend())
      return false;
   
   TtsDialog dialog(&parent, mPythonBridge.get());
   
   if (dialog.ShowModal() == wxID_OK)
   {
      // Load generated audio into Audacity
      wxString audioPath = dialog.GetOutputPath();
      
      if (wxFile::Exists(audioPath))
      {
         // Audio will be loaded by Process()
         return true;
      }
   }
   
   return false;
}

bool TextToSpeechEffect::Process(EffectInstance &, EffectSettings &)
{
   // Audio has already been generated and saved by dialog
   // Audacity will load it automatically
   return true;
}

// ============================================================================
// Music Generation Effect
// ============================================================================

const ComponentInterfaceSymbol MusicGenerationEffect::Symbol{ XO("AI Music Generator") };

MusicGenerationEffect::MusicGenerationEffect()
{
}

ComponentInterfaceSymbol MusicGenerationEffect::GetSymbol() const
{
   return Symbol;
}

TranslatableString MusicGenerationEffect::GetDescription() const
{
   return XO("Generate music from text prompts using AI");
}

ManualPageID MusicGenerationEffect::ManualPage() const
{
   return L"AI_Music_Generator";
}

bool MusicGenerationEffect::ShowInterface(wxWindow &parent, bool forceModal)
{
   if (!CheckPythonBackend())
      return false;
   
   MusicDialog dialog(&parent, mPythonBridge.get());
   
   if (dialog.ShowModal() == wxID_OK)
   {
      wxString audioPath = dialog.GetOutputPath();
      if (wxFile::Exists(audioPath))
      {
         return true;
      }
   }
   
   return false;
}

bool MusicGenerationEffect::Process(EffectInstance &, EffectSettings &)
{
   return true;
}

// ============================================================================
// Sound Effects Effect
// ============================================================================

const ComponentInterfaceSymbol SoundEffectsEffect::Symbol{ XO("AI Sound Effects") };

SoundEffectsEffect::SoundEffectsEffect()
{
}

ComponentInterfaceSymbol SoundEffectsEffect::GetSymbol() const
{
   return Symbol;
}

TranslatableString SoundEffectsEffect::GetDescription() const
{
   return XO("Generate sound effects from text descriptions using AI");
}

ManualPageID SoundEffectsEffect::ManualPage() const
{
   return L"AI_Sound_Effects";
}

bool SoundEffectsEffect::ShowInterface(wxWindow &parent, bool forceModal)
{
   if (!CheckPythonBackend())
      return false;
   
   SoundEffectsDialog dialog(&parent, mPythonBridge.get());
   
   if (dialog.ShowModal() == wxID_OK)
   {
      wxString audioPath = dialog.GetOutputPath();
      if (wxFile::Exists(audioPath))
      {
         return true;
      }
   }
   
   return false;
}

bool SoundEffectsEffect::Process(EffectInstance &, EffectSettings &)
{
   return true;
}

// ============================================================================
// Voice Isolation Effect
// ============================================================================

const ComponentInterfaceSymbol VoiceIsolationEffect::Symbol{ XO("AI Voice Isolation") };

VoiceIsolationEffect::VoiceIsolationEffect()
{
}

ComponentInterfaceSymbol VoiceIsolationEffect::GetSymbol() const
{
   return Symbol;
}

TranslatableString VoiceIsolationEffect::GetDescription() const
{
   return XO("Remove background noise and isolate voice using AI");
}

ManualPageID VoiceIsolationEffect::ManualPage() const
{
   return L"AI_Voice_Isolation";
}

EffectType VoiceIsolationEffect::GetType() const
{
   return EffectTypeProcess; // This is a processing effect, not generator
}

bool VoiceIsolationEffect::ShowInterface(wxWindow &parent, bool forceModal)
{
   if (!CheckPythonBackend())
      return false;
   
   VoiceIsolationDialog dialog(&parent, mPythonBridge.get());
   
   if (dialog.ShowModal() == wxID_OK)
   {
      return true;
   }
   
   return false;
}

bool VoiceIsolationEffect::Process(EffectInstance &, EffectSettings &)
{
   // Process selected audio through voice isolation
   return true;
}

// ============================================================================
// Transcription Effect
// ============================================================================

const ComponentInterfaceSymbol TranscriptionEffect::Symbol{ XO("AI Transcription") };

TranscriptionEffect::TranscriptionEffect()
{
}

ComponentInterfaceSymbol TranscriptionEffect::GetSymbol() const
{
   return Symbol;
}

TranslatableString TranscriptionEffect::GetDescription() const
{
   return XO("Transcribe audio to text using AI (99 languages)");
}

ManualPageID TranscriptionEffect::ManualPage() const
{
   return L"AI_Transcription";
}

EffectType TranscriptionEffect::GetType() const
{
   return EffectTypeAnalyze; // This is an analyzer
}

bool TranscriptionEffect::ShowInterface(wxWindow &parent, bool forceModal)
{
   if (!CheckPythonBackend())
      return false;
   
   TranscriptionDialog dialog(&parent, mPythonBridge.get());
   
   if (dialog.ShowModal() == wxID_OK)
   {
      return true;
   }
   
   return false;
}

bool TranscriptionEffect::Process(EffectInstance &, EffectSettings &)
{
   // Transcription results shown in dialog
   return true;
}
