#ifndef CLOUD_AI_EFFECT_H
#define CLOUD_AI_EFFECT_H

#include "Effect.h"
#include "PythonBridge.h"
#include <wx/string.h>
#include <memory>

// Base class for all Cloud AI effects
class CloudAIEffect : public Effect
{
public:
   CloudAIEffect();
   virtual ~CloudAIEffect();

   // Effect identification
   ComponentInterfaceSymbol GetSymbol() const override = 0;
   VendorSymbol GetVendor() const override;
   wxString GetPath() const override;
   
   EffectType GetType() const override;
   EffectFamilySymbol GetFamily() const override;

protected:
   std::unique_ptr<PythonBridge> mPythonBridge;
   
   bool CheckPythonBackend();
   wxString GetPythonExecutable() const;
};

// Text-to-Speech Effect
class TextToSpeechEffect final : public CloudAIEffect
{
public:
   static const ComponentInterfaceSymbol Symbol;
   
   TextToSpeechEffect();
   
   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;
   
   bool ShowInterface(wxWindow &parent, bool forceModal = false) override;
   
private:
   bool Process(EffectInstance &instance, EffectSettings &settings) override;
};

// Music Generation Effect
class MusicGenerationEffect final : public CloudAIEffect
{
public:
   static const ComponentInterfaceSymbol Symbol;
   
   MusicGenerationEffect();
   
   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;
   
   bool ShowInterface(wxWindow &parent, bool forceModal = false) override;
   
private:
   bool Process(EffectInstance &instance, EffectSettings &settings) override;
};

// Sound Effects Generation Effect
class SoundEffectsEffect final : public CloudAIEffect
{
public:
   static const ComponentInterfaceSymbol Symbol;
   
   SoundEffectsEffect();
   
   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;
   
   bool ShowInterface(wxWindow &parent, bool forceModal = false) override;
   
private:
   bool Process(EffectInstance &instance, EffectSettings &settings) override;
};

// Voice Isolation Effect
class VoiceIsolationEffect final : public CloudAIEffect
{
public:
   static const ComponentInterfaceSymbol Symbol;
   
   VoiceIsolationEffect();
   
   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;
   EffectType GetType() const override;
   
   bool ShowInterface(wxWindow &parent, bool forceModal = false) override;
   
private:
   bool Process(EffectInstance &instance, EffectSettings &settings) override;
};

// Audio Transcription Analyzer
class TranscriptionEffect final : public CloudAIEffect
{
public:
   static const ComponentInterfaceSymbol Symbol;
   
   TranscriptionEffect();
   
   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;
   EffectType GetType() const override;
   
   bool ShowInterface(wxWindow &parent, bool forceModal = false) override;
   
private:
   bool Process(EffectInstance &instance, EffectSettings &settings) override;
};

#endif // CLOUD_AI_EFFECT_H
