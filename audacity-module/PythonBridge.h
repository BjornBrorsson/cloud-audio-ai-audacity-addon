#ifndef PYTHON_BRIDGE_H
#define PYTHON_BRIDGE_H

#include <wx/string.h>
#include <wx/process.h>
#include <memory>
#include <functional>

// Bridge between C++ module and Python backend
class PythonBridge
{
public:
   PythonBridge();
   ~PythonBridge();

   // Check if Python backend is available
   bool IsAvailable() const;
   
   // Get Python executable path
   wxString GetPythonPath() const;
   
   // Execute Python command and get output
   bool Execute(const wxString &command, wxString &output, wxString &error);
   
   // Text-to-Speech
   struct TtsParams {
      wxString text;
      wxString voice;
      wxString outputPath;
      float stability = 0.5f;
      float similarity = 0.75f;
   };
   bool GenerateTTS(const TtsParams &params, 
                    std::function<void(int)> progressCallback = nullptr);
   
   // Music Generation
   struct MusicParams {
      wxString prompt;
      int duration = 30;
      wxString outputPath;
   };
   bool GenerateMusic(const MusicParams &params,
                      std::function<void(int)> progressCallback = nullptr);
   
   // Sound Effects
   struct SfxParams {
      wxString prompt;
      int duration = 5;
      wxString outputPath;
   };
   bool GenerateSoundEffect(const SfxParams &params,
                           std::function<void(int)> progressCallback = nullptr);
   
   // Voice Isolation
   struct IsolationParams {
      wxString inputPath;
      wxString outputPath;
   };
   bool IsolateVoice(const IsolationParams &params,
                    std::function<void(int)> progressCallback = nullptr);
   
   // Transcription
   struct TranscriptionParams {
      wxString inputPath;
      wxString language;
   };
   struct TranscriptionResult {
      wxString text;
      float confidence;
      std::vector<std::pair<float, float>> timestamps; // start, end
   };
   bool Transcribe(const TranscriptionParams &params,
                  TranscriptionResult &result,
                  std::function<void(int)> progressCallback = nullptr);
   
   // Get available voices
   struct Voice {
      wxString id;
      wxString name;
      wxString category;
      wxString description;
   };
   std::vector<Voice> GetAvailableVoices();
   
   // Test API connection
   bool TestConnection(wxString &message);

private:
   wxString mPythonPath;
   wxString mScriptPath;
   
   bool FindPythonExecutable();
   bool FindScriptPath();
   
   wxString BuildCommand(const wxString &action, const wxString &args) const;
   bool ExecuteAsync(const wxString &command,
                    std::function<void(int)> progressCallback);
};

#endif // PYTHON_BRIDGE_H
