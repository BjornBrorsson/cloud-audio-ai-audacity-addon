#include "PythonBridge.h"
#include <wx/process.h>
#include <wx/txtstrm.h>
#include <wx/filename.h>
#include <wx/stdpaths.h>
#include <wx/log.h>
#include <wx/utils.h>

PythonBridge::PythonBridge()
{
   FindPythonExecutable();
   FindScriptPath();
}

PythonBridge::~PythonBridge()
{
}

bool PythonBridge::IsAvailable() const
{
   return !mPythonPath.IsEmpty() && !mScriptPath.IsEmpty();
}

wxString PythonBridge::GetPythonPath() const
{
   return mPythonPath;
}

bool PythonBridge::FindPythonExecutable()
{
   // Try common Python locations
   wxArrayString candidates = {
      wxT("python"),
      wxT("python3"),
      wxT("python3.11"),
      wxT("python3.10"),
      wxT("python3.9"),
      wxT("python3.8"),
   };
   
#ifdef __WXMSW__
   // Windows-specific paths
   candidates.Add(wxT("C:\\Python311\\python.exe"));
   candidates.Add(wxT("C:\\Python310\\python.exe"));
   candidates.Add(wxT("C:\\Program Files\\Python311\\python.exe"));
   candidates.Add(wxT("C:\\Program Files\\Python310\\python.exe"));
#endif
   
   for (const auto &candidate : candidates)
   {
      wxString output, error;
      if (Execute(candidate + wxT(" --version"), output, error))
      {
         mPythonPath = candidate;
         wxLogMessage("Found Python: %s", candidate);
         return true;
      }
   }
   
   wxLogError("Python executable not found!");
   return false;
}

bool PythonBridge::FindScriptPath()
{
   // Look for audacity_cloudai.py in common locations
   wxArrayString searchPaths = {
      wxStandardPaths::Get().GetExecutablePath(),
      wxGetCwd(),
      wxFileName::GetHomeDir() + wxT("/audacity-cloudai"),
      wxT("/usr/local/share/audacity-cloudai"),
      wxT("C:\\Program Files\\Audacity Cloud AI"),
   };
   
   for (const auto &basePath : searchPaths)
   {
      wxFileName fn(basePath, wxT("audacity_cloudai.py"));
      if (fn.FileExists())
      {
         mScriptPath = fn.GetFullPath();
         wxLogMessage("Found script: %s", mScriptPath);
         return true;
      }
      
      // Try parent directory
      fn.SetPath(wxFileName(basePath).GetPath());
      fn.SetFullName(wxT("audacity_cloudai.py"));
      if (fn.FileExists())
      {
         mScriptPath = fn.GetFullPath();
         wxLogMessage("Found script: %s", mScriptPath);
         return true;
      }
   }
   
   wxLogError("audacity_cloudai.py not found!");
   return false;
}

bool PythonBridge::Execute(const wxString &command, wxString &output, wxString &error)
{
   wxArrayString outputLines;
   wxArrayString errorLines;
   
   long exitCode = wxExecute(command, outputLines, errorLines, wxEXEC_SYNC);
   
   output = wxJoin(outputLines, '\n');
   error = wxJoin(errorLines, '\n');
   
   return exitCode == 0;
}

wxString PythonBridge::BuildCommand(const wxString &action, const wxString &args) const
{
   return wxString::Format(
      wxT("%s \"%s\" %s %s"),
      mPythonPath,
      mScriptPath,
      action,
      args
   );
}

bool PythonBridge::GenerateTTS(const TtsParams &params,
                               std::function<void(int)> progressCallback)
{
   wxString args = wxString::Format(
      wxT("tts --text \"%s\" --voice \"%s\" --output \"%s\" --stability %.2f --similarity %.2f"),
      params.text,
      params.voice,
      params.outputPath,
      params.stability,
      params.similarity
   );
   
   wxString command = BuildCommand(wxT(""), args);
   wxString output, error;
   
   wxLogMessage("Executing: %s", command);
   
   if (progressCallback)
      progressCallback(50);
   
   bool success = Execute(command, output, error);
   
   if (progressCallback)
      progressCallback(100);
   
   if (!success)
   {
      wxLogError("TTS generation failed: %s", error);
   }
   
   return success;
}

bool PythonBridge::GenerateMusic(const MusicParams &params,
                                 std::function<void(int)> progressCallback)
{
   wxString args = wxString::Format(
      wxT("music --prompt \"%s\" --duration %d --output \"%s\""),
      params.prompt,
      params.duration,
      params.outputPath
   );
   
   wxString command = BuildCommand(wxT(""), args);
   wxString output, error;
   
   if (progressCallback)
      progressCallback(50);
   
   bool success = Execute(command, output, error);
   
   if (progressCallback)
      progressCallback(100);
   
   return success;
}

bool PythonBridge::GenerateSoundEffect(const SfxParams &params,
                                       std::function<void(int)> progressCallback)
{
   wxString args = wxString::Format(
      wxT("sfx --prompt \"%s\" --duration %d --output \"%s\""),
      params.prompt,
      params.duration,
      params.outputPath
   );
   
   wxString command = BuildCommand(wxT(""), args);
   wxString output, error;
   
   if (progressCallback)
      progressCallback(50);
   
   bool success = Execute(command, output, error);
   
   if (progressCallback)
      progressCallback(100);
   
   return success;
}

bool PythonBridge::IsolateVoice(const IsolationParams &params,
                                std::function<void(int)> progressCallback)
{
   wxString args = wxString::Format(
      wxT("isolate --input \"%s\" --output \"%s\""),
      params.inputPath,
      params.outputPath
   );
   
   wxString command = BuildCommand(wxT(""), args);
   wxString output, error;
   
   if (progressCallback)
      progressCallback(50);
   
   bool success = Execute(command, output, error);
   
   if (progressCallback)
      progressCallback(100);
   
   return success;
}

bool PythonBridge::Transcribe(const TranscriptionParams &params,
                              TranscriptionResult &result,
                              std::function<void(int)> progressCallback)
{
   wxString args = wxString::Format(
      wxT("transcribe --input \"%s\" --language \"%s\""),
      params.inputPath,
      params.language
   );
   
   wxString command = BuildCommand(wxT(""), args);
   wxString output, error;
   
   if (progressCallback)
      progressCallback(50);
   
   bool success = Execute(command, output, error);
   
   if (progressCallback)
      progressCallback(100);
   
   if (success)
   {
      // Parse JSON output
      result.text = output;
      result.confidence = 0.95f; // TODO: Parse from JSON
   }
   
   return success;
}

std::vector<PythonBridge::Voice> PythonBridge::GetAvailableVoices()
{
   std::vector<Voice> voices;
   
   wxString command = BuildCommand(wxT("voices"), wxT("--list"));
   wxString output, error;
   
   if (Execute(command, output, error))
   {
      // TODO: Parse JSON output
      // For now, add a default voice
      Voice v;
      v.id = wxT("21m00Tcm4TlvDq8ikWAM");
      v.name = wxT("Rachel");
      v.category = wxT("Premade");
      v.description = wxT("Calm and professional");
      voices.push_back(v);
   }
   
   return voices;
}

bool PythonBridge::TestConnection(wxString &message)
{
   if (!IsAvailable())
   {
      message = wxT("Python backend not found");
      return false;
   }
   
   wxString output, error;
   wxString command = BuildCommand(wxT("test"), wxT(""));
   
   if (Execute(command, output, error))
   {
      message = wxT("Connection successful!");
      return true;
   }
   else
   {
      message = wxT("Connection failed: ") + error;
      return false;
   }
}
