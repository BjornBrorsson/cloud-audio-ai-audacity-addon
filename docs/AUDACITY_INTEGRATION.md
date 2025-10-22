# Audacity Native Integration Guide

This document outlines how to integrate Audacity Cloud AI directly into Audacity so it feels like native functionality.

## Current State

**What we have now:**
- ✅ Nyquist plugins (`.ny` files) - Basic integration
- ✅ Standalone GUI application
- ✅ Command-line interface
- ❌ Deep Audacity integration

**What users experience:**
- Audacity menu items that launch Python scripts
- Audio appears in Audacity after generation
- **BUT:** Feels like a separate tool, not native

---

## Proposed Integration Architecture

### Option 1: Audacity Module (C++ Plugin) - **RECOMMENDED**

**Architecture:**
```
┌─────────────────────────────┐
│      Audacity Menu Item     │
│  "Generate → AI Text-to-Speech" │
└──────────────┬──────────────┘
               │
               ▼
┌─────────────────────────────┐
│   Audacity Module (C++)     │
│  - Opens native dialog      │
│  - Validates inputs         │
│  - Shows progress bar       │
└──────────────┬──────────────┘
               │
               ▼  (subprocess or HTTP)
┌─────────────────────────────┐
│   Python Backend Service    │
│  - Runs audacity_cloudai.py │
│  - Calls ElevenLabs API     │
│  - Returns audio data       │
└──────────────┬──────────────┘
               │
               ▼
┌─────────────────────────────┐
│   Audacity Audio Track      │
│  - Audio appears directly   │
│  - User can edit immediately│
└─────────────────────────────┘
```

**Benefits:**
- ✅ True native Audacity experience
- ✅ Progress bars and cancellation
- ✅ Undo/redo support
- ✅ Multi-track support
- ✅ Audacity's native file handling

**Implementation:**
```cpp
// mod-ai-audio/AiAudioModule.cpp
class AiAudioModule : public EffectPlugin {
    void OnGenerateAiSpeech() {
        // Show dialog
        AiSpeechDialog dlg(mParent);
        if (dlg.ShowModal() == wxID_OK) {
            // Call Python backend
            wxString text = dlg.GetText();
            wxString voice = dlg.GetVoice();
            
            // Execute: python audacity_cloudai.py tts --text "..." --voice "..."
            wxString output = CallPythonBackend(text, voice);
            
            // Load audio into track
            LoadAudioFromFile(output);
        }
    }
};
```

### Option 2: HTTP Server Backend

**Architecture:**
```
┌──────────────┐         ┌─────────────────┐
│   Audacity   │  HTTP   │ Python Server   │
│   (C++ mod)  │◄───────►│ (Flask/FastAPI) │
└──────────────┘         └─────────────────┘
                                 │
                                 ▼
                         ┌──────────────┐
                         │ ElevenLabs   │
                         │     API      │
                         └──────────────┘
```

**Benefits:**
- ✅ Clean separation of concerns
- ✅ Can run server in background
- ✅ Easier to debug Python code
- ✅ WebSocket for progress updates

**Implementation:**
```python
# server.py
from flask import Flask, request, jsonify
app = Flask(__name__)

@app.route('/api/tts', methods=['POST'])
def generate_tts():
    data = request.json
    text = data['text']
    voice = data['voice']
    
    # Generate audio
    generator = TextToSpeechGenerator()
    audio = generator.generate(text, voice_id=voice)
    
    # Save and return path
    output_path = save_audio(audio)
    return jsonify({'audio_path': output_path})

if __name__ == '__main__':
    app.run(port=5000)
```

### Option 3: Enhanced Nyquist + Python Bridge

**Current Nyquist limitation:** No native progress bars, limited UI

**Enhanced approach:**
```lisp
;; audacity-ai-tts.ny
(defun ai-generate-tts (text voice)
  ;; Launch Python with GUI
  (setf python-path (get-plugin-path))
  (setf cmd (strcat python-path "/python audacity_cloudai.py tts-gui"))
  
  ;; Python script shows rich GUI
  (system cmd)
  
  ;; Load generated audio
  (s-read "temp_output.wav"))
```

---

## Installation Integration

### Windows Installer (.exe)

**Current:** Installs to `C:\Program Files\Audacity Cloud AI\`

**Proposed:** Auto-detect and integrate into Audacity

```iss
[Setup]
; Inno Setup Script

[Code]
function GetAudacityPath(): String;
begin
  // Check common Audacity install locations
  if DirExists('C:\Program Files\Audacity') then
    Result := 'C:\Program Files\Audacity'
  else if DirExists(ExpandConstant('{pf}\Audacity 3.4')) then
    Result := ExpandConstant('{pf}\Audacity 3.4')
  else
    Result := '';
end;

procedure CurStepChanged(CurStep: TSetupStep);
var
  AudacityPath: String;
  PluginPath: String;
begin
  if CurStep = ssPostInstall then
  begin
    // Find Audacity installation
    AudacityPath := GetAudacityPath();
    
    if AudacityPath <> '' then
    begin
      // Install Nyquist plugins
      PluginPath := AudacityPath + '\Plug-Ins';
      FileCopy(
        ExpandConstant('{app}\nyquist\*.ny'), 
        PluginPath, 
        False
      );
      
      // Install module (if available)
      if FileExists(ExpandConstant('{app}\modules\mod-ai-audio.dll')) then
        FileCopy(
          ExpandConstant('{app}\modules\mod-ai-audio.dll'),
          AudacityPath + '\modules\',
          False
        );
        
      MsgBox('Audacity Cloud AI installed to Audacity!', mbInformation, MB_OK);
    end
    else
      MsgBox('Audacity not found. You can manually copy plugins.', mbInformation, MB_OK);
  end;
end;
```

### Linux (.deb)

```bash
# postinst script
#!/bin/bash

AUDACITY_PLUGIN_DIRS=(
    "$HOME/.audacity-data/Plug-Ins"
    "/usr/share/audacity/plug-ins"
    "/usr/local/share/audacity/plug-ins"
)

for dir in "${AUDACITY_PLUGIN_DIRS[@]}"; do
    if [ -d "$dir" ]; then
        echo "Installing to $dir"
        cp /usr/share/audacity-cloudai/nyquist/*.ny "$dir/"
        break
    fi
done

echo "Audacity Cloud AI installed!"
```

### macOS (.dmg)

```bash
# Install script
#!/bin/bash

AUDACITY_APP="/Applications/Audacity.app"
PLUGIN_DIR="$AUDACITY_APP/Contents/plug-ins"

if [ -d "$AUDACITY_APP" ]; then
    mkdir -p "$PLUGIN_DIR"
    cp -r nyquist/*.ny "$PLUGIN_DIR/"
    echo "✅ Installed to Audacity"
else
    echo "⚠️  Audacity not found at $AUDACITY_APP"
    echo "Please manually copy plugins from:"
    echo "  $HOME/Library/Application Support/audacity-cloudai/nyquist/"
fi
```

---

## User Experience Flow

### Ideal User Experience:

1. **User opens Audacity**
2. **Sees new menu items:**
   ```
   Generate ▼
     ├─ Tone...
     ├─ Noise...
     ├─ Silence...
     ├─ ───────────────
     ├─ 🎤 AI Text-to-Speech...     ← NEW!
     ├─ 🎵 AI Music...               ← NEW!
     └─ 🔊 AI Sound Effects...       ← NEW!
   
   Effect ▼
     ├─ Amplify...
     ├─ Bass and Treble...
     ├─ ───────────────
     └─ 🎧 AI Voice Isolation...     ← NEW!
   
   Analyze ▼
     ├─ Plot Spectrum...
     ├─ ───────────────
     └─ 📝 AI Transcription...       ← NEW!
   ```

3. **Clicks "AI Text-to-Speech..."**
4. **Native Audacity dialog appears:**
   ```
   ┌─────────────────────────────────────────┐
   │ AI Text-to-Speech Generator             │
   ├─────────────────────────────────────────┤
   │ Text to speak:                          │
   │ ┌─────────────────────────────────────┐ │
   │ │ Hello, this is a test.              │ │
   │ └─────────────────────────────────────┘ │
   │                                         │
   │ Voice: [Dorothy ▼]  [🔍 Browse...]    │
   │                                         │
   │ [Preview] [Generate] [Cancel]           │
   └─────────────────────────────────────────┘
   ```

5. **Clicks Generate**
6. **Progress bar shows:** "Generating audio... 75%"
7. **Audio appears** in new track, ready to edit

---

## Implementation Priority

### Phase 1: Enhanced Nyquist (Quick Win) ✅
- Improve existing `.ny` plugins
- Add better error messages
- Add voice selection UI
- **Time:** 1-2 days
- **Impact:** Medium

### Phase 2: HTTP Server Backend 🚀
- Python REST API server
- Auto-start with system
- WebSocket for progress
- **Time:** 1 week
- **Impact:** High

### Phase 3: Native C++ Module (Ultimate) 🏆
- Full Audacity integration
- Native dialogs and progress
- Undo/redo support
- **Time:** 2-3 weeks
- **Impact:** Highest

---

## Next Steps

1. **Decide on approach:**
   - Quick: Enhanced Nyquist
   - Best: HTTP Server + C++ Module

2. **Update installer:**
   - Auto-detect Audacity
   - Copy plugins automatically
   - Configure on first run

3. **Create C++ module skeleton:**
   ```bash
   git clone https://github.com/audacity/audacity.git
   cd audacity/modules
   mkdir mod-ai-audio
   # Create module code
   ```

4. **Test integration:**
   - Install Audacity
   - Run installer
   - Verify menu items appear
   - Generate audio

---

## FAQ

**Q: Can I install this without Audacity?**  
A: Yes! The standalone GUI and CLI work independently.

**Q: Will this work with Audacity 2.x?**  
A: Nyquist plugins yes, C++ module needs Audacity 3.x+

**Q: Can I use multiple AI voices in one project?**  
A: Yes! Generate to different tracks, then mix.

**Q: Does this require internet?**  
A: Yes, for AI generation. Voice isolation can work offline.

---

**Want to help build the C++ module?** See [CONTRIBUTING.md](CONTRIBUTING.md)
