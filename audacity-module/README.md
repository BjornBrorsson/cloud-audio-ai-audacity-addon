# Audacity Cloud AI - Native C++ Module

Native Audacity module for seamless AI audio generation integration.

## Features

✅ **Native Audacity dialogs** - Full UI integration  
✅ **Progress bars** - Real-time generation progress  
✅ **Undo/Redo support** - Full Audacity history integration  
✅ **Multi-track** - Generate directly to tracks  
✅ **No external windows** - Everything in Audacity  

## Menu Items

After installation, you'll see:

```
Generate ▼
  ├─ 🎤 AI Text-to-Speech
  ├─ 🎵 AI Music  
  └─ 🔊 AI Sound Effects

Effect ▼
  └─ 🎧 AI Voice Isolation

Analyze ▼
  └─ 📝 AI Transcription
```

## Architecture

```
┌─────────────────┐
│  Audacity UI    │ ← Native wxWidgets dialogs
└────────┬────────┘
         │
┌────────▼────────┐
│  C++ Module     │ ← mod-cloud-ai.dll/so/dylib
│  (This folder)  │
└────────┬────────┘
         │ subprocess
┌────────▼────────┐
│  Python Backend │ ← audacity_cloudai.py
│  (../src)       │
└────────┬────────┘
         │ HTTPS
┌────────▼────────┐
│  ElevenLabs API │
└─────────────────┘
```

## Building

See [BUILD.md](BUILD.md) for detailed build instructions.

Quick start:
```bash
# Copy to Audacity source
cp -r audacity-module /path/to/audacity/modules/mod-cloud-ai

# Build Audacity
cd /path/to/audacity
mkdir build && cd build
cmake ..
make
```

## Files

- `ModCloudAI.cpp/h` - Module entry point
- `CloudAIEffect.cpp/h` - Effect base classes
- `PythonBridge.cpp/h` - C++ ↔ Python communication
- `TtsDialog.cpp/h` - Text-to-Speech UI
- `MusicDialog.cpp/h` - Music generation UI
- `SoundEffectsDialog.cpp/h` - Sound effects UI
- `VoiceIsolationDialog.cpp/h` - Voice isolation UI
- `TranscriptionDialog.cpp/h` - Transcription UI
- `CMakeLists.txt` - Build configuration

## Status

🚧 **Under Development** 🚧

- [x] Module skeleton
- [x] Python bridge
- [ ] TTS dialog (in progress)
- [ ] Music dialog
- [ ] SFX dialog
- [ ] Voice isolation
- [ ] Transcription
- [ ] Progress bars
- [ ] Error handling
- [ ] Testing

## Contributing

Want to help? See [../docs/CONTRIBUTING.md](../docs/CONTRIBUTING.md)

Priority tasks:
1. Implement TtsDialog UI
2. Test on Windows/Linux/macOS
3. Add progress bar threading
4. Implement voice browser
5. Add settings dialog

## License

MIT License - See [../LICENSE](../LICENSE)
