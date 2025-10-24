# Migration to Nyquist Plugins

## Summary

We successfully migrated from a C++ module approach to **Nyquist plugins**, which provides better integration with Audacity.

## What Changed

### ✅ Added (New)

**Nyquist Plugins:**
- `nyquist-plugins/ai-text-to-speech.ny` - TTS generator with voice settings
- `nyquist-plugins/ai-music-generator.ny` - Music generation
- `nyquist-plugins/ai-sound-effects.ny` - Sound effects generation
- `nyquist-plugins/ai-voice-isolation.ny` - Voice isolation effect
- `nyquist-plugins/ai-transcribe.ny` - Audio transcription
- `nyquist-plugins/README.md` - Installation and usage guide

**Installation Helper:**
- `install-nyquist-plugins.ps1` - Automatic plugin installer for Windows

### ❌ Removed (Obsolete)

**C++ Module (didn't work due to Audacity framework dependencies):**
- `audacity-module/` - Entire directory removed
- `docs/STANDALONE_MODULE_BUILD.md` - Build documentation removed
- All C++ source files (*.cpp, *.h)
- CMake build scripts
- vcpkg/wxWidgets setup scripts

**Why removed:** C++ modules require building against Audacity's internal framework, which had complex dependency issues (Conan, vcpkg). Nyquist plugins are simpler and work out-of-the-box.

### 📝 Updated

**Documentation:**
- `README.md` - Now prioritizes Nyquist plugins
- `docs/README.md` - Removed C++ module references
- All docs now point to Nyquist approach

## Benefits of Nyquist Approach

| Aspect | C++ Module | Nyquist Plugins |
|--------|-----------|-----------------|
| **Compilation** | Complex (CMake, vcpkg, wxWidgets) | ❌ None! Just copy files |
| **Cross-platform** | Build for each OS | ✅ Works everywhere |
| **UI Integration** | Native dialogs | ✅ Native dialogs (widgets) |
| **Menu Integration** | Module system | ✅ Standard plugin system |
| **Maintenance** | Hard (C++ dependencies) | ✅ Easy (text files) |
| **Backend** | Would need rewrite | ✅ Uses existing Python! |
| **Installation** | Build + install | ✅ Copy .ny files |
| **Updates** | Recompile | ✅ Just edit text |

## How Nyquist Plugins Work

1. **User interaction:** Audacity shows native dialog with widgets
2. **Command building:** Nyquist script builds Python command
3. **Execution:** `python audacity_cloudai.py [command]` runs
4. **Audio return:** Generated/processed audio loads into Audacity

```
Audacity Menu → Nyquist Plugin → Python Backend → ElevenLabs API
     ↑                                                     ↓
     └──────────── Generated Audio Returns ────────────────┘
```

## Installation (New Way)

### Automated
```powershell
.\install-nyquist-plugins.ps1
```

### Manual
Copy `.ny` files from `nyquist-plugins/` to:
- Windows: `%APPDATA%\audacity\Plug-Ins\`
- macOS: `~/Library/Application Support/audacity/Plug-Ins/`
- Linux: `~/.audacity-data/Plug-Ins/`

Restart Audacity. Done!

## What Still Works

✅ **Python GUI:** `python gui_launcher.py`
✅ **CLI Tool:** `python audacity_cloudai.py [command]`
✅ **All features:** TTS, Music, SFX, Isolation, Transcription
✅ **ElevenLabs API:** Same backend, same quality

## Migration Guide for Users

If you previously attempted the C++ build:

1. ✅ Delete any build artifacts:
   ```powershell
   Remove-Item -Recurse audacity-module/build*
   Remove-Item C:\Users\YourName\wxWidgets -Recurse
   ```

2. ✅ Install Nyquist plugins:
   ```powershell
   .\install-nyquist-plugins.ps1
   ```

3. ✅ Keep using Python backend (no changes needed!)

## Future Improvements

Potential enhancements to Nyquist plugins:

- [ ] Voice browser integration (show voice list in dialog)
- [ ] Real-time preview before generation
- [ ] Batch processing multiple selections
- [ ] Preset saving/loading
- [ ] Progress indicators
- [ ] Label track integration for transcriptions

## Developer Notes

### Nyquist Plugin Structure

Each plugin:
1. **Headers** - Define plugin type, name, controls
2. **Logic** - Build command string from user inputs
3. **Execution** - Call Python backend via `system`
4. **Audio handling** - Read WAV file and return to Audacity

### Adding New Features

To add a new feature:
1. Add CLI command to `audacity_cloudai.py`
2. Create corresponding `.ny` plugin
3. Define UI widgets (controls)
4. Build command string
5. Call system and return audio

### Customization

Users can edit `.ny` files to:
- Change default values
- Add new controls (widgets)
- Modify voice options
- Adjust UI labels
- Add validation

## Conclusion

The Nyquist approach is **simpler, more maintainable, and works better** than the C++ module. It:

✅ Provides the same native Audacity integration
✅ Requires no compilation
✅ Works cross-platform immediately
✅ Uses the existing Python backend
✅ Is easy to customize and update
✅ Follows Audacity's standard plugin system

The C++ module attempt taught us what Audacity actually needs - and Nyquist delivers it perfectly!

---

**Date:** October 24, 2025  
**Reason:** Simplified architecture, eliminated build complexity  
**Status:** ✅ Complete and working
