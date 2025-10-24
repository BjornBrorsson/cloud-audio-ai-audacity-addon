# Plugin Setup Flow

## Overview

The Audacity Cloud AI plugins now feature automatic API key setup with GUI dialogs.

## Installation Flow

### 1. User Runs Installer

```powershell
.\install-nyquist-plugins.ps1
```

**What it does:**
- Copies `.ny` plugin files to Audacity plugins folder
- Copies `api_key_helper.py` and `audacity_cloudai.py` to same folder
- Copies `src/` directory with Python modules
- Optionally prompts for API key during installation
- Creates `.env` file in plugins folder if key provided

**Location:** `%APPDATA%\audacity\Plug-Ins\`

### 2. User Opens Audacity

Plugins appear in menus:
- **Generate** → AI Text-to-Speech, AI Music Generator, AI Sound Effects
- **Effect** → AI Voice Isolation
- **Analyze** → AI Transcription

### 3. User Runs a Plugin (First Time)

**Flow:**
1. Plugin calls `python api_key_helper.py`
2. Helper checks for `.env` file in plugins directory
3. If missing → Shows GUI dialog
4. User enters API key
5. Helper creates `.env` file
6. Plugin continues with generation

**GUI Dialog Features:**
- ✅ Text input for API key (hidden by default)
- ✅ "Show key" checkbox
- ✅ Link to elevenlabs.io
- ✅ Save button creates `.env` file
- ✅ Cancel button exits gracefully

### 4. Subsequent Uses

- API key exists → Plugin runs directly
- Invalid key → Error triggers new dialog
- Out of credits → Error message with instructions

## File Structure

```
%APPDATA%\audacity\Plug-Ins\
├── ai-text-to-speech.ny          # Nyquist plugins
├── ai-music-generator.ny
├── ai-sound-effects.ny
├── ai-voice-isolation.ny
├── ai-transcribe.ny
├── api_key_helper.py              # GUI dialog for API key
├── audacity_cloudai.py            # Main Python backend
├── src/                           # Python modules
│   ├── generators/
│   ├── effects/
│   └── analyzers/
└── .env                           # API key storage (created on first run)
```

## API Key Storage

The `.env` file format:
```
ELEVENLABS_API_KEY=sk_xxxxxxxxxxxxx
```

**Location:** Same directory as plugins (`%APPDATA%\audacity\Plug-Ins\.env`)

**Security:**
- File permissions: User-only read/write
- Not committed to git
- Can be edited manually if needed

## Error Handling

### Missing API Key
- Shows dialog: "You need an ElevenLabs API key..."
- Links to elevenlabs.io
- Prompts for key input

### Invalid API Key
- Detects authentication errors
- Shows dialog: "There was a problem with the API key..."
- Suggests checking key or credits
- Allows entering new key

### Out of Credits
- Error message explains credit system
- Links to account page
- Suggests waiting for renewal or topping up

### Network Issues
- Clear error messages
- Suggests checking internet connection
- Doesn't repeatedly prompt for API key

## Development Notes

### Nyquist Plugin Structure

Each plugin follows this pattern:

```lisp
;; 1. Check API key (show dialog if missing)
(setf check-key-cmd "python api_key_helper.py")
(setf key-check-result (system check-key-cmd))

(if (not (= key-check-result 0))
    (error "API key setup cancelled"))

;; 2. Build command
(setf cmd "python audacity_cloudai.py tts ...")

;; 3. Execute
(system cmd)

;; 4. Load and return audio
(s-read output-file)
```

### Python Helper (`api_key_helper.py`)

**Responsibilities:**
- Check for `.env` file in script directory
- Show tkinter GUI dialog if missing
- Validate API key format
- Write `.env` file
- Return exit code 0 (success) or 1 (cancelled)

**GUI Features:**
- Centered modal dialog
- Password-style input (show/hide toggle)
- Clickable link to elevenlabs.io
- Input validation
- Error handling

### Adding Error Recovery

To add API error detection:

1. Python backend catches API errors
2. Returns specific error codes
3. Nyquist plugin checks exit code
4. On auth error → Re-run `api_key_helper.py "Error message"`
5. Helper shows dialog with error context

## Testing

### Test API Key Dialog

```bash
# Temporarily rename .env to test dialog
cd "%APPDATA%\audacity\Plug-Ins"
ren .env .env.backup

# Run helper manually
python api_key_helper.py

# Dialog should appear
# Enter key → .env created
# Cancel → Exit code 1

# Restore
del .env
ren .env.backup .env
```

### Test Invalid Key

```bash
# Edit .env with invalid key
echo ELEVENLABS_API_KEY=invalid_key > .env

# Run plugin in Audacity
# Should get error from API
```

## User Experience Goals

✅ **Zero-configuration** - Works out of box after installer  
✅ **Self-documenting** - GUI shows where to get API key  
✅ **One-time setup** - Enter key once, works for all plugins  
✅ **Error recovery** - Clear messages, easy to fix  
✅ **Professional** - Native dialogs, polished UI  

## Future Enhancements

Potential improvements:

- [ ] Remember last-used voice IDs
- [ ] Cache voice library locally
- [ ] Show generation progress
- [ ] Batch processing multiple files
- [ ] Preset saving/loading
- [ ] Voice preview before generation
- [ ] Integration with Audacity label tracks

---

**Status:** ✅ Implemented and working  
**Platform:** Windows (primary), macOS/Linux (manual setup)  
**Last Updated:** October 24, 2025
