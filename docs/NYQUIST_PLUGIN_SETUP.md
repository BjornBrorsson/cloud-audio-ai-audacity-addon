# Nyquist Plugin Setup Guide

## Overview

The Nyquist plugins provide a direct integration with Audacity, allowing you to generate speech and music from within Audacity's Generate menu.

## Installation

### Step 1: Install the Main Plugin

1. **Copy plugin files** to Audacity's plugin directory:
   - **Windows:** `C:\Program Files\Audacity\Plug-Ins\`
   - **macOS:** `/Applications/Audacity.app/Contents/plug-ins/`
   - **Linux:** `~/.audacity-data/plug-ins/`

2. **Files to copy:**
   - `elevenlabs-tts.ny`
   - `elevenlabs-music.ny`

### Step 2: Set Environment Variable (Recommended)

Set the `AUDACITY_CLOUDAI_PATH` environment variable to point to your installation:

**Windows:**
```cmd
setx AUDACITY_CLOUDAI_PATH "C:\Path\To\Audacity-CloudAI"
```

**macOS/Linux:**
```bash
# Add to ~/.bashrc or ~/.zshrc
export AUDACITY_CLOUDAI_PATH="/path/to/Audacity-CloudAI"
```

If not set, plugins will default to: `C:/Coding Projects/Audacity-CloudAI`

### Step 3: Restart Audacity

Close and reopen Audacity for the plugins to appear in the Generate menu.

## Usage

### Text-to-Speech Plugin

1. **Select:** Generate → ElevenLabs Text-to-Speech
2. **Configure:**
   - **Text to speak:** Enter your text
   - **Voice ID:** Voice name or ID (default: Rachel)
   - **Model:** Choose AI model (Flash, Turbo, or Multilingual)
   - **Stability:** 0.0-1.0 (voice consistency)
   - **Similarity Boost:** 0.0-1.0 (voice matching)
   - **Style:** 0.0-1.0 (expressiveness)
3. **Click OK** - Audio will be generated and inserted at cursor

### Music Generation Plugin

1. **Select:** Generate → ElevenLabs Music Generation
2. **Configure:**
   - **Music description:** Describe the music you want
   - **Duration:** 10-120 seconds
   - **Genre:** Optional (e.g., "electronic", "classical")
   - **Mood:** Optional (e.g., "upbeat", "melancholic")
   - **Use composition plan:** Yes/No
3. **Click OK** - Music will be generated (may take 30-60 seconds)

## How It Works

### Architecture

```
Audacity → Nyquist Plugin → Python Script → ElevenLabs API → Audio File → Audacity
```

1. **User enters parameters** in Audacity's plugin dialog
2. **Nyquist plugin builds command** with parameters
3. **Python script executes** via `(system)` command
4. **Audio file is generated** by Python script
5. **Nyquist loads audio** using `(s-read)` 
6. **Audio appears in Audacity** track

### Behind the Scenes

The Nyquist plugin:
```lisp
;; 1. Build command
(setf full-cmd "python .../text_to_speech.py \"Hello\" -o temp.wav")

;; 2. Execute Python script
(setf exit-code (system full-cmd))

;; 3. Load resulting audio
(setf *track* (s-read "temp.wav"))

;; 4. Return to Audacity
*track*
```

## Troubleshooting

### Plugin Not Appearing in Menu

**Problem:** Plugins don't show up in Generate menu

**Solutions:**
1. Check files are in correct plugin directory
2. File extension must be `.ny`
3. Restart Audacity completely
4. Check Audacity → Effect → Plugin Manager
5. Enable/scan for new plugins

### "System Command Disabled" Error

**Problem:** `(system)` command is blocked

**Cause:** Security setting in Audacity prevents external command execution

**Solutions:**

**Option 1: Enable in Preferences**
1. Edit → Preferences → Modules
2. Enable "Allow Nyquist system commands"
3. Restart Audacity

**Option 2: Use CLI/GUI Instead**
The plugins are a convenience feature. You can always:
```bash
# Generate audio with CLI
python audacity_cloudai.py tts "Hello world" -o output.wav

# Then import to Audacity:
# File → Import → Audio
```

**Option 3: Use mod-script-pipe** (Advanced)
Configure Audacity's scripting module for safer external communication.

### "Python Not Found" Error

**Problem:** Plugin can't find Python

**Solutions:**
1. Ensure Python is in system PATH
2. Or use full Python path in plugin:
   ```lisp
   (setf python-cmd "C:/Python311/python.exe ...")
   ```
3. Test in terminal: `python --version`

### "Module Not Found" Error

**Problem:** Python script can't import dependencies

**Solutions:**
1. Ensure you ran: `pip install -r requirements.txt`
2. Use virtual environment:
   ```bash
   cd Audacity-CloudAI
   python -m venv venv
   venv\Scripts\activate  # Windows
   pip install -r requirements.txt
   ```
3. Update plugin to activate venv:
   ```lisp
   (setf python-cmd "\"C:/path/to/venv/Scripts/python.exe\" ...")
   ```

### "API Key Not Found" Error

**Problem:** ElevenLabs API key not configured

**Solutions:**
1. Create `.env` file with:
   ```
   ELEVENLABS_API_KEY=your_key_here
   ```
2. Or set environment variable:
   ```cmd
   setx ELEVENLABS_API_KEY "your_key_here"
   ```

### Audio File Not Loading

**Problem:** Plugin executes but no audio appears

**Solutions:**
1. Check temp file is created: `temp_tts.wav` or `temp_music.wav`
2. Try loading temp file manually in Audacity
3. Check Python script runs standalone:
   ```bash
   python src/generators/text_to_speech.py "Test" -o test.wav
   ```
4. Check file permissions on plugin directory
5. Try absolute path for temp file

## Advanced Configuration

### Custom Plugin Path

Edit the `.ny` files to change default path:

```lisp
;; Default path if environment variable not set
(if (not plugin-path)
  (setf plugin-path "C:/Your/Custom/Path"))
```

### Custom Python Interpreter

Use specific Python version:

```lisp
(setf python-cmd "C:/Python311/python.exe ...")
```

### Timeout Settings

For long generations, you may need to increase Nyquist timeout in Audacity preferences.

## Security Considerations

### Why `(system)` May Be Disabled

The `(system)` command can run any shell command, which is a security risk if malicious plugins are installed. This is why:

1. **Some Audacity builds disable it by default**
2. **Enterprise/education environments may block it**
3. **User can disable it in preferences**

### Safer Alternatives

1. **CLI + Manual Import:** Most secure, slightly less convenient
2. **GUI Application:** Standalone, no Audacity integration
3. **mod-script-pipe:** More complex but secure inter-process communication

## Performance Tips

### For Faster Generation

1. **Use Flash v2.5 model** (fastest)
2. **Shorter text/duration** for testing
3. **Pre-generate** audio, then import
4. **Batch processing** via CLI for multiple files

### For Better Quality

1. **Use Multilingual v2** for best quality
2. **Adjust stability/similarity** for your voice
3. **Add pauses** with `...` or punctuation
4. **Use SSML tags** (if supported by ElevenLabs)

## Alternative: Manual Workflow

If plugins don't work, use this workflow:

```bash
# 1. Generate audio with CLI
python audacity_cloudai.py tts "Your text here" -o speech.wav

# 2. Open Audacity
# 3. File → Import → Audio → Select speech.wav
# 4. Edit as needed
# 5. File → Export → Export as WAV/MP3
```

This is actually more flexible for complex projects!

## Support

**Plugin Issues:**
- Check Audacity debug log: Help → Show Log
- Test Python script standalone
- Try CLI/GUI as alternative

**API Issues:**
- Check API key configuration
- Verify ElevenLabs account status
- Check network connectivity

**For Help:**
- See main [README](../README.md)
- Check [Installation Guide](INSTALLATION.md)
- File an issue on GitHub

---

**Note:** Nyquist plugins are a convenience feature. The CLI and GUI are more reliable and full-featured!
