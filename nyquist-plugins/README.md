# Audacity Cloud AI - Nyquist Plugins

Native Audacity plugins that integrate ElevenLabs AI features directly into Audacity menus.

## Features

- **AI Text-to-Speech** - Generate voice from text (Generate menu)
- **AI Music Generator** - Create music from descriptions (Generate menu)
- **AI Sound Effects** - Generate sound effects (Generate menu)
- **AI Voice Isolation** - Remove background noise (Effect menu)
- **AI Transcription** - Convert speech to text (Analyze menu)

## Installation

### 1. Use the Installer (Recommended)

**Windows:**
```powershell
.\install-nyquist-plugins.ps1
```

The installer will:
- Copy all plugins to Audacity
- Prompt for your API key
- Set up Python dependencies

### 2. Manual Installation

#### Install Python Backend

```bash
cd "C:\Coding Projects\Audacity-CloudAI"
pip install -r requirements.txt
```

#### Install Plugins

Copy all `.ny` files AND `api_key_helper.py` to your Audacity plugins folder:

**Windows:**
```
C:\Users\YourName\AppData\Roaming\audacity\Plug-Ins\
```

**macOS:**
```
~/Library/Application Support/audacity/Plug-Ins/
```

**Linux:**
```
~/.audacity-data/Plug-Ins/
```

Or use Audacity's user plugins folder:
- Windows: `%APPDATA%\audacity\Plug-Ins\`
- macOS: `~/Library/Application Support/audacity/Plug-Ins/`
- Linux: `~/.audacity-data/Plug-Ins/`

### 3. Restart Audacity

The plugins will appear in their respective menus:
- Generate → AI Text-to-Speech, AI Music Generator, AI Sound Effects
- Effect → AI Voice Isolation
- Analyze → AI Transcription

## First-Time Usage

**When you first run any plugin without an API key:**

An error dialog will appear with instructions:
- Where to get a free API key (elevenlabs.io)
- Where to create the `.env` file
- What to put in the file

**Quick Setup:**

Run this helper script:
```powershell
.\create-env-file.ps1
```

Or manually create `.env` in: `%APPDATA%\audacity\Plug-Ins\`

With content:
```
ELEVENLABS_API_KEY=your_key_here
```

**One-time setup** - all plugins share the same API key!

## Usage

### Text-to-Speech

1. Generate → AI Text-to-Speech
2. Enter your text
3. (Optional) Enter Voice ID from ElevenLabs
4. Adjust voice settings
5. Click OK

### Music Generator

1. Generate → AI Music Generator
2. Describe the music you want
3. Set duration (5-180 seconds)
4. Adjust prompt influence
5. Click OK

### Sound Effects

1. Generate → AI Sound Effects
2. Describe the sound effect
3. Set duration (1-60 seconds)
4. Click OK

### Voice Isolation

1. Select audio in track
2. Effect → AI Voice Isolation
3. Click OK
4. Voice will be isolated, background removed

### Transcription

1. Select audio in track
2. Analyze → AI Transcription
3. Choose language (or auto-detect)
4. Select output format
5. Click OK
6. Transcription appears in dialog

## Requirements

- **Audacity** 3.0 or later
- **Python** 3.8+
- **ElevenLabs API key**
- **Internet connection**

## Troubleshooting

### "Could not generate audio"

- Check Python is installed and in PATH
- Verify `audacity_cloudai.py` is accessible
- Check API key is set correctly
- Ensure internet connection

### Plugins don't appear

- Check plugins are in correct folder
- Restart Audacity after copying
- Enable plugins in Plug-In Manager (Effect → Plug-In Manager)

### Python not found

Make sure Python is in your system PATH, or edit the `.ny` files to use full Python path:
```lisp
(setf cmd (format nil "C:\\Python312\\python.exe audacity_cloudai.py ..."))
```

## Advanced

### Custom Voice IDs

Get voice IDs from https://elevenlabs.io/app/voices:
1. Browse voices
2. Click voice → Copy ID
3. Paste in "Voice ID" field

### Batch Processing

Use Audacity's Macro feature (Tools → Macros) to:
1. Apply plugins to multiple files
2. Automate workflows
3. Chain effects together

## Support

- **Documentation:** See main README.md
- **Issues:** GitHub Issues
- **Python Backend:** ../audacity_cloudai.py
- **GUI Version:** ../gui_launcher.py

## License

MIT License - See LICENSE file
