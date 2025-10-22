# Audacity Cloud AI - GUI Guide

## Launching the GUI

### Windows
Double-click `Start GUI.bat` or run:
```cmd
python gui_launcher.py
```

### Mac/Linux
```bash
python3 gui_launcher.py
```

## First-Time Setup

When you launch the GUI for the first time, you'll see a Setup Wizard:

1. **Enter your ElevenLabs API Key**
   - Go to https://elevenlabs.io
   - Sign up or log in
   - Navigate to Profile → API Keys
   - Copy your API key

2. **Paste it in the Setup Wizard**
   - The wizard will validate your key
   - Click "Test & Save"

3. **Start using the plugin!**

## Main Window

The GUI has 5 tabs for different features:

### 🎤 Text-to-Speech Tab
- **Text Input**: Enter or paste your text
- **Voice Selection**: Choose from popular voices or browse the library
- **Model Selection**: Pick your quality/speed preference
- **Voice Settings**: 
  - Stability (0.0-1.0): Higher = more consistent
  - Similarity (0.0-1.0): Higher = closer to original voice
  - Style (0.0-1.0): Higher = more expressive
- **Output**: Choose where to save the file
- Click **Generate Speech**

### 🎶 Music Tab
- **Description**: Describe the music you want
- **Duration**: Set length in seconds (10-120)
- **Composition Plan**: Enable for more structured music
- **Output**: Choose save location
- Click **Generate Music**

### 🔊 Sound Effects Tab
- **Description**: Describe the sound effect
- **Duration**: Optional (0 = auto-determined)
- **Output**: Choose save location
- Click **Generate Sound Effect**

### 🎧 Voice Isolator Tab
- **Input**: Select noisy audio file
- **Output**: Choose where to save cleaned audio
- Click **Isolate Voice**

### 📝 Transcription Tab
- **Input**: Select audio to transcribe
- **Options**:
  - Enable speaker diarization (identifies who's speaking)
  - Create Audacity labels (for easy editing)
- **Output**: Choose save location (.txt, .json, or .srt)
- Click **Transcribe Audio**
- Transcript appears in the text box

## Menu Bar

### File Menu
- **Settings**: Configure API key, defaults, output directory
- **Exit**: Close the application

### Tools Menu
- **Voice Library**: Browse thousands of community voices
  - Search by keyword
  - Filter by gender, age, accent
  - Preview voices
  - Add to your account
- **List My Voices**: See all voices in your account
- **Check Configuration**: Verify API key and subscription status

### Help Menu
- **Documentation**: Opens GitHub repository
- **Examples**: View example prompts for music and sound effects
- **About**: Version and license information

## Voice Library Browser

Access via **Tools → Voice Library**:

1. **Search**: Enter keywords (e.g., "deep male narrator")
2. **Browse**: Click "Featured" to see popular voices
3. **Select**: Click on a voice to see details
4. **Preview**: Audio preview URLs are shown
5. **Add**: Click "Add to My Voices" and give it a name

## Settings Dialog

Access via **File → Settings**:

### API Configuration
- **API Key**: Update your ElevenLabs API key
- **Show/Hide**: Toggle visibility

### Defaults
- **Default Voice**: Set your preferred voice ID
- **Default Model**: Choose your preferred quality/speed model
- **Output Directory**: Where files are saved by default

Click **Save** to apply changes.

## Tips for Best Results

### Text-to-Speech
- **Short sentences** work best
- Use **punctuation** for natural pauses
- **Lower stability** = more emotion/variation
- **Higher stability** = more consistent/professional
- **Style** only works with v2 models

### Music Generation
- Be **specific** about genre, mood, instruments
- Mention **tempo** (fast/slow)
- Specify if you want **vocals** or just instrumental
- Use **composition plan** for longer, structured pieces

### Sound Effects
- Be **descriptive** (not just "explosion" but "large explosion with debris")
- Mention **environment** (indoor/outdoor, distance)
- Include **materials** (metal door, wooden floor)

### Voice Isolation
- Works best with **clear speech**
- May not work well if voice and background are similar volume
- Good for cleaning up **podcast recordings**

### Transcription
- **Enable speaker diarization** for interviews/meetings
- Create **Audacity labels** for easy editing in Audacity
- Export as **SRT** for video subtitles

## Importing to Audacity

After generating audio:

1. Open Audacity
2. **File → Import → Audio**
3. Select your generated file
4. Edit, mix, and export as usual!

For transcriptions with Audacity labels:
1. Open Audacity
2. Open your audio file
3. **File → Import → Labels**
4. Select the `.txt` label file
5. Labels appear as a label track

## Troubleshooting

### "Failed to launch GUI: no module named tkinter"
**Solution**: Install tkinter
- Windows: Reinstall Python, check "tcl/tk and IDLE"
- Ubuntu: `sudo apt-get install python3-tk`
- Mac: `brew install python-tk`

### "Invalid API Key"
**Solution**:
- Check your key in Settings
- Verify it's correct at elevenlabs.io
- Make sure you have an active subscription

### "Generation failed: Insufficient credits"
**Solution**:
- Check your account balance at elevenlabs.io
- Upgrade your subscription or add credits

### GUI is unresponsive
**Solution**:
- Wait for the current operation to complete
- Check the status bar at the bottom
- Close and restart the application

### Can't find generated files
**Solution**:
- Check the "Save to:" path before generating
- Default location is in the `temp/` folder
- Use Settings to change default output directory

## Keyboard Shortcuts

- **Ctrl+Q**: Quit (file menu)
- **Tab**: Move between fields
- **Enter**: Activate focused button

## Command-Line Alternative

Prefer the terminal? Use:
```bash
python audacity_cloudai.py --help
```

See [README.md](README.md) for CLI documentation.

## Getting Help

- 🐛 Report bugs: [GitHub Issues](https://github.com/yourusername/audacity-cloudai/issues)
- 💬 Ask questions: [GitHub Discussions](https://github.com/yourusername/audacity-cloudai/discussions)
- 📖 Documentation: [README.md](README.md)

---

**Enjoy creating amazing audio with AI!** 🎵✨
