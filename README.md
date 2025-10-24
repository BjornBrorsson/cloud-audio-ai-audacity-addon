# Audacity Cloud AI 🎵☁️

Integrate ElevenLabs AI capabilities directly into Audacity with native Nyquist plugins:

- 🎤 **Text-to-Speech** - Generate professional voiceovers from text
- 🎶 **Music Generation** - Create original music from text prompts
- 🔊 **Sound Effects** - Generate realistic sound effects from descriptions
- 🎧 **Voice Isolation** - Remove background noise and isolate voice
- 📝 **Transcription** - Convert speech to text (30+ languages)

## How It Works

This project provides **three ways** to use ElevenLabs AI with Audacity:

1. **Nyquist Plugins** (Recommended) - Native Audacity menu integration
2. **Python GUI** - Standalone app with all features
3. **CLI Tool** - Command-line interface for scripting

## Features

### Text-to-Speech Generator
- Convert text to lifelike speech using ElevenLabs voices
- Support for 32+ languages
- Multiple voice models (Turbo, Flash, Multilingual)
- Adjustable voice settings (stability, similarity, style)
- Browse Voice Library with thousands of voices

### Music Generator
- Generate original music from text descriptions
- Control genre, mood, instruments, and tempo
- Composition plan support
- Multi-scene soundtrack creation

### Sound Effects Generator
- Create realistic sound effects from text
- Nature, UI, ambience, mechanical sounds
- Adjustable prompt influence
- Perfect for games and videos

### Voice Isolator
- Remove background noise from audio
- Isolate voice from music/ambient sounds
- Clean up recordings automatically
- Batch processing support

### Audio Transcription
- Speech-to-text with 99 languages
- Speaker diarization (identify speakers)
- Export as text, JSON, or SRT subtitles
- Create Audacity label tracks
- Word-level timestamps

### Voice Library Browser
- Browse thousands of community voices
- Search and filter by gender, age, accent
- Preview voices before using
- Add voices to your account
- Interactive browsing mode

## Installation

### Prerequisites
- Audacity 3.0 or later
- Python 3.8 or later
- ElevenLabs API key (get one at [elevenlabs.io](https://elevenlabs.io))

### Setup

1. **Clone or download this repository**
   ```bash
   git clone https://github.com/yourusername/audacity-cloudai.git
   cd audacity-cloudai
   ```

2. **Install Python dependencies**
   ```bash
   pip install -r requirements.txt
   ```

3. **Configure your API key**
   
   Create a `.env` file in the plugin directory:
   ```
   ELEVENLABS_API_KEY=your_api_key_here
   ```
   
   Or set it as an environment variable:
   ```bash
   # Windows
   set ELEVENLABS_API_KEY=your_api_key_here
   
   # Linux/Mac
   export ELEVENLABS_API_KEY=your_api_key_here
   ```

4. **Install Nyquist plugins** (for Audacity integration)
   
   Copy all `.ny` files from `nyquist-plugins/` to your Audacity plugin directory:
   
   - **Windows**: `%APPDATA%\audacity\Plug-Ins\`
   - **Mac**: `~/Library/Application Support/audacity/Plug-Ins/`
   - **Linux**: `~/.audacity-data/Plug-Ins/`
   
   Then restart Audacity. Plugins will appear in Generate, Effect, and Analyze menus.

## Usage

### Option 1: Nyquist Plugins (Native Audacity Integration)

**After installation, use plugins directly from Audacity menus:**

**Generate Menu:**
- **AI Text-to-Speech** - Enter text, select voice, generate
- **AI Music Generator** - Describe music, set duration
- **AI Sound Effects** - Describe sound, set duration

**Effect Menu:**
- **AI Voice Isolation** - Select audio, apply effect

**Analyze Menu:**
- **AI Transcription** - Select audio, get transcription

See [nyquist-plugins/README.md](nyquist-plugins/README.md) for detailed usage.

### Option 2: Python GUI (Standalone App)

```bash
python gui_launcher.py
```

- Full-featured GUI application
- Voice browser, preview features
- Works independently of Audacity
- Import generated audio into Audacity

### Option 3: Command-Line Interface (Scripting)

For automation and scripting:

```bash
# Generate speech
python audacity_cloudai.py tts "Hello world" -o hello.wav

# Generate music
python audacity_cloudai.py music "Epic cinematic music" -d 30 -o music.wav

# Generate sound effects
python audacity_cloudai.py sfx "Thunder and rain" -o storm.wav

# Isolate voice
python audacity_cloudai.py isolate noisy.wav -o clean.wav

# Transcribe audio
python audacity_cloudai.py transcribe podcast.mp3 -o transcript.txt

# Browse voices
python audacity_cloudai.py voices --interactive
```

Then import the generated files into Audacity:
1. Open Audacity
2. **File → Import → Audio**
3. Select your generated file

### All Commands

```bash
python audacity_cloudai.py --help           # Show all commands
python audacity_cloudai.py tts --help       # TTS options
python audacity_cloudai.py check-config     # Verify setup
python audacity_cloudai.py list-voices      # List your voices
python audacity_cloudai.py example-prompts  # Example prompts
```

## Configuration

### Voice Models

- **eleven_flash_v2_5** - Fast, low-latency (recommended for most use cases)
- **eleven_turbo_v2_5** - Balanced quality and speed
- **eleven_multilingual_v2** - High quality, supports 32 languages

### Voice Settings

- **Stability** (0.0 - 1.0): Controls voice consistency
- **Similarity Boost** (0.0 - 1.0): Enhances voice similarity to the selected voice
- **Style** (0.0 - 1.0): Adds expressiveness (only for v2 models)

## 📚 Documentation

- **[Installation Guide](docs/INSTALLATION.md)** - Complete setup instructions
- **[Quick Start](docs/QUICKSTART.md)** - Get started quickly
- **[GUI Guide](docs/GUI_README.md)** - Graphical interface tutorial
- **[Extended Documentation](docs/README_EXTENDED.md)** - All features and workflows
- **[Feature Summary](docs/FEATURES_SUMMARY.md)** - Complete feature reference
- **[Contributing](docs/CONTRIBUTING.md)** - How to contribute

📖 **[Full Documentation Index](docs/README.md)**

## Development

### Project Structure

```
audacity-cloudai/
├── src/
│   ├── generators/
│   │   ├── text_to_speech.py
│   │   └── music_generator.py
│   ├── effects/
│   │   └── voice_converter.py
│   ├── utils/
│   │   ├── elevenlabs_api.py
│   │   ├── audio_utils.py
│   │   └── config.py
│   └── ui/
│       └── dialogs.py
├── nyquist/
│   ├── elevenlabs-tts.ny
│   └── elevenlabs-music.ny
├── tests/
├── requirements.txt
├── setup.py
└── README.md
```

### Building from Source

```bash
# Clone the repository
git clone https://github.com/yourusername/audacity-cloudai.git
cd audacity-cloudai

# Install development dependencies
pip install -r requirements-dev.txt

# Run tests
pytest tests/

# Build package
python setup.py build
```

## API Costs

This plugin uses the ElevenLabs API, which is a paid service:
- Text-to-Speech: Charged per character
- Music Generation: Charged per generation
- Check [ElevenLabs pricing](https://elevenlabs.io/pricing) for current rates

## Troubleshooting

### "API Key not found"
- Ensure your `.env` file exists with `ELEVENLABS_API_KEY`
- Or set the environment variable before launching Audacity

### "Failed to generate audio"
- Check your internet connection
- Verify your API key is valid
- Check your ElevenLabs account has sufficient credits

### "Plugin not appearing in Audacity"
- Ensure files are in the correct Plug-Ins directory
- Restart Audacity after installation
- Check Audacity's plugin manager (Edit → Preferences → Effects)

## 📚 Documentation

This project has comprehensive documentation organized by audience:

### For Users
- **[docs/](docs/README.md)** - Complete documentation hub with guides for:
  - Installation, Quick Start, GUI usage
  - All features and examples
  - See [docs/README.md](docs/README.md) for full index

### For Developers
- **[nyquist-plugins/README.md](nyquist-plugins/README.md)** - Nyquist plugin documentation
  - Installation and usage
  - Customization and troubleshooting
- **[examples/README.md](examples/README.md)** - Usage examples
  - CLI examples for all features
  - Workflow examples (podcasts, games, education)
- **[tests/README.md](tests/README.md)** - Testing documentation
  - Running tests, writing tests
  - Coverage goals

### For Package Maintainers
- **[installer/README.md](installer/README.md)** - Installer build instructions
  - Windows (Inno Setup), Linux (.deb), macOS (.dmg)
  - CI/CD integration

## Contributing

Contributions are welcome! This is an open-source project.

See **[docs/CONTRIBUTING.md](docs/CONTRIBUTING.md)** for detailed guidelines.

Quick start:
1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- [Audacity](https://www.audacityteam.org/) - Open-source audio editor
- [ElevenLabs](https://elevenlabs.io/) - AI voice and music generation
- [OpenVINO AI Plugins](https://github.com/intel/openvino-plugins-ai-audacity) - Inspiration for plugin architecture

## Support

- 🐛 [Report bugs](https://github.com/yourusername/audacity-cloudai/issues)
- 💡 [Request features](https://github.com/yourusername/audacity-cloudai/issues)
- 📧 Contact: your.email@example.com

---

Made with ❤️ for the Audacity and AI community
