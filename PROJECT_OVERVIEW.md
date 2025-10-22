# Audacity Cloud AI - Project Overview

## What We've Built

A comprehensive Audacity plugin that integrates ElevenLabs Cloud AI capabilities for:
- **Text-to-Speech Generation** - Professional AI voice-overs
- **Music Generation** - AI-powered music creation
- **Voice Conversion** - Transform audio with AI voices (framework ready)

## Project Structure

```
audacity-cloudai/
│
├── 📄 README.md                    # Main documentation
├── 📄 INSTALLATION.md              # Step-by-step setup guide
├── 📄 QUICKSTART.md                # Quick start tutorial
├── 📄 CONTRIBUTING.md              # Contribution guidelines
├── 📄 LICENSE                      # MIT License
├── 📄 .gitignore                   # Git ignore rules
├── 📄 .env.example                 # Environment variables template
├── 📄 requirements.txt             # Python dependencies
├── 📄 setup.py                     # Package setup
├── 📄 audacity_cloudai.py          # Main CLI interface
│
├── 📁 src/                         # Source code
│   ├── 📁 generators/
│   │   ├── text_to_speech.py      # TTS generator
│   │   ├── music_generator.py     # Music generator
│   │   └── __init__.py
│   │
│   └── 📁 utils/
│       ├── config.py               # Configuration management
│       ├── elevenlabs_api.py      # API wrapper
│       ├── audio_utils.py         # Audio processing utilities
│       └── __init__.py
│
├── 📁 nyquist/                     # Audacity plugin files
│   ├── elevenlabs-tts.ny          # TTS Nyquist plugin
│   └── elevenlabs-music.ny        # Music Nyquist plugin
│
├── 📁 examples/                    # Example scripts
│   ├── demo.py                    # Interactive demo
│   └── README.md                  # Examples documentation
│
└── 📁 temp/                        # Generated files (auto-created)
    └── cache/                      # Cache directory (auto-created)
```

## Key Features

### ✅ Implemented

1. **Text-to-Speech Generator**
   - Multiple voice models (Flash, Turbo, Multilingual)
   - 32+ language support
   - Customizable voice settings (stability, similarity, style)
   - Batch generation with merging
   - Voice preview functionality

2. **Music Generator**
   - Text-to-music from prompts
   - Composition plan support
   - Genre, mood, and instrument control
   - Multi-scene soundtrack creation
   - Loopable music generation

3. **API Integration**
   - Full ElevenLabs API wrapper
   - Streaming support
   - Error handling and retry logic
   - User account info retrieval

4. **Audio Utilities**
   - Format conversion (MP3 ↔ WAV)
   - Audio merging with crossfade
   - Normalization
   - Duration calculation

5. **CLI Interface**
   - User-friendly command-line tool
   - Voice and model listing
   - Configuration checker
   - Example prompt generator

6. **Documentation**
   - Comprehensive README
   - Installation guide
   - Quick start tutorial
   - Contributing guidelines
   - Example workflows

### 🚧 Framework Ready (For Future Development)

1. **Voice Conversion**
   - API integration complete
   - Needs UI implementation

2. **Audacity Native Integration**
   - Nyquist plugins created
   - Needs bridge implementation for full integration

3. **GUI Application**
   - Core functionality ready
   - Can be wrapped in tkinter/PyQt

## How It Works

### Architecture

```
User Input (CLI/Audacity)
         ↓
  Main Interface
         ↓
   Generators ←→ ElevenLabs API
         ↓
  Audio Utils (Convert/Process)
         ↓
   WAV Output → Audacity
```

### Workflow

1. **User provides text/prompt**
2. **Generator validates input**
3. **API call to ElevenLabs**
4. **Audio received (MP3)**
5. **Converted to WAV** (Audacity-compatible)
6. **Saved to file**
7. **User imports to Audacity**

## Technology Stack

- **Language**: Python 3.8+
- **API**: ElevenLabs Cloud API
- **Audio Processing**: pydub, numpy
- **HTTP Client**: requests
- **Configuration**: python-dotenv
- **Plugin Interface**: Nyquist (Audacity's scripting language)

## API Endpoints Used

### Text-to-Speech
- `POST /v1/text-to-speech/{voice_id}` - Generate speech
- `POST /v1/text-to-speech/{voice_id}/stream` - Stream speech

### Music
- `POST /v1/music` - Generate music
- `POST /v1/music/composition-plan` - Create composition plan

### Utilities
- `GET /v1/voices` - List available voices
- `GET /v1/models` - List available models
- `GET /v1/user` - Get user info

### Voice Conversion (Ready)
- `POST /v1/speech-to-speech/{voice_id}` - Convert voice

## Use Cases

1. **Podcast Production**
   - Generate intro/outro
   - Create background music
   - Produce narration

2. **YouTube Videos**
   - Professional voice-overs
   - Background music
   - Transitions

3. **Audiobooks**
   - Chapter narration
   - Consistent voice across chapters

4. **Game Development**
   - Character voices
   - Background music
   - Sound effects narration

5. **Educational Content**
   - Lesson narration
   - Explanatory voice-overs
   - Background music

## Getting Started

### Quick Setup (3 steps)

1. **Install dependencies**
   ```bash
   pip install -r requirements.txt
   ```

2. **Set API key**
   ```bash
   # Create .env file
   echo ELEVENLABS_API_KEY=your_key_here > .env
   ```

3. **Generate first audio**
   ```bash
   python audacity_cloudai.py tts "Hello world" -o test.wav
   ```

### Full Documentation

- 📖 [Installation Guide](INSTALLATION.md)
- 🚀 [Quick Start](QUICKSTART.md)
- 📚 [Full README](README.md)

## Development Roadmap

### Phase 1: Core Features ✅ (Complete)
- Text-to-Speech generation
- Music generation
- CLI interface
- Documentation

### Phase 2: Enhanced Integration (Future)
- [ ] Native Audacity plugin (full integration)
- [ ] GUI wrapper application
- [ ] Voice cloning interface
- [ ] Batch processing UI

### Phase 3: Advanced Features (Future)
- [ ] Project templates
- [ ] Voice library browser
- [ ] Music remixing
- [ ] Real-time preview
- [ ] Export presets

### Phase 4: Community (Future)
- [ ] Preset sharing
- [ ] Voice marketplace integration
- [ ] Tutorial videos
- [ ] Sample projects

## Performance

- **TTS Generation**: ~1-3 seconds per request
- **Music Generation**: ~10-30 seconds per track
- **File Size**: ~1MB per minute of audio (WAV)
- **Supported Length**: 
  - TTS: Up to 40,000 characters (Flash models)
  - Music: Up to 120 seconds per generation

## Security

- ✅ API keys stored in `.env` (not committed)
- ✅ Environment variable support
- ✅ HTTPS for all API calls
- ✅ No sensitive data logged
- ✅ MIT License (open source)

## Requirements

- **Python**: 3.8 or higher
- **Audacity**: 3.0 or later (for import)
- **Internet**: Required for API calls
- **ElevenLabs Account**: API key needed
- **Disk Space**: ~100MB for dependencies + generated files

## Costs

This plugin uses ElevenLabs paid API:
- Text-to-Speech: ~$0.30 per 1000 characters
- Music Generation: ~$0.10 per generation
- Free tier available with limited credits

Check [ElevenLabs Pricing](https://elevenlabs.io/pricing) for current rates.

## Support

- 🐛 [Report Bugs](https://github.com/yourusername/audacity-cloudai/issues)
- 💡 [Feature Requests](https://github.com/yourusername/audacity-cloudai/issues)
- 💬 [Discussions](https://github.com/yourusername/audacity-cloudai/discussions)
- 📧 Email: your.email@example.com

## License

MIT License - See [LICENSE](LICENSE) file

## Credits

- **Audacity Team** - For the amazing audio editor
- **ElevenLabs** - For the powerful AI APIs
- **Intel OpenVINO Team** - For plugin architecture inspiration
- **Contributors** - Everyone who helps improve this project

## Next Steps

1. ⭐ **Star the repository** if you find it useful
2. 📖 **Read the [Quick Start](QUICKSTART.md)** guide
3. 🎵 **Generate your first audio**
4. 🤝 **Contribute** improvements
5. 📣 **Share** with the community

---

**Ready to create amazing audio with AI?** 🚀

Start here: `python audacity_cloudai.py check-config`
