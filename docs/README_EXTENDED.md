# Audacity Cloud AI Plugin - Extended Features 🎵☁️

Complete AI audio toolkit for Audacity using ElevenLabs Cloud APIs

## Features Overview

### 🎤 Text-to-Speech Generation
- Convert text to lifelike speech using AI voices
- **32+ languages** supported
- **Multiple models**: Flash (fast), Turbo (balanced), Multilingual (high quality)
- **Voice Library integration** - Browse and discover thousands of voices
- Customizable voice settings (stability, similarity, style)
- Batch generation with merging

### 🎶 Music Generation
- Generate original music from text descriptions
- Control genre, mood, instruments, and tempo
- **Composition plan support** for structured music
- Multi-scene soundtrack creation
- Loopable music generation

### 🔊 Sound Effects Generation **[NEW]**
- Create realistic sound effects from text
- Nature sounds, UI sounds, ambience, and more
- Adjustable prompt influence
- Perfect for games, videos, and podcasts

### 🎧 Voice Isolator **[NEW]**
- Remove background noise from audio
- Isolate voice from music/ambient sounds
- Clean up podcast recordings
- Improve voice quality automatically

### 📝 Audio Transcription **[NEW]**
- Convert speech to text with **99 languages** support
- **Speaker diarization** - identify who's speaking
- Export as text, JSON, or SRT subtitles
- **Audacity label track** creation
- Word-level timestamps

### 👥 Voice Library Browser **[NEW]**
- Browse thousands of community voices
- Search by gender, age, accent, language
- Filter by use case (narration, characters, etc.)
- Preview and add voices to your account
- Interactive browser mode

## Ethical Considerations for Voice Library

### ⚠️ Important Note About Professional Voice Clones

Some voices in the ElevenLabs Voice Library are **Professional Voice Clones** (PVC) where the creator receives compensation when their voice is used. This includes revenue sharing for API usage.

**Our approach:**
- ✅ **Neutral defaults**: We use ElevenLabs' standard free voices as defaults
- ✅ **Transparent discovery**: Voice Library browser shows all available voices
- ✅ **User choice**: You decide which voices to use
- ✅ **Full disclosure**: Voice creators are clearly credited

**If you're a plugin developer with PVCs:**
- **Don't** set your revenue-generating voices as hard-coded defaults
- **Do** include them in the browsable Voice Library
- **Do** disclose any financial relationships in your documentation
- **Do** let users discover and choose voices themselves

This maintains user trust and follows best practices for open-source projects.

## Installation

See [INSTALLATION.md](INSTALLATION.md) for detailed setup instructions.

**Quick setup:**
```bash
pip install -r requirements.txt
cp .env.example .env
# Edit .env and add your ELEVENLABS_API_KEY
python audacity_cloudai.py check-config
```

## New Commands

### Sound Effects
```bash
# Generate a sound effect
python audacity_cloudai.py sfx "Thunder and rain" -o storm.wav

# With duration
python audacity_cloudai.py sfx "Dog barking" -d 5 -o bark.wav

# Show examples
python audacity_cloudai.py example-prompts
```

### Voice Isolation
```bash
# Clean up audio
python audacity_cloudai.py isolate noisy_recording.wav -o clean.wav

# Batch process multiple files
python audacity_cloudai.py isolate *.wav --output cleaned/
```

### Transcription
```bash
# Basic transcription
python audacity_cloudai.py transcribe interview.mp3 -o transcript.txt

# With speaker diarization
python audacity_cloudai.py transcribe meeting.mp3 \
  --speakers \
  --num-speakers 3 \
  -o meeting_transcript.txt

# Create Audacity labels
python audacity_cloudai.py transcribe podcast.mp3 \
  --labels \
  -o transcript.txt

# Export as subtitles
python audacity_cloudai.py transcribe video.mp3 -o subtitles.srt
```

### Voice Library
```bash
# Browse featured voices
python audacity_cloudai.py voices --featured

# Search for specific voices
python audacity_cloudai.py voices --search "deep male narrator"

# Interactive browser
python audacity_cloudai.py voices --interactive
```

## Complete Workflow Examples

### Podcast Production
```bash
# 1. Generate intro
python audacity_cloudai.py tts "Welcome to Tech Talk Podcast" \
  -v 21m00Tcm4TlvDq8ikWAM \
  -o intro.wav

# 2. Generate background music
python audacity_cloudai.py music "Upbeat podcast intro music" \
  -d 10 \
  -o music.wav

# 3. Clean up interview recording
python audacity_cloudai.py isolate interview_raw.wav \
  -o interview_clean.wav

# 4. Transcribe the interview
python audacity_cloudai.py transcribe interview_clean.wav \
  --speakers \
  --labels \
  -o transcript.txt

# 5. Generate outro
python audacity_cloudai.py tts "Thanks for listening! Subscribe for more." \
  -v 21m00Tcm4TlvDq8ikWAM \
  -o outro.wav

# Import all files into Audacity for final mixing
```

### YouTube Video
```bash
# 1. Generate narration
python audacity_cloudai.py tts "In this tutorial..." \
  -v pNInz6obpgDQGcFmaJgB \
  --stability 0.7 \
  -o narration.wav

# 2. Generate background music
python audacity_cloudai.py music "Calm educational background music" \
  -d 300 \
  -o bg_music.wav

# 3. Generate transition sounds
python audacity_cloudai.py sfx "Smooth transition whoosh" \
  -o transition.wav

# 4. Create subtitles from narration
python audacity_cloudai.py transcribe narration.wav \
  -o subtitles.srt
```

### Game Development
```bash
# Generate character voices
python audacity_cloudai.py tts "Hero: Let's go!" -v hero_voice -o hero_1.wav
python audacity_cloudai.py tts "Villain: You'll never win!" -v villain_voice -o villain_1.wav

# Generate sound effects
python audacity_cloudai.py sfx "Sword swinging through air" -o sword_swing.wav
python audacity_cloudai.py sfx "Explosion impact" -o explosion.wav
python audacity_cloudai.py sfx "Medieval door creaking open" -o door.wav

# Generate background music
python audacity_cloudai.py music "Epic battle music" -d 120 -o battle.wav
python audacity_cloudai.py music "Calm village ambience" -d 180 -o village.wav
```

## API Costs

All features use the ElevenLabs API (paid service):

| Feature | Approximate Cost |
|---------|-----------------|
| Text-to-Speech | $0.30 per 1,000 characters |
| Music Generation | $0.10 per generation |
| Sound Effects | $0.05 per generation |
| Voice Isolation | $0.15 per minute |
| Transcription | $0.25 per minute |
| Voice Library | Free to browse |

Check [ElevenLabs Pricing](https://elevenlabs.io/pricing) for current rates and free tier details.

## File Structure

```
audacity-cloudai/
├── src/
│   ├── generators/
│   │   ├── text_to_speech.py     # TTS generator
│   │   ├── music_generator.py    # Music generator
│   │   └── sound_effects.py      # Sound effects [NEW]
│   ├── effects/
│   │   └── voice_isolator.py     # Voice isolation [NEW]
│   ├── analyzers/
│   │   └── transcription.py      # Transcription [NEW]
│   └── utils/
│       ├── voice_library.py      # Voice browser [NEW]
│       ├── elevenlabs_api.py     # API wrapper
│       ├── config.py              # Configuration
│       └── audio_utils.py         # Audio utilities
├── nyquist/                       # Audacity plugins
├── examples/                      # Example scripts
├── tests/                         # Unit tests
└── audacity_cloudai.py           # Main CLI

```

## Use Cases

1. **Podcasts** - Generate intros, outros, transcribe episodes, clean audio
2. **YouTube Videos** - Narration, music, sound effects, subtitles
3. **Audiobooks** - Professional narration with transcription
4. **Game Development** - Character voices, sound effects, music
5. **E-Learning** - Lesson narration, background music, transcripts
6. **Accessibility** - Create transcripts and subtitles for videos
7. **Music Production** - Generate backing tracks and loops
8. **Sound Design** - Create custom sound effects for any project

## All Available Commands

```bash
# Generation
python audacity_cloudai.py tts "text" -o file.wav
python audacity_cloudai.py music "description" -d 30 -o file.wav
python audacity_cloudai.py sfx "description" -o file.wav

# Processing
python audacity_cloudai.py isolate input.wav -o output.wav
python audacity_cloudai.py transcribe input.wav -o transcript.txt

# Discovery
python audacity_cloudai.py voices --interactive
python audacity_cloudai.py voices --search "query"
python audacity_cloudai.py voices --featured

# Utilities
python audacity_cloudai.py list-voices
python audacity_cloudai.py list-models
python audacity_cloudai.py example-prompts
python audacity_cloudai.py check-config
```

## Documentation

- 📖 [Installation Guide](INSTALLATION.md) - Setup instructions
- 🚀 [Quick Start](QUICKSTART.md) - Quick tutorials
- 🤝 [Contributing](CONTRIBUTING.md) - How to contribute
- 📋 [Project Overview](PROJECT_OVERVIEW.md) - Technical details

## Roadmap

- [x] Text-to-Speech
- [x] Music Generation
- [x] Sound Effects **NEW**
- [x] Voice Isolation **NEW**
- [x] Transcription **NEW**
- [x] Voice Library Browser **NEW**
- [ ] Native Audacity integration
- [ ] GUI application
- [ ] Voice cloning interface
- [ ] Real-time preview
- [ ] Preset system

## Support

- 🐛 [Report Issues](https://github.com/yourusername/audacity-cloudai/issues)
- 💬 [Discussions](https://github.com/yourusername/audacity-cloudai/discussions)
- 📧 Email: your.email@example.com

## License

MIT License - See [LICENSE](LICENSE)

## Credits

- **ElevenLabs** - AI voice and audio APIs
- **Audacity** - Open-source audio editor
- **Community** - Voice creators and contributors

---

**Made with ❤️ for the Audacity and AI community**

*Note: This is an independent project and not officially affiliated with Audacity or ElevenLabs.*
