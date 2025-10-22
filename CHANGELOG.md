# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0] - 2025-10-22

### 🎉 Initial Release

#### Added
- **Text-to-Speech Generation**
  - Support for 32+ languages
  - Multiple models (Flash v2.5, Turbo v2.5, Multilingual v2)
  - Customizable voice settings (stability, similarity, style)
  - 9+ pre-configured popular voices
  - Voice Library integration

- **Music Generation**
  - Text-to-music from prompts
  - Composition plan support
  - Adjustable duration (10-120 seconds)
  - Genre, mood, and instrument control

- **Sound Effects Generation**
  - Realistic SFX from text descriptions
  - Adjustable prompt influence
  - Auto or manual duration control
  - 10+ example prompts included

- **Voice Isolation**
  - Background noise removal
  - Voice isolation from music/ambient sounds
  - Batch processing support
  - Compare before/after functionality

- **Audio Transcription**
  - Speech-to-text with 99 languages
  - Speaker diarization (identify speakers)
  - Multiple export formats (TXT, JSON, SRT)
  - Audacity label track creation
  - Word-level timestamps

- **Voice Library Browser**
  - Browse thousands of community voices
  - Search and filter by gender, age, accent, language
  - Preview voices
  - Add voices to account
  - Interactive browsing mode

- **Dual Interface System**
  - Command-Line Interface (CLI) for power users
  - Graphical User Interface (GUI) for ease of use
  - Setup wizard with API key validation
  - 5-tab GUI with all features
  - Settings dialog
  - Progress indicators

- **Comprehensive Documentation**
  - Main README
  - GUI User Guide
  - Extended Documentation
  - Installation Guide
  - Quick Start Tutorial
  - Contributing Guidelines
  - Feature Summary
  - Ethics Guidelines

- **Cross-Platform Support**
  - Windows
  - macOS
  - Linux

#### Technical
- Complete ElevenLabs API wrapper
- Audio format conversion (MP3 ↔ WAV)
- Threaded operations (non-blocking GUI)
- Error handling and validation
- Configuration management
- Audacity integration utilities

#### Developer Experience
- Unit test templates
- Example workflows
- GitHub Actions CI/CD
- Automated releases
- PyPI package support

### Ethics & Best Practices
- Neutral voice defaults
- Transparent Voice Library integration
- Creator attribution
- User choice emphasized
- Revenue disclosure guidelines

---

## [Unreleased]

### Planned Features
- Native Audacity plugin integration
- Real-time audio preview
- Voice cloning interface
- Preset system
- Project templates
- VST/AU plugin version

### Under Consideration
- Mobile app integration
- Cloud storage sync
- Collaborative features
- Custom voice training
- Advanced audio processing

---

**Full Changelog**: https://github.com/BjornBrorsson/cloud-audio-ai-audacity-addon/commits/main
