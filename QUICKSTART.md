# Quick Start Guide

Get up and running with Audacity Cloud AI in minutes!

## Prerequisites

✅ Installed Python 3.8+  
✅ Installed dependencies (`pip install -r requirements.txt`)  
✅ Created `.env` file with your ElevenLabs API key  

If you haven't done these steps, see [INSTALLATION.md](INSTALLATION.md).

## Basic Usage

### 1. Generate Your First Voice-Over

```bash
python audacity_cloudai.py tts "Welcome to my podcast" -o welcome.wav
```

This creates a WAV file you can import into Audacity!

### 2. Generate Music

```bash
python audacity_cloudai.py music "Upbeat electronic music" -d 30 -o music.wav
```

### 3. Import into Audacity

1. Open Audacity
2. **File → Import → Audio**
3. Select your generated file (e.g., `welcome.wav`)
4. Done! Edit, mix, and export as usual

## Common Workflows

### Podcast Intro

```bash
# Generate intro voice
python audacity_cloudai.py tts "Welcome to the Tech Talk podcast, I'm your host Alex" \
  -v 21m00Tcm4TlvDq8ikWAM -o intro.wav

# Generate background music
python audacity_cloudai.py music "Energetic podcast intro music" \
  -d 10 -o intro-music.wav
```

Then in Audacity:
1. Import both files
2. Use the **Time Shift Tool** to position them
3. Adjust volumes with the **Envelope Tool**
4. Add fade effects

### YouTube Video Narration

```bash
# Generate narration with a clear voice
python audacity_cloudai.py tts "In this video, we'll explore..." \
  -v pNInz6obpgDQGcFmaJgB \
  --stability 0.7 \
  --similarity 0.8 \
  -o narration.wav
```

### Background Music Loop

```bash
# Generate a 30-second loop
python audacity_cloudai.py music "Calm lo-fi hip hop beat for studying" \
  -d 30 -o loop.wav
```

In Audacity, use **Effect → Repeat** to loop the music as needed.

## Voice Selection

### List All Available Voices

```bash
python audacity_cloudai.py list-voices
```

### Popular Voices

| Voice ID | Name | Description |
|----------|------|-------------|
| `21m00Tcm4TlvDq8ikWAM` | Rachel | Calm, young female |
| `pNInz6obpgDQGcFmaJgB` | Adam | Deep, middle-aged male |
| `TxGEqnHWrfWFTfGW9XjX` | Josh | Deep, young male |
| `EXAVITQu4vr4xnSDxMaL` | Bella | Soft, young female |

### Try Different Voices

```bash
# Female voice
python audacity_cloudai.py tts "Hello world" -v 21m00Tcm4TlvDq8ikWAM -o rachel.wav

# Male voice
python audacity_cloudai.py tts "Hello world" -v pNInz6obpgDQGcFmaJgB -o adam.wav
```

## Advanced Options

### Fine-Tune Voice Settings

```bash
python audacity_cloudai.py tts "Important announcement" \
  --stability 0.8 \      # More consistent (0.0-1.0)
  --similarity 0.9 \     # More similar to original voice (0.0-1.0)
  --style 0.5 \          # More expressive (0.0-1.0)
  -o announcement.wav
```

**Tips:**
- **High stability** (0.7-0.9): Consistent, professional narration
- **Low stability** (0.3-0.5): More variation, emotional range
- **High similarity**: Closer to the original voice
- **Higher style**: More expressive (only works with v2 models)

### Choose Different Models

```bash
# Fast generation (low latency)
python audacity_cloudai.py tts "Quick test" -m eleven_flash_v2_5

# Highest quality
python audacity_cloudai.py tts "Professional voiceover" -m eleven_multilingual_v2

# Balanced
python audacity_cloudai.py tts "General purpose" -m eleven_turbo_v2_5
```

### Music Generation Options

```bash
# Basic
python audacity_cloudai.py music "Cinematic orchestral music" -d 45

# With composition plan (more controlled)
python audacity_cloudai.py music "Epic battle music" --use-plan -d 60
```

## Example Music Prompts

View pre-made examples:
```bash
python audacity_cloudai.py example-prompts
```

**Try these:**

```bash
# Lo-fi for videos
python audacity_cloudai.py music "Relaxing lo-fi hip hop beat with vinyl crackle" -d 30

# Game soundtrack
python audacity_cloudai.py music "Retro 8-bit video game music, upbeat adventure" -d 45

# Ambient background
python audacity_cloudai.py music "Calm ambient soundscape with soft pads" -d 60
```

## Workflow Tips

### 1. Batch Processing

Create a script to generate multiple segments:

```bash
# Windows (create batch.bat)
python audacity_cloudai.py tts "Segment 1 text" -o seg1.wav
python audacity_cloudai.py tts "Segment 2 text" -o seg2.wav
python audacity_cloudai.py tts "Segment 3 text" -o seg3.wav
```

### 2. Organize Your Files

Keep a folder structure:
```
my-project/
  ├── voice-overs/
  │   ├── intro.wav
  │   └── outro.wav
  ├── music/
  │   ├── background.wav
  │   └── transition.wav
  └── final/
      └── mixed.wav
```

### 3. Quality Settings

For best quality in Audacity:
1. **Project Rate**: Set to 44100 Hz (matches plugin output)
2. **Format**: 32-bit float for editing
3. **Export**: WAV or FLAC for lossless, MP3 for compressed

## Troubleshooting Quick Fixes

**Problem**: Audio sounds robotic  
**Solution**: Adjust stability lower (0.3-0.5) and increase style (0.5-0.7)

**Problem**: Music too repetitive  
**Solution**: Generate longer clips or use different prompts with more variation

**Problem**: Voice doesn't match expected gender/age  
**Solution**: Use `list-voices` to find appropriate voice and check the description

**Problem**: Generation fails  
**Solution**: Run `check-config` to verify API key and account status

## Next Steps

- Explore the full [README.md](README.md) for all features
- Check [INSTALLATION.md](INSTALLATION.md) for troubleshooting
- Read the [ElevenLabs Documentation](https://elevenlabs.io/docs) for API details

## Examples Repository

Check the `examples/` directory (if available) for:
- Sample scripts
- Template projects
- Audio demos

## Getting Help

- 💬 [Community Discussions](https://github.com/yourusername/audacity-cloudai/discussions)
- 🐛 [Report Issues](https://github.com/yourusername/audacity-cloudai/issues)
- 📖 [Full Documentation](README.md)

Happy creating! 🎵🎙️
