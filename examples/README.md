# Examples

This directory contains example scripts and workflows for the Audacity Cloud AI plugin.

## Running the Demo

```bash
python examples/demo.py
```

This interactive demo showcases:
- Text-to-Speech with various voices and settings
- Music generation with different styles
- Complete podcast workflow

## Example Use Cases

### 1. Podcast Production

Generate intro, outro, and background music:

```bash
# Intro voice
python audacity_cloudai.py tts "Welcome to my podcast" -o intro.wav

# Background music
python audacity_cloudai.py music "Upbeat podcast intro music" -d 10 -o bg.wav

# Outro
python audacity_cloudai.py tts "Thanks for listening" -o outro.wav
```

### 2. YouTube Video Narration

```bash
# Professional narration
python audacity_cloudai.py tts "In this tutorial, we'll learn about..." \
  -v pNInz6obpgDQGcFmaJgB \
  --stability 0.7 \
  -o narration.wav
```

### 3. Game Soundtrack

```bash
# Menu music
python audacity_cloudai.py music "Calm ambient menu music" -d 120 -o menu.wav

# Action music
python audacity_cloudai.py music "Fast-paced action game music" -d 90 -o action.wav

# Game over music
python audacity_cloudai.py music "Sad game over music" -d 15 -o gameover.wav
```

### 4. Audiobook Production

```bash
# Chapter narration
python audacity_cloudai.py tts "Chapter 1: The Beginning..." \
  -v 21m00Tcm4TlvDq8ikWAM \
  --stability 0.8 \
  -o chapter1.wav
```

### 5. Educational Content

```bash
# Lesson narration
python audacity_cloudai.py tts "Today we'll learn about photosynthesis" \
  -v EXAVITQu4vr4xnSDxMaL \
  -o lesson.wav

# Background music
python audacity_cloudai.py music "Calm educational background music" \
  -d 300 -o lesson_bg.wav
```

## Tips

1. **Always preview** different voices before settling on one
2. **Use consistent settings** across a project for cohesive sound
3. **Generate music first** to set the mood, then match narration
4. **Save presets** by documenting your favorite voice IDs and settings
5. **Import to Audacity** for final mixing and effects

## More Examples

Check the plugin's `src/generators/` directory for code examples of:
- Batch generation
- Custom voice settings
- Multi-scene soundtracks
- Looping music

## Community Examples

Share your creations! We'd love to feature your work.
