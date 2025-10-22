#!/usr/bin/env python3
"""
Demo script showcasing Audacity Cloud AI capabilities
"""

import sys
from pathlib import Path

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / 'src'))

from generators import TextToSpeechGenerator, MusicGenerator
from utils import Config


def demo_text_to_speech():
    """Demonstrate text-to-speech generation."""
    print("\n" + "="*80)
    print("📢 Text-to-Speech Demo")
    print("="*80)
    
    generator = TextToSpeechGenerator()
    
    # Example 1: Simple generation
    print("\n1️⃣ Simple voice generation...")
    audio = generator.generate(
        text="Welcome to the Audacity Cloud AI plugin demo!",
        output_file=Config.TEMP_DIR / "demo_simple.wav"
    )
    print(f"   ✓ Generated: {Config.TEMP_DIR / 'demo_simple.wav'}")
    
    # Example 2: Different voice
    print("\n2️⃣ Using different voice (Adam - deep male)...")
    audio = generator.generate(
        text="This is a different voice speaking.",
        voice_id="pNInz6obpgDQGcFmaJgB",  # Adam
        output_file=Config.TEMP_DIR / "demo_adam.wav"
    )
    print(f"   ✓ Generated: {Config.TEMP_DIR / 'demo_adam.wav'}")
    
    # Example 3: Custom settings
    print("\n3️⃣ Expressive voice with custom settings...")
    audio = generator.generate(
        text="This demonstrates expressive speech with more variation!",
        stability=0.3,      # Lower = more variation
        similarity_boost=0.8,
        style=0.7,          # Higher = more expressive
        output_file=Config.TEMP_DIR / "demo_expressive.wav"
    )
    print(f"   ✓ Generated: {Config.TEMP_DIR / 'demo_expressive.wav'}")
    
    # Example 4: Batch generation
    print("\n4️⃣ Batch generating multiple segments...")
    texts = [
        "First segment of narration.",
        "Second segment continues the story.",
        "Final segment concludes."
    ]
    segments = generator.batch_generate(texts, merge=True)
    
    from utils import AudioConverter
    converter = AudioConverter()
    converter.save_to_file(
        segments[0],
        Config.TEMP_DIR / "demo_batch.wav",
        format='wav'
    )
    print(f"   ✓ Generated merged: {Config.TEMP_DIR / 'demo_batch.wav'}")
    
    print("\n✅ Text-to-Speech demos completed!")
    print(f"   Files saved to: {Config.TEMP_DIR}")


def demo_music_generation():
    """Demonstrate music generation."""
    print("\n" + "="*80)
    print("🎵 Music Generation Demo")
    print("="*80)
    
    generator = MusicGenerator()
    
    # Example 1: Simple music generation
    print("\n1️⃣ Generating upbeat electronic music...")
    audio = generator.generate(
        prompt="Upbeat electronic dance music with energetic drums",
        duration=20,
        output_file=Config.TEMP_DIR / "demo_edm.wav"
    )
    print(f"   ✓ Generated: {Config.TEMP_DIR / 'demo_edm.wav'}")
    
    # Example 2: Ambient music
    print("\n2️⃣ Generating calm ambient soundscape...")
    audio = generator.generate(
        prompt="Calm ambient soundscape with soft pads and gentle piano",
        duration=30,
        output_file=Config.TEMP_DIR / "demo_ambient.wav"
    )
    print(f"   ✓ Generated: {Config.TEMP_DIR / 'demo_ambient.wav'}")
    
    # Example 3: Detailed generation
    print("\n3️⃣ Generating with detailed specifications...")
    audio = generator.generate_with_details(
        prompt="Cinematic background music",
        genre="orchestral",
        mood="epic",
        instruments=["strings", "brass", "percussion"],
        tempo="moderate",
        duration=25,
        output_file=Config.TEMP_DIR / "demo_cinematic.wav"
    )
    print(f"   ✓ Generated: {Config.TEMP_DIR / 'demo_cinematic.wav'}")
    
    # Example 4: Creating a soundtrack
    print("\n4️⃣ Creating multi-scene soundtrack...")
    scenes = [
        {"prompt": "Mysterious intro with piano", "duration": 15},
        {"prompt": "Building tension with strings", "duration": 15},
        {"prompt": "Epic climax with full orchestra", "duration": 20}
    ]
    audio = generator.create_soundtrack(
        scenes=scenes,
        merge=True,
        crossfade_ms=2000
    )
    
    from utils import AudioConverter
    converter = AudioConverter()
    converter.save_to_file(
        audio,
        Config.TEMP_DIR / "demo_soundtrack.wav",
        format='wav'
    )
    print(f"   ✓ Generated soundtrack: {Config.TEMP_DIR / 'demo_soundtrack.wav'}")
    
    print("\n✅ Music generation demos completed!")
    print(f"   Files saved to: {Config.TEMP_DIR}")


def demo_podcast_workflow():
    """Demonstrate a complete podcast workflow."""
    print("\n" + "="*80)
    print("🎙️ Podcast Workflow Demo")
    print("="*80)
    
    tts = TextToSpeechGenerator()
    music = MusicGenerator()
    
    # Step 1: Generate intro
    print("\n1️⃣ Generating podcast intro...")
    intro = tts.generate(
        text="Welcome to Tech Talk, the podcast where we discuss the latest in technology.",
        output_file=Config.TEMP_DIR / "podcast_intro.wav"
    )
    print(f"   ✓ Intro saved")
    
    # Step 2: Generate background music
    print("\n2️⃣ Generating background music...")
    bg_music = music.generate(
        prompt="Upbeat tech podcast intro music",
        duration=10,
        output_file=Config.TEMP_DIR / "podcast_music.wav"
    )
    print(f"   ✓ Music saved")
    
    # Step 3: Generate main content
    print("\n3️⃣ Generating main content...")
    content = tts.generate(
        text="Today we're exploring artificial intelligence and its impact on creative industries.",
        output_file=Config.TEMP_DIR / "podcast_content.wav"
    )
    print(f"   ✓ Content saved")
    
    # Step 4: Generate outro
    print("\n4️⃣ Generating outro...")
    outro = tts.generate(
        text="Thanks for listening! Don't forget to subscribe.",
        output_file=Config.TEMP_DIR / "podcast_outro.wav"
    )
    print(f"   ✓ Outro saved")
    
    print("\n✅ Podcast workflow completed!")
    print("\n📝 Next steps:")
    print("   1. Open Audacity")
    print("   2. Import all generated files:")
    print(f"      - {Config.TEMP_DIR / 'podcast_intro.wav'}")
    print(f"      - {Config.TEMP_DIR / 'podcast_music.wav'}")
    print(f"      - {Config.TEMP_DIR / 'podcast_content.wav'}")
    print(f"      - {Config.TEMP_DIR / 'podcast_outro.wav'}")
    print("   3. Arrange, mix, and add effects")
    print("   4. Export your final podcast!")


def main():
    """Run all demos."""
    print("\n" + "🌟"*40)
    print("   Audacity Cloud AI - Feature Demonstration")
    print("🌟"*40)
    
    try:
        # Validate configuration
        Config.validate()
        print(f"\n✓ Configuration validated")
        print(f"✓ Output directory: {Config.TEMP_DIR}")
        
        # Run demos
        choice = input("\nSelect demo:\n"
                      "  1 - Text-to-Speech\n"
                      "  2 - Music Generation\n"
                      "  3 - Podcast Workflow\n"
                      "  4 - All Demos\n"
                      "Choice (1-4): ").strip()
        
        if choice == '1':
            demo_text_to_speech()
        elif choice == '2':
            demo_music_generation()
        elif choice == '3':
            demo_podcast_workflow()
        elif choice == '4':
            demo_text_to_speech()
            demo_music_generation()
            demo_podcast_workflow()
        else:
            print("Invalid choice")
            return
        
        print("\n" + "🌟"*40)
        print("   All demos completed successfully!")
        print("🌟"*40)
        print(f"\n📁 Output files: {Config.TEMP_DIR}")
        print("\n💡 Import these files into Audacity to edit and mix them!")
        
    except Exception as e:
        print(f"\n❌ Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == '__main__':
    main()
