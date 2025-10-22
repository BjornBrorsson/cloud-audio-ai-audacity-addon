#!/usr/bin/env python3
"""
Audacity Cloud AI - Standalone Interface
Main entry point for generating audio using ElevenLabs APIs
"""

import sys
import argparse
from pathlib import Path

# Add src to path
sys.path.insert(0, str(Path(__file__).parent / 'src'))

from generators import TextToSpeechGenerator, MusicGenerator, SoundEffectsGenerator
from effects import VoiceIsolator
from analyzers import AudioTranscriber
from utils import Config, VoiceLibraryBrowser


def text_to_speech_command(args):
    """Handle text-to-speech generation."""
    generator = TextToSpeechGenerator()
    
    audio = generator.generate(
        text=args.text,
        voice_id=args.voice,
        model_id=args.model,
        stability=args.stability,
        similarity_boost=args.similarity,
        style=args.style,
        output_file=args.output
    )
    
    duration = generator.converter.get_duration(audio)
    print(f"\n✓ Success! Generated {duration:.2f} seconds of speech")
    if args.output:
        print(f"✓ Saved to: {args.output}")


def music_generation_command(args):
    """Handle music generation."""
    generator = MusicGenerator()
    
    audio = generator.generate(
        prompt=args.prompt,
        duration=args.duration,
        output_file=args.output,
        use_composition_plan=args.use_plan
    )
    
    duration = generator.converter.get_duration(audio)
    print(f"\n✓ Success! Generated {duration:.2f} seconds of music")
    if args.output:
        print(f"✓ Saved to: {args.output}")


def sound_effects_command(args):
    """Handle sound effects generation."""
    generator = SoundEffectsGenerator()
    
    audio = generator.generate(
        description=args.description,
        duration_seconds=args.duration,
        prompt_influence=args.influence,
        output_file=args.output
    )
    
    duration = generator.converter.get_duration(audio)
    print(f"\n✓ Success! Generated {duration:.2f} seconds of sound effects")
    if args.output:
        print(f"✓ Saved to: {args.output}")


def voice_isolator_command(args):
    """Handle voice isolation."""
    isolator = VoiceIsolator()
    
    isolated = isolator.isolate(
        input_file=args.input,
        output_file=args.output
    )
    
    duration = isolator.converter.get_duration(isolated)
    print(f"\n✓ Success! Isolated {duration:.2f} seconds of voice")
    if args.output:
        print(f"✓ Saved to: {args.output}")


def transcription_command(args):
    """Handle audio transcription."""
    transcriber = AudioTranscriber()
    
    result = transcriber.transcribe(
        input_file=args.input,
        language=args.language,
        enable_speaker_diarization=args.speakers,
        num_speakers=args.num_speakers,
        output_file=args.output,
        create_audacity_labels=args.labels
    )
    
    print(f"\n✓ Transcription complete!")
    if args.output:
        print(f"✓ Saved to: {args.output}")


def voice_library_command(args):
    """Handle voice library browsing."""
    browser = VoiceLibraryBrowser()
    
    if args.interactive:
        browser.browse_interactive()
    elif args.search:
        result = browser.search_voices(query=args.search)
        browser.display_voices(result.get('voices', []), show_details=True)
    elif args.featured:
        result = browser.get_featured_voices()
        browser.display_voices(result.get('voices', []), show_details=True)
    else:
        # Default: show some featured voices
        result = browser.get_featured_voices()
        browser.display_voices(result.get('voices', [])[:10], show_details=True)
        print("\n💡 Tip: Use --search, --interactive, or --featured for more options")


def list_voices_command(args):
    """List available voices."""
    generator = TextToSpeechGenerator()
    voices = generator.get_available_voices()
    
    print("\n📢 Available ElevenLabs Voices:")
    print("=" * 80)
    
    for voice in voices:
        print(f"\nVoice ID: {voice['voice_id']}")
        print(f"Name: {voice['name']}")
        if 'description' in voice:
            print(f"Description: {voice['description']}")
        if 'labels' in voice:
            labels = voice['labels']
            print(f"Labels: {', '.join([f'{k}: {v}' for k, v in labels.items()])}")


def list_models_command(args):
    """List available models."""
    models = Config.get_available_models()
    
    print("\n🤖 Available Models:")
    print("=" * 80)
    
    for model_id, info in models.items():
        print(f"\nModel: {info['name']} ({model_id})")
        print(f"Description: {info['description']}")
        print(f"Use case: {info['use_case']}")
        if info['languages'] != 'N/A':
            print(f"Languages: {info['languages']}")


def example_prompts_command(args):
    """Show example music and sound effect prompts."""
    print("\n🎵 Example Music Prompts:")
    print("=" * 80)
    
    music_gen = MusicGenerator()
    music_examples = music_gen.get_example_prompts()
    for category, prompt in music_examples.items():
        print(f"\n{category}:")
        print(f"  {prompt}")
    
    print("\n\n🔊 Example Sound Effect Prompts:")
    print("=" * 80)
    
    sfx_gen = SoundEffectsGenerator()
    sfx_examples = sfx_gen.get_example_prompts()
    for category, prompt in sfx_examples.items():
        print(f"\n{category}:")
        print(f"  {prompt}")


def check_config_command(args):
    """Check configuration."""
    print("\n⚙️  Configuration Check:")
    print("=" * 80)
    
    try:
        Config.validate()
        print("✓ API Key: Found")
    except ValueError as e:
        print(f"✗ API Key: {e}")
        return
    
    print(f"✓ API URL: {Config.ELEVENLABS_API_URL}")
    print(f"✓ Default Voice: {Config.DEFAULT_VOICE_ID}")
    print(f"✓ Default Model: {Config.DEFAULT_MODEL}")
    print(f"✓ Temp Directory: {Config.TEMP_DIR}")
    print(f"✓ Cache Directory: {Config.CACHE_DIR}")
    
    # Try to get user info
    try:
        from utils import ElevenLabsAPI
        api = ElevenLabsAPI()
        user_info = api.get_user_info()
        print(f"\n👤 Account Info:")
        print(f"  Subscription: {user_info.get('subscription', {}).get('tier', 'N/A')}")
        
        if 'subscription' in user_info and 'character_count' in user_info['subscription']:
            char_count = user_info['subscription']['character_count']
            char_limit = user_info['subscription']['character_limit']
            print(f"  Characters used: {char_count} / {char_limit}")
            
    except Exception as e:
        print(f"⚠ Could not fetch account info: {e}")


def main():
    """Main command-line interface."""
    parser = argparse.ArgumentParser(
        description='Audacity Cloud AI - Generate speech and music using ElevenLabs',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Generate speech
  python audacity_cloudai.py tts "Hello world" -o hello.wav
  
  # Generate music
  python audacity_cloudai.py music "Upbeat electronic dance music" -d 30 -o dance.wav
  
  # Generate sound effects
  python audacity_cloudai.py sfx "Thunder and rain" -o storm.wav
  
  # Isolate voice from audio
  python audacity_cloudai.py isolate input.wav -o clean.wav
  
  # Transcribe audio
  python audacity_cloudai.py transcribe podcast.mp3 -o transcript.txt
  
  # Browse voice library
  python audacity_cloudai.py voices --interactive
  
  # Check configuration
  python audacity_cloudai.py check-config
        """
    )
    
    subparsers = parser.add_subparsers(dest='command', help='Command to execute')
    
    # Text-to-speech command
    tts_parser = subparsers.add_parser('tts', help='Generate speech from text')
    tts_parser.add_argument('text', help='Text to convert to speech')
    tts_parser.add_argument('-o', '--output', type=Path, help='Output file path (.wav)')
    tts_parser.add_argument('-v', '--voice', default=Config.DEFAULT_VOICE_ID, help='Voice ID')
    tts_parser.add_argument('-m', '--model', default=Config.DEFAULT_MODEL, help='Model ID')
    tts_parser.add_argument('--stability', type=float, default=Config.DEFAULT_STABILITY, help='Stability (0.0-1.0)')
    tts_parser.add_argument('--similarity', type=float, default=Config.DEFAULT_SIMILARITY_BOOST, help='Similarity boost (0.0-1.0)')
    tts_parser.add_argument('--style', type=float, default=Config.DEFAULT_STYLE, help='Style (0.0-1.0)')
    
    # Music generation command
    music_parser = subparsers.add_parser('music', help='Generate music from prompt')
    music_parser.add_argument('prompt', help='Music description')
    music_parser.add_argument('-o', '--output', type=Path, help='Output file path (.wav)')
    music_parser.add_argument('-d', '--duration', type=int, default=30, help='Duration in seconds')
    music_parser.add_argument('--use-plan', action='store_true', help='Use composition plan')
    
    # Sound effects command
    sfx_parser = subparsers.add_parser('sfx', help='Generate sound effects')
    sfx_parser.add_argument('description', help='Sound effect description')
    sfx_parser.add_argument('-o', '--output', type=Path, help='Output file path (.wav)')
    sfx_parser.add_argument('-d', '--duration', type=float, help='Duration in seconds')
    sfx_parser.add_argument('--influence', type=float, default=0.3, help='Prompt influence (0.0-1.0)')
    
    # Voice isolator command
    isolate_parser = subparsers.add_parser('isolate', help='Isolate voice from audio')
    isolate_parser.add_argument('input', type=Path, help='Input audio file')
    isolate_parser.add_argument('-o', '--output', type=Path, help='Output file path (.wav)')
    
    # Transcription command
    transcribe_parser = subparsers.add_parser('transcribe', help='Transcribe audio to text')
    transcribe_parser.add_argument('input', type=Path, help='Input audio file')
    transcribe_parser.add_argument('-o', '--output', type=Path, help='Output file path (.txt/.json/.srt)')
    transcribe_parser.add_argument('-l', '--language', help='Language code (e.g., en, es)')
    transcribe_parser.add_argument('--speakers', action='store_true', help='Enable speaker diarization')
    transcribe_parser.add_argument('--num-speakers', type=int, help='Expected number of speakers')
    transcribe_parser.add_argument('--labels', action='store_true', help='Create Audacity labels')
    
    # Voice library command
    voices_parser = subparsers.add_parser('voices', help='Browse voice library')
    voices_parser.add_argument('-s', '--search', help='Search query')
    voices_parser.add_argument('--interactive', action='store_true', help='Interactive browser')
    voices_parser.add_argument('--featured', action='store_true', help='Show featured voices')
    
    # Utility commands
    subparsers.add_parser('list-voices', help='List your available voices')
    subparsers.add_parser('list-models', help='List available models')
    subparsers.add_parser('example-prompts', help='Show example prompts')
    subparsers.add_parser('check-config', help='Check configuration')
    
    args = parser.parse_args()
    
    if not args.command:
        parser.print_help()
        return
    
    try:
        # Route to appropriate command
        if args.command == 'tts':
            text_to_speech_command(args)
        elif args.command == 'music':
            music_generation_command(args)
        elif args.command == 'sfx':
            sound_effects_command(args)
        elif args.command == 'isolate':
            voice_isolator_command(args)
        elif args.command == 'transcribe':
            transcription_command(args)
        elif args.command == 'voices':
            voice_library_command(args)
        elif args.command == 'list-voices':
            list_voices_command(args)
        elif args.command == 'list-models':
            list_models_command(args)
        elif args.command == 'example-prompts':
            example_prompts_command(args)
        elif args.command == 'check-config':
            check_config_command(args)
        
    except Exception as e:
        print(f"\n❌ Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == '__main__':
    main()
