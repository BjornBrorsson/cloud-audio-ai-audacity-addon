"""Sound effects generator using ElevenLabs API."""

import sys
from pathlib import Path
from typing import Optional
from datetime import datetime

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from utils import Config, ElevenLabsAPI, AudioConverter


class SoundEffectsGenerator:
    """Generator for creating sound effects using ElevenLabs."""
    
    def __init__(self, api_key: Optional[str] = None):
        """
        Initialize the sound effects generator.
        
        Args:
            api_key: ElevenLabs API key (optional)
        """
        Config.validate()
        self.api = ElevenLabsAPI(api_key)
        self.converter = AudioConverter()
    
    def generate(
        self,
        description: str,
        duration_seconds: Optional[float] = None,
        prompt_influence: float = 0.3,
        output_file: Optional[Path] = None,
        return_wav: bool = True
    ) -> bytes:
        """
        Generate sound effects from text description.
        
        Args:
            description: Description of the sound effect
            duration_seconds: Target duration (auto-determined if None)
            prompt_influence: How much to follow the prompt (0.0-1.0)
            output_file: Optional path to save the audio
            return_wav: Return as WAV format (True) or MP3 (False)
            
        Returns:
            Audio data as bytes (WAV or MP3)
        """
        print(f"Generating sound effect: '{description}'")
        if duration_seconds:
            print(f"Target duration: {duration_seconds} seconds")
        
        # Generate sound effect
        mp3_data = self.api.generate_sound_effects(
            text=description,
            duration_seconds=duration_seconds,
            prompt_influence=prompt_influence
        )
        
        print(f"Sound effect generated successfully ({len(mp3_data)} bytes)")
        
        # Convert to WAV if requested
        if return_wav:
            print("Converting to WAV format...")
            audio_data = self.converter.mp3_to_wav(mp3_data)
            format_ext = 'wav'
        else:
            audio_data = mp3_data
            format_ext = 'mp3'
        
        # Save to file if specified
        if output_file:
            self.converter.save_to_file(audio_data, output_file, format=format_ext)
            print(f"Sound effect saved to: {output_file}")
        
        return audio_data
    
    def batch_generate(
        self,
        descriptions: list[str],
        merge: bool = False,
        **kwargs
    ) -> list[bytes]:
        """
        Generate multiple sound effects.
        
        Args:
            descriptions: List of sound effect descriptions
            merge: Merge all effects into one audio file
            **kwargs: Additional generation parameters
            
        Returns:
            List of audio data bytes (or single merged audio if merge=True)
        """
        audio_segments = []
        
        for i, description in enumerate(descriptions, 1):
            print(f"Generating sound effect {i}/{len(descriptions)}...")
            audio = self.generate(
                description=description,
                **kwargs
            )
            audio_segments.append(audio)
        
        if merge and len(audio_segments) > 1:
            print("Merging sound effects...")
            merged = self.converter.merge_audio(audio_segments, crossfade_ms=50)
            return [merged]
        
        return audio_segments
    
    def get_example_prompts(self) -> dict[str, str]:
        """
        Get example sound effect prompts.
        
        Returns:
            Dictionary of category: prompt examples
        """
        return {
            'Nature': "Birds chirping in a forest at dawn",
            'Ambience': "Busy city street with traffic and people talking",
            'UI': "Smooth button click sound",
            'Game': "Sword swinging through the air",
            'Sci-Fi': "Futuristic spaceship engine humming",
            'Horror': "Creaking door in an old mansion",
            'Footsteps': "Footsteps walking on gravel",
            'Weather': "Heavy rain with distant thunder",
            'Animals': "Dog barking in the distance",
            'Mechanical': "Old clock ticking rhythmically"
        }


def main():
    """Command-line interface for the sound effects generator."""
    import argparse
    
    parser = argparse.ArgumentParser(description='Generate sound effects using ElevenLabs')
    parser.add_argument('description', help='Description of the sound effect')
    parser.add_argument('-o', '--output', type=Path, help='Output file path')
    parser.add_argument('-d', '--duration', type=float, help='Duration in seconds')
    parser.add_argument('--influence', type=float, default=0.3, help='Prompt influence (0.0-1.0)')
    parser.add_argument('--examples', action='store_true', help='Show example prompts')
    
    args = parser.parse_args()
    
    try:
        generator = SoundEffectsGenerator()
        
        if args.examples:
            print("\nExample sound effect prompts:")
            examples = generator.get_example_prompts()
            for category, prompt in examples.items():
                print(f"\n{category}:")
                print(f"  {prompt}")
            return
        
        # Set default output path if not specified
        if not args.output:
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            args.output = Config.TEMP_DIR / f"sfx_{timestamp}.wav"
        
        # Generate sound effect
        audio = generator.generate(
            description=args.description,
            duration_seconds=args.duration,
            prompt_influence=args.influence,
            output_file=args.output
        )
        
        duration = generator.converter.get_duration(audio)
        print(f"\nSuccess! Generated {duration:.2f} seconds of sound effects")
        print(f"Output: {args.output}")
        
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == '__main__':
    main()
