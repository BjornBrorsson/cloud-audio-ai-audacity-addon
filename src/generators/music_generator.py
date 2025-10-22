"""Music generator using ElevenLabs API."""

import sys
from pathlib import Path
from typing import Optional, Dict, Any, List
from datetime import datetime

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from utils import Config, ElevenLabsAPI, AudioConverter


class MusicGenerator:
    """Generator for creating music using ElevenLabs."""
    
    def __init__(self, api_key: Optional[str] = None):
        """
        Initialize the music generator.
        
        Args:
            api_key: ElevenLabs API key (optional)
        """
        Config.validate()
        self.api = ElevenLabsAPI(api_key)
        self.converter = AudioConverter()
    
    def generate(
        self,
        prompt: str,
        duration: Optional[int] = None,
        output_file: Optional[Path] = None,
        return_wav: bool = True,
        use_composition_plan: bool = False
    ) -> bytes:
        """
        Generate music from a text prompt.
        
        Args:
            prompt: Description of the music to generate
            duration: Duration in seconds (optional)
            output_file: Optional path to save the audio
            return_wav: Return as WAV format (True) or MP3 (False)
            use_composition_plan: Generate a composition plan first
            
        Returns:
            Audio data as bytes (WAV or MP3)
        """
        print(f"Generating music: '{prompt}'")
        if duration:
            print(f"Target duration: {duration} seconds")
        
        # Generate music
        if use_composition_plan:
            print("Creating composition plan...")
            plan = self.api.create_composition_plan(prompt)
            print(f"Composition plan created: {plan.get('description', 'N/A')}")
            
            mp3_data = self.api.generate_music_with_plan(
                prompt=prompt,
                composition_plan=plan
            )
        else:
            mp3_data = self.api.generate_music(
                prompt=prompt,
                duration=duration
            )
        
        print(f"Music generated successfully ({len(mp3_data)} bytes)")
        
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
            print(f"Music saved to: {output_file}")
        
        return audio_data
    
    def generate_with_details(
        self,
        prompt: str,
        genre: Optional[str] = None,
        mood: Optional[str] = None,
        instruments: Optional[List[str]] = None,
        tempo: Optional[str] = None,
        vocals: bool = False,
        **kwargs
    ) -> bytes:
        """
        Generate music with detailed specifications.
        
        Args:
            prompt: Base description
            genre: Music genre (e.g., 'electronic', 'rock', 'classical')
            mood: Mood/emotion (e.g., 'energetic', 'calm', 'dark')
            instruments: List of instruments to include
            tempo: Tempo description (e.g., 'fast', 'slow', 'moderate')
            vocals: Include vocals
            **kwargs: Additional generation parameters
            
        Returns:
            Audio data as bytes
        """
        # Build enhanced prompt
        enhanced_prompt = prompt
        
        if genre:
            enhanced_prompt += f", {genre} genre"
        
        if mood:
            enhanced_prompt += f", {mood} mood"
        
        if instruments:
            instruments_str = ", ".join(instruments)
            enhanced_prompt += f", featuring {instruments_str}"
        
        if tempo:
            enhanced_prompt += f", {tempo} tempo"
        
        if vocals:
            enhanced_prompt += ", with vocals"
        
        print(f"Enhanced prompt: {enhanced_prompt}")
        
        return self.generate(
            prompt=enhanced_prompt,
            **kwargs
        )
    
    def create_soundtrack(
        self,
        scenes: List[Dict[str, Any]],
        merge: bool = True,
        crossfade_ms: int = 1000
    ) -> bytes:
        """
        Create a soundtrack with multiple scenes/segments.
        
        Args:
            scenes: List of scene dictionaries with 'prompt' and optional 'duration'
            merge: Merge all scenes into one track
            crossfade_ms: Crossfade duration between scenes in milliseconds
            
        Returns:
            Audio data as bytes (merged or list)
        """
        audio_segments = []
        
        for i, scene in enumerate(scenes, 1):
            print(f"\nGenerating scene {i}/{len(scenes)}")
            print(f"Description: {scene.get('prompt', 'N/A')}")
            
            audio = self.generate(
                prompt=scene['prompt'],
                duration=scene.get('duration'),
                return_wav=True
            )
            audio_segments.append(audio)
        
        if merge and len(audio_segments) > 1:
            print(f"\nMerging {len(audio_segments)} scenes...")
            merged = self.converter.merge_audio(audio_segments, crossfade_ms=crossfade_ms)
            return merged
        elif len(audio_segments) == 1:
            return audio_segments[0]
        
        return audio_segments
    
    def generate_loop(
        self,
        prompt: str,
        loop_duration: int = 30,
        num_loops: int = 4
    ) -> bytes:
        """
        Generate a loopable music segment and repeat it.
        
        Args:
            prompt: Music description
            loop_duration: Duration of single loop in seconds
            num_loops: Number of times to repeat
            
        Returns:
            Audio data as bytes
        """
        print(f"Generating {loop_duration}s loop (x{num_loops})...")
        
        # Generate single loop
        loop_audio = self.generate(
            prompt=prompt + " (seamless loop)",
            duration=loop_duration,
            return_wav=True
        )
        
        # Repeat the loop
        if num_loops > 1:
            loops = [loop_audio] * num_loops
            return self.converter.merge_audio(loops, crossfade_ms=500)
        
        return loop_audio
    
    def get_example_prompts(self) -> Dict[str, str]:
        """
        Get example music generation prompts.
        
        Returns:
            Dictionary of category: prompt examples
        """
        return {
            'Electronic': "Upbeat electronic dance music with synth bass and energetic drums",
            'Ambient': "Calm ambient soundscape with soft pads and gentle piano",
            'Rock': "High-energy rock music with electric guitar riffs and powerful drums",
            'Classical': "Elegant classical piece with strings and piano, romantic mood",
            'Jazz': "Smooth jazz with saxophone, piano, and walking bass",
            'Cinematic': "Epic cinematic orchestral music with dramatic crescendos",
            'Lo-fi': "Relaxing lo-fi hip hop beat with vinyl crackle and mellow piano",
            'Game': "Retro 8-bit video game music, upbeat and adventurous"
        }


def main():
    """Command-line interface for the music generator."""
    import argparse
    
    parser = argparse.ArgumentParser(description='Generate music using ElevenLabs')
    parser.add_argument('prompt', help='Description of the music to generate')
    parser.add_argument('-o', '--output', type=Path, help='Output file path')
    parser.add_argument('-d', '--duration', type=int, help='Duration in seconds')
    parser.add_argument('--plan', action='store_true', help='Use composition plan')
    parser.add_argument('--examples', action='store_true', help='Show example prompts')
    
    # Detailed generation options
    parser.add_argument('--genre', help='Music genre')
    parser.add_argument('--mood', help='Mood/emotion')
    parser.add_argument('--tempo', help='Tempo (fast/slow/moderate)')
    parser.add_argument('--vocals', action='store_true', help='Include vocals')
    
    args = parser.parse_args()
    
    try:
        generator = MusicGenerator()
        
        if args.examples:
            print("\nExample music prompts:")
            examples = generator.get_example_prompts()
            for category, prompt in examples.items():
                print(f"\n{category}:")
                print(f"  {prompt}")
            return
        
        # Set default output path if not specified
        if not args.output:
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            args.output = Config.TEMP_DIR / f"music_{timestamp}.wav"
        
        # Generate music
        if args.genre or args.mood or args.tempo or args.vocals:
            audio = generator.generate_with_details(
                prompt=args.prompt,
                genre=args.genre,
                mood=args.mood,
                tempo=args.tempo,
                vocals=args.vocals,
                duration=args.duration,
                output_file=args.output,
                use_composition_plan=args.plan
            )
        else:
            audio = generator.generate(
                prompt=args.prompt,
                duration=args.duration,
                output_file=args.output,
                use_composition_plan=args.plan
            )
        
        duration = generator.converter.get_duration(audio)
        print(f"\nSuccess! Generated {duration:.2f} seconds of music")
        print(f"Output: {args.output}")
        
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == '__main__':
    main()
