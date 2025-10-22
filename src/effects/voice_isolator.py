"""Voice isolation effect using ElevenLabs API."""

import sys
from pathlib import Path
from typing import Optional, List, Tuple
from datetime import datetime

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from utils import Config, ElevenLabsAPI, AudioConverter


class VoiceIsolator:
    """Remove background noise and isolate voice from audio."""
    
    def __init__(self, api_key: Optional[str] = None):
        """
        Initialize the voice isolator.
        
        Args:
            api_key: ElevenLabs API key (optional)
        """
        Config.validate()
        self.api = ElevenLabsAPI(api_key)
        self.converter = AudioConverter()
    
    def isolate(
        self,
        input_file: Path,
        output_file: Optional[Path] = None,
        return_wav: bool = True
    ) -> bytes:
        """
        Isolate voice from audio file.
        
        Args:
            input_file: Path to input audio file
            output_file: Optional path to save the isolated audio
            return_wav: Return as WAV format (True) or MP3 (False)
            
        Returns:
            Isolated audio data as bytes (WAV or MP3)
        """
        print(f"Isolating voice from: {input_file}")
        
        # Read input file
        with open(input_file, 'rb') as f:
            audio_data = f.read()
        
        # Isolate voice
        mp3_data = self.api.isolate_voice(audio_data)
        
        print(f"Voice isolated successfully ({len(mp3_data)} bytes)")
        
        # Convert to WAV if requested
        if return_wav:
            print("Converting to WAV format...")
            isolated_data = self.converter.mp3_to_wav(mp3_data)
            format_ext = 'wav'
        else:
            isolated_data = mp3_data
            format_ext = 'mp3'
        
        # Save to file if specified
        if output_file:
            self.converter.save_to_file(isolated_data, output_file, format=format_ext)
            print(f"Isolated audio saved to: {output_file}")
        
        return isolated_data
    
    def batch_isolate(
        self,
        input_files: List[Path],
        output_dir: Optional[Path] = None
    ) -> List[bytes]:
        """
        Isolate voice from multiple audio files.
        
        Args:
            input_files: List of input audio file paths
            output_dir: Optional directory to save isolated files
            
        Returns:
            List of isolated audio data bytes
        """
        isolated_segments = []
        
        if output_dir:
            output_dir.mkdir(parents=True, exist_ok=True)
        
        for i, input_file in enumerate(input_files, 1):
            print(f"\nProcessing file {i}/{len(input_files)}: {input_file.name}")
            
            output_file = None
            if output_dir:
                output_file = output_dir / f"isolated_{input_file.stem}.wav"
            
            isolated = self.isolate(
                input_file=input_file,
                output_file=output_file
            )
            isolated_segments.append(isolated)
        
        return isolated_segments
    
    def compare_before_after(
        self,
        input_file: Path,
        output_dir: Path
    ) -> Tuple[bytes, bytes]:
        """
        Create before/after comparison files.
        
        Args:
            input_file: Path to input audio file
            output_dir: Directory to save comparison files
            
        Returns:
            Tuple of (original, isolated) audio bytes
        """
        output_dir.mkdir(parents=True, exist_ok=True)
        
        # Read original
        with open(input_file, 'rb') as f:
            original = f.read()
        
        # Save original as WAV
        original_wav = self.converter.mp3_to_wav(original) if input_file.suffix.lower() == '.mp3' else original
        original_path = output_dir / f"original_{input_file.stem}.wav"
        self.converter.save_to_file(original_wav, original_path, format='wav')
        print(f"Original saved: {original_path}")
        
        # Isolate and save
        isolated_path = output_dir / f"isolated_{input_file.stem}.wav"
        isolated = self.isolate(input_file, isolated_path)
        
        return original_wav, isolated


def main():
    """Command-line interface for the voice isolator."""
    import argparse
    
    parser = argparse.ArgumentParser(description='Isolate voice from audio using ElevenLabs')
    parser.add_argument('input', type=Path, help='Input audio file')
    parser.add_argument('-o', '--output', type=Path, help='Output file path')
    parser.add_argument('--batch', type=Path, nargs='+', help='Process multiple files')
    parser.add_argument('--compare', action='store_true', help='Create before/after comparison')
    
    args = parser.parse_args()
    
    try:
        isolator = VoiceIsolator()
        
        if args.batch:
            # Batch processing
            output_dir = args.output or Config.TEMP_DIR / 'isolated'
            isolator.batch_isolate(args.batch, output_dir)
            print(f"\n✓ Batch processing complete!")
            print(f"  Files saved to: {output_dir}")
        
        elif args.compare:
            # Before/after comparison
            output_dir = args.output or Config.TEMP_DIR / 'comparison'
            isolator.compare_before_after(args.input, output_dir)
            print(f"\n✓ Comparison files created!")
            print(f"  Files saved to: {output_dir}")
        
        else:
            # Single file processing
            if not args.output:
                timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
                args.output = Config.TEMP_DIR / f"isolated_{timestamp}.wav"
            
            isolated = isolator.isolate(args.input, args.output)
            
            duration = isolator.converter.get_duration(isolated)
            print(f"\n✓ Success! Processed {duration:.2f} seconds of audio")
            print(f"  Output: {args.output}")
        
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == '__main__':
    main()
