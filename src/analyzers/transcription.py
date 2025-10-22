"""Audio transcription using ElevenLabs Scribe API."""

import sys
from pathlib import Path
from typing import Optional, Dict, Any
from datetime import datetime
import json

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from utils import Config, ElevenLabsAPI, AudacityInterface


class AudioTranscriber:
    """Transcribe audio to text using ElevenLabs Scribe."""
    
    def __init__(self, api_key: Optional[str] = None):
        """
        Initialize the transcriber.
        
        Args:
            api_key: ElevenLabs API key (optional)
        """
        Config.validate()
        self.api = ElevenLabsAPI(api_key)
    
    def transcribe(
        self,
        input_file: Path,
        language: Optional[str] = None,
        enable_speaker_diarization: bool = False,
        num_speakers: Optional[int] = None,
        output_file: Optional[Path] = None,
        create_audacity_labels: bool = False
    ) -> Dict[str, Any]:
        """
        Transcribe audio file to text.
        
        Args:
            input_file: Path to input audio file
            language: Language code (e.g., 'en', 'es')
            enable_speaker_diarization: Identify different speakers
            num_speakers: Expected number of speakers
            output_file: Optional path to save transcription
            create_audacity_labels: Create Audacity label track file
            
        Returns:
            Transcription result dictionary
        """
        print(f"Transcribing: {input_file}")
        if language:
            print(f"Language: {language}")
        if enable_speaker_diarization:
            print(f"Speaker diarization enabled (speakers: {num_speakers or 'auto'})")
        
        # Read input file
        with open(input_file, 'rb') as f:
            audio_data = f.read()
        
        # Transcribe
        result = self.api.transcribe_audio(
            audio_file=audio_data,
            language=language,
            enable_speaker_diarization=enable_speaker_diarization,
            num_speakers=num_speakers
        )
        
        print(f"\n✓ Transcription complete!")
        
        # Extract text
        if 'text' in result:
            print(f"\nTranscribed text ({len(result['text'])} characters):")
            print("-" * 80)
            print(result['text'])
            print("-" * 80)
        
        # Show speaker information if available
        if 'segments' in result and enable_speaker_diarization:
            speakers = set()
            for segment in result['segments']:
                if 'speaker' in segment:
                    speakers.add(segment['speaker'])
            if speakers:
                print(f"\nDetected {len(speakers)} speaker(s): {', '.join(sorted(speakers))}")
        
        # Save to file if specified
        if output_file:
            self._save_transcription(result, output_file)
            print(f"\n✓ Transcription saved to: {output_file}")
        
        # Create Audacity labels if requested
        if create_audacity_labels and 'segments' in result:
            label_file = output_file.with_suffix('.txt') if output_file else Config.TEMP_DIR / f"labels_{datetime.now().strftime('%Y%m%d_%H%M%S')}.txt"
            self._create_audacity_labels(result['segments'], label_file)
            print(f"✓ Audacity labels saved to: {label_file}")
        
        return result
    
    def _save_transcription(self, result: Dict[str, Any], output_file: Path) -> None:
        """Save transcription result to file."""
        output_file.parent.mkdir(parents=True, exist_ok=True)
        
        # Save as JSON for full data
        if output_file.suffix.lower() == '.json':
            with open(output_file, 'w', encoding='utf-8') as f:
                json.dump(result, f, indent=2, ensure_ascii=False)
        
        # Save as plain text
        elif output_file.suffix.lower() == '.txt':
            with open(output_file, 'w', encoding='utf-8') as f:
                if 'text' in result:
                    f.write(result['text'])
                    f.write('\n\n')
                
                # Add metadata
                f.write('--- Metadata ---\n')
                if 'language' in result:
                    f.write(f"Language: {result['language']}\n")
                if 'duration' in result:
                    f.write(f"Duration: {result['duration']:.2f}s\n")
        
        # Save as SRT (subtitles) if segments available
        elif output_file.suffix.lower() == '.srt':
            self._save_as_srt(result, output_file)
    
    def _save_as_srt(self, result: Dict[str, Any], output_file: Path) -> None:
        """Save transcription as SRT subtitle file."""
        if 'segments' not in result:
            return
        
        with open(output_file, 'w', encoding='utf-8') as f:
            for i, segment in enumerate(result['segments'], 1):
                # SRT format:
                # 1
                # 00:00:00,000 --> 00:00:05,000
                # Text here
                
                start = segment.get('start', 0)
                end = segment.get('end', 0)
                text = segment.get('text', '')
                
                f.write(f"{i}\n")
                f.write(f"{self._format_timestamp(start)} --> {self._format_timestamp(end)}\n")
                
                # Add speaker if available
                if 'speaker' in segment:
                    f.write(f"[{segment['speaker']}] {text}\n")
                else:
                    f.write(f"{text}\n")
                
                f.write('\n')
    
    def _format_timestamp(self, seconds: float) -> str:
        """Format timestamp for SRT format (HH:MM:SS,mmm)."""
        hours = int(seconds // 3600)
        minutes = int((seconds % 3600) // 60)
        secs = int(seconds % 60)
        millis = int((seconds % 1) * 1000)
        return f"{hours:02d}:{minutes:02d}:{secs:02d},{millis:03d}"
    
    def _create_audacity_labels(self, segments: list, output_file: Path) -> None:
        """Create Audacity label track from transcription segments."""
        labels = []
        for segment in segments:
            start = segment.get('start', 0)
            end = segment.get('end', 0)
            text = segment.get('text', '').strip()
            
            # Add speaker prefix if available
            if 'speaker' in segment:
                text = f"[{segment['speaker']}] {text}"
            
            labels.append((start, end, text))
        
        # Create label track
        label_text = AudacityInterface.create_label_track(labels)
        
        with open(output_file, 'w', encoding='utf-8') as f:
            f.write(label_text)
    
    def batch_transcribe(
        self,
        input_files: list[Path],
        output_dir: Optional[Path] = None,
        **kwargs
    ) -> list[Dict[str, Any]]:
        """
        Transcribe multiple audio files.
        
        Args:
            input_files: List of input audio file paths
            output_dir: Optional directory to save transcriptions
            **kwargs: Additional transcription parameters
            
        Returns:
            List of transcription results
        """
        results = []
        
        if output_dir:
            output_dir.mkdir(parents=True, exist_ok=True)
        
        for i, input_file in enumerate(input_files, 1):
            print(f"\n{'='*80}")
            print(f"Processing file {i}/{len(input_files)}: {input_file.name}")
            print(f"{'='*80}")
            
            output_file = None
            if output_dir:
                output_file = output_dir / f"{input_file.stem}_transcript.txt"
            
            result = self.transcribe(
                input_file=input_file,
                output_file=output_file,
                **kwargs
            )
            results.append(result)
        
        return results


def main():
    """Command-line interface for the transcriber."""
    import argparse
    
    parser = argparse.ArgumentParser(description='Transcribe audio using ElevenLabs Scribe')
    parser.add_argument('input', type=Path, help='Input audio file')
    parser.add_argument('-o', '--output', type=Path, help='Output file path (.txt, .json, or .srt)')
    parser.add_argument('-l', '--language', help='Language code (e.g., en, es, fr)')
    parser.add_argument('--speakers', action='store_true', help='Enable speaker diarization')
    parser.add_argument('--num-speakers', type=int, help='Expected number of speakers')
    parser.add_argument('--labels', action='store_true', help='Create Audacity label track')
    parser.add_argument('--batch', type=Path, nargs='+', help='Process multiple files')
    
    args = parser.parse_args()
    
    try:
        transcriber = AudioTranscriber()
        
        if args.batch:
            # Batch processing
            output_dir = args.output or Config.TEMP_DIR / 'transcriptions'
            transcriber.batch_transcribe(
                args.batch,
                output_dir=output_dir,
                language=args.language,
                enable_speaker_diarization=args.speakers,
                num_speakers=args.num_speakers,
                create_audacity_labels=args.labels
            )
            print(f"\n✓ Batch transcription complete!")
            print(f"  Files saved to: {output_dir}")
        
        else:
            # Single file processing
            if not args.output:
                timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
                args.output = Config.TEMP_DIR / f"transcript_{timestamp}.txt"
            
            transcriber.transcribe(
                input_file=args.input,
                language=args.language,
                enable_speaker_diarization=args.speakers,
                num_speakers=args.num_speakers,
                output_file=args.output,
                create_audacity_labels=args.labels
            )
        
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == '__main__':
    main()
