"""Text-to-Speech generator using ElevenLabs API."""

import sys
from pathlib import Path
from typing import Optional, Dict, Any
from datetime import datetime

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from utils import Config, ElevenLabsAPI, AudioConverter


class TextToSpeechGenerator:
    """Generator for creating speech from text using ElevenLabs."""
    
    def __init__(self, api_key: Optional[str] = None):
        """
        Initialize the TTS generator.
        
        Args:
            api_key: ElevenLabs API key (optional)
        """
        Config.validate()
        self.api = ElevenLabsAPI(api_key)
        self.converter = AudioConverter()
    
    def generate(
        self,
        text: str,
        voice_id: Optional[str] = None,
        model_id: Optional[str] = None,
        stability: Optional[float] = None,
        similarity_boost: Optional[float] = None,
        style: Optional[float] = None,
        use_speaker_boost: Optional[bool] = None,
        output_file: Optional[Path] = None,
        return_wav: bool = True
    ) -> bytes:
        """
        Generate speech from text.
        
        Args:
            text: Text to convert to speech
            voice_id: Voice to use (default: Config.DEFAULT_VOICE_ID)
            model_id: Model to use (default: Config.DEFAULT_MODEL)
            stability: Voice stability 0.0-1.0
            similarity_boost: Similarity boost 0.0-1.0
            style: Style exaggeration 0.0-1.0
            use_speaker_boost: Enable speaker boost
            output_file: Optional path to save the audio
            return_wav: Return as WAV format (True) or MP3 (False)
            
        Returns:
            Audio data as bytes (WAV or MP3 depending on return_wav)
        """
        # Use defaults if not specified
        voice_id = voice_id or Config.DEFAULT_VOICE_ID
        model_id = model_id or Config.DEFAULT_MODEL
        stability = stability if stability is not None else Config.DEFAULT_STABILITY
        similarity_boost = similarity_boost if similarity_boost is not None else Config.DEFAULT_SIMILARITY_BOOST
        style = style if style is not None else Config.DEFAULT_STYLE
        use_speaker_boost = use_speaker_boost if use_speaker_boost is not None else Config.DEFAULT_USE_SPEAKER_BOOST
        
        # Generate speech
        print(f"Generating speech with voice '{voice_id}' using model '{model_id}'...")
        mp3_data = self.api.text_to_speech(
            text=text,
            voice_id=voice_id,
            model_id=model_id,
            stability=stability,
            similarity_boost=similarity_boost,
            style=style,
            use_speaker_boost=use_speaker_boost
        )
        
        print(f"Speech generated successfully ({len(mp3_data)} bytes)")
        
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
            print(f"Audio saved to: {output_file}")
        
        return audio_data
    
    def generate_with_timestamps(
        self,
        text: str,
        voice_id: Optional[str] = None,
        model_id: Optional[str] = None,
        **kwargs
    ) -> Dict[str, Any]:
        """
        Generate speech with word-level timestamps.
        
        Args:
            text: Text to convert
            voice_id: Voice ID
            model_id: Model ID
            **kwargs: Additional voice settings
            
        Returns:
            Dictionary containing audio data and timestamps
        """
        # Note: This feature requires ElevenLabs API support
        # For now, generate regular audio
        audio_data = self.generate(text, voice_id, model_id, **kwargs)
        
        return {
            'audio': audio_data,
            'duration': self.converter.get_duration(audio_data),
            'text': text,
            'timestamps': []  # Would contain word-level timestamps if available
        }
    
    def batch_generate(
        self,
        texts: list[str],
        voice_id: Optional[str] = None,
        model_id: Optional[str] = None,
        merge: bool = False,
        **kwargs
    ) -> list[bytes]:
        """
        Generate multiple speech segments.
        
        Args:
            texts: List of texts to convert
            voice_id: Voice ID
            model_id: Model ID
            merge: Merge all segments into one audio file
            **kwargs: Additional voice settings
            
        Returns:
            List of audio data bytes (or single merged audio if merge=True)
        """
        audio_segments = []
        
        for i, text in enumerate(texts, 1):
            print(f"Generating segment {i}/{len(texts)}...")
            audio = self.generate(
                text=text,
                voice_id=voice_id,
                model_id=model_id,
                **kwargs
            )
            audio_segments.append(audio)
        
        if merge and len(audio_segments) > 1:
            print("Merging audio segments...")
            merged = self.converter.merge_audio(audio_segments, crossfade_ms=100)
            return [merged]
        
        return audio_segments
    
    def get_available_voices(self) -> list[Dict[str, Any]]:
        """
        Get list of available voices from ElevenLabs.
        
        Returns:
            List of voice dictionaries
        """
        return self.api.get_voices()
    
    def preview_voice(
        self,
        voice_id: str,
        sample_text: str = "Hello, this is a preview of this voice."
    ) -> bytes:
        """
        Generate a quick preview of a voice.
        
        Args:
            voice_id: Voice ID to preview
            sample_text: Text to use for preview
            
        Returns:
            Audio data as bytes
        """
        return self.generate(
            text=sample_text,
            voice_id=voice_id,
            model_id='eleven_flash_v2_5'  # Use fast model for preview
        )


def main():
    """Command-line interface for the TTS generator."""
    import argparse
    
    parser = argparse.ArgumentParser(description='Generate speech from text using ElevenLabs')
    parser.add_argument('text', help='Text to convert to speech')
    parser.add_argument('-o', '--output', type=Path, help='Output file path')
    parser.add_argument('-v', '--voice', default=Config.DEFAULT_VOICE_ID, help='Voice ID')
    parser.add_argument('-m', '--model', default=Config.DEFAULT_MODEL, help='Model ID')
    parser.add_argument('--stability', type=float, default=Config.DEFAULT_STABILITY, help='Stability (0.0-1.0)')
    parser.add_argument('--similarity', type=float, default=Config.DEFAULT_SIMILARITY_BOOST, help='Similarity boost (0.0-1.0)')
    parser.add_argument('--style', type=float, default=Config.DEFAULT_STYLE, help='Style (0.0-1.0)')
    parser.add_argument('--list-voices', action='store_true', help='List available voices')
    
    args = parser.parse_args()
    
    try:
        generator = TextToSpeechGenerator()
        
        if args.list_voices:
            print("\nAvailable voices:")
            voices = generator.get_available_voices()
            for voice in voices:
                print(f"  {voice['voice_id']}: {voice['name']}")
            return
        
        # Set default output path if not specified
        if not args.output:
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            args.output = Config.TEMP_DIR / f"tts_{timestamp}.wav"
        
        # Generate speech
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
        print(f"\nSuccess! Generated {duration:.2f} seconds of audio")
        print(f"Output: {args.output}")
        
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == '__main__':
    main()
