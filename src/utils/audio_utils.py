"""Audio utilities for converting between formats and working with Audacity."""

import io
import wave
import numpy as np
from pathlib import Path
from typing import Tuple, Optional, List
from pydub import AudioSegment
from .config import Config


class AudioConverter:
    """Utility class for audio format conversion."""
    
    @staticmethod
    def mp3_to_wav(mp3_data: bytes, sample_rate: int = Config.DEFAULT_SAMPLE_RATE) -> bytes:
        """
        Convert MP3 data to WAV format.
        
        Args:
            mp3_data: MP3 audio data as bytes
            sample_rate: Target sample rate
            
        Returns:
            WAV audio data as bytes
        """
        # Load MP3 from bytes
        audio = AudioSegment.from_mp3(io.BytesIO(mp3_data))
        
        # Convert to target sample rate if needed
        if audio.frame_rate != sample_rate:
            audio = audio.set_frame_rate(sample_rate)
        
        # Export as WAV
        wav_io = io.BytesIO()
        audio.export(wav_io, format='wav')
        wav_io.seek(0)
        
        return wav_io.read()
    
    @staticmethod
    def bytes_to_numpy(audio_bytes: bytes, sample_rate: int = Config.DEFAULT_SAMPLE_RATE) -> Tuple[np.ndarray, int]:
        """
        Convert audio bytes to numpy array.
        
        Args:
            audio_bytes: Audio data as bytes (WAV format)
            sample_rate: Sample rate
            
        Returns:
            Tuple of (numpy array, sample rate)
        """
        # Load audio
        audio = AudioSegment.from_wav(io.BytesIO(audio_bytes))
        
        # Convert to mono if stereo
        if audio.channels > 1:
            audio = audio.set_channels(1)
        
        # Convert to numpy array
        samples = np.array(audio.get_array_of_samples())
        
        # Normalize to -1.0 to 1.0 range
        if audio.sample_width == 2:  # 16-bit
            samples = samples.astype(np.float32) / 32768.0
        elif audio.sample_width == 4:  # 32-bit
            samples = samples.astype(np.float32) / 2147483648.0
        
        return samples, audio.frame_rate
    
    @staticmethod
    def numpy_to_wav(samples: np.ndarray, sample_rate: int = Config.DEFAULT_SAMPLE_RATE) -> bytes:
        """
        Convert numpy array to WAV bytes.
        
        Args:
            samples: Audio samples as numpy array (-1.0 to 1.0)
            sample_rate: Sample rate
            
        Returns:
            WAV audio data as bytes
        """
        # Convert to 16-bit PCM
        samples_int16 = (samples * 32767).astype(np.int16)
        
        # Create WAV file in memory
        wav_io = io.BytesIO()
        with wave.open(wav_io, 'wb') as wav_file:
            wav_file.setnchannels(1)  # Mono
            wav_file.setsampwidth(2)  # 16-bit
            wav_file.setframerate(sample_rate)
            wav_file.writeframes(samples_int16.tobytes())
        
        wav_io.seek(0)
        return wav_io.read()
    
    @staticmethod
    def save_to_file(audio_data: bytes, filepath: Path, format: str = 'wav') -> None:
        """
        Save audio data to a file.
        
        Args:
            audio_data: Audio data as bytes
            filepath: Path to save the file
            format: Output format ('wav', 'mp3', etc.)
        """
        filepath.parent.mkdir(parents=True, exist_ok=True)
        
        if format == 'wav':
            with open(filepath, 'wb') as f:
                f.write(audio_data)
        else:
            # Convert using pydub if needed
            audio = AudioSegment.from_file(io.BytesIO(audio_data))
            audio.export(filepath, format=format)
    
    @staticmethod
    def get_duration(audio_data: bytes) -> float:
        """
        Get duration of audio in seconds.
        
        Args:
            audio_data: Audio data as bytes
            
        Returns:
            Duration in seconds
        """
        audio = AudioSegment.from_file(io.BytesIO(audio_data))
        return len(audio) / 1000.0  # pydub uses milliseconds
    
    @staticmethod
    def merge_audio(audio_segments: List[bytes], crossfade_ms: int = 0) -> bytes:
        """
        Merge multiple audio segments.
        
        Args:
            audio_segments: List of audio data as bytes
            crossfade_ms: Crossfade duration in milliseconds
            
        Returns:
            Merged audio as bytes
        """
        if not audio_segments:
            return b''
        
        if len(audio_segments) == 1:
            return audio_segments[0]
        
        # Load first segment
        merged = AudioSegment.from_file(io.BytesIO(audio_segments[0]))
        
        # Append remaining segments
        for audio_data in audio_segments[1:]:
            segment = AudioSegment.from_file(io.BytesIO(audio_data))
            if crossfade_ms > 0:
                merged = merged.append(segment, crossfade=crossfade_ms)
            else:
                merged = merged + segment
        
        # Export as WAV
        output = io.BytesIO()
        merged.export(output, format='wav')
        output.seek(0)
        
        return output.read()
    
    @staticmethod
    def normalize_audio(audio_data: bytes, target_db: float = -20.0) -> bytes:
        """
        Normalize audio to a target dB level.
        
        Args:
            audio_data: Audio data as bytes
            target_db: Target dB level
            
        Returns:
            Normalized audio as bytes
        """
        audio = AudioSegment.from_file(io.BytesIO(audio_data))
        
        # Calculate the change in dBFS
        change_in_db = target_db - audio.dBFS
        
        # Apply normalization
        normalized_audio = audio.apply_gain(change_in_db)
        
        # Export as WAV
        output = io.BytesIO()
        normalized_audio.export(output, format='wav')
        output.seek(0)
        
        return output.read()


class AudacityInterface:
    """Utilities for interfacing with Audacity."""
    
    @staticmethod
    def create_label_track(labels: List[Tuple[float, float, str]]) -> str:
        """
        Create Audacity label track format.
        
        Args:
            labels: List of (start_time, end_time, label_text) tuples
            
        Returns:
            Label track text in Audacity format
        """
        label_lines = []
        for start, end, text in labels:
            label_lines.append(f"{start}\t{end}\t{text}")
        
        return "\n".join(label_lines)
    
    @staticmethod
    def export_raw_audio(samples: np.ndarray, filepath: Path) -> None:
        """
        Export raw audio data for Audacity import.
        
        Args:
            samples: Audio samples as numpy array
            filepath: Path to save the raw file
        """
        # Convert to 32-bit float for maximum quality
        samples_float32 = samples.astype(np.float32)
        samples_float32.tofile(filepath)
    
    @staticmethod
    def write_import_script(audio_file: Path, sample_rate: int = Config.DEFAULT_SAMPLE_RATE) -> str:
        """
        Generate Audacity macro/script to import audio.
        
        Args:
            audio_file: Path to audio file
            sample_rate: Sample rate
            
        Returns:
            Audacity script commands
        """
        script = f"""Import2: Filename="{audio_file}" 
SelectAll:
SetTrackStatus: Name="GeneratedAudio" 
"""
        return script
