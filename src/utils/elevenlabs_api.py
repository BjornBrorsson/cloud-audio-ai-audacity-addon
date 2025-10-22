"""ElevenLabs API wrapper for Audacity Cloud AI Plugin."""

import requests
from typing import Optional, Dict, Any, List
from pathlib import Path
import io
from .config import Config


class ElevenLabsAPI:
    """Wrapper for ElevenLabs API calls."""
    
    def __init__(self, api_key: Optional[str] = None):
        """
        Initialize the API wrapper.
        
        Args:
            api_key: ElevenLabs API key. If None, uses Config.ELEVENLABS_API_KEY
        """
        self.api_key = api_key or Config.ELEVENLABS_API_KEY
        if not self.api_key:
            raise ValueError("API key is required. Set ELEVENLABS_API_KEY in .env or pass it to the constructor.")
        
        self.base_url = Config.ELEVENLABS_API_URL
        self.session = requests.Session()
        self.session.headers.update({
            'xi-api-key': self.api_key,
            'Content-Type': 'application/json'
        })
    
    def text_to_speech(
        self,
        text: str,
        voice_id: str = Config.DEFAULT_VOICE_ID,
        model_id: str = Config.DEFAULT_MODEL,
        stability: float = Config.DEFAULT_STABILITY,
        similarity_boost: float = Config.DEFAULT_SIMILARITY_BOOST,
        style: float = Config.DEFAULT_STYLE,
        use_speaker_boost: bool = Config.DEFAULT_USE_SPEAKER_BOOST,
        output_format: str = 'mp3_44100_128'
    ) -> bytes:
        """
        Convert text to speech using ElevenLabs API.
        
        Args:
            text: Text to convert to speech
            voice_id: ID of the voice to use
            model_id: ID of the model to use
            stability: Voice stability (0.0-1.0)
            similarity_boost: Similarity boost (0.0-1.0)
            style: Style exaggeration (0.0-1.0)
            use_speaker_boost: Enable speaker boost
            output_format: Audio output format
            
        Returns:
            Audio data as bytes
        """
        url = f"{self.base_url}/text-to-speech/{voice_id}"
        
        payload = {
            "text": text,
            "model_id": model_id,
            "voice_settings": {
                "stability": stability,
                "similarity_boost": similarity_boost,
                "style": style,
                "use_speaker_boost": use_speaker_boost
            }
        }
        
        params = {
            "output_format": output_format
        }
        
        response = self.session.post(url, json=payload, params=params)
        response.raise_for_status()
        
        return response.content
    
    def text_to_speech_stream(
        self,
        text: str,
        voice_id: str = Config.DEFAULT_VOICE_ID,
        model_id: str = Config.DEFAULT_MODEL,
        **kwargs
    ):
        """
        Stream text to speech (for real-time playback).
        
        Args:
            text: Text to convert
            voice_id: Voice ID
            model_id: Model ID
            **kwargs: Additional voice settings
            
        Yields:
            Audio chunks as they're generated
        """
        url = f"{self.base_url}/text-to-speech/{voice_id}/stream"
        
        payload = {
            "text": text,
            "model_id": model_id,
            "voice_settings": {
                "stability": kwargs.get('stability', Config.DEFAULT_STABILITY),
                "similarity_boost": kwargs.get('similarity_boost', Config.DEFAULT_SIMILARITY_BOOST),
                "style": kwargs.get('style', Config.DEFAULT_STYLE),
                "use_speaker_boost": kwargs.get('use_speaker_boost', Config.DEFAULT_USE_SPEAKER_BOOST)
            }
        }
        
        with self.session.post(url, json=payload, stream=True) as response:
            response.raise_for_status()
            for chunk in response.iter_content(chunk_size=4096):
                if chunk:
                    yield chunk
    
    def generate_music(
        self,
        prompt: str,
        duration: Optional[int] = None,
        output_format: str = 'mp3_44100_128'
    ) -> bytes:
        """
        Generate music from a text prompt.
        
        Args:
            prompt: Description of the music to generate
            duration: Duration in seconds (optional)
            output_format: Audio output format
            
        Returns:
            Audio data as bytes
        """
        url = f"{self.base_url}/music"
        
        payload = {
            "prompt": prompt
        }
        
        if duration:
            payload["duration"] = duration
        
        params = {
            "output_format": output_format
        }
        
        response = self.session.post(url, json=payload, params=params)
        response.raise_for_status()
        
        return response.content
    
    def generate_music_with_plan(
        self,
        prompt: str,
        composition_plan: Optional[Dict[str, Any]] = None,
        output_format: str = 'mp3_44100_128'
    ) -> bytes:
        """
        Generate music with a detailed composition plan.
        
        Args:
            prompt: Music description
            composition_plan: Detailed composition structure
            output_format: Audio output format
            
        Returns:
            Audio data as bytes
        """
        url = f"{self.base_url}/music"
        
        payload = {
            "prompt": prompt
        }
        
        if composition_plan:
            payload["composition_plan"] = composition_plan
        
        params = {
            "output_format": output_format
        }
        
        response = self.session.post(url, json=payload, params=params)
        response.raise_for_status()
        
        return response.content
    
    def create_composition_plan(
        self,
        prompt: str
    ) -> Dict[str, Any]:
        """
        Create a composition plan from a prompt.
        
        Args:
            prompt: Music description
            
        Returns:
            Composition plan dictionary
        """
        url = f"{self.base_url}/music/composition-plan"
        
        payload = {
            "prompt": prompt
        }
        
        response = self.session.post(url, json=payload)
        response.raise_for_status()
        
        return response.json()
    
    def get_voices(self) -> List[Dict[str, Any]]:
        """
        Get list of available voices.
        
        Returns:
            List of voice dictionaries
        """
        url = f"{self.base_url}/voices"
        
        response = self.session.get(url)
        response.raise_for_status()
        
        return response.json().get('voices', [])
    
    def get_models(self) -> List[Dict[str, Any]]:
        """
        Get list of available models.
        
        Returns:
            List of model dictionaries
        """
        url = f"{self.base_url}/models"
        
        response = self.session.get(url)
        response.raise_for_status()
        
        return response.json()
    
    def get_user_info(self) -> Dict[str, Any]:
        """
        Get user account information.
        
        Returns:
            User info dictionary including subscription details
        """
        url = f"{self.base_url}/user"
        
        response = self.session.get(url)
        response.raise_for_status()
        
        return response.json()
    
    def voice_conversion(
        self,
        audio_file: bytes,
        voice_id: str,
        model_id: str = 'eleven_multilingual_sts_v2',
        output_format: str = 'mp3_44100_128'
    ) -> bytes:
        """
        Convert audio to a different voice.
        
        Args:
            audio_file: Input audio file bytes
            voice_id: Target voice ID
            model_id: Voice conversion model ID
            output_format: Audio output format
            
        Returns:
            Converted audio as bytes
        """
        url = f"{self.base_url}/speech-to-speech/{voice_id}"
        
        # Remove Content-Type header for multipart upload
        headers = {
            'xi-api-key': self.api_key
        }
        
        files = {
            'audio': ('input.mp3', io.BytesIO(audio_file), 'audio/mpeg')
        }
        
        data = {
            'model_id': model_id
        }
        
        params = {
            'output_format': output_format
        }
        
        response = requests.post(url, headers=headers, files=files, data=data, params=params)
        response.raise_for_status()
        
        return response.content
    
    def generate_sound_effects(
        self,
        text: str,
        duration_seconds: Optional[float] = None,
        prompt_influence: float = 0.3,
        output_format: str = 'mp3_44100_128'
    ) -> bytes:
        """
        Generate sound effects from text description.
        
        Args:
            text: Description of the sound effect to generate
            duration_seconds: Target duration (optional, usually auto-determined)
            prompt_influence: How much to follow the prompt (0.0-1.0)
            output_format: Audio output format
            
        Returns:
            Sound effect audio as bytes
        """
        url = f"{self.base_url}/sound-generation"
        
        payload = {
            "text": text,
            "prompt_influence": prompt_influence
        }
        
        if duration_seconds:
            payload["duration_seconds"] = duration_seconds
        
        params = {
            "output_format": output_format
        }
        
        response = self.session.post(url, json=payload, params=params)
        response.raise_for_status()
        
        return response.content
    
    def isolate_voice(
        self,
        audio_file: bytes,
        output_format: str = 'mp3_44100_128'
    ) -> bytes:
        """
        Remove background noise and isolate voice from audio.
        
        Args:
            audio_file: Input audio file bytes
            output_format: Audio output format
            
        Returns:
            Isolated voice audio as bytes
        """
        url = f"{self.base_url}/audio-isolation"
        
        # Remove Content-Type header for multipart upload
        headers = {
            'xi-api-key': self.api_key
        }
        
        files = {
            'audio': ('input.mp3', io.BytesIO(audio_file), 'audio/mpeg')
        }
        
        params = {
            'output_format': output_format
        }
        
        response = requests.post(url, headers=headers, files=files, params=params)
        response.raise_for_status()
        
        return response.content
    
    def isolate_voice_stream(
        self,
        audio_file: bytes
    ):
        """
        Stream isolated voice audio.
        
        Args:
            audio_file: Input audio file bytes
            
        Yields:
            Audio chunks as they're processed
        """
        url = f"{self.base_url}/audio-isolation/stream"
        
        headers = {
            'xi-api-key': self.api_key
        }
        
        files = {
            'audio': ('input.mp3', io.BytesIO(audio_file), 'audio/mpeg')
        }
        
        with requests.post(url, headers=headers, files=files, stream=True) as response:
            response.raise_for_status()
            for chunk in response.iter_content(chunk_size=4096):
                if chunk:
                    yield chunk
    
    def transcribe_audio(
        self,
        audio_file: bytes,
        model_id: str = 'scribe_v1',
        language: Optional[str] = None,
        enable_speaker_diarization: bool = False,
        num_speakers: Optional[int] = None
    ) -> Dict[str, Any]:
        """
        Transcribe audio to text using ElevenLabs Scribe.
        
        Args:
            audio_file: Input audio file bytes
            model_id: Transcription model ID
            language: Language code (e.g., 'en', 'es', auto-detected if None)
            enable_speaker_diarization: Enable speaker identification
            num_speakers: Expected number of speakers (for diarization)
            
        Returns:
            Transcription result dictionary with text and metadata
        """
        url = f"{self.base_url}/audio-to-text"
        
        headers = {
            'xi-api-key': self.api_key
        }
        
        files = {
            'audio': ('input.mp3', io.BytesIO(audio_file), 'audio/mpeg')
        }
        
        data = {
            'model_id': model_id
        }
        
        if language:
            data['language'] = language
        
        if enable_speaker_diarization:
            data['enable_speaker_diarization'] = 'true'
            if num_speakers:
                data['num_speakers'] = str(num_speakers)
        
        response = requests.post(url, headers=headers, files=files, data=data)
        response.raise_for_status()
        
        return response.json()
    
    def get_shared_voices(
        self,
        page_size: int = 30,
        category: Optional[str] = None,
        gender: Optional[str] = None,
        age: Optional[str] = None,
        accent: Optional[str] = None,
        language: Optional[str] = None,
        search: Optional[str] = None,
        use_cases: Optional[List[str]] = None
    ) -> Dict[str, Any]:
        """
        Get voices from the ElevenLabs Voice Library.
        
        Args:
            page_size: Number of voices per page
            category: Filter by category
            gender: Filter by gender ('male', 'female')
            age: Filter by age group
            accent: Filter by accent
            language: Filter by language
            search: Search query
            use_cases: Filter by use cases
            
        Returns:
            Dictionary with voices list and pagination info
        """
        url = f"{self.base_url}/voice-library/shared-voices"
        
        params = {
            'page_size': page_size
        }
        
        if category:
            params['category'] = category
        if gender:
            params['gender'] = gender
        if age:
            params['age'] = age
        if accent:
            params['accent'] = accent
        if language:
            params['language'] = language
        if search:
            params['search'] = search
        if use_cases:
            params['use_cases'] = ','.join(use_cases)
        
        response = self.session.get(url, params=params)
        response.raise_for_status()
        
        return response.json()
    
    def add_voice_from_library(
        self,
        public_user_id: str,
        voice_id: str,
        new_name: str
    ) -> Dict[str, Any]:
        """
        Add a voice from the Voice Library to your account.
        
        Args:
            public_user_id: Public user ID of the voice creator
            voice_id: ID of the voice to add
            new_name: Name to give the voice in your account
            
        Returns:
            Dictionary with the added voice info
        """
        url = f"{self.base_url}/voice-library/add/{public_user_id}/{voice_id}"
        
        payload = {
            "new_name": new_name
        }
        
        response = self.session.post(url, json=payload)
        response.raise_for_status()
        
        return response.json()
