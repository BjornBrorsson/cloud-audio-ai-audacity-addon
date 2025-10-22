"""Configuration management for Audacity Cloud AI Plugin."""

import os
from pathlib import Path
from typing import Optional
from dotenv import load_dotenv

# Load environment variables from .env file
env_path = Path(__file__).parent.parent.parent / '.env'
load_dotenv(env_path)


class Config:
    """Configuration manager for the plugin."""
    
    # API Configuration
    ELEVENLABS_API_KEY: Optional[str] = os.getenv('ELEVENLABS_API_KEY')
    ELEVENLABS_API_URL: str = os.getenv('ELEVENLABS_API_URL', 'https://api.elevenlabs.io/v1')
    
    # Default Settings
    DEFAULT_VOICE_ID: str = os.getenv('DEFAULT_VOICE_ID', '21m00Tcm4TlvDq8ikWAM')  # Rachel
    DEFAULT_MODEL: str = os.getenv('DEFAULT_MODEL', 'eleven_flash_v2_5')
    
    # Audio Settings
    DEFAULT_SAMPLE_RATE: int = 44100  # Audacity's default
    DEFAULT_BIT_DEPTH: int = 16
    DEFAULT_CHANNELS: int = 1  # Mono
    
    # TTS Settings
    DEFAULT_STABILITY: float = 0.5
    DEFAULT_SIMILARITY_BOOST: float = 0.75
    DEFAULT_STYLE: float = 0.0
    DEFAULT_USE_SPEAKER_BOOST: bool = True
    
    # Music Settings
    DEFAULT_MUSIC_DURATION: int = 30  # seconds
    
    # File paths
    TEMP_DIR: Path = Path(__file__).parent.parent.parent / 'temp'
    CACHE_DIR: Path = Path(__file__).parent.parent.parent / 'cache'
    
    @classmethod
    def validate(cls) -> bool:
        """Validate that required configuration is present."""
        if not cls.ELEVENLABS_API_KEY:
            raise ValueError(
                "ELEVENLABS_API_KEY not found. "
                "Please set it in your .env file or as an environment variable."
            )
        return True
    
    @classmethod
    def setup_directories(cls) -> None:
        """Create necessary directories if they don't exist."""
        cls.TEMP_DIR.mkdir(parents=True, exist_ok=True)
        cls.CACHE_DIR.mkdir(parents=True, exist_ok=True)
    
    @classmethod
    def get_available_models(cls) -> dict:
        """Get available ElevenLabs models with descriptions."""
        return {
            'eleven_flash_v2_5': {
                'name': 'Flash v2.5',
                'description': 'Fastest model with low latency (~75ms)',
                'languages': 32,
                'use_case': 'Real-time applications, quick generation'
            },
            'eleven_turbo_v2_5': {
                'name': 'Turbo v2.5',
                'description': 'Balanced quality and speed',
                'languages': 32,
                'use_case': 'General purpose, good quality'
            },
            'eleven_multilingual_v2': {
                'name': 'Multilingual v2',
                'description': 'Highest quality, rich emotional expression',
                'languages': 32,
                'use_case': 'Professional content, audiobooks'
            },
            'eleven_music': {
                'name': 'Eleven Music',
                'description': 'Studio-grade music generation',
                'languages': 'N/A',
                'use_case': 'Music creation, soundtracks'
            }
        }
    
    @classmethod
    def get_popular_voices(cls) -> dict:
        """Get a list of popular ElevenLabs voices."""
        return {
            '21m00Tcm4TlvDq8ikWAM': 'Rachel - Calm, Young Female',
            'AZnzlk1XvdvUeBnXmlld': 'Domi - Strong, Young Female',
            'EXAVITQu4vr4xnSDxMaL': 'Bella - Soft, Young Female',
            'ErXwobaYiN019PkySvjV': 'Antoni - Well-rounded, Young Male',
            'MF3mGyEYCl7XYWbV9V6O': 'Elli - Emotional, Young Female',
            'TxGEqnHWrfWFTfGW9XjX': 'Josh - Deep, Young Male',
            'VR6AewLTigWG4xSOukaG': 'Arnold - Crisp, Middle-Aged Male',
            'pNInz6obpgDQGcFmaJgB': 'Adam - Deep, Middle-Aged Male',
            'yoZ06aMxZJJ28mfd3POQ': 'Sam - Raspy, Young Male',
        }


# Initialize directories on import
Config.setup_directories()
