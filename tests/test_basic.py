"""
Basic smoke tests to ensure modules can be imported.
These tests don't require API keys and should pass on all Python versions.
"""

import pytest
from pathlib import Path
import sys
import os

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / 'src'))

# Set dummy API key for imports
os.environ['ELEVENLABS_API_KEY'] = 'test_dummy_key_12345'


def test_import_generators():
    """Test that generator modules can be imported."""
    from generators import TextToSpeechGenerator, MusicGenerator, SoundEffectsGenerator
    assert TextToSpeechGenerator is not None
    assert MusicGenerator is not None
    assert SoundEffectsGenerator is not None


def test_import_effects():
    """Test that effect modules can be imported."""
    from effects import VoiceIsolator
    assert VoiceIsolator is not None


def test_import_analyzers():
    """Test that analyzer modules can be imported."""
    from analyzers import AudioTranscriber
    assert AudioTranscriber is not None


def test_import_utils():
    """Test that utility modules can be imported."""
    from utils import Config, ElevenLabsAPI, AudioConverter
    assert Config is not None
    assert ElevenLabsAPI is not None
    assert AudioConverter is not None


def test_config_validation():
    """Test that config validation works."""
    from utils import Config
    # Config should have the dummy API key set
    assert Config.ELEVENLABS_API_KEY is not None
    assert len(Config.ELEVENLABS_API_KEY) > 0


def test_python_version():
    """Test that Python version is acceptable."""
    assert sys.version_info >= (3, 8)
    print(f"Running on Python {sys.version_info.major}.{sys.version_info.minor}.{sys.version_info.micro}")


# Mark integration tests as skipped by default
@pytest.mark.skip(reason="Integration tests require real API key")
def test_real_api_call():
    """Integration test - requires real API key."""
    pass
