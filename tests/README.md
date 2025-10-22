# Tests

Unit tests for the Audacity Cloud AI plugin.

## Running Tests

```bash
# Install test dependencies
pip install pytest pytest-cov pytest-mock

# Run all tests
pytest

# Run with coverage
pytest --cov=src --cov-report=html

# Run specific test file
pytest tests/test_text_to_speech.py
```

## Test Structure

- `test_text_to_speech.py` - TTS generator tests
- `test_music_generator.py` - Music generator tests (to be created)
- `test_api.py` - API wrapper tests (to be created)
- `test_audio_utils.py` - Audio utility tests (to be created)

## Writing Tests

Use mocks for API calls to avoid hitting the actual API:

```python
from unittest.mock import Mock, patch

def test_api_call():
    with patch('src.utils.elevenlabs_api.requests.Session') as mock_session:
        mock_response = Mock()
        mock_response.content = b'fake_audio_data'
        mock_session.return_value.post.return_value = mock_response
        
        # Test your code here
```

## Test Coverage Goals

- [ ] 80%+ overall coverage
- [ ] All critical paths tested
- [ ] Error handling verified
- [ ] Integration tests for workflows
