# Contributing to Audacity Cloud AI

Thank you for your interest in contributing! This document provides guidelines for contributing to the project.

## Ways to Contribute

- 🐛 **Report bugs** - Found an issue? Let us know!
- 💡 **Suggest features** - Have an idea? We'd love to hear it!
- 📝 **Improve documentation** - Help make the docs clearer
- 🔧 **Submit code** - Fix bugs or add features
- 🎨 **Design improvements** - Better UI/UX ideas welcome
- 🧪 **Add tests** - Help improve code quality

## Getting Started

### 1. Fork and Clone

```bash
# Fork the repository on GitHub, then:
git clone https://github.com/YOUR-USERNAME/audacity-cloudai.git
cd audacity-cloudai
```

### 2. Set Up Development Environment

```bash
# Create virtual environment
python -m venv venv

# Activate it
# Windows:
venv\Scripts\activate
# Mac/Linux:
source venv/bin/activate

# Install dependencies
pip install -r requirements.txt
pip install -r requirements-dev.txt  # If available
```

### 3. Create a Branch

```bash
git checkout -b feature/your-feature-name
# or
git checkout -b fix/bug-description
```

## Development Guidelines

### Code Style

- **Python**: Follow PEP 8
- Use `black` for formatting: `black src/`
- Use `flake8` for linting: `flake8 src/`
- Type hints are encouraged

### Commit Messages

Use clear, descriptive commit messages:

```
✅ Good:
- "Add voice cloning feature"
- "Fix API timeout handling"
- "Update documentation for music generation"

❌ Bad:
- "Update"
- "Fix stuff"
- "Changes"
```

### Testing

Before submitting:

```bash
# Run tests (if available)
pytest

# Test your changes manually
python audacity_cloudai.py check-config
python audacity_cloudai.py tts "Test" -o test.wav
```

## Submitting Changes

### Pull Request Process

1. **Update documentation** if needed
2. **Test your changes** thoroughly
3. **Commit your changes**:
   ```bash
   git add .
   git commit -m "Add feature: description"
   ```

4. **Push to your fork**:
   ```bash
   git push origin feature/your-feature-name
   ```

5. **Create Pull Request** on GitHub

6. **Describe your changes**:
   - What does this PR do?
   - Why is this change needed?
   - Any breaking changes?
   - Screenshots (if UI changes)

### PR Template

```markdown
## Description
Brief description of changes

## Type of Change
- [ ] Bug fix
- [ ] New feature
- [ ] Documentation update
- [ ] Performance improvement

## Testing
How was this tested?

## Checklist
- [ ] Code follows project style
- [ ] Documentation updated
- [ ] Tests added/updated
- [ ] All tests pass
```

## Feature Requests

Have an idea? [Open an issue](https://github.com/yourusername/audacity-cloudai/issues/new) with:

- **Clear title**: What you want to add
- **Use case**: Why is this useful?
- **Proposal**: How it might work
- **Examples**: Mock-ups or examples if applicable

## Bug Reports

Found a bug? [Report it](https://github.com/yourusername/audacity-cloudai/issues/new) with:

1. **Title**: Short description
2. **Environment**:
   - OS version
   - Python version
   - Plugin version
3. **Steps to reproduce**
4. **Expected behavior**
5. **Actual behavior**
6. **Logs/screenshots** if available

### Bug Report Template

```markdown
## Bug Description
What happened?

## Environment
- OS: [e.g., Windows 11]
- Python: [e.g., 3.11.0]
- Plugin Version: [e.g., 1.0.0]

## Steps to Reproduce
1. Run command...
2. See error...

## Expected Behavior
What should happen?

## Actual Behavior
What actually happened?

## Logs
```
paste error messages here
```

## Screenshots
If applicable
```

## Areas We Need Help

### High Priority
- 🔧 **Full Audacity integration** - Native plugin interface
- 🧪 **Unit tests** - Test coverage for all modules
- 📱 **GUI wrapper** - Standalone GUI application
- 🌍 **Internationalization** - Multi-language support

### Medium Priority
- 📝 **More examples** - Sample projects and use cases
- 🎨 **Voice library browser** - Better voice selection UI
- ⚡ **Performance optimization** - Caching, batch processing
- 📊 **Usage analytics** - Track generation stats

### Good First Issues
- 📚 **Documentation improvements**
- 🐛 **Minor bug fixes**
- ✨ **Code cleanup**
- 🎯 **Add more example prompts**

## Code Structure

```
audacity-cloudai/
├── src/
│   ├── generators/       # TTS and music generators
│   │   ├── text_to_speech.py
│   │   └── music_generator.py
│   └── utils/            # Utilities
│       ├── config.py
│       ├── elevenlabs_api.py
│       └── audio_utils.py
├── nyquist/             # Audacity plugin files
├── tests/               # Unit tests
└── audacity_cloudai.py  # Main CLI
```

## Development Tips

### Adding a New Generator

1. Create file in `src/generators/`
2. Inherit from base generator (if exists) or create standalone
3. Add to `src/generators/__init__.py`
4. Update `audacity_cloudai.py` CLI
5. Add documentation
6. Write tests

### Adding a New API Endpoint

1. Add method to `src/utils/elevenlabs_api.py`
2. Add docstring with parameters
3. Handle errors appropriately
4. Update relevant generator to use it

## Release Process

For maintainers:

1. Update version in `setup.py`
2. Update `CHANGELOG.md`
3. Create git tag: `git tag v1.x.x`
4. Push tag: `git push origin v1.x.x`
5. Create GitHub release

## Code of Conduct

### Our Standards

- **Be respectful** and inclusive
- **Be patient** with others
- **Accept constructive criticism** gracefully
- **Focus on what's best** for the community

### Unacceptable Behavior

- Harassment or discrimination
- Trolling or insulting comments
- Publishing others' private information
- Other unprofessional conduct

## Questions?

- 💬 [Start a discussion](https://github.com/yourusername/audacity-cloudai/discussions)
- 📧 Email: your.email@example.com
- 🐦 Twitter: @yourhandle

## License

By contributing, you agree that your contributions will be licensed under the MIT License.

---

Thank you for contributing to Audacity Cloud AI! 🎉
