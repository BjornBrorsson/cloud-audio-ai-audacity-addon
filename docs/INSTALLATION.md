# Installation Guide

This guide will help you install and configure the Audacity Cloud AI plugin.

## Prerequisites

### 1. Software Requirements

- **Audacity 3.0 or later** - [Download Audacity](https://www.audacityteam.org/download/)
- **Python 3.8 or later** - [Download Python](https://www.python.org/downloads/)
- **Git** (optional, for cloning the repository)

### 2. ElevenLabs Account

1. Sign up for a free account at [elevenlabs.io](https://elevenlabs.io)
2. Navigate to your [Profile Settings](https://elevenlabs.io/app/settings/api-keys)
3. Generate an API key
4. **Save this key securely** - you'll need it for configuration

## Installation Steps

### Step 1: Download the Plugin

**Option A: Clone with Git**
```bash
git clone https://github.com/yourusername/audacity-cloudai.git
cd audacity-cloudai
```

**Option B: Download ZIP**
1. Download the repository as ZIP
2. Extract to a folder (e.g., `C:\Coding Projects\Audacity-CloudAI`)

### Step 2: Install Python Dependencies

Open a terminal/command prompt in the plugin directory and run:

```bash
pip install -r requirements.txt
```

Or install with development dependencies:

```bash
pip install -e .
```

### Step 3: Configure Your API Key

Create a `.env` file in the plugin directory:

**Windows:**
```cmd
copy .env.example .env
notepad .env
```

**Mac/Linux:**
```bash
cp .env.example .env
nano .env
```

Edit the `.env` file and add your API key:
```
ELEVENLABS_API_KEY=your_actual_api_key_here
```

### Step 4: Test the Installation

Run the configuration check:

```bash
python audacity_cloudai.py check-config
```

You should see:
```
✓ API Key: Found
✓ API URL: https://api.elevenlabs.io/v1
✓ Default Voice: 21m00Tcm4TlvDq8ikWAM
...
```

### Step 5: Test Generation

Try generating a simple voice-over:

```bash
python audacity_cloudai.py tts "Hello, this is a test" -o test.wav
```

If successful, you'll see:
```
✓ Success! Generated X.XX seconds of speech
✓ Saved to: test.wav
```

## Audacity Integration

### Option 1: Standalone Usage (Recommended for Now)

Use the plugin from the command line:

```bash
# Generate speech
python audacity_cloudai.py tts "Your text here" -o output.wav

# Generate music
python audacity_cloudai.py music "Epic cinematic music" -d 30 -o music.wav
```

Then import the generated files into Audacity:
1. Open Audacity
2. File → Import → Audio
3. Select your generated file

### Option 2: Nyquist Plugin Integration (Advanced)

**Note:** The Nyquist scripts are included but require additional setup to fully integrate with Audacity. This is an advanced feature.

1. Copy the Nyquist files to Audacity's plugin directory:

   **Windows:**
   ```cmd
   copy nyquist\*.ny "%APPDATA%\audacity\Plug-Ins\"
   ```

   **Mac:**
   ```bash
   cp nyquist/*.ny ~/Library/Application\ Support/audacity/Plug-Ins/
   ```

   **Linux:**
   ```bash
   cp nyquist/*.ny ~/.audacity-data/Plug-Ins/
   ```

2. Set the environment variable `AUDACITY_CLOUDAI_PATH`:

   **Windows:**
   ```cmd
   setx AUDACITY_CLOUDAI_PATH "C:\Coding Projects\Audacity-CloudAI"
   ```

   **Mac/Linux:**
   Add to `~/.bashrc` or `~/.zshrc`:
   ```bash
   export AUDACITY_CLOUDAI_PATH="/path/to/audacity-cloudai"
   ```

3. Restart Audacity

4. The plugins should appear under:
   - Generate → ElevenLabs Text-to-Speech
   - Generate → ElevenLabs Music Generation

## Verification

### 1. Check Available Voices

```bash
python audacity_cloudai.py list-voices
```

### 2. Check Available Models

```bash
python audacity_cloudai.py list-models
```

### 3. View Example Music Prompts

```bash
python audacity_cloudai.py example-prompts
```

## Troubleshooting

### "API Key not found"

**Solution:** Ensure your `.env` file exists and contains your API key:
```
ELEVENLABS_API_KEY=your_key_here
```

### "Module not found" errors

**Solution:** Install dependencies:
```bash
pip install -r requirements.txt
```

### "Failed to generate audio"

**Possible causes:**
1. **No internet connection** - The plugin requires internet to access ElevenLabs API
2. **Invalid API key** - Check your API key in the `.env` file
3. **Insufficient credits** - Check your ElevenLabs account balance
4. **Rate limiting** - Wait a few moments and try again

### Plugin not appearing in Audacity

**Solution:**
1. Verify files are in the correct Plug-Ins directory
2. Restart Audacity
3. Check Edit → Preferences → Effects → Enable "Nyquist Prompt"
4. Use the standalone mode instead (recommended)

### Windows: Python not recognized

**Solution:**
1. Install Python from [python.org](https://python.org)
2. During installation, check "Add Python to PATH"
3. Restart your terminal

## Next Steps

- Read the [README.md](README.md) for usage examples
- Check out [QUICKSTART.md](QUICKSTART.md) for quick tutorials
- See [CONTRIBUTING.md](CONTRIBUTING.md) if you want to contribute

## Getting Help

- 🐛 [Report bugs](https://github.com/yourusername/audacity-cloudai/issues)
- 💬 [Ask questions](https://github.com/yourusername/audacity-cloudai/discussions)
- 📧 Email: your.email@example.com

## Uninstallation

To remove the plugin:

1. Delete the plugin directory
2. Remove Nyquist files from Audacity's Plug-Ins directory
3. (Optional) Uninstall Python packages:
   ```bash
   pip uninstall audacity-cloudai elevenlabs requests python-dotenv pydub
   ```
