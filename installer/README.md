# Installer Build Scripts

This directory contains scripts to build platform-specific installers.

## 🪟 Windows Installer

### Requirements
- [Inno Setup](https://jrsoftware.org/isdl.php) 6.0+

### Build
```bash
cd installer/windows
build_installer.bat
```

**Output:** `dist/AudacityCloudAI-Setup-v1.0.0.exe`

**Features:**
- ✅ Full GUI installer
- ✅ Python version check
- ✅ Desktop shortcut creation
- ✅ Start menu entry
- ✅ Automatic dependency installation
- ✅ Clean uninstaller

---

## 🐧 Linux Package (.deb)

### Requirements
- `dpkg-deb` (usually pre-installed)
- Build tools: `sudo apt-get install build-essential`

### Build
```bash
cd installer/linux
chmod +x build_deb.sh
./build_deb.sh
```

**Output:** `dist/audacity-cloud-ai_1.0.0_all.deb`

**Install:**
```bash
sudo dpkg -i audacity-cloud-ai_1.0.0_all.deb
sudo apt-get install -f  # Install dependencies
```

**Features:**
- ✅ Proper Debian package
- ✅ System-wide installation
- ✅ Automatic dependency handling
- ✅ Desktop launcher
- ✅ Command-line tools in PATH

---

## 🍎 macOS Disk Image (.dmg)

### Requirements
- macOS with Xcode Command Line Tools
- `hdiutil` (pre-installed on macOS)

### Build
```bash
cd installer/macos
chmod +x build_dmg.sh
./build_dmg.sh
```

**Output:** `dist/AudacityCloudAI-v1.0.0.dmg`

**Features:**
- ✅ Drag-to-Applications installer
- ✅ Native .app bundle
- ✅ Python version check
- ✅ Automatic dependency installation
- ✅ Retina-ready

---

## 📦 Alternative: PyPI Package

For all platforms, users can also install via pip:

```bash
pip install audacity-cloud-ai
```

Then run:
```bash
audacity-cloudai-gui  # GUI
audacity-cloudai --help  # CLI
```

---

## 🤖 CI/CD Integration

The GitHub Actions workflow (`.github/workflows/release.yml`) automatically builds installers when you create a release tag.

**Manual trigger:**
```bash
git tag -a v1.0.1 -m "Release v1.0.1"
git push origin v1.0.1
```

The workflow will:
1. Build Windows installer (if Inno Setup available)
2. Build Linux .deb package
3. Build macOS .dmg
4. Attach all to GitHub Release

---

## 🔧 Testing Installers

### Windows
1. Run the `.exe` installer
2. Follow the wizard
3. Launch from Start Menu or Desktop

### Linux
```bash
sudo dpkg -i audacity-cloud-ai_1.0.0_all.deb
audacity-cloud-ai-gui
```

### macOS
1. Open the `.dmg` file
2. Drag app to Applications
3. Launch from Applications folder

---

## 📝 Notes

- **Windows**: Installer requires admin rights only if installing to Program Files
- **Linux**: Supports Ubuntu 20.04+, Debian 11+, and derivatives
- **macOS**: Requires macOS 10.13 (High Sierra) or later
- **Python**: All installers check for Python 3.8+ and prompt if missing

---

## 🐛 Troubleshooting

### Windows: "Unknown Publisher" Warning
This is normal for unsigned installers. Users can click "More info" → "Run anyway"

To sign the installer, you need a code signing certificate.

### Linux: Dependency Issues
Run: `sudo apt-get install -f` to fix missing dependencies

### macOS: "App can't be opened because it is from an unidentified developer"
Users need to: Right-click → Open → Open

To avoid this, sign the app with an Apple Developer certificate.

---

**Questions?** See the main [README](../README.md) or [docs/](../docs/)
