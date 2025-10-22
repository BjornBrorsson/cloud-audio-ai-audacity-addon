# Building Installers

Quick guide to building platform-specific installers locally.

## 🪟 Windows Installer (.exe)

### Step 1: Install Inno Setup
Download from: https://jrsoftware.org/isdl.php

### Step 2: Build
```cmd
cd installer\windows
build_installer.bat
```

### Output
`dist\AudacityCloudAI-Setup-v1.0.0.exe` - Ready to distribute!

**Features:**
- Full GUI installer wizard
- Python version check
- Desktop & Start Menu shortcuts
- Automatic dependency installation
- Professional uninstaller

---

## 🐧 Linux Package (.deb)

### Step 1: Install build tools (if needed)
```bash
sudo apt-get update
sudo apt-get install build-essential dpkg-dev
```

### Step 2: Build
```bash
cd installer/linux
chmod +x build_deb.sh
./build_deb.sh
```

### Output
`dist/audacity-cloud-ai_1.0.0_all.deb` - Ready to distribute!

**Install:**
```bash
sudo dpkg -i audacity-cloud-ai_1.0.0_all.deb
```

**Features:**
- Proper Debian/Ubuntu package
- System-wide installation
- Desktop launcher
- CLI commands in PATH (`audacity-cloudai`, `audacity-cloudai-gui`)

---

## 🍎 macOS Disk Image (.dmg)

### Requirements
macOS with Xcode Command Line Tools:
```bash
xcode-select --install
```

### Build
```bash
cd installer/macos
chmod +x build_dmg.sh
./build_dmg.sh
```

### Output
`dist/AudacityCloudAI-v1.0.0.dmg` - Ready to distribute!

**Features:**
- Native .app bundle
- Drag-to-Applications installer
- Automatic Python check
- Retina-ready

---

## 🤖 Automatic Building (CI/CD)

Installers are automatically built when you create a release:

```bash
git tag -a v1.0.1 -m "Release v1.0.1"
git push origin v1.0.1
```

GitHub Actions will:
1. ✅ Build all three installers
2. ✅ Create GitHub Release
3. ✅ Attach installers to release
4. ✅ Generate release notes

---

## 📦 Distribution Checklist

After building:

- [ ] Test installer on clean system
- [ ] Verify all dependencies install correctly
- [ ] Check desktop shortcut works
- [ ] Test uninstaller (Windows)
- [ ] Upload to GitHub Releases
- [ ] Update download links in README
- [ ] Announce release

---

## 🎯 Quick Test

### Windows
1. Run `.exe` installer
2. Click through wizard
3. Launch from Desktop or Start Menu
4. Verify GUI opens

### Linux
```bash
sudo dpkg -i audacity-cloud-ai_*.deb
audacity-cloud-ai-gui
```

### macOS
1. Mount `.dmg`
2. Drag to Applications
3. Launch from Applications folder
4. Verify GUI opens

---

**See [installer/README.md](installer/README.md) for detailed documentation.**
