#!/bin/bash
# Build Debian package for Audacity Cloud AI

set -e

VERSION="1.0.0"
PACKAGE_NAME="audacity-cloud-ai"
BUILD_DIR="build/${PACKAGE_NAME}_${VERSION}"
DEB_DIR="${BUILD_DIR}/DEBIAN"

echo "Building Debian package for Audacity Cloud AI v${VERSION}..."

# Create directory structure
mkdir -p "${BUILD_DIR}/usr/share/${PACKAGE_NAME}"
mkdir -p "${BUILD_DIR}/usr/bin"
mkdir -p "${BUILD_DIR}/usr/share/applications"
mkdir -p "${BUILD_DIR}/usr/share/doc/${PACKAGE_NAME}"
mkdir -p "${DEB_DIR}"

# Copy application files
echo "Copying application files..."
cp -r ../../src "${BUILD_DIR}/usr/share/${PACKAGE_NAME}/"
cp -r ../../docs "${BUILD_DIR}/usr/share/${PACKAGE_NAME}/"
cp -r ../../examples "${BUILD_DIR}/usr/share/${PACKAGE_NAME}/"
cp -r ../../nyquist "${BUILD_DIR}/usr/share/${PACKAGE_NAME}/"
cp ../../*.py "${BUILD_DIR}/usr/share/${PACKAGE_NAME}/"
cp ../../requirements.txt "${BUILD_DIR}/usr/share/${PACKAGE_NAME}/"
cp ../../.env.example "${BUILD_DIR}/usr/share/${PACKAGE_NAME}/"

# Copy documentation
cp ../../LICENSE "${BUILD_DIR}/usr/share/doc/${PACKAGE_NAME}/"
cp ../../CHANGELOG.md "${BUILD_DIR}/usr/share/doc/${PACKAGE_NAME}/"
cp ../../README.md "${BUILD_DIR}/usr/share/doc/${PACKAGE_NAME}/"

# Create launcher scripts
cat > "${BUILD_DIR}/usr/bin/audacity-cloud-ai" << 'EOF'
#!/bin/bash
cd /usr/share/audacity-cloud-ai
python3 audacity_cloudai.py "$@"
EOF

cat > "${BUILD_DIR}/usr/bin/audacity-cloud-ai-gui" << 'EOF'
#!/bin/bash
cd /usr/share/audacity-cloud-ai
python3 gui_launcher.py
EOF

chmod +x "${BUILD_DIR}/usr/bin/audacity-cloud-ai"
chmod +x "${BUILD_DIR}/usr/bin/audacity-cloud-ai-gui"

# Create desktop entry
cat > "${BUILD_DIR}/usr/share/applications/${PACKAGE_NAME}.desktop" << EOF
[Desktop Entry]
Name=Audacity Cloud AI
Comment=AI-powered audio generation for Audacity
Exec=audacity-cloud-ai-gui
Terminal=false
Type=Application
Categories=AudioVideo;Audio;
Keywords=audacity;audio;ai;tts;music;
EOF

# Create control file
cat > "${DEB_DIR}/control" << EOF
Package: ${PACKAGE_NAME}
Version: ${VERSION}
Section: sound
Priority: optional
Architecture: all
Depends: python3 (>= 3.8), python3-pip, python3-tk
Maintainer: Bjorn Brorsson <bjorn@example.com>
Description: AI-powered audio generation plugin for Audacity
 Generate speech, music, and sound effects using ElevenLabs Cloud APIs.
 Features include text-to-speech, music generation, sound effects,
 voice isolation, and audio transcription. Includes both CLI and GUI.
Homepage: https://github.com/BjornBrorsson/cloud-audio-ai-audacity-addon
EOF

# Create postinst script
cat > "${DEB_DIR}/postinst" << 'EOF'
#!/bin/bash
set -e

echo "Installing Python dependencies..."
pip3 install -r /usr/share/audacity-cloud-ai/requirements.txt 2>/dev/null || true

echo ""
echo "Audacity Cloud AI installed successfully!"
echo "To configure, edit: ~/.config/audacity-cloud-ai/.env"
echo "Launch with: audacity-cloud-ai-gui"
echo ""

exit 0
EOF

chmod +x "${DEB_DIR}/postinst"

# Build the package
echo "Building package..."
dpkg-deb --build "${BUILD_DIR}" "../../dist/${PACKAGE_NAME}_${VERSION}_all.deb"

echo ""
echo "SUCCESS! Package created:"
ls -lh "../../dist/${PACKAGE_NAME}_${VERSION}_all.deb"
echo ""
echo "Install with: sudo dpkg -i ${PACKAGE_NAME}_${VERSION}_all.deb"
