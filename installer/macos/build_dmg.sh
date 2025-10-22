#!/bin/bash
# Build macOS DMG installer for Audacity Cloud AI

set -e

VERSION="1.0.0"
APP_NAME="Audacity Cloud AI"
DMG_NAME="AudacityCloudAI-v${VERSION}.dmg"
VOLUME_NAME="Audacity Cloud AI"
BUILD_DIR="build/dmg"

echo "Building macOS DMG for ${APP_NAME} v${VERSION}..."

# Create build directory
rm -rf "${BUILD_DIR}"
mkdir -p "${BUILD_DIR}"

# Create app bundle structure
APP_BUNDLE="${BUILD_DIR}/${APP_NAME}.app"
mkdir -p "${APP_BUNDLE}/Contents/MacOS"
mkdir -p "${APP_BUNDLE}/Contents/Resources"

# Copy application files
echo "Copying application files..."
cp -r ../../src "${APP_BUNDLE}/Contents/Resources/"
cp -r ../../docs "${APP_BUNDLE}/Contents/Resources/"
cp -r ../../examples "${APP_BUNDLE}/Contents/Resources/"
cp -r ../../nyquist "${APP_BUNDLE}/Contents/Resources/"
cp ../../*.py "${APP_BUNDLE}/Contents/Resources/"
cp ../../requirements.txt "${APP_BUNDLE}/Contents/Resources/"
cp ../../.env.example "${APP_BUNDLE}/Contents/Resources/"
cp ../../LICENSE "${APP_BUNDLE}/Contents/Resources/"
cp ../../README.md "${APP_BUNDLE}/Contents/Resources/"

# Create launcher script
cat > "${APP_BUNDLE}/Contents/MacOS/${APP_NAME}" << 'EOF'
#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "${DIR}/../Resources"

# Check Python
if ! command -v python3 &> /dev/null; then
    osascript -e 'display dialog "Python 3.8+ is required. Install from python.org or homebrew." buttons {"OK"} default button "OK"'
    exit 1
fi

# Install dependencies if needed
if [ ! -f ".dependencies_installed" ]; then
    osascript -e 'display notification "Installing dependencies..." with title "Audacity Cloud AI"'
    pip3 install -r requirements.txt && touch .dependencies_installed
fi

# Launch GUI
python3 gui_launcher.py
EOF

chmod +x "${APP_BUNDLE}/Contents/MacOS/${APP_NAME}"

# Create Info.plist
cat > "${APP_BUNDLE}/Contents/Info.plist" << EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleName</key>
    <string>${APP_NAME}</string>
    <key>CFBundleDisplayName</key>
    <string>${APP_NAME}</string>
    <key>CFBundleIdentifier</key>
    <string>com.github.bjornbrorsson.audacity-cloud-ai</string>
    <key>CFBundleVersion</key>
    <string>${VERSION}</string>
    <key>CFBundleShortVersionString</key>
    <string>${VERSION}</string>
    <key>CFBundleExecutable</key>
    <string>${APP_NAME}</string>
    <key>CFBundlePackageType</key>
    <string>APPL</string>
    <key>LSMinimumSystemVersion</key>
    <string>10.13</string>
    <key>NSHighResolutionCapable</key>
    <true/>
</dict>
</plist>
EOF

# Copy README to DMG
cp ../../README.md "${BUILD_DIR}/"

# Create symlink to Applications
ln -s /Applications "${BUILD_DIR}/Applications"

# Create DMG
echo "Creating DMG..."
rm -f "../../dist/${DMG_NAME}"
mkdir -p ../../dist

hdiutil create -volname "${VOLUME_NAME}" \
    -srcfolder "${BUILD_DIR}" \
    -ov -format UDZO \
    "../../dist/${DMG_NAME}"

echo ""
echo "SUCCESS! DMG created:"
ls -lh "../../dist/${DMG_NAME}"
echo ""
echo "Users can drag ${APP_NAME}.app to Applications folder"
