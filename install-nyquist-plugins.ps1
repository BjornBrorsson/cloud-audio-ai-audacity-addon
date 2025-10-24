# Install Nyquist plugins to Audacity
# Automatically detects Audacity plugin directory and copies files

Write-Host "========================================" -ForegroundColor Cyan
Write-Host "Audacity Cloud AI - Plugin Installer" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

# Find Audacity plugin directory
$audacityPaths = @(
    "$env:APPDATA\audacity\Plug-Ins",
    "$env:LOCALAPPDATA\Audacity\Plug-Ins",
    "C:\Program Files\Audacity\Plug-Ins",
    "C:\Program Files (x86)\Audacity\Plug-Ins"
)

$pluginDir = $null
foreach ($path in $audacityPaths) {
    if (Test-Path $path) {
        $pluginDir = $path
        break
    }
}

if (-not $pluginDir) {
    Write-Host "Could not find Audacity plugin directory!" -ForegroundColor Red
    Write-Host ""
    Write-Host "Please manually copy .ny files from nyquist-plugins/ to:" -ForegroundColor Yellow
    Write-Host "  %APPDATA%\audacity\Plug-Ins\" -ForegroundColor White
    Write-Host ""
    Write-Host "Or specify the path:" -ForegroundColor Yellow
    $customPath = Read-Host "Audacity plugin directory path (or press Enter to exit)"
    if ($customPath) {
        $pluginDir = $customPath
    } else {
        exit 1
    }
}

Write-Host "Installing plugins to:" -ForegroundColor Green
Write-Host "  $pluginDir" -ForegroundColor White
Write-Host ""

# Create directory if it doesn't exist
if (-not (Test-Path $pluginDir)) {
    New-Item -ItemType Directory -Path $pluginDir -Force | Out-Null
    Write-Host "Created plugin directory" -ForegroundColor Green
}

# Copy all .ny files
$sourceDir = Join-Path $PSScriptRoot "nyquist-plugins"
$nyFiles = Get-ChildItem -Path $sourceDir -Filter "*.ny"

if ($nyFiles.Count -eq 0) {
    Write-Host "No .ny files found in nyquist-plugins/" -ForegroundColor Red
    exit 1
}

$installed = 0
foreach ($file in $nyFiles) {
    try {
        Copy-Item $file.FullName -Destination $pluginDir -Force
        Write-Host "  ✓ Installed: $($file.Name)" -ForegroundColor Green
        $installed++
    } catch {
        Write-Host "  ✗ Failed: $($file.Name) - $_" -ForegroundColor Red
    }
}

Write-Host ""
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "Installation Complete!" -ForegroundColor Green
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Installed $installed plugin(s)" -ForegroundColor Green
Write-Host ""
Write-Host "Next steps:" -ForegroundColor Cyan
Write-Host "  1. Restart Audacity" -ForegroundColor White
Write-Host "  2. Look for AI features in:" -ForegroundColor White
Write-Host "     - Generate → AI Text-to-Speech, AI Music Generator, AI Sound Effects" -ForegroundColor Gray
Write-Host "     - Effect → AI Voice Isolation" -ForegroundColor Gray
Write-Host "     - Analyze → AI Transcription" -ForegroundColor Gray
Write-Host ""
Write-Host "  3. Make sure Python backend is set up:" -ForegroundColor White
Write-Host "     pip install -r requirements.txt" -ForegroundColor Gray
Write-Host ""
Write-Host "  4. Set your ElevenLabs API key:" -ForegroundColor White
Write-Host "     Create .env file with ELEVENLABS_API_KEY=your_key" -ForegroundColor Gray
Write-Host ""
Write-Host "Documentation: nyquist-plugins/README.md" -ForegroundColor Cyan
Write-Host ""
