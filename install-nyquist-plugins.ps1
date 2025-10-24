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
        Write-Host "  [OK] Installed: $($file.Name)" -ForegroundColor Green
        $installed++
    } catch {
        Write-Host "  [FAILED] $($file.Name) - $_" -ForegroundColor Red
    }
}

# Copy Python helper scripts
$helperFiles = @("api_key_helper.py", "audacity_cloudai.py")
foreach ($helperFile in $helperFiles) {
    $helperPath = Join-Path $PSScriptRoot $helperFile
    if (Test-Path $helperPath) {
        try {
            Copy-Item $helperPath -Destination $pluginDir -Force
            Write-Host "  [OK] Installed: $helperFile" -ForegroundColor Green
            $installed++
        } catch {
            Write-Host "  [FAILED] $helperFile - $_" -ForegroundColor Red
        }
    }
}

# Copy src directory for Python modules
$srcDir = Join-Path $PSScriptRoot "src"
if (Test-Path $srcDir) {
    $destSrcDir = Join-Path $pluginDir "src"
    try {
        if (Test-Path $destSrcDir) {
            Remove-Item $destSrcDir -Recurse -Force
        }
        Copy-Item $srcDir -Destination $destSrcDir -Recurse -Force
        Write-Host "  [OK] Installed: src/ (Python modules)" -ForegroundColor Green
    } catch {
        Write-Host "  [FAILED] Could not copy src directory - $_" -ForegroundColor Red
    }
}

Write-Host ""
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "Installation Complete!" -ForegroundColor Green
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Installed $installed plugin(s)" -ForegroundColor Green
Write-Host ""

# Check for .env file and offer to create it (in plugins directory)
$envFilePlugin = Join-Path $pluginDir ".env"
if (-not (Test-Path $envFilePlugin)) {
    Write-Host "API Key Setup" -ForegroundColor Cyan
    Write-Host "-------------" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "No API key found. Would you like to set up your ElevenLabs API key now?" -ForegroundColor Yellow
    Write-Host ""
    Write-Host "You can get a free API key at: https://elevenlabs.io" -ForegroundColor Gray
    Write-Host ""
    $response = Read-Host "Enter your API key (or press Enter to skip)"
    
    if ($response -and $response.Trim().Length -gt 0) {
        try {
            # Create .env file in plugins directory
            $envContent = "ELEVENLABS_API_KEY=$($response.Trim())"
            Set-Content -Path $envFilePlugin -Value $envContent -NoNewline
            Write-Host ""
            Write-Host "[OK] API key saved!" -ForegroundColor Green
            Write-Host "     Location: $envFilePlugin" -ForegroundColor Gray
            Write-Host ""
        } catch {
            Write-Host ""
            Write-Host "[FAILED] Could not create .env file: $_" -ForegroundColor Red
            Write-Host "The plugins will prompt you for the API key when first used." -ForegroundColor Yellow
            Write-Host ""
        }
    } else {
        Write-Host ""
        Write-Host "Skipped API key setup." -ForegroundColor Yellow
        Write-Host "The plugins will prompt you for the API key when first used." -ForegroundColor Yellow
        Write-Host ""
    }
}

Write-Host "Next steps:" -ForegroundColor Cyan
Write-Host "  1. Make sure Python backend is set up:" -ForegroundColor White
Write-Host "     pip install -r requirements.txt" -ForegroundColor Gray
Write-Host ""
Write-Host "  2. Restart Audacity" -ForegroundColor White
Write-Host ""
Write-Host "  3. Look for AI features in:" -ForegroundColor White
Write-Host "     - Generate > AI Text-to-Speech, AI Music Generator, AI Sound Effects" -ForegroundColor Gray
Write-Host "     - Effect > AI Voice Isolation" -ForegroundColor Gray
Write-Host "     - Analyze > AI Transcription" -ForegroundColor Gray
Write-Host ""
Write-Host "Documentation: nyquist-plugins/README.md" -ForegroundColor Cyan
Write-Host ""
