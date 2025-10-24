# Quick script to create .env file for Audacity plugins
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "Create API Key File for Audacity Plugins" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

$pluginDir = "$env:APPDATA\audacity\Plug-Ins"
$envFile = Join-Path $pluginDir ".env"

Write-Host "This will create an .env file at:" -ForegroundColor Yellow
Write-Host "  $envFile" -ForegroundColor White
Write-Host ""

if (Test-Path $envFile) {
    Write-Host "Warning: .env file already exists!" -ForegroundColor Yellow
    $overwrite = Read-Host "Overwrite existing file? (y/n)"
    if ($overwrite -ne "y") {
        Write-Host "Cancelled." -ForegroundColor Gray
        exit 0
    }
}

Write-Host "Get a free API key at: https://elevenlabs.io" -ForegroundColor Cyan
Write-Host ""
$apiKey = Read-Host "Enter your ElevenLabs API key"

if (-not $apiKey -or $apiKey.Trim().Length -lt 10) {
    Write-Host ""
    Write-Host "Error: Invalid API key" -ForegroundColor Red
    exit 1
}

try {
    # Create directory if it doesn't exist
    if (-not (Test-Path $pluginDir)) {
        New-Item -ItemType Directory -Path $pluginDir -Force | Out-Null
    }
    
    # Create .env file
    $content = "ELEVENLABS_API_KEY=$($apiKey.Trim())"
    Set-Content -Path $envFile -Value $content -NoNewline
    
    Write-Host ""
    Write-Host "[OK] API key saved successfully!" -ForegroundColor Green
    Write-Host ""
    Write-Host "You can now use the Audacity Cloud AI plugins!" -ForegroundColor Green
    Write-Host ""
    
} catch {
    Write-Host ""
    Write-Host "[FAILED] Could not create .env file: $_" -ForegroundColor Red
    Write-Host ""
    Write-Host "Try creating the file manually:" -ForegroundColor Yellow
    Write-Host "  1. Open Notepad" -ForegroundColor White
    Write-Host "  2. Type: ELEVENLABS_API_KEY=your_key_here" -ForegroundColor White
    Write-Host "  3. Save as: $envFile" -ForegroundColor White
    Write-Host ""
    exit 1
}
