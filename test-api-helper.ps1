# Quick test of API key helper
Write-Host "Testing API key helper..." -ForegroundColor Cyan
Write-Host ""

$pluginDir = "$env:APPDATA\audacity\Plug-Ins"
$helperPath = Join-Path $pluginDir "api_key_helper.py"
$envPath = Join-Path $pluginDir ".env"

Write-Host "Plugin directory: $pluginDir" -ForegroundColor Gray
Write-Host "Helper exists: $(Test-Path $helperPath)" -ForegroundColor Gray
Write-Host ".env exists: $(Test-Path $envPath)" -ForegroundColor Gray
Write-Host ""

if (Test-Path $helperPath) {
    Write-Host "Running helper script..." -ForegroundColor Yellow
    Push-Location $pluginDir
    python api_key_helper.py
    $exitCode = $LASTEXITCODE
    Pop-Location
    Write-Host ""
    Write-Host "Exit code: $exitCode" -ForegroundColor $(if ($exitCode -eq 0) { "Green" } else { "Red" })
} else {
    Write-Host "Helper script not found!" -ForegroundColor Red
}
