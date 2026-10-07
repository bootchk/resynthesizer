# Resynthesizer installer for GIMP 3.x on Windows
# Extracts the release zip into the correct GIMP 3 plug-in folder structure.
#
# Usage: .\install.ps1 [-ZipPath path\to\resynthesizer.zip] [-GimpVersion 3.2]
#
# GIMP 3 requires each plug-in in its own named subfolder. This script
# sets up that structure automatically.

param(
    [string]$ZipPath = "",
    [string]$GimpVersion = "3.2"
)

$ErrorActionPreference = "Stop"

# Find the GIMP user plug-ins folder
$plugInBase = Join-Path $env:APPDATA "GIMP\$GimpVersion\plug-ins"
if (-not (Test-Path $plugInBase)) {
    Write-Host "GIMP plug-ins folder not found at: $plugInBase" -ForegroundColor Red
    Write-Host "Check your GIMP version (Edit > Preferences > Folders > Plug-ins)" -ForegroundColor Yellow
    exit 1
}

Write-Host "GIMP plug-ins folder: $plugInBase" -ForegroundColor Green

# If no zip path given, look for one in the current directory
if (-not $ZipPath) {
    $zips = Get-ChildItem -Filter "*.zip" | Where-Object { $_.Name -match "resynth" }
    if ($zips.Count -eq 1) {
        $ZipPath = $zips[0].FullName
    } else {
        Write-Host "Usage: .\install.ps1 -ZipPath path\to\resynthesizer.zip" -ForegroundColor Yellow
        exit 1
    }
}

if (-not (Test-Path $ZipPath)) {
    Write-Host "Zip file not found: $ZipPath" -ForegroundColor Red
    exit 1
}

Write-Host "Using zip: $ZipPath" -ForegroundColor Green

# Extract to a temp directory
$tempDir = Join-Path $env:TEMP "resynth-install-$(Get-Random)"
Expand-Archive -Path $ZipPath -DestinationPath $tempDir -Force

# Find the folder containing the actual plugin files (handles both flat and nested zips)
$pluginSource = $tempDir
$scmFiles = Get-ChildItem $tempDir -Filter "*.scm" -Recurse
if ($scmFiles.Count -eq 0) {
    Write-Host "No .scm files found in the zip!" -ForegroundColor Red
    Remove-Item $tempDir -Recurse -Force
    exit 1
}

# Use the directory containing the .scm files as the source root
$pluginSource = $scmFiles[0].Directory.FullName

# Move resynthesizer.exe into its own folder
$exe = Get-ChildItem $pluginSource -Filter "resynthesizer.exe" -Recurse | Select-Object -First 1
if ($exe) {
    $exeDest = Join-Path $plugInBase "resynthesizer"
    New-Item -ItemType Directory -Path $exeDest -Force | Out-Null
    Copy-Item $exe.FullName -Destination $exeDest -Force
    Write-Host "  Installed: resynthesizer\resynthesizer.exe" -ForegroundColor Green
}

# Move each .scm file into its own named folder
foreach ($scm in $scmFiles) {
    $folderName = $scm.BaseName  # e.g. "plug-in-heal-selection"
    $dest = Join-Path $plugInBase $folderName
    New-Item -ItemType Directory -Path $dest -Force | Out-Null
    Copy-Item $scm.FullName -Destination $dest -Force
    Write-Host "  Installed: $folderName\$($scm.Name)" -ForegroundColor Green
}

# Clean up
Remove-Item $tempDir -Recurse -Force

# Delete plugin cache to force re-scan
$pluginrc = Join-Path $env:APPDATA "GIMP\$GimpVersion\pluginrc"
if (Test-Path $pluginrc) {
    Remove-Item $pluginrc -Force
    Write-Host "Deleted plugin cache: $pluginrc" -ForegroundColor Yellow
}

Write-Host ""
Write-Host "Done! Restart GIMP to load the plugins." -ForegroundColor Green
Write-Host "They will appear under Filters > Enhance, Map, Render, Fill, and Script-Fu." -ForegroundColor Green
