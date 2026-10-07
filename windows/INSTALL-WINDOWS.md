# Installing Resynthesizer on Windows (GIMP 3.x)

This guide replaces the outdated instructions that referenced GIMP 2 `.py` scripts.

## Prerequisites

- GIMP 3.0 or later installed on Windows

## Installation

### 1. Find your GIMP plug-ins folder

Open GIMP → **Edit > Preferences > Folders > Plug-ins**. Look for the entry starting with
`C:\Users\<you>\AppData\Roaming\GIMP\` — that is your user plug-ins folder.

### 2. Download the release zip

Download the latest release zip from the [Releases page](https://github.com/bootchk/resynthesizer/releases).

### 3. Extract with the correct folder structure

**GIMP 3 requires each plug-in to be in its own subfolder** where the folder name matches
the executable or script name. The zip is pre-structured — extract its contents directly
into your user plug-ins folder so you end up with:

```
plug-ins/
  resynthesizer/
    resynthesizer.exe
  plug-in-heal-selection/
    plug-in-heal-selection.scm
  plug-in-heal-transparency/
    plug-in-heal-transparency.scm
  plug-in-map-style/
    plug-in-map-style.scm
  plug-in-render-texture/
    plug-in-render-texture.scm
  plug-in-uncrop/
    plug-in-uncrop.scm
  plug-in-resynth-fill-pattern/
    plug-in-resynth-fill-pattern.scm
  plug-in-resynth-controls/
    plug-in-resynth-controls.scm
```

**Do NOT put all files in a single folder** — GIMP 3 will silently skip them.

### 4. Delete the plugin cache (if upgrading)

If you have previously installed plugins or are upgrading, delete the cache file to force
GIMP to re-scan:

```
del "%APPDATA%\GIMP\3.2\pluginrc"
```

(Adjust `3.2` to match your GIMP version.)

### 5. Restart GIMP

The plugins will appear in the Filters menu:

- **Filters > Enhance** — Heal Selection, Heal Transparency, Uncrop
- **Filters > Map** — Map Style
- **Filters > Render** — Render Texture
- **Filters > Fill** — Fill Pattern
- **Filters > Script-Fu** — Resynthesizer Controls

## Troubleshooting

If the plugins don't appear, run GIMP from a terminal with verbose output to see what it finds:

```powershell
& "C:\Users\<you>\AppData\Local\Programs\GIMP 3\bin\gimp-3.exe" --verbose
```

Look for `Querying plug-in:` lines mentioning `resynthesizer` or `plug-in-heal-selection`.

## Building from source (Windows cross-compile)

A Linux-based cross-compilation script is provided in `windows/build-cross-compile.sh`.
It uses a Fedora container with the mingw-w64 toolchain and GIMP development headers
from MSYS2 packages.

## Known limitations

- `plug-in-resynth-sharpen.scm` and `plug-in-resynth-enlarge.scm` may show
  `script-fu-register-i18n` errors on GIMP 3.2 — these are unported plugins
  and can be safely removed from the plug-ins folder if they cause issues.
- `resynthesizer-gui.exe` is not needed in GIMP 3 — the controls are provided by
  the `plug-in-resynth-controls.scm` Script-Fu plugin.
