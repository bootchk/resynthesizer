Dir contains outer plugins.

Outer plugins copy or massage an image and call the engine plugin.

Implementations in this repo are in Scheme language (ScriptFu)

Why only ScriptFu:
Python plugins for Gimp v3 uses GIR and is vastly incompatible with v2 GimpFu.
While ScriptFu plugins for v2 are more (but not completely) compatible with Gimp v3.


## Kinds

All but one plugin do specific filter effects.
The plugins encapsulate or know the parameters to the engine
to accomplish the effect.

One plugin is the control panel for the engine.
It lets a user do many effects,
but the user must have deeper knowledge of the engine.

## Changelog

These scripts originate from itr-tert/gimp-resynthesizer-scm repo.
Author is itr-tert.
Who translated from Python to Scheme.

Python originated in bootchk/resynthesizer repo.
Which some were translated from original scripts by Paul Harrison.

Mods since itr-tert:
   - insert shebang, for Gimp v3 standalone interpreter
   - various PDB API name changes
       active=>selected
       width=>get-width
   - various PDB signature changes
      (multilayer: pass drawables vector vs single drawable)

