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

