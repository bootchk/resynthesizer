About plugin-resynth-controls.scm

History:
  Before resynthesizer v3, a C language plugin using GTK library
  Since  resynthesizer deprecatations branch, in python using GimpFu declarative GUI
  Since  resynthesizer3 branch, in ScriptFu Scheme with declarative GUI

Implementation notes.

Derived from resynthesizer-gui.c, a C language, GIMP plugin using GTK (hereinafter referred to as "the original.")
It registers the same way as the original: Map>Resynthesize
Since this replaces the original, the source code for the original is now deleted from the repository.

Purpose of replacing the original:
- use a higher level language (Python and GimpFu)
- and decrease dependency on the GTK library.
The original was likely to become obsolete because it used obsolete Gtk2. Now uses Gtk3.
Now, the onus to use a newer GTK is put onto GimpFu.
I presume that GimpFu will be maintained to use a newer GTK library.

The plugin is essentially a control panel.
It mostly just passes widget (control) values straight to the resynthesizer engine plugin.
With some exceptions:
- many GUI controls are mapped into the useBorder parameter of the engine
- some error checking is done here, to avoid crashing the engine

C and GTK allows you to do more (GUI wise) such as:
- enable and disable widgets based on user selections,
- group widgets,
- more immediate semantic checking (in callbacks, not just when OK is clicked)
- use a tabbed widget
Some of that user-friendliness is lost in the new plugin.
Namely:
- no tabs for "tweaks" group of controls
- no grouping of some widgets under "output" (really, all the controls affect the output)
- no auto enabling within the group of widgets for weight maps

The original called the C-language engine directly,
calling gimp_run_procedure2(), passing a parameter struct.
This calls the engine indirectly, calling pdb.plug_in_resynthesizer().
