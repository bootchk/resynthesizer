This is the "engine".

The engine is independent of Gimp.
It takes data in a generic pixmap structure,
not in a structure defined by Gimp.

The engine can also be built independent of GLib.

The engine is currently built as a static library (archive .a).
The engine is not installed, just linked into the "plugin engine."

The engine does many things, according to various arguments.
The most common is "heal selection" but e.g. also "texture transfer."

## Plugins

Now, the plugins are all Gimp plugins.

The "engine plugin" adapts Gimp data structures to the generic pixmap
then calls the engine.

The "control panel plugin" is a rawish GUI to the "engine plugin" arguments.

The other "outer plugins" specialize the "engine plugin" i.e. do one of the "many things"
by massaging an image and formulating a specific set of arguments to the "engine plugin".