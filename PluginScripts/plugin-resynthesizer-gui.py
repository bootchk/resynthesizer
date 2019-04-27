#!/usr/bin/env python

'''
Gimp plugin "Resynthesize"

Copyright 2019 lloyd konneker (bootch at nc.rr.com)


Version:
  1.0 lloyd konneker Initial version in python (original in C w GTK)

License:

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  The GNU Public License is available at
  http://www.gnu.org/copyleft/gpl.html

'''

'''
Implementation notes.

Derived from resynthesizer-gui.c, a C language, GIMP plugin using GTK (hereinafter referred to as "the original.")
Since this replaces the original, the original is now deleted from the repository.

The purpose of replacing the original is to use a higher level language (Python)
and to decrease dependency on the GTK library.
The original was likely to become unmaintainable because GTK had progressed, to version 3.
Now, the onus to use a newer GTK is put onto PyGimp.
I presume that PyGimp will be maintained to use a newer GTK library.

C and GTK allows you to do more (GUI wise) such as:
- enable and disable widgets based on user selections,
- group widgets,
- more immediate semantic checking (in callbacks)
- use a tabbed widget
Some of that user-friendliness is lost in the new plugin.
Namely:
- no tabs for "tweaks"
- no grouping of some widgets under "output" (really, all the controls affect the output)
- no auto enabling within the group of widgets for weight maps
- TODO other losses???

The original called the C-language engine directly, calling gimp_run_procedure2(), passing a parameter struct.
This calls the engine indirectly, calling pdb.plug_in_resynthesizer().
'''



from gimpfu import *

# i18n
gettext.install("resynthesizer", gimp.locale_directory, unicode=True)

debug = False

def resynthesize(timg, tdrawable, sourceDrawable, fillingOrderChoice=0, 
     horizontalTileableChoice=0, verticallyTileableChoice=0, 
     useWeightMapsChoice=0, matchWeightDrawableChoice=None, transferWeightDrawableChoice=None, mapImportanceChoice=.50,
     generateVectorFieldChoice=0):
  '''
  This plugin is the GUI to the engine.

  It does some checking of user choices, for user friendliness.

  Generally one-to-one correspondence between GUI widgets and engine parameters.
  Except for the "generate vector field" checkbox
  '''

  # Some user-friendliness

  # target non-empty?
  if pdb.gimp_selection_is_empty(timg):
    pdb.gimp_message(_("You must first select a region to resynthesize, in the target image."))
    return

  sourceImage = pdb.gimp_drawable_get_image(sourceDrawable)

  # source non-empty
  if pdb.gimp_selection_is_empty(sourceImage):
    pdb.gimp_message(_("You must first select a region to sample from, in the source image."))
    return

  # source not same as target
  # It is really the selections that should not be the same, but same images implies same selections.
  if timg == sourceImage and tdrawable == sourceDrawable:
     pdb.gimp_message(_("Target and corpus images the same makes little sense."));

  # source mode is correct, not INDEXED
  # synchronize mode of target and source
  # FUTURE extract from plugin-map-style.py 
  
  pdb.gimp_image_undo_group_start(timg)
  
  # Mangle empty weight map choice
  
  # Encode into useBorder parameter
  fillingOrder = fillingOrderChoice
  if generateVectorFieldChoice == 1: 
      # TODO fillingOrder += 10
      # TEMP hack
      fillingOrder = 5;
  

  # Note that the API hasn't changed but use_border param now has more values.
  pdb.plug_in_resynthesizer(timg, tdrawable, 
       horizontalTileableChoice, verticalTileableChoice, 
       fillingOrder, sourceDrawable.ID, -1, -1, 0.0, 0.117, 16, 500)
  
  pdb.gimp_image_undo_group_end(timg)


register(
  "python_fu_resynthesize_gui",
  N_("Resynthesize, with all controls."),
  "Requires separate resynthesizer engine plugin.",
  "Lloyd Konneker",
  "2019 Lloyd Konneker",  # Copyright 
  "2019",
  N_("_Resynthesize..."),
  "RGB*, GRAY*",
  [
    (PF_IMAGE, "image",       "Input image", None),    # First two parameters have no rep in GUI
    (PF_DRAWABLE, "drawable", "Input drawable", None),

    (PF_DRAWABLE, "sourceDrawable", _("Source (corpus):"), None),

    (PF_OPTION, "fillingOrderChoice",   _("Filling order:"), 0, [_("Random"),
      _("Inwards towards center"), _("Outwards from center") ]),

    (PF_TOGGLE, "horizontalTileableChoice", _("Make output seamlessly tileable horizontally:"), 0 ),
    (PF_TOGGLE, "verticalTileableChoice", _("Make output seamlessly tileable vertically:"), 0 ),

    # formerly, latter widgets disabled until first widget chosen
    (PF_TOGGLE, "useWeightMapsChoice", _("Use weight maps:"), 0 ),
    (PF_DRAWABLE, "matchWeightDrawableChoice", _("   Weight map for matching:"), None),
    (PF_DRAWABLE, "transferWeightDrawableChoice", _("   Weight map for transferring:"), None),
    (PF_SLIDER, "mapImportanceChoice", _("Map importance:"), 0, (0.01, 1, 0.5)),

    # formerly in the "tweaks" tab


    (PF_TOGGLE, "generateVectorFieldChoice", _("Generate false color vector field of matches:"), 0 )
  ],
  [],
  resynthesize,
  menu="<Image>/Filters/Map",
  domain=("resynthesizer", gimp.locale_directory)
  )

main()
