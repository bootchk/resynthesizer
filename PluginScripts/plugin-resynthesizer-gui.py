#!/usr/bin/env python

'''
Gimp plugin "Map>Resynthesize"

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
It registers the same way as the original: Map>Resynthesize
Since this replaces the original, the source code for the original is now deleted from the repository.

Purpose of replacing the original:
- use a higher level language (Python and PyGimp)
- and decrease dependency on the GTK library.
The original was likely to become obsolete because GTK had progressed, to version 3.
Now, the onus to use a newer GTK is put onto PyGimp.
I presume that PyGimp will be maintained to use a newer GTK library.

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

The original called the C-language engine directly, calling gimp_run_procedure2(), passing a parameter struct.
This calls the engine indirectly, calling pdb.plug_in_resynthesizer().
'''



from gimpfu import *

# i18n
gettext.install("resynthesizer", gimp.locale_directory, unicode=True)

debug = False

def resynthesize(timg, tdrawable, sourceDrawable, fillingOrderChoice=0, 
     horizontalTileableChoice=0, verticalTileableChoice=0, 
     useWeightMapsChoice=0, matchWeightDrawableChoice=None, transferWeightDrawableChoice=None, mapImportanceChoice=.50,
     neighborhoodSizeChoice=30,
     searchThoroughnessChoice=200,
     sensitivityToOutliersChoice=0.12,
     generateVectorFieldChoice=0):
  '''
  This plugin is the GUI to the engine.

  It does some checking of user choices, for user friendliness.

  Generally one-to-one correspondence between GUI widgets and engine parameters.
  Except for the "generate vector field" checkbox
  '''

  # User-friendliness considerations

  # empty selection in target is not an error, the engine will select all
  # if pdb.gimp_selection_is_empty(timg):

  # empty selection in source is not an error, the engine will select all
  # if pdb.gimp_selection_is_empty(sourceImage):

  # source same as target is not an error.
  # Random fill order will generally produce no visible change,
  # but some fill orders may produce a subtly altered result.
  # Same images implies same selections.
  # if timg == sourceImage and tdrawable == sourceDrawable:

  # The source image chooser widget (provided by PyGimp) does NOT filter by image mode.
  sourceImage = pdb.gimp_drawable_get_image(sourceDrawable)
  source_image_mode = pdb.gimp_image_base_type(sourceImage)
  target_image_mode = pdb.gimp_image_base_type(timg)
  
  # Engine requires source mode NOT be INDEXED, else it crashes.
  if source_image_mode == INDEXED :
    pdb.gimp_message(_("The source image cannot be of mode INDEXED"));
    return

  # Engine requires base modes same (but allows differences in alpha)
  if target_image_mode != source_image_mode :
    pdb.gimp_message(_("The source and target images must have same mode except for alpha e.g. RGB and RGBA"));
    return



  pdb.gimp_image_undo_group_start(timg)
  
  # Implement alternative to GTK widget enabling.
  # The map image choosers are always enabled but only pass them to engine if "use weight maps" checkbox is Yes
  if useWeightMapsChoice == 0:
     # None => -1
     matchingMap = -1
     transferringMap = -1
  else:
     matchingMap = matchWeightDrawableChoice.ID
     transferringMap = transferWeightDrawableChoice.ID
  # Resynthesizer ignores mapWeight param if maps are none
     
  
  # Encode into useBorder parameter
  # Add 10 to mean: use same filling order, but post process to deliver false color
  fillingOrder = fillingOrderChoice
  if generateVectorFieldChoice == 1:
      fillingOrder += 10
  
  # engine API hasn't changed in a long time, but use_border param now has more values.

  # Order of params different from order in GUI
  pdb.plug_in_resynthesizer(timg, tdrawable, 
       horizontalTileableChoice, verticalTileableChoice, 
       fillingOrder, sourceDrawable.ID,
       matchingMap, transferringMap, mapImportanceChoice,
       sensitivityToOutliersChoice,
       neighborhoodSizeChoice,
       searchThoroughnessChoice
       )
  
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

    (PF_OPTION, "fillingOrderChoice",   _("Randomized filling order:"), 1, [
      _("Without context"),
      _("Without bands"),
      _("Bands concentric, inward (squeeze)"),
      _("Bands horizontal, inward (squeeze from top and bottom)"),
      _("Bands vertical, inward (squeeze from left and right)"),
      _("Bands concentric, outward (e.g. for uncrop)"),
      _("Bands horizontal, outward (expand to top and bottom)"),
      _("Bands vertical, outward (expand to left and right)"),
      _("Bands concentric, inward and outward (squeeze donut)"),
       ]),

    (PF_TOGGLE, "horizontalTileableChoice", _("Make output seamlessly tileable horizontally:"), 0 ),
    (PF_TOGGLE, "verticalTileableChoice", _("Make output seamlessly tileable vertically:"), 0 ),

    # formerly, latter widgets disabled until first widget chosen
    # Now, indentation of label indicates grouping
    (PF_TOGGLE, "useWeightMapsChoice", _("Use weight maps:"), 0 ),
    (PF_DRAWABLE, "matchWeightDrawableChoice", _("     Weight map for matching:"), None),
    (PF_DRAWABLE, "transferWeightDrawableChoice", _("     Weight map for transferring:"), None),
    (PF_SLIDER, "mapImportanceChoice", _("     Map importance:"), 0.5, (0.01, 1, 0.01)),

    # Spinner is digital and linear, slider is analog but exponential
    # extra tuple is (min, max, step)
    #(PF_SPINNER, "resize_ratio", _("foo"), 2, (0.5, 10, 0.5)),

    # formerly in the "tweaks" tab
    (PF_SLIDER, "neighborhoodSizeChoice", _("Neighborhood size:"), 30, (1, 100, 1)),
    (PF_SLIDER, "searchThoroughnessChoice", _("Search thoroughness:"), 200, (1, 500, 1)),
    (PF_SLIDER, "sensitivityToOutliersChoice", _("Sensitivity to outliers:"), 0.12, (0.1, 1, 0.01)),

    (PF_TOGGLE, "generateVectorFieldChoice", _("Generate false color field of match coords:"), 0 )
  ],
  [],
  resynthesize,
  menu="<Image>/Filters/Map",
  domain=("resynthesizer", gimp.locale_directory)
  )

main()
