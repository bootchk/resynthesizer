#!/usr/bin/python3

'''
Gimp plugin "Heal selection"

Copyright 2009 lloyd konneker (bootch at nc.rr.com)
Based on smart_remove.scm Copyright 2000 by Paul Harrison.

Version:
  1.0 lloyd konneker lkk 9/21/2009 Initial version in python.
  (See release notes for differences over P. Harrison's prior version in scheme language.)

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
import gi
import sys

gi.require_version('Gimp', '3.0')
gi.require_version('GimpUi', '3.0')

from gi.repository import GLib
from gi.repository import GObject
from gi.repository import Gimp
from gi.repository import GimpUi

PLUGIN_NAME = 'resynthesizer-heal-selection'
def N_(message): return message
def _(message): return GLib.dgettext(None, message)

debug = False

def dprint(s: str):
  if debug: print(s)

class HealSel (Gimp.PlugIn):
  ## GimpPlugIn virtual methods ##
  def do_set_i18n(self, procname):
    return True, 'gimp30-python', None

  def do_query_procedures(self):
    return [PLUGIN_NAME]

  def do_create_procedure(self, name):
    if name == PLUGIN_NAME:
      procedure: Gimp.ImageProcedure = Gimp.ImageProcedure.new(self, name,
                                                               Gimp.PDBProcType.PLUGIN,
                                                               self.run, None)
      procedure.set_image_types("RGB*, GRAY*")
      procedure.set_sensitivity_mask(
          Gimp.ProcedureSensitivityMask.DRAWABLE)
      procedure.set_documentation(_("resynthesizer heal selection"),
                                  _("heal selection with the resynthesizer algorithm"),
                                  name)
      procedure.set_menu_label(_("_heal selection"))
      procedure.set_attribution("James Henstridge",
                                "James Henstridge",
                                "1999,2007")
      procedure.add_menu_path("<Image>/Filters/Enhance")

      procedure.add_int_argument(
          name="samplingRadiusParam",
          nick=_("Sampling radius"),
          blurb=_("The sampling radius (in pixels)"),
          min=1,
          max=1000,
          value=50,
          flags=GObject.ParamFlags.READWRITE
      )

      direction_choice = Gimp.Choice.new()

      direction_choice.add("all_around", 0, _("All around"), "")
      direction_choice.add("sides_only", 1, _("Sides only"), "")
      direction_choice.add("above_and_below", 2,
                             _("Above and below only"), "")

      procedure.add_choice_argument("directionParam", _("Direct_ion"), _("Where to sample pixels from"),
                                        direction_choice, "all_around", GObject.ParamFlags.READWRITE)
      order_choice = Gimp.Choice.new()
      order_choice.add("random", 0, _("Random"), "")
      order_choice.add("inwards", 1, _("Inwards"), "")
      order_choice.add("outwards", 2, _("Outwards"), "")

      procedure.add_choice_argument("orderParam", _("Order"), _("The order to fill the selection in"),
                                    order_choice, "random", GObject.ParamFlags.READWRITE)
      return procedure
    return None

  def run(self, procedure: Gimp.Procedure, run_mode: Gimp.RunMode, image: Gimp.Image, layers, config, data):
    if Gimp.Selection.is_empty(image):
      Gimp.message("You must first select a region to heal.")
      return procedure.new_return_values(Gimp.PDBStatusType.CALLING_ERROR, GLib.Error(None, None, "Select something first."))

    if run_mode == Gimp.RunMode.INTERACTIVE:
      GimpUi.init(PLUGIN_NAME)
      dialog = GimpUi.ProcedureDialog(procedure=procedure, config=config)
      dialog.fill(None)
      if not dialog.run():
        dialog.destroy()
        return procedure.new_return_values(Gimp.PDBStatusType.CANCEL, GLib.Error())
      else:
        dialog.destroy()

    samplingRadius: int = config.get_property('samplingRadiusParam')
    directionParam: str = config.get_property('directionParam')
    order: str = config.get_property('orderParam')

    image.undo_group_start()

    # select the bounds of the bottom-most layer.
    target_bounds = layers[0].mask_bounds()

    temp: Gimp.Image = image.duplicate()

    if not temp:
      raise RuntimeError("Failed duplicate image")

    if debug:
      try:
        disp: Gimp.Display = Gimp.Display.new(image=temp)
        Gimp.displays_flush()
      except RuntimeError:  # thrown if non-interactive
        pass
      from time import sleep
      sleep(2)

    selected_drawables = temp.get_selected_drawables()
    if len(selected_drawables) == 0:
      raise RuntimeError("No drawables selected.")

    work_drawable: Gimp.Layer = selected_drawables[0]
    if not work_drawable:
      raise RuntimeError("Failed get active drawable")

    selection: Gimp.Selection = image.get_selection()

    orig_selection: Gimp.Channel = selection.save(temp)
    if not selection.grow(temp, samplingRadius):
      Gimp.message("Could not grow selection")
      return procedure.new_return_values(Gimp.PDBStatusType.EXECUTION_ERROR, GLib.Error(None, None, "couldn't grow the selection somehow."))

    # !!! Note that if selection is a bordering ring already, growing expanded it inwards.
    # Which is what we want, to make a corpus inwards.
    grown_selection: Gimp.Channel = selection.save(temp)

    # Cut hole where the original selection was, so we don't sample from it.
    temp.select_item(Gimp.ChannelOps.SUBTRACT, orig_selection)

    # crop the temp image to size of selection to save memory and for directional healing!!
    frisketBounds = grown_selection.mask_bounds()
    frisketLowerLeftX = frisketBounds.x1
    frisketLowerLeftY = frisketBounds.y1
    frisketUpperRightX = frisketBounds.x2
    frisketUpperRightY = frisketBounds.y2

    dprint(f"{frisketBounds=}")

    targetLowerLeftX = target_bounds.x1
    targetLowerLeftY = target_bounds.y1
    targetUpperRightX = target_bounds.x2
    targetUpperRightY = target_bounds.y2

    dprint(f"{target_bounds=}")

    frisketWidth = frisketUpperRightX - frisketLowerLeftX
    frisketHeight = frisketUpperRightY - frisketLowerLeftY

    dprint(f"{frisketWidth=}, {frisketHeight=}")

    newWidth, newHeight, newLLX, newLLY = (0, 0, 0, 0)
    direction = 0
    # User's choice of direction affects the corpus shape, and is also passed to resynthesizer plugin
    if directionParam == 'all_around':  # all around
      direction = 0
      # Crop to the entire frisket
      newWidth, newHeight, newLLX, newLLY = (
        frisketWidth,
        frisketHeight,
        frisketLowerLeftX,
        frisketLowerLeftY
      )
    elif directionParam == 'sides_only':  # sides
      direction = 1
      # Crop to target height and frisket width:  XTX
      newWidth, newHeight, newLLX, newLLY = (
        frisketWidth,
        targetUpperRightY-targetLowerLeftY,
        frisketLowerLeftX,
        targetLowerLeftY
      )
    elif directionParam == 'above_and_below':  # above and below
      direction = 2
      # X Crop to target width and frisket height
      # T
      # X
      newWidth, newHeight, newLLX, newLLY = (
        targetUpperRightX-targetLowerLeftX,
        frisketHeight,
        targetLowerLeftX,
        frisketLowerLeftY
      )

    dprint(f"{newWidth=} {newHeight=} {newLLX=} {newLLY=}")

    # Restrict crop to image size (condition of gimp_image_crop) eg when off edge of image
    newWidth = min(temp.get_width() - newLLX, newWidth)
    newHeight = min(temp.get_height() - newLLY, newHeight)

    dprint(f"resized {newWidth=} {newHeight=}")

    temp.crop(newWidth, newHeight, newLLX, newLLY)

    # default, just to declare the value.
    useBorder = 1

    # Encode two script params into one resynthesizer param.
    # use border 1 means fill target in random order
    # use border 0 is for texture mapping operations, not used by this script
    if order == 'random':
      useBorder = 1   # User wants NO order, ie random filling
    elif order == 'inwards':  # Inward to corpus.  2,3,4
      # !!! Offset by 2 to get past the original two boolean values
      useBorder = direction + 2
    else:
      # Outward from image center.
      # 5+0=5 outward concentric
      # 5+1=6 outward from sides
      # 5+2=7 outward above and below
      useBorder = direction + 5

    # Note that the old resynthesizer required an inverted selection !!

    # Not necessary to restore image to initial condition of selection, activity,
    # the original image should not have been changed,
    # and the resynthesizer should only heal, not change selection.

    # Note that the API hasn't changed but use_border param now has more values.
    pdb: Gimp.PDB = Gimp.get_pdb()
    pdb_proc: Gimp.Procedure = pdb.lookup_procedure('plug-in-resynthesizer')
    pdb_config: Gimp.ProcedureConfig = pdb_proc.create_config()
    pdb_config.set_property('run-mode', Gimp.RunMode.NONINTERACTIVE)
    pdb_config.set_property('image', image)
    # A hacky way to pass in python arrays directly,
    # see: https://gitlab.gnome.org/GNOME/gobject-introspection/-/issues/492
    pdb_config.set_core_object_array('drawables', layers)
    pdb_config.set_property('h-tile', 0)
    pdb_config.set_property('v-tile', 0)
    pdb_config.set_property('use-border', useBorder)
    pdb_config.set_property('corpus-drawable', work_drawable)
    pdb_config.set_property('input-map', None)
    pdb_config.set_property('output-map', None)
    pdb_config.set_property('map-weight', 0.0)
    pdb_config.set_property('autism', 0.117)
    pdb_config.set_property('neighbours', 16)
    pdb_config.set_property('trys', 500)
    pdb_proc.run(pdb_config)

    # Clean up (comment out to debug)
    temp.delete()
    image.undo_group_end()
    return procedure.new_return_values(Gimp.PDBStatusType.SUCCESS, GLib.Error())

Gimp.main(HealSel.__gtype__, sys.argv)
