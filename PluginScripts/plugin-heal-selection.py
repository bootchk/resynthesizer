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

import sys
import gi
gi.require_version('Gimp', '3.0')
from gi.repository import Gimp
gi.require_version('GimpUi', '3.0')
from gi.repository import GimpUi
from gi.repository import GObject
from gi.repository import GLib

PLUGIN_NAME = 'resynthesizer-heal-selection'
def N_(message): return message
def _(message): return GLib.dgettext(None, message)

debug = False

class HealSel (Gimp.PlugIn):
        ## Parameters ##
    __gproperties__ = {
        "samplingRadiusParam" : (int,
                        _("the sampling radius (in pixels)"),
                        _("How far to sample. Min 1, Max 1000, Default 50"),
                        1, # min 
                        1000, # max
                        50, # default
                        GObject.ParamFlags.READWRITE),
        "directionParam": (int,
                  _("Sample from:"),
                  _("Where to sample from. 0 = all around, 1 = sides only, 2 = above and below only."),
                  0, 2, 0, # 0 = all around, 1 = sides, 2 = above and below.
                  GObject.ParamFlags.READWRITE),
        "orderParam": (int,
                  _("Filling order"),
                  _("How to go about healing the selection. 0 = randomly, 1 = inwards toward center, 2 = outward from center."),
                  0, 2, 0, # 0 = random, 1 = inwards toward center, 2 = outward from center.
                  GObject.ParamFlags.READWRITE),
    }


    ## GimpPlugIn virtual methods ##
    def do_set_i18n(self, procname):
        return True, 'gimp30-python', None

    def do_query_procedures(self):
        return [ PLUGIN_NAME ]

    def do_create_procedure(self, name):
        if (name == PLUGIN_NAME):
            procedure: Gimp.ImageProcedure = Gimp.ImageProcedure.new(self, name,
                                                Gimp.PDBProcType.PLUGIN,
                                                self.run, None)
            procedure.set_image_types("RGB*, GRAY*")
            procedure.set_sensitivity_mask (Gimp.ProcedureSensitivityMask.DRAWABLE)
            procedure.set_documentation (_("resynthesizer heal selection"),
                                         _("heal selection with the resynthesizer algorithm"),
                                         name)
            procedure.set_menu_label(_("_heal selection"))
            procedure.set_attribution("James Henstridge",
                                      "James Henstridge",
                                      "1999,2007")
            procedure.add_menu_path ("<Image>/Filters/Enhance")
            procedure.add_argument_from_property(self, "samplingRadiusParam")
            procedure.add_argument_from_property(self, "directionParam")
            procedure.add_argument_from_property(self, "orderParam")
            
            return procedure
        return None
    
    def run(self, procedure: Gimp.Procedure, run_mode: Gimp.RunMode, image: Gimp.Image, n_layers, layers, config, data):

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
      direction: int = config.get_property('directionParam')
      order: int = config.get_property('orderParam')

      image.undo_group_start()

      # select the bounds of the bottom-most layer.
      target_bounds = layers[0].mask_bounds()

      temp: Gimp.Image = image.duplicate()

      if debug:
        try:
          disp: Gimp.Display = Gimp.Display.new(image=temp)
          Gimp.displays_flush()
        except RuntimeError:  # thrown if non-interactive
          pass
        from time import sleep
        sleep(2)

      # We merge all visible layers together to make things easier for us.
      
      work_drawable: Gimp.Layer = temp.merge_visible_layers(Gimp.MergeType.CLIP_TO_IMAGE)

      selection: Gimp.Selection = image.get_selection()

      orig_selection: Gimp.Channel = selection.save(temp)
      if not selection.grow(temp, samplingRadius):
         Gimp.message("Could not grow selection")
         return procedure.new_return_values(Gimp.PDBStatusType.EXECUTION_ERROR, GLib.Error(None, None, "couldn't grow the selection somehow."))
      
      grown_selection: Gimp.Channel = selection.save(temp)
      temp.select_item(Gimp.ChannelOps.SUBTRACT, orig_selection)

      # crop the temp image to size of selection to save memory and for directional healing!!
      frisketBounds = grown_selection.mask_bounds()
      frisketLowerLeftX   = frisketBounds[0]
      frisketLowerLeftY   = frisketBounds[1]
      frisketUpperRightX  = frisketBounds[2]
      frisketUpperRightY  = frisketBounds[3]

      targetLowerLeftX    = target_bounds[0]
      targetLowerLeftY    = target_bounds[1]
      targetUpperRightX   = target_bounds[2]
      targetUpperRightY   = target_bounds[3]

      frisketWidth = abs(frisketUpperRightX - frisketLowerLeftX)
      frisketHeight = abs(frisketUpperRightY - frisketLowerLeftY)


      newWidth, newHeight, newLLX, newLLY = (0, 0, 0, 0)

      # User's choice of direction affects the corpus shape, and is also passed to resynthesizer plugin
      if direction == 0: # all around
          # Crop to the entire frisket
          newWidth, newHeight, newLLX, newLLY = (
             frisketWidth,
             frisketHeight,
             frisketLowerLeftX,
             frisketLowerLeftY
            )
      elif direction == 1: # sides
          # Crop to target height and frisket width:  XTX
          newWidth, newHeight, newLLX, newLLY =  (
            frisketWidth,
            targetUpperRightY-targetLowerLeftY,
            frisketLowerLeftX,
            targetLowerLeftY
          )
      elif direction == 2: # above and below
          # X Crop to target width and frisket height
          # T
          # X
          newWidth, newHeight, newLLX, newLLY = (
             targetUpperRightX-targetLowerLeftX,
             frisketHeight,
             targetLowerLeftX,
             frisketLowerLeftY
            )
          
      # Restrict crop to image size (condition of gimp_image_crop) eg when off edge of image
      newWidth = min(temp.get_width() - newLLX, newWidth)
      newHeight = min(temp.get_height() - newLLY, newHeight)
      temp.crop(newWidth, newHeight, newLLX, newLLY)

      # default, just to declare the value.
      useBorder = 1
  
      # Encode two script params into one resynthesizer param.
      # use border 1 means fill target in random order
      # use border 0 is for texture mapping operations, not used by this script
      if order == 0:
          useBorder = 1   # User wants NO order, ie random filling
      elif order == 1 :  # Inward to corpus.  2,3,4
          useBorder = direction + 2   # !!! Offset by 2 to get past the original two boolean values
      else:
          # Outward from image center.  
          # 5+0=5 outward concentric
          # 5+1=6 outward from sides
          # 5+2=7 outward above and below
          useBorder = direction + 5
      
      # Note that the old resynthesizer required an inverted selection !!
  
      # we need to convert the layers python array into a gobject one.
      layer_gobj_array: Gimp.ObjectArray = Gimp.ObjectArray.new(Gimp.Drawable, layers, False)
  
      # Not necessary to restore image to initial condition of selection, activity,
      # the original image should not have been changed,
      # and the resynthesizer should only heal, not change selection.

      # Note that the API hasn't changed but use_border param now has more values.
      pdb: Gimp.PDB = Gimp.get_pdb()
      pdb_proc: Gimp.Procedure = pdb.lookup_procedure('plug-in-resynthesizer')
      pdb_config: Gimp.ProcedureConfig = pdb_proc.create_config()
      pdb_config.set_property('run-mode', Gimp.RunMode.NONINTERACTIVE)
      pdb_config.set_property('image', image)
      pdb_config.set_property('num-drawables', layer_gobj_array.length)
      pdb_config.set_property('drawables', layer_gobj_array)
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
      layer_gobj_array.free()

      image.undo_group_end()
      return procedure.new_return_values(Gimp.PDBStatusType.SUCCESS, GLib.Error())

Gimp.main(HealSel.__gtype__, sys.argv)