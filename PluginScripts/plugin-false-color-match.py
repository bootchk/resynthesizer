#!/usr/bin/env python

'''
Gimp plugin "Show false color match"

Copyright 2019 lloyd konneker (bootch at nc.rr.com)


Version:
  1.0 lloyd konneker lkk 7/19/2019 Initial version in python.


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
Derived from plugin-heal-selection.py.
The controls are the same.
Both perform the same algorithm (i.e. finding best match as if healing)
but this plugin returns false colors instead of colors of the best match.

Another difference is that a new image is returned (instead of altering the original source image.)
Because the result is less useful.
The use case is that the false color is only temporary, for comparison to an actual heal operation.
The original is left untouched, so user can do an actual heal operation, for comparison.
'''

from gimpfu import *
from array import array
from math import sqrt

gettext.install("resynthesizer", gimp.locale_directory, unicode=True)

debug = False




def duplicateImageAndDrawable(sourceImage):
  dupeImage = pdb.gimp_image_duplicate(sourceImage)
  if not dupeImage:
      raise RuntimeError, "Failed duplicate image"
  
  # !!! The drawable can be a mask (grayscale channel), don't restrict to layer.
  dupeDrawable = pdb.gimp_image_get_active_drawable(dupeImage)
  if not dupeDrawable:
      raise RuntimeError, "Failed get active drawable"

  return dupeImage, dupeDrawable



def createFrisketSelectionInCorpusImage(corpusImage, samplingRadiusParam):
  '''
  corpusImage is copy of passed source image, with a selection that is the target.
  Create a new selection that is the inverse of the target selection
  grow and punch hole, making a frisket iow stencil iow donut
  
  '''
  orgSelection = pdb.gimp_selection_save(corpusImage) # save for later use
  pdb.gimp_selection_grow(corpusImage, samplingRadiusParam)
  # ??? returns None , docs say it returns SUCCESS
  
  # !!! Note that if selection is a bordering ring already, growing expanded it inwards.
  # Which is what we want, to make a corpus inwards.
  
  grownSelection = pdb.gimp_selection_save(corpusImage)
  
  # Cut hole where the original selection was, so we don't sample from it.
  # !!! Note that gimp enums/constants are not prefixed with GIMP_
  pdb.gimp_image_select_item(corpusImage, CHANNEL_OP_SUBTRACT, orgSelection)
  
  '''
  Selection (to be the corpus) is donut or frisket around the original target T
    xxx
    xTx
    xxx
  '''
  return grownSelection


def cropCorpusImage(corpusImage, grownSelection, targetDrawable, directionParam):
  # crop the corpus image to size of selection to save memory and for directional healing!!

  # bounds of target original place in source image.
  # Same as bounds of hole in frisket selection in corpus
  targetBounds = targetDrawable.mask_bounds

  frisketBounds = grownSelection.mask_bounds
  frisketLowerLeftX = frisketBounds[0]
  frisketLowerLeftY = frisketBounds[1]
  frisketUpperRightX = frisketBounds[2]
  frisketUpperRightY = frisketBounds[3]
  targetLowerLeftX = targetBounds[0]
  targetLowerLeftY = targetBounds[1]
  targetUpperRightX = targetBounds[2]
  targetUpperRightY = targetBounds[3]
  
  frisketWidth = frisketUpperRightX - frisketLowerLeftX
  frisketHeight = frisketUpperRightY - frisketLowerLeftY
  
  # User's choice of direction affects the corpus shape, and is also passed to resynthesizer plugin
  if directionParam == 0: # all around
      # Crop to the entire frisket
      newWidth, newHeight, newLLX, newLLY = ( frisketWidth, frisketHeight, 
        frisketLowerLeftX, frisketLowerLeftY )
  elif directionParam == 1: # sides
      # Crop to target height and frisket width:  XTX
      newWidth, newHeight, newLLX, newLLY =  ( frisketWidth, targetUpperRightY-targetLowerLeftY, 
        frisketLowerLeftX, targetLowerLeftY )
  elif directionParam == 2: # above and below
      # X Crop to target width and frisket height
      # T
      # X
      newWidth, newHeight, newLLX, newLLY = ( targetUpperRightX-targetLowerLeftX, frisketHeight, 
        targetLowerLeftX, frisketLowerLeftY )
  # Restrict crop to image size (condition of gimp_image_crop) eg when off edge of image
  newWidth = min(pdb.gimp_image_width(corpusImage) - newLLX, newWidth)
  newHeight = min(pdb.gimp_image_height(corpusImage) - newLLY, newHeight)
  pdb.gimp_image_crop(corpusImage, newWidth, newHeight, newLLX, newLLY)

  # Return origin of former target (now a hole) in the corpus.
  # Since coords of best match will be in frame of cropped corpus.
  return (targetLowerLeftX - newLLX) , targetLowerLeftY - newLLY


def encodeScriptParamsIntoEngineParam(orderParam, directionParam):
  # Encode two script params into one resynthesizer param.
  # use border 1 means fill target in random order
  # use border 0 is for texture mapping operations, not used by this script
  if not orderParam :
      useBorder = 1   # User wants NO order, ie random filling
  elif orderParam == 1 :  # Inward to corpus.  2,3,4
      useBorder = directionParam+2   # !!! Offset by 2 to get past the original two boolean values
  else:
      # Outward from image center.  
      # 5+0=5 outward concentric
      # 5+1=6 outward from sides
      # 5+2=7 outward above and below
      useBorder = directionParam+5
  return useBorder


def cropToSelection(image):
  selectionBounds = image.active_layer.mask_bounds

  selectionLowerLeftX = selectionBounds[0]
  selectionLowerLeftY = selectionBounds[1]
  selectionUpperRightX = selectionBounds[2]
  selectionUpperRightY = selectionBounds[3]

  selectionWidth = selectionUpperRightX - selectionLowerLeftX
  selectionHeight = selectionUpperRightY - selectionLowerLeftY

  pdb.gimp_image_crop(image, selectionWidth, selectionHeight, selectionLowerLeftX, selectionLowerLeftY)



def arrayOfBytesFromPixelRegion(pixelRegion):
  # second arg must be iterable
  # iterating over pixels yields strings(bytes) of pixelels
  return array("B", pixelRegion[0:pixelRegion.w, 0:pixelRegion.h])

def listOfIntFromImage(image):
  list = [0 for i in range(image.height*image.width)]
  return list



def unnormalizeToCorpusFrame(red, green, corpusWidth, corpusHeight):
   '''
   red, green were normalized to [0,255] but from coordinates in corpus frame
   Unnormalize to actual coordinates (with loss of precision.)
   '''
   return (red * corpusWidth)/255, (green * corpusHeight)/255


'''
def targetOriginInCorpus(targetImage):
  selectionBounds = image.active_layer.mask_bounds

  selectionLowerLeftX = selectionBounds[0]
  selectionLowerLeftY = selectionBounds[1]
'''


def vectorFromTargetToBestMatchInCorpusFrame(unnormX, unnormY, targetOriginX, targetOriginY):
  '''
  Given vector is in corpus frame, to best match.
  Subtract vector to target pixel, in corpus frame.
  Yields a result vector in corpus frame that, when translated to target frame, 
  points from target pixel to best match.

  Result vector can be negative, but we want positive length.
  
  Note vectors from corpus origin to best match can be much longer than 
  vector from target pixel to best match (when corpus is larger than target.)
  '''
  return (unnormX - targetOriginX) , (unnormY - targetOriginY)



def falseColorPixel(pixelelArray, pixelIndex, corpusWidth, corpusHeight, targetX, targetY):
   '''
   Return a single false color value derived from coordinate vector in R,G.
   Here, derivation is ultimately length of a vector, derived from coordinate vector in R,G.

   We will later set blue pixelel, which is now zero, to said false color value.

   Rationale: differences in R,G values are too subtle.  Length in B amplifies apparent differences.
   '''
   # TEST pixelelArray[pixelIndex + 2] = 255  # intense blue

   red,green  = pixelelArray[pixelIndex], pixelelArray[pixelIndex+1]

   unnormX, unnormY = unnormalizeToCorpusFrame(red, green, corpusWidth, corpusHeight)
   # coord vector in corpus frame, from origin to best match in corpus

   # convert to vector from target pixel to best match pixel (in corpus)
   # vector subtraction
   unnormX, unnormY = vectorFromTargetToBestMatchInCorpusFrame(unnormX, unnormY, targetX, targetY)

   length = sqrt(unnormX**2 + unnormY**2)

   # length can be greater than 255 max pixelel value
   return int(length)




def normalizeFalseColors(falseColorPixelList):
   '''
   Given an array of false colors whose values might be greater than 255.
   Normalize to [0,255]
   We can't just clamp, it truncates at one end of range of length values.
   Clamp idiom:  max(0, min(length, 255))

   When vector is normalized to [0,255], this gives max vector length
   maxVectorLength = sqrt(2*(255**2))

   But vectors are no longer normalized, but are in corpus frame.

   Here we normalize on the high end only (omit low end)
   '''
   # See below.  Must be float to get floating division.
   maxFalseColor = float(max(falseColorPixelList))
   print(maxFalseColor)

   # Normalize each value in array
   # coerce to int
   for index, value in enumerate(falseColorPixelList):
      # value is an int, maxFalseColor is float.  Thus is true division, not floor division of two ints.
      norm = int((value/maxFalseColor)*255)
      # print(norm)
      falseColorPixelList[index] = norm



def falseColorDistance(resultImage, corpusWidth, corpusHeight, targetX, targetY):
  '''
  Put vector length of x,y vector in blue channel,
  x, y are in R,G channels!

  Uses arrays instead of pdb access to pixels, for speed.
  See "Fast pixel ops in GIMP-Python"

  Assert resultImage is only the target (the selection in the source, but source is same as corpus.)
  targetX, targetY is origin of target within source
  TODO rename to targetLLX
  '''

  # TODO I don't understand non-rectangular selection,
  # whether a pixel region is rectangular, but no operations are effective except in the selection mask?

  # pixel region of the selection
  width = resultImage.width
  height = resultImage.height
  bpp = resultImage.active_layer.bpp

  pr = resultImage.active_layer.get_pixel_rgn(0, 0, width, height, False, False)

  # array of pixelels, for however many channels in image
  targetPixelelArray = arrayOfBytesFromPixelRegion(pr)

  # list of ints (!!! not just a byte) for one channel
  falseColorIntList = listOfIntFromImage(resultImage)

  # iterate over target pixels, creating false color from pixelels of pixel
  for x in range(0, pr.w):
     for y in range(0, pr.h):
        pixelIndexInChannel = (x + width * y)
        pixelIndexInRegion = pixelIndexInChannel * bpp
        # TODO pass coords of pixel in target frame, not just origin of target
        falseColorIntList[pixelIndexInChannel] = falseColorPixel(targetPixelelArray, pixelIndexInRegion, corpusWidth, corpusHeight, targetX, targetY)

  normalizeFalseColors(falseColorIntList)

  # Put normalized false color in blue channel.
  for x in range(0, pr.w):
     for y in range(0, pr.h):
        pixelIndexInChannel = (x + width * y)
        pixelIndexInRegion = pixelIndexInChannel * bpp
        # +2 is blue pixelel
        targetPixelelArray[pixelIndexInRegion + 2] = falseColorIntList[pixelIndexInChannel]

  # Copy whole array back to the pixel region:
  pr[0:width, 0:height] = targetPixelelArray.tostring() 



def show_false_color_match(timg, tdrawable, samplingRadiusParam=50, directionParam=0, orderParam=0):
  '''
  Show false colors of the matches that would be found if we were doing "Heal selection".
  Plugin resynthesizer does the substantive work.
  Here we do outer work:
  - prepare corpus
  - prepare a separate result image
  - pass parameters to engine that mean: return coords of best match
  Some of the outer work is similar to the heal selection plugin.
  '''

  if pdb.gimp_selection_is_empty(timg):
    pdb.gimp_message(_("You must first select a region."))
    return
  
  pdb.gimp_image_undo_group_start(timg)

  # Duplicate source image for result (don't alter original.)
  resultImage, resultDrawable = duplicateImageAndDrawable(timg)

  # In duplicate image, create the sample (corpus).
  # (I tried to use a temporary layer but found it easier to use duplicate image.)
  corpusImage, corpusDrawable = duplicateImageAndDrawable(timg)

  # Create frisket selection in corpus around the target
  grownSelection = createFrisketSelectionInCorpusImage(corpusImage, samplingRadiusParam)
  # Crop corpus to relevant pixels, for efficiency.
  # Pass corpus, selection, target drawable, parameter.
  # Remember target origin in cropped corpus.
  targetOriginXInCorpus, targetOriginYInCorpus = cropCorpusImage(corpusImage, grownSelection, tdrawable, directionParam)
  
  useBorder = encodeScriptParamsIntoEngineParam(orderParam, directionParam)

  # Specify false color operation
  useBorder += 10
      
  # Note that the old resynthesizer required an inverted selection !!
  
  if debug:
    try:
      gimp.Display(corpusImage) 
      gimp.displays_flush()
    except RuntimeError:  # thrown if non-interactive
      pass
    from time import sleep
    sleep(2)
  
  # Not necessary to restore image to initial condition of selection, activity,
  # the original image should not have been changed,
  # and the resynthesizer should only heal, not change selection.

  # Note that the API hasn't changed but use_border param now has more values.
  pdb.plug_in_resynthesizer(resultImage, resultDrawable, 0,0, useBorder, corpusDrawable.ID, -1, -1, 0.0, 0.117, 16, 500)
  
  # Crop result to the selection, which contains all the added value (original still shows the context)
  cropToSelection(resultImage)

  # Pass full target image, not just drawable.
  # Pass origin of target within corpus
  falseColorDistance(resultImage, corpusImage.width, corpusImage.height, targetOriginXInCorpus, targetOriginYInCorpus)

  # Display result (original is also still displayed)
  try:
      gimp.Display(resultImage) 
      gimp.displays_flush()
  except RuntimeError:  # thrown if non-interactive
      pass
  
  # Clean up (comment out to debug)
  gimp.delete(corpusImage)
  pdb.gimp_image_undo_group_end(timg)


register(
  "python_fu_false_color_match",
  N_("Show false color map of best match for selection."),
  "Requires separate resynthesizer plugin.",
  "Lloyd Konneker",
  "2009 Lloyd Konneker",  # Copyright 
  "2009",
  N_("_False color match..."),
  "RGB*, GRAY*",
  [
    (PF_IMAGE, "image",       "Input image", None),
    (PF_DRAWABLE, "drawable", "Input drawable", None),
    (PF_INT, "samplingRadiusParam", _("Context sampling width (pixels):"), 50),
    (PF_OPTION,"directionParam",   _("Sample from:"),0,[_("All around"),_("Sides"),_("Above and below")]),
    (PF_OPTION, "orderParam",   _("Filling order:"), 0, [_("Random"),
      _("Inwards towards center"), _("Outwards from center") ])
  ],
  [],
  show_false_color_match,
  menu="<Image>/Filters/Map",
  domain=("resynthesizer", gimp.locale_directory)
  )

main()
