#!/usr/bin/env python3
# Gimp3 plugins have a shebang


'''
A script to test the resynthesizer.

Lloyd Konneker

Suite of several test cases.
Most test cases are the outer plugins that call the resynthesizer engine plugin.
A few are direct calls to the outer resynthesizer engine plugin.
No tests of the outer, control panel plugin.
Note the plugins must be installed, this calls them through Gimp.

Uses diff to compare output image with known good results (reference images).
Output is to a temp directory ( typically /tmp).

Result type enumeration:
FAILED: output does not match reference image
SUCCESS: does match
UNKNOWN: no reference image exists, one was created, manually judge the reference and run testsuite again
EXCEPTION: a called plugin threw a RuntimeError exception
IMPROPER: a test file could not be opened, or similar

Reference images are created on the first run.
Delete a reference image and run testsuite again to create a new reference image.
OR review the output in temp and if OK, paste it over the reference image.

Note these call a version of the resynthesizer that is repeatable especially its random seed.
Note that anytime you change the code
so that the prng pseudo random number generator sequence is changed
this automated test will usually fail but the functional result will be as expected.

The test harness is itself a plugin.
Invoke: Test>Resynthesizer

It takes several minutes.
It creates and displays result images.
Then review the log, where a summary is printed last.  Everything should pass.

Note save PPM because PPM format is large but repeatable,
whereas PNG is not repeatable because of timestamp.

Note that the test harness creates a new image from a file and
saves the result separately from the input.

Note uses the logging module, mainly to avoid issues with print to stdout on some platforms.
If you want to see the results in real time, use 'tail -f resynth-test.log'
'''

# Since Gimp3, use gi
import gi
gi.require_version('Gimp', '3.0')
from gi.repository import Gimp
gi.require_version('GimpUi', '3.0')
from gi.repository import GimpUi
from gi.repository import GObject
from gi.repository import GLib
from gi.repository import Gio



import sys
import os
import subprocess
import time
import tempfile
import logging



'''
paths
'''

# testdir is *not* the working dir
# testdir = './'
# use absolute path to the test dir of the repo
testdir = "/work/resynthesizer/test/"

# put test results in a tmp directory (typically in /tmp)
tmpdir = tempfile.mkdtemp()

# test images must be in this subdir of the working dir
indir = testdir + 'in_images/'

# For testing pure GIMP/glib with GRand: reference files different
referencedir = testdir + 'reference_out_images/'

# For testing with stdc rand():
## referencedir = testdir + 'reference_out_images_rand/'

'''
Other globals
'''

test_summary = ""

# selection rects
select1 = (100, 90, 100, 50)  # ufo
select2 = (90, 175, 135, 100) # donkey

# Names of input files
# They are all in the input directory
# They all have suffix .png
# This abstraction lets us change the input files later,
# and lets us use shorter text in the test program.
grass =      'grass-input'
grassAlpha = 'grass-input-alpha'
brick  =     'brick'
donkey  =    'donkey_original'
# Not image of angel, but another subject, with colors
angelTexture = 'angel_texture'
# Image of angel
angelTarget = 'angel_target'
wander  =    'wander-texture'
zap  =       'zap-texture'
zapOutputMap = 'zap-output-map'
ufo  =       'ufo-input'
ufoAlpha =   'ufo-input-w-alpha'
ufoAlphaGray = 'ufo-input-w-alpha-gray'
ufoSmall =   'ufo-input-small'



'''
Other test data.
We need a GimpPattern.
# !!! You can't call Gimp until after Gimp.main()
so this is initialized with memoization later.
'''
testPattern = None

def initTestPattern():
  global testPattern

  # Once, when not memoized, it crashes after four calls??? TODO
  if testPattern is None:
    # pattern must be an object, not a string name.
    testPattern = Gimp.Resource.get_by_name(Gimp.Pattern, 'Maple Leaves')
    print (testPattern.get_name())


def record_test_result(testname, result, outfilename = ""):
  global test_summary
  logging.info(result + " " + testname)
  test_summary += result + " " + testname + " " + outfilename + "\n"



'''
Opens test input image file at name.

This knows the path to in test files, and their suffix.
'''
def openTestFilename(filename, select=None, invert=False):
  infilepath = indir + filename + '.png'
  return openTestFilepath (infilepath, select, invert)



'''
Opens test image file.
Optionally selects area in the image.
Optionally inverts the selection.
Returns image, drawable.
'''
def openTestFilepath(filepath, select=None, invert=False):

  # v3 file load takes a GFile
  infile = Gio.file_new_for_path(filepath)
  image = Gimp.file_load(Gimp.RunMode.NONINTERACTIVE, infile)
  assert image is not None

  # require each test image have exactly one image, need not be chosen
  # TODO get_selected_layers and get_layers are not in the GI, marked in image.pdb to $skip-GI ?
  # count, layers = image.get_layers()
  layers = image.get_layers()
  assert len(layers) == 1
  drawable = layers[0]
  assert drawable is not None

  # TODO optionally flatten
  # i.e. remove all layers but one
  # TODO does flatten remove the alpha?

  if select is not None:
    # Make selection    x,y, width, height
    # Gimp.rect_select(image, 100, 90, 100, 50, 0, False, 0)
    image.select_rectangle(Gimp.ChannelOps.ADD, select[0], select[1], select[2], select[3])
  if invert:
    Gimp.Selection.invert(image)

  return image, drawable


'''
Wrapper functions.

Since v3 a call to the PDB is more complicated than in v2 GimpFu
In v2 you could directly call "pdb.plug-in-foo"
Now the test harness calls the wrapper instead of directly.
'''

def callHealSelection(targetImage, targetDrawable, corpusWidth, sampleFrom, synthDirection):
  result = Gimp.get_pdb().run_procedure(
    "script-fu-heal-selection",
    [
      # takes run_mode and image
      GObject.Value(Gimp.RunMode,     Gimp.RunMode.NONINTERACTIVE),
      GObject.Value(Gimp.Image,       targetImage),

      GObject.Value(Gimp.Drawable,    targetDrawable),
      # corpus-width
      GObject.Value(GObject.TYPE_INT,     corpusWidth),
      # sample from areas
      GObject.Value(GObject.TYPE_INT,     sampleFrom),
      # synthesize in direction
      GObject.Value(GObject.TYPE_INT,     synthDirection),
    ])
  # TODO result returned?

def callHealTransparency(targetImage, targetDrawable, corpusWidth, synthDirection):
  result = Gimp.get_pdb().run_procedure(
    "script-fu-heal-transparency",
    [
      # takes run_mode and image
      GObject.Value(Gimp.RunMode,     Gimp.RunMode.NONINTERACTIVE),
      GObject.Value(Gimp.Image,       targetImage),

      GObject.Value(Gimp.Drawable,    targetDrawable),
      # corpus-width
      GObject.Value(GObject.TYPE_INT,     corpusWidth),

      # No sampleFrom, the target and corpus are not compact
      # but usually spread over the entire image.

      # synthesize in direction
      GObject.Value(GObject.TYPE_INT,     synthDirection),
    ])

def callUncrop(targetImage, targetDrawable, percentSizeIncrease):
  result = Gimp.get_pdb().run_procedure(
    "script-fu-uncrop",
    [
      # takes run_mode and image
      GObject.Value(Gimp.RunMode,     Gimp.RunMode.NONINTERACTIVE),
      GObject.Value(Gimp.Image,       targetImage),

      GObject.Value(Gimp.Drawable,    targetDrawable),

      # No sampleFrom, corpus is always just inside the image edge
      # No corpusWidth corpus is always just tens of pixels deep
      # No direction, always outward

      GObject.Value(GObject.TYPE_INT,     percentSizeIncrease),
    ])


def callFillPatternResynth(targetImage, targetDrawable, pattern):
  result = Gimp.get_pdb().run_procedure(
    "script-fu-fill-pattern-resynth",
    [
      # takes run_mode and image
      GObject.Value(Gimp.RunMode,     Gimp.RunMode.NONINTERACTIVE),
      GObject.Value(Gimp.Image,       targetImage),

      GObject.Value(Gimp.Drawable,    targetDrawable),

      #GObject.Value(Gimp.Pattern,    pattern),
      GObject.Value(Gimp.Resource,    pattern),
    ])


def callRenderTexture(targetImage, targetDrawable, sizeRatio, isTileable):
  result = Gimp.get_pdb().run_procedure(
    "script-fu-render-texture",
    [
      # takes run_mode and image
      GObject.Value(Gimp.RunMode,     Gimp.RunMode.NONINTERACTIVE),
      GObject.Value(Gimp.Image,       targetImage),

      GObject.Value(Gimp.Drawable,    targetDrawable),

      GObject.Value(GObject.TYPE_DOUBLE,  sizeRatio),
      GObject.Value(GObject.TYPE_BOOLEAN, isTileable),
    ])
  # TODO, a script returns no values
  # Get the newest image and return it as value or assign to global


def callMapStyle(targetImage, targetDrawable, corpusFilename, percentTransfer=50, mapBy=0):

  # open the corpus
  # open with no selection and no inversion of selection
  unusedImage, corpusDrawable = openTestFilename (corpusFilename)

  result = Gimp.get_pdb().run_procedure(
    "script-fu-map-style",
    [
      # takes run_mode and image
      GObject.Value(Gimp.RunMode,     Gimp.RunMode.NONINTERACTIVE),
      GObject.Value(Gimp.Image,       targetImage),

      GObject.Value(Gimp.Drawable,    targetDrawable),

      GObject.Value(Gimp.Drawable,    corpusDrawable),

      GObject.Value(GObject.TYPE_INT,  percentTransfer),
      # an enum: 0-2
      GObject.Value(GObject.TYPE_INT,  mapBy),
    ])
  # TODO, a script returns no values
  # Get the newest image and return it as value or assign to global


def callSharpen(targetImage, targetDrawable, factor=1.0):
  result = Gimp.get_pdb().run_procedure(
    "script-fu-sharpen-resynthesized",
    [
      # takes run_mode and image
      GObject.Value(Gimp.RunMode,     Gimp.RunMode.NONINTERACTIVE),
      GObject.Value(Gimp.Image,       targetImage),

      GObject.Value(Gimp.Drawable,    targetDrawable),

      GObject.Value(GObject.TYPE_DOUBLE,  factor),
    ])


def callEnlarge(targetImage, targetDrawable, factor=1.0):
  result = Gimp.get_pdb().run_procedure(
    "script-fu-enlarge-resynthesized",
    [
      # takes run_mode and image
      GObject.Value(Gimp.RunMode,     Gimp.RunMode.NONINTERACTIVE),
      GObject.Value(Gimp.Image,       targetImage),

      GObject.Value(Gimp.Drawable,    targetDrawable),

      GObject.Value(GObject.TYPE_DOUBLE,  factor),
    ])



def callResynthesizer(
    targetImage, targetDrawable,
    # resynth engine plugin requires corpus drawable
    corpusFilename,
    # These are defaults for testing.
    # The plugin defaults are not effective,
    # i.e. they are for a GUI dialog and otherwise don't contribute

    # a group, usually both the same
    hTile          = False,
    vTile          = False,

    # TODO encoded
    useBorder      = 1,

    # A group, either all nil or all non-nil
    input_map      = None,
    output_map     = None,
    map_weight     = 0.0,

    # AKA tweaks, basic engine params
    autism         = 0.117,
    neighbours     = 16,
    trys           = 500,

    # affect the corpus
    select         = None,
    invert         = False
    ):

  # open the corpus
  unusedImage, corpusDrawable = openTestFilename (corpusFilename, select=select, invert=invert)
  # drawable_of_file_with_anti_selection('"+ zappath + "', select1)


  result = Gimp.get_pdb().run_procedure(
    'plug-in-resynthesizer',
    [
      # not taking run_mode or image

      GObject.Value(Gimp.Drawable,        targetDrawable),
      GObject.Value(GObject.TYPE_BOOLEAN, hTile),
      GObject.Value(GObject.TYPE_BOOLEAN, vTile),
      GObject.Value(GObject.TYPE_INT,     useBorder),
      GObject.Value(Gimp.Drawable,        corpusDrawable),
      GObject.Value(Gimp.Drawable,        input_map),
      GObject.Value(Gimp.Drawable,        output_map),
      GObject.Value(GObject.TYPE_DOUBLE,  map_weight),
      GObject.Value(GObject.TYPE_DOUBLE,  autism),
      GObject.Value(GObject.TYPE_INT,     neighbours),
      GObject.Value(GObject.TYPE_INT,     trys),
    ])




def exportImage (image, outfilepath):

  # file_save requires a GFile
  outfile = Gio.file_new_for_path(outfilepath)

  layers = image.get_layers()
  # require flattened
  assert len(layers) == 1
  drawable = layers[0]

  '''
  Since the file type is .ppm, this is an export.
  The displayed image remains untitled, in .xcf format,
  with a larger file size including the undo steps.
  '''
  # Requires a sequence type e.g. list of layers
  Gimp.file_save(Gimp.RunMode.NONINTERACTIVE, image, layers, outfile)

  '''
  Set the filename so we know what test the image is for.
  Note the suffix must be .xcf, but the image is still dirty
  and that file does not exist.
  The display image should show same view as the exported file.
  '''
  # Just append ".xcf"
  image.set_file(
    Gio.file_new_for_path(
      outfilepath + ".xcf")
    )

  '''
  Alternatively, close the image and open the exported file,
  returning the new image reference.
  resultImage =

  TODO some plugins do not alter the targetImage but return a new image.
  We should always delete the targetImage and display the result image.
  The selection in the result image won't show the selection we started with?
  '''
  return image








'''
flatten and save to .ppm format
'''
def postProcessOutImage (image, outfilename, outfilepath):
  # flatten always, even if image might already be flat (no alpha and one layer)
  image.flatten()

  resultImage = exportImage (image, outfilepath)

  # Log actual exported filepath
  # OLD outfile.get_path()
  logging.info("Test exported file name " + outfilepath)

  return resultImage


def areImagesSame(outfilepath, referencefilepath):
  # diff reference image with recent image out
  result = subprocess.run(['diff', outfilepath, referencefilepath], stdout=subprocess.PIPE)
  # result is-a CompletedProcess, having attribute returncode

  # diff returns zero when files are same
  return result.returncode == 0


'''
Run a single test case

testname: identify test
filename: image file to target
wrapperName: wrapper of outer plugin to test
testparameter: a string of params e.g. "50, 0, 0"
select: optional rect to select in target
'''
def runtest(filename, testname, wrapperName, testparameters, select=None):

  global test_summary

  logging.info("Running test " + testname)
  print(">>>>>>>>>>>>>>> Running test " + testname)


  '''
  Define some path names and files.
  '''
  # since some tests use the same input file, cat the testname to make an outfilename
  outfilename = testname + '-' + filename
  # out and reference have same name, different directories
  outfilepath = tmpdir + '/' + outfilename + '.ppm'

  #require referencedir ends in /
  referencefilepath = referencedir + outfilename + '.ppm'


  # open test input file
  try:
    targetImage, targetDrawable = openTestFilename(filename, select)
  except Exception as inst:
    record_test_result(testname, "IMPROPER preprocessing")
    print (inst)
    return

  # Build an executable Python string that is call to wrapper of tested plugin
  # E.G. callHealSelection(targetImage, targetDrawable, 0, 0)
  teststring = wrapperName + "(" + "targetImage, targetDrawable, " + testparameters + ")"
  logging.info("Test string:" + teststring)


  start = time.time()
  try:
    # Invoke the test
    # Not eval, it only takes expressions, not statements
    # All python vars names in the teststring must be defined in the current scope
    # i.e. targetImage, targetDrawable, testPattern
    exec (teststring)
  except RuntimeError as inst:
    record_test_result(testname, "EXCEPTION")
    print(inst)
    return
  logging.info("Elapsed time: " + str(time.time() - start))

  '''
  !!! Note that the test string can assign to image if the test returns a new image
  (rather than alter the image.)  Special handling here.
  '''
  logging.info("Test target image name" + filename)
  # TODO formerly logged the return image name
  # logging.info("Test image name" + targetImage.name)


  # post process output image, saving to /tmp
  try:
    resultImage = postProcessOutImage (targetImage, outfilename, outfilepath)
  except Exception as inst:
    record_test_result(testname, "IMPROPER post processing")
    print(inst)
    return

  # targetImage should not be used again
  targetImage = None

  # Display the result image
  Gimp.Display.new(resultImage)

  # Update the GUI with filenames on open images
  # Gimp.displays_flush()


  # If not reference out exists
  if not os.path.exists(referencefilepath) :
    # make a new reference image (the first test run)

    # Reference images are usually in a repo.
    # Thus you must commit changes to actually perpetuate new ref images
    logging.debug("Making reference output image file.")
    subprocess.run(['cp', outfilepath, referencefilepath], stdout=subprocess.PIPE)
    # status, output = getstatusoutput('cp ' + outfilepath + ' ' + referencefilepath)
    record_test_result(testname, "UNKNOWN ", outfilename)
    # test inconclusive, no reference data, must rerun
  else:
    if not os.path.exists(outfilepath) :
       raise RuntimeError

    if not areImagesSame(outfilepath, referencefilepath):
      record_test_result(testname, "FAILED", outfilename)
    else:
      record_test_result(testname, "PASSED", outfilename)



'''
Groups of tests of an outer plugin.
'''

def testEnginePlugin():
  '''
  Engine plugin tests (without using an outer plugin)
  '''

  pluginName = "plug-in-resynthesizer"
  pluginWrapperName = 'callResynthesizer'

  '''
  Similar to heal selection:
  synthesize a small selection from the inverted selection of the same image.

  But the results are worse since the corpus is the full image
  and the user doesn't choose things like direction
  that tend to produce better results.
  '''

  # This is historical, matches v2 reference image.
  # parameters = "0,0, True, drawable_of_file_with_anti_selection('"+ zap  + "', select1).ID, -1, -1, 0.0, 0.117, 16, 500"

  # New call, defaulting most parameters
  # corpus is zap , selection corpus is same i.e. select1
  # Keep the corpus name wrapped in single quotes
  parameters = "'" + zap + "'"  + " , select=select1, invert=True"
  runtest(zap, 'resynthfullzap', pluginWrapperName, parameters, select1)

  # Resynthesis of ufo from full context
  # parameters = "0,0, True, drawable_of_file_with_anti_selection('"+ ufo  + "', select1).ID, -1, -1, 0.0, 0.117, 16, 500"
  parameters = "'" + ufo + "'"  + " , select=select1, invert=True"
  runtest(ufo, 'resynthfullufo', pluginWrapperName, parameters, select1)

  # Resynthesis of rect selection in bricks from full context
  # parameters = "0,0, True, drawable_of_file_with_anti_selection('"+ brick  + "', select1).ID, -1, -1, 0.0, 0.117, 16, 500"
  parameters = "'" + brick + "'"  + " , select=select1, invert=True"
  runtest(brick, 'resynthfullbrick', pluginWrapperName, parameters, select1)

  # Resynthesis of rect selection in donkey from full context
  # parameters = "0,0, True, drawable_of_file_with_anti_selection('"+ donkey  + "', select2).ID, -1, -1, 0.0, 0.117, 16, 500"
  # select1 in corpus fails?
  parameters = "'" + donkey + "'"  + " , select=select2, invert=True"
  runtest(donkey, 'resynthfulldonkey', pluginWrapperName, parameters, select2)


  '''
  Unusual engine tests.

  No user would usually want to do these things, but it tests edge cases.
  '''

  # Straight resynthesis, from selection in drawable into same selection in drawable
  # !!! Note the target image is not in this parameter string, it is the corpus.
  # parameters = "0,0, True, drawable.ID, -1, -1, 0.0, 0.117, 16, 500"
  # Not invert selection !!!
  parameters = "'" + ufo + "'"  + " , select=select1"
  runtest(ufo, 'resynthSelf', pluginWrapperName, parameters, select1)

  # Straight resynthesis, from separate image with no selection into selection in drawable
  #parameters = "0,0, True, drawable_of_file('"+ grass + "').ID, -1, -1, 0.0, 0.117, 16, 500"
  # corpus is an unrelated image !!!!
  parameters = "'" + grass + "'"  + " , select=select1"
  runtest(ufo, 'resynthtwoimages', pluginWrapperName, parameters, select1)

  # Straight resynthesis, from separate image with no selection into selection in drawable
  # Tileable without using context: 1,1,0,...
  #parameters = "1, 1, 0, drawable_of_file('"+ grass + "').ID, -1, -1, 0.0, 0.117, 16, 500"
  parameters = "'" + ufo + "'"  + " , select=select1, useBorder=0, hTile=True, vTile=True"
  runtest(ufo, 'resynthtileable', pluginWrapperName, parameters, select1)


def testMapStyle():
  '''
  "Map Style" outer

  These tests also test engine's treatment of differences in bpp (RGB vs RGBA)
  from corpus to target.
  '''
  pluginWrapperName = "callMapStyle"

  # with corpus having alpha
  # map style: RGBA to RGB  50% w/ color maps
  # parameters = "drawable_of_file('"+ grass + "'), 50.0, 0"
  parameters = "'" + grassAlpha + "'"  + ", 50.0, 0"
  runtest(ufo, 'mapstylealpha', pluginWrapperName, parameters, select=None)


  # with target having alpha
  # map style: RGB to RGBA 50% w/ color maps
  # TODO this is not a pluginWrapperName of the resynth since the plug-in is adding an alpha to texture
  # parameters = "drawable_of_file('"+ grass + "'), 50.0, 0"
  parameters = "'" + grass + "'"  + ", 50.0, 0"
  runtest(ufoAlpha, 'mapstyletoalpha', pluginWrapperName, parameters, select=None)

  # map style: RGB to RGB 50% w/ color maps
  # parameters = "drawable_of_file('"+ grass + "'), 50.0, 0"
  parameters = "'" + grass + "'"  + ", 50.0, 0"
  runtest(ufo, 'mapstyle', pluginWrapperName, parameters, select=None)

  # map style: RGB to RGB w/ color maps
  # This is conceptually little different from the above test,
  # but it is a popular documented example: style of a color swatch to image of angels
  # parameters = "drawable_of_file('"+ angel  + "'), 10.0, 0"
  parameters = "'" + angelTexture + "'"  + ", 10.0, 0"
  runtest(angelTarget, 'mapstyleangel', pluginWrapperName, parameters, select=None)

  # map style: RGB to RGB w/ GRAY maps and map by brightness
  # parameters = "drawable_of_file('"+ grass + "'), 50.0, 1"
  # source image, fifty percent transfer, map by brightness
  parameters = "'" + grass + "'"  + ", 50.0, 1"
  runtest(ufo, 'mapstylebrightness', pluginWrapperName, parameters, select=None)

  # mapstyle: GRAY to GRAY, 50%, map by color
  # maps an image to itself?
  # parameters = "drawable_of_file('"+ wander  + "'), 50.0, 0"
  parameters = "'" + wander + "'"  + ", 50.0, 0"
  runtest('wander', 'mapstylegraygray', pluginWrapperName, parameters, select=None)

  # mapstyle: RGB to GRAY, 50%, map by colors
  # parameters = "drawable_of_file('"+ zap  + "'), 10.0, 0"
  parameters = "'" + zap + "'"  + ", 50.0, 0"
  runtest(zapOutputMap, 'mapstylergbgray', pluginWrapperName, parameters, select=None)


def testHealSelection():
  ''' "Heal selection" outer plugin. '''
  pluginName = "script-fu-heal-selection"
  pluginWrapperName = 'callHealSelection'

  # Heal from donut corpus
  parameters = "50, 1, 1" # 1 = sample from sides, 1 = direction inward
  runtest(ufo, 'heal', pluginWrapperName, parameters, select1)

  # This is a common example: the donkey
  # Heal from corpus on the sides, nearby
  # select2 selects the donkey
  # This usually gives better results than corpus entire image, direction random
  parameters = "50, 1, 1" # 1 = sample from all around, 1 = direction inward
  runtest(donkey, 'heal', pluginWrapperName, parameters, select2)

  # Heal from donut corpus
  # also randomly all around
  parameters = "50, 0, 0" # 0 = sample from all around, 0 = randomly
  runtest(ufo, 'healaroundrandom', pluginWrapperName, parameters, select1)

  # Heal where target includes alpha layer.
  # This selects from an area having a transparent area.
  # The transparent area should NOT be resynthesized, but yield white upon flattening.
  parameters = "50, 1, 1"
  runtest('ufo-input-w-alpha', 'healincludedalpha', pluginWrapperName, parameters, select1)

  # Heal where target has distant alpha layer.
  # This selects from a black sky area with a nearby transparent area.
  # The result should be black resynthesized, NOT white of transparent flattened.
  parameters = "50, 1, 1"
  runtest('apollo11_w_alpha', 'healalpha', pluginWrapperName, parameters, select1)

  # Heal a grayscale
  parameters = "50, 1, 1"
  runtest('wander', 'healgray', pluginWrapperName, parameters, select1)

  # Heal a grayscale w alpha
  parameters = "50, 0, 1"
  runtest('ufo-input-w-alpha-gray', 'healalphagray', pluginWrapperName, parameters, select1)


def testHealTransparency():
  ''' Heal transparency outer plugin. '''
  pluginName = "script-fu-heal-transparency"
  pluginWrapperName = 'callHealTransparency'

  # Heal transparency outward
  parameters = "50, 2" # pixels of context width, outward
  runtest('apollo11_w_alpha', 'healtransparency', pluginWrapperName, parameters, select=None)

  # Heal transparency inward
  parameters = "50, 1" # pixels of context width, 1=inward
  runtest('apollo11_w_alpha', 'healtransparencyinward', pluginWrapperName, parameters, select=None)


def testRenderTexture():
  ''' "Render texture" outer plugin. '''

  # !!! Note here the plugin returns a new image which we assign to image variable
  # TODO except scriptFu cannot return a value

  pluginWrapperName = "callRenderTexture"
  parameters = "2.0, True"
  runtest(grass, 'rendertexture', pluginWrapperName, parameters)


def testUncrop():
  ''' Uncrop outer plugin '''

  # Uncrop 20%
  pluginWrapperName = "callUncrop"
  parameters = "20"
  runtest(ufo, 'uncrop', pluginWrapperName, parameters, select=None)


def testFillPattern():
  ''' Fill Pattern outer plugin '''

  # TODO Known to fail issue with SF binding for SF-PATTERN
  return

  pluginName = "script-fu-fill-pattern-resynth"
  pluginWrapperName = "callFillPatternResynth"

  # parameter string refer to pattern object in scope of runtest()
  parameters = "testPattern"
  runtest(ufo, 'fillpattern', pluginWrapperName, parameters, select1)

'''
Sharpen with resynthesis
Too slow to test regularly.
Image stays the same resolution (count of pixels.)
Image might appear more sharp (more grainy?)
Count of distinct colors might be reduced? Eliminating some colors that interpolate?
'''
def testSharpen():
  pluginName = "plug-in-sharpen-resynthesized"
  pluginWrapperName = "callSharpen"
  parameters = "factor=2"
  runtest(ufoSmall, 'sharpen', pluginWrapperName, parameters, select=None)


def testEnlarge():
  '''
  Enlarge with resynthesis.
  Image dimensions increase by factor.
  Image count of pixels increase by factor squared.
  The image looks the same as the original, in gross.
  The image detail MIGHT be better than the usual scale,
  which interpolates.

  !!! Very slow. Too slow to test regularly?
  '''
  pluginWrapperName = "callEnlarge"
  parameters = "factor=2"
  runtest(ufoSmall, 'enlarge', pluginWrapperName, parameters, select=None)


def testAll():

  '''
  Run test cases for resynthesizer plugin and outer plugins that call it.

  Notes on the test string:  optionally use names "image" and "drawable" to match the variable names used
  in the runtest() function; the names will be eval'ed.
  '''

  # Open the log
  LOG_FILENAME = '/work/resynth-test.log'
  logging.basicConfig(filename=LOG_FILENAME, level=logging.DEBUG)

  logging.info("Temp directory: " + tmpdir)

  initTestPattern()

  # Begin test groups

  ## testEnginePlugin()
  ## testMapStyle()
  testHealSelection()
  ## testHealTransparency()
  testRenderTexture()
  testUncrop()
  testFillPattern()
  testSharpen()
  testEnlarge()

  # TODO a much harder test with many layers and alpha channels

  logging.info("\nTemp directory: " + tmpdir)
  logging.info( "\n" + test_summary)





'''
The testing plugin itself.
'''


def test_resynth(procedure, args, data):
    '''
    Just a standard shell for a plugin.
    '''
    print("test_resynth")
    testAll()

    return procedure.new_return_values(Gimp.PDBStatusType.SUCCESS, GLib.Error())

class TestResynthPlugin (Gimp.PlugIn):

    # Copied from pallette-offset.py
    # Must have a run-mode arg, even if is not an image procedure

    ## Parameter: run-mode ##
    @GObject.Property(type=Gimp.RunMode,
                      default=Gimp.RunMode.NONINTERACTIVE,
                      nick="Run mode", blurb="The run mode")
    def run_mode(self):
        """Read-write integer property."""
        return self._run_mode

    @run_mode.setter
    def run_mode(self, run_mode):
        self._run_mode = run_mode



    ## GimpPlugIn virtual methods ##
    def do_set_i18n(self, procname):
        print("query")
        return True, 'gimp30-python', None

    def do_query_procedures(self):
        print("query")
        return [ 'plug-in-test-resynth' ]

    def do_create_procedure(self, name):
        print("create")
        procedure = Gimp.Procedure.new(self, name,
                                            Gimp.PDBProcType.PLUGIN,
                                            test_resynth, # run_func
                                            None)

        # not require image to be open
        procedure.set_sensitivity_mask (Gimp.ProcedureSensitivityMask.NO_IMAGE)

        procedure.set_documentation ("Test Resynthesizer",
                                     "Test Resynthesizer",
                                     name)
        procedure.set_attribution("Lloyd Konneker",
                                  "Lloyd Konneker",
                                  "2023")

        procedure.add_enum_argument ("run-mode", "Run mode",
                                         "The run mode", Gimp.RunMode,
                                         Gimp.RunMode.INTERACTIVE,
                                         GObject.ParamFlags.READWRITE)

        procedure.set_menu_label("Test Resynthesizer...")
        # Top level menu "Test"
        procedure.add_menu_path ("<Image>/Test")

        return procedure


Gimp.main(TestResynthPlugin.__gtype__, sys.argv)
