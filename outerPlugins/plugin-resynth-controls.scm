#!/usr/bin/env gimp-script-fu-interpreter-3.0


; Gimp plugin "Map>Resynthesize"

; This is a "control panel" GIMP plugin calling the resynthesizer engine, a GIMP plugin.

; Copyright 2019 lloyd konneker (github bootchk )


"
This plugin is the GUI to the engine.

It does some checking of user choices, for user friendliness.

Generally one-to-one correspondence between GUI widgets and engine parameters.
Except for the FUTURE generate vector field checkbox

User-friendliness considerations

empty selection in target is not an error, the engine will select all
if pdb.gimp_selection_is_empty(timg):

empty selection in source is not an error, the engine will select all
if pdb.gimp_selection_is_empty(sourceImage):

source same as target is not an error.
Random fill order will generally produce no visible change,
but some fill orders may produce a subtly altered result.
Same images implies same selections.
if timg == sourceImage and tdrawable == sourceDrawable:
"

;debug = False

; run func, must match name of plugin
(define
  (script-fu-resynth-controls
    timg
    tdrawable
    sourceDrawable
    fillingOrderChoice
    horizontalTileableChoice
    verticalTileableChoice
    useWeightMapsChoice
    matchWeightDrawableChoice
    transferWeightDrawableChoice
    mapImportanceChoice
    neighborhoodSizeChoice
    searchThoroughnessChoice
    sensitivityToOutliersChoice)

    ; FUTURE generateVectorFieldChoice

  (gimp-message "controls")


  ; Here we check requirements of the engine.
  ; After the fact of the user's choice.
  ; Ideally, and in the original Gtk implementation,
  ; we enabled list items to meet the constraints.
  ; Here, we let the user choose anything,
  ; but then yell at them and refuse to continue.

  ; Constraints
  ;     Source image must not be indexed mode
  ;     Corpus image must not be indexed mode
  ;     Source and corpus must be same base type
  ;        e.g. not one RGB and the other gray.
  ;        They can differ in bpp i.e. one can have alpha channel.

  ; The source image chooser widget (provided by GimpFu) does NOT filter by image mode.
  ; Since GimpFu v3, calls to pdb cannot be nested.
  ; = pdb.gimp_item_get_image(sourceDrawable)
  ;;source_image_mode = pdb.gimp_image_base_type(sourceImage)
  ;; = pdb.gimp_image_base_type(timg)

  ; Engine requires source mode NOT be INDEXED, else it crashes.
  ;;if source_image_mode == INDEXED :
    ;;pdb.gimp_message(_"The source image cannot be of mode INDEXED"));
    ;;return

  ;Engine requires base modes same (but allows differences in alpha)
  ;;if target_image_mode != source_image_mode :
    ;;pdb.gimp_message(_"The source and target images must have same mode except for alpha e.g. RGB and RGBA"));
    ;;return



  ;;pdb.gimp_image_undo_group_start(timg)

  ; Implement alternative to GTK widget enabling.
  ; Formerly, the map chooser widgets were not enabled until the "use maps" checkbox was chosen.
  ; Here, they are enabled and may have default values.
  ; We don't use the values unless the checkbox is enabled.

  ; The map image choosers are always enabled but only pass them to engine if "use weight maps" checkbox is Yes

  ; TODO we should also check that the maps ARE proper base type?
  ; Or convert them to gray before passing them?
  ; TODO study the original, see if the engine allows non-gray map

  ;;if useWeightMapsChoice == 0:
     ; None => -1
     ;;matchingMap = -1
     ;;transferringMap = -1
  ;;else:
     ;;matchingMap = matchWeightDrawableChoice
     ;;transferringMap = transferWeightDrawableChoice
  ; Resynthesizer ignores mapWeight param if maps are none


  ; FUTURE
  ; Encode generateVectorFieldChoice into useBorder parameter
  ; Add 10 to mean: use same filling order, but post process to deliver false color
  ;; generateVectorFieldChoice == 1:
  ;;    fillingOrder = fillingOrderChoice + 10


  ; engine API hasn't changed in a long time, but use_border param now has more values.

  ; Order of params different from order in GUI
  ; Since GimpFu v3, pass vector of drawables
  (plug-in-resynthesizer
    ; OLD API to engine plugin
    ;RUN-NONINTERACTIVE  ; run-mode
    ;timg                ; image
    ;1                   ; count drawables
    ;(make-vector 1 tdrawable)  ; vector of one drawable

    tdrawable
    horizontalTileableChoice
    verticalTileableChoice
    fillingOrderChoice
    sourceDrawable
    ; TODO mangle this matching map
    matchWeightDrawableChoice
    ; TODO mangle this transferringMap
    transferWeightDrawableChoice
    mapImportanceChoice
    sensitivityToOutliersChoice
    neighborhoodSizeChoice
    searchThoroughnessChoice
    ))

  ;;pdb.gimp_image_undo_group_end(timg)


; TODO colons edited in labels
; SF is adding them doubly


; Spinner is digital and linear, slider is analog but exponential
; Use slider for floats?

(script-fu-register
  "script-fu-resynth-controls"
  _"_Resynthesize..."
  (string-append
    _"Use the resynthesizer engine's control panel."
    _"Requires plugin 'plug-in-resynthesizer' is installed.")
  "Lloyd Konneker"
  "Copyright 2023 Lloyd Konneker"
  "2023"
  "RGB* GRAY*"
  SF-IMAGE      "Image"             0
  SF-DRAWABLE   "Drawable"          0
  SF-DRAWABLE  _"Source (corpus):"  0
  SF-OPTION    _"Randomized filling order:"
    (list
      _"Without context"
      _"Without bands"
      _"Bands concentric, inward (squeeze)"
      _"Bands horizontal, inward (squeeze from top and bottom)"
      _"Bands vertical, inward (squeeze from left and right)"
      _"Bands concentric, outward (e.g. for uncrop)"
      _"Bands horizontal, outward (expand to top and bottom)"
      _"Bands vertical, outward (expand to left and right)"
      _"Bands concentric, inward and outward (squeeze donut)")

  SF-TOGGLE _"Output seamlessly tileable horizontally:" 0
  SF-TOGGLE _"Output seamlessly tileable vertically:"   0

  ; formerly, latter widgets disabled until first widget chosen
  ; Now, indentation of label intended to indicate grouping,
  ; but labels are right justified, so indentation is lost.
  SF-TOGGLE     _"Use weight maps:"                  0
  SF-DRAWABLE   _"     Weight map for matching:"     0
  SF-DRAWABLE   _"     Weight map for transferring:" 0
  SF-ADJUSTMENT _"     Map importance:"
     (list
       0.5
       0.01 1.0
       0.01 0.1
       2 ; decimal places
       SF-SLIDER)
  ; formerly in the "tweaks" tab
  SF-ADJUSTMENT _"Neighborhood size:"
    (list
    30
      1 100
      1 10
      0
      SF-SPINNER)
  SF-ADJUSTMENT _"Search thoroughness:"
    (list
      200
      1 500
      10 100
      0
      SF-SPINNER
      )
  SF-ADJUSTMENT _"Sensitivity to outliers:"
    (list
      0.12
      0.1 1.0
      0.01 0.1
      2
      SF-SLIDER)
 )

(script-fu-menu-register "script-fu-resynth-controls"
			 "<Image>/Filters/Enhance")

(script-fu-menu-register "script-fu-resynth-controls"
			 (string-append "<Image>/Filters/"
					_"Resynthesizer(scm)"))


