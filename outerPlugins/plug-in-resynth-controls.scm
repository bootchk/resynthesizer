#!/usr/bin/env gimp-script-fu-interpreter-3.0
;!# Close comment started on first line. Needed by gettext.

; Gimp plugin "Map>Resynthesize"

; This is a "control panel" GIMP plugin calling the resynthesizer engine, a GIMP plugin.

; Copyright 2019 lloyd konneker (github bootchk)


"
This plugin is the raw GUI to the engine.

It does some checking of user choices, for user friendliness.

Generally one-to-one correspondence between GUI widgets and engine parameters.
Except for the FUTURE generate vector field checkbox

"

"
Engine requires:
  target drawable not NULL
  source drawable not NULL

  target image not mode INDEXED
  source drawable not mode INDEXED

  target and source drawables must be same base type
  e.g. not one RGB and the other gray.
  They can differ in bpp i.e. one can have alpha channel.


"

"
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
  (plug-in-resynth-controls
    timg
    tdrawables
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

  (gimp-message "controls")   ; debug

  (let* ((targetDrawable '()))
  

    ; Assert GIMP does not call with targetImage and targetDrawable NULL.
    ; Assert GIMP does not call with targetImage other than declared image types.


    ; When user multi-selected drawables, arbitrarily use the first
    ; and warn but proceed.
    (when (> (vector-length tdrawables) 1 )
      (gimp-message "Using only the first drawable"))

    (set! targetDrawable (vector-ref tdrawables 0))


    ; Here we check requirements of the engine.
    ; After the fact of the user's choice.
    ; Ideally, and in the original Gtk implementation,
    ; we enabled/disabled widgets to meet some constraints.
    ; Here, we let the user choose anything,
    ; but then yell at them and warn or refuse to continue.

    ; When user failed to choose a source drawable, use the target.
    ; And warn the effect probably not intended.
    ; The widget defaults to NULL.
    (when (= sourceDrawable -1)  ; ID for NULL
       (set! sourceDrawable targetDrawable )
       (gimp-message "Using target as source"))

    
    ; TODO check target and source modes match

    ; The source image chooser widget (provided by GIMP) does NOT filter by image mode.
    
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


    ; Now call engine

    (gimp-image-undo-group-start timg)

    ; engine API hasn't changed in a long time, but use_border param now has more values.

    ; Order of params to engine different from order in GUI
    (plug-in-resynthesizer
      targetDrawable
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
      )

    (gimp-image-undo-group-end timg)
  )) ; end let and define
  


; Spinner is digital and linear, slider is analog but exponential
; Use slider for floats?

(script-fu-register-filter
  "plug-in-resynth-controls"
  _"_Resynthesize..."
  (string-append
    _"Use the resynthesizer engine's control panel."
    _"Requires plugin 'plug-in-resynthesizer' is installed.")
  "Lloyd Konneker"
  "Copyright 2023 Lloyd Konneker"
  "2023"
  "RGB* GRAY*"
  ; This is a filter: user must first choose target image and layer.
  ; Target image and drawable implicit, not declared.
  SF-ONE-OR-MORE-DRAWABLE      ; arity of defined PDB procedure
  ; User can choose a different image for source.
  SF-DRAWABLE  _"Source (corpus)"  0
  SF-OPTION    _"Randomized filling order"
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

  SF-TOGGLE _"Output seamlessly tileable horizontally" 0
  SF-TOGGLE _"Output seamlessly tileable vertically"   0

  ; formerly, latter widgets disabled until first widget chosen
  ; Now, indentation of label intended to indicate grouping.
  ; Justification of labels by GIMP is inconsistent currently,
  ; so indentation is haphazard.
  SF-TOGGLE     _"Use weight maps"                  0
  SF-DRAWABLE   _"     Weight map for matching"     0
  SF-DRAWABLE   _"     Weight map for transferring" 0
  SF-ADJUSTMENT _"     Map importance"
     (list
       0.5
       0.01 1.0
       0.01 0.1
       2 ; decimal places
       SF-SLIDER)
  ; formerly in the "tweaks" tab
  SF-ADJUSTMENT _"Neighborhood size"
    (list
      30      ; default
      1 100   ; lower, upper
      1 10    ; step, page step
      0       ; decimal places
      SF-SPINNER)
  SF-ADJUSTMENT _"Search thoroughness"
    (list
      200
      1 500
      10 100
      0
      SF-SPINNER
      )
  SF-ADJUSTMENT _"Sensitivity to outliers"
    (list
      0.12
      0.1 1.0
      0.01 0.1
      2
      SF-SLIDER)
 )

(plug-in-menu-register "plug-in-resynth-controls"
			 "<Image>/Filters/Map")


