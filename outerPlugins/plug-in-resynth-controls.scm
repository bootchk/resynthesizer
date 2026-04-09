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
This is not user friendly.
The original GTK implementation enabled and disabled widgets to prevent user from making invalid choices.
Here, we have not implemented that enabling/disabling of widgets, because it is not possible in Script-Fu.

Also the terminology is not understandable to users, and the labels are not clear.
But we retained the original labels, which are already translated, to avoid breaking translation.

The rationale is: very few users will use this plugin, 
and those who do will be advanced users who are more likely to understand the terminology 
and less likely to be confused by the lack of enabling/disabling of widgets.
"


"
Filling order choices are even more problematic for translations.
Some filling order choices were added after original implementation and translations.
The choices are in one enumeration, but they are about different things, and hard to understand.
Paul Harrion's original implementation had only the first two filling order choices, which are not about bands.
The band filling order choices were added later.
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

; FUTURE: improve labels of widgets, requiring translation, 
; and also add tooltips, to make it more clear to users what the widgets do.
; In trailing comments below
; are better labels for the widgets
; but they break translation, so for now we use the less clear labels that are already translated.

(script-fu-register-filter
  "plug-in-resynth-controls"
  _"_Resynthesize..."
  (string-append
    _"Use the resynthesizer engine's control panel."
    _"Requires plugin 'plug-in-resynthesizer' is installed.")
  "Lloyd Konneker"
  "Copyright 2023 Lloyd Konneker"
  "2023"
  "*"
  ; This is a filter: user must first choose target image and layer.
  ; Target image and drawable implicit, not declared. 
  SF-ONE-OR-MORE-DRAWABLE      ; arity of defined PDB procedure
  ; User can choose a different image for source.
  SF-DRAWABLE  _"Texture source" 0  ;  _"Source (corpus)"
  SF-OPTION    _"Randomized filling order"
    (list
      _"Random order, without context" ; "Without context"
      _"Random order, with context"    ; "Without bands"
      _"Randomized bands, concentric, inwards"                                        ; "Bands concentric, inward (squeeze)"
      _"Randomized bands, horizontally, inwards (i.e. squeezing from top and bottom)" ; "Bands horizontal, inward (squeeze from top and bottom)"
      _"Randomized bands, vertically, inwards (i.e. squeezing from left and right)"   ; "Bands vertical, inward (squeeze from left and right)"
      _"Randomized bands, concentric, outwards (e.g. for uncrop)"                     ; "Bands concentric, outward (e.g. for uncrop)"
      _"Randomized bands, horizontally, outwards, (i.e. expanding to top and bottom)" ; "Bands horizontal, outward (expand to top and bottom)"
      _"Randomized bands, vertically, outwards (i.e. expanding to left and right)"   ; "Bands vertical, outward (expand to left and right)"
      _"Randomized bands, concentric, inwards and outwards (i.e. squeezing in and out a donut)") ; "Bands concentric, inward and outward (squeeze donut)")

  SF-TOGGLE _"Make horizontally tileable" 0 ; _"Output seamlessly tileable horizontally"
  SF-TOGGLE _"Make vertically tileable" 0 ; _"Output seamlessly tileable vertically"

  ; Formerly, latter widgets disabled until first widget chosen.
  ; I.E. the map choosers disabled until "use weight maps" checkbox chosen.
  ; In the conversion from GTK to Script-Fu, we have not implemented that enabling/disabling of widgets.
  ; We might use indentation of label intended to indicate grouping, but that breaks translation.
  
  SF-TOGGLE     _"Use texture transfer" 0  ; _"Use weight maps for matching and transferring"
  SF-DRAWABLE   _"Input map"            0  ; _"Weight map for matching"
  SF-DRAWABLE   _"Output map"           0  ; _"Weight map for transferring"
  SF-ADJUSTMENT _"Map importance"
     (list
       0.5
       0.01 1.0
       0.01 0.1
       2 ; decimal places
       SF-SLIDER)
  ; formerly in the "tweaks" tab
  SF-ADJUSTMENT _"Neighbourhood size"  ; _"Size of neighborhood considered for matching, in pixels"
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

(script-fu-menu-register "plug-in-resynth-controls"
			 "<Image>/Filters/Map")

(script-fu-register-i18n "plug-in-resynth-controls" "resynthesizer3")

