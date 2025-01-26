#!/usr/bin/env gimp-script-fu-interpreter-3.0
;!# Close comment started on first line. Needed by gettext.


; Gimp plugin "Heal transparency"

; License:
;
;   This program is free software; you can redistribute it and/or modify
;   it under the terms of the GNU General Public License as published by
;   the Free Software Foundation; either version 2 of the License, or
;   (at your option) any later version.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;   GNU General Public License for more details.
;
;   The GNU Public License is available at
;   http://www.gnu.org/copyleft/gpl.html

; Copyright 2025 l.konneker
;  Based on plugin-heal-transparency.scm Copyright 2022 itr-tert
;   Based on plugin-heal-transparency.py Copyright 2010 lloyd konneker




; How it works
; Select the transparent pixels.
; Make them white, since resynthesizer won't actually change transparency.
; Submit the image to the "heal selection" plugin,
; with the special selection and suitable parameters.

; When this plugin satisfies user expectation:
; This plugin works best for small bits of transparency (holes)
; or for thin edges of transparency.
; Often unsatisfactory, wierd, for large areas of transparency.

(define (plugin-heal-transparency
           timg tdrawables samplingRadiusParam orderParam)
  (let 
    ((org-selection '())
     (targetDrawable (vector-ref tdrawables 0)))

    ; Require an alpha channel exists.
    ; The GIMP menu enabling mechanism won't let user choose
    ; this plugin when no alpha.
    ; But the plugin can also be called noninteractive by other plugins.
    ; So check the precondition again.
    (when (= FALSE (car (gimp-drawable-has-alpha targetDrawable)))
      (gimp-message _"The active layer has no alpha channel to heal.")
      (quit -1))

    ; Handle multi-select layers.
    ; When multiple layers selected, warn but proceed.
    ; When called non-interactively, serves as debugging statement.
    (when (> (vector-length tdrawables)
              1)
      (gimp-message "Only healing the first selected layer"))

    (gimp-image-undo-group-start timg)

    ; We play with the selection mask but will leave it in original condition.
    ; Save selection for later restoration.
    ; The argument is the image, not the drawable.
    (set! org-selection (car (gimp-selection-save timg)))
    ; assert org-selection is a new channel named "selection mask copy".
    ; assert org-selection is the active item i.e. drawable.

    ; We want to operate on the drawable, not the selection mask.
    ; Make the single drawable active, i.e. chosen and the target of operations.
    ; Since PDB API v3, pass vector of layers.
    ; Since v3 wording change "active" => "selected" i.e. chosen by user.
    (gimp-image-set-selected-layers timg (make-vector 1 targetDrawable))

    ; Make a selection mask from the alpha channel.
    ; AKA alpha-to-selection, but that procedure does not exist
    (gimp-image-select-item timg CHANNEL-OP-REPLACE targetDrawable)
    ; Assert the selection mask now equals the alpha channel
    ; Note the selection mask is-a channel, but is not visible in GUI.
    ; The selection is all pixels that are not transparent.

    ; Want the transparent, not the opaque.
    (gimp-selection-invert timg)

    ; Since transparency was probably anti-aliased (dithered with partial transpancy),
    ; grow the selection to get past the dithering.
    (gimp-selection-grow timg 1)

    ; Remove the alpha from this layer.
    ; IE compose with current background color (often white.)
    ; Resynthesizer won't heal pixels that are transparent.
    (gimp-layer-flatten targetDrawable)

    ; Call heal selection (not the resynthesizer engine), which will create a proper corpus.
    (plugin-heal-selection
    ; Must pass run-mode, the name is not RUNMODE-.
      RUN-NONINTERACTIVE 
      timg
      ; Since GIMP v3 multi-select, plugins take vector of drawables.
      (make-vector 1 targetDrawable)
      samplingRadiusParam 
      0 ; sample from: all around
      ; We specialize "all around" since "from sides" etc. don't usually make sense for transparency.
      orderParam)

    ; Restore image to initial conditions of user, except for later cleanup.

    ; Restore selection mask.
    ; The operand is a channel
    (gimp-image-select-item timg CHANNEL-OP-REPLACE org-selection)
    ; Assert the selection mask equals the given channel.

    ; Delete the channel that is selection mask copy, that we created
    ; TODO this fails because channel is not inserted in image?
    ; (gimp-channel-delete org-selection)
    ; but it is visible to user?


    ; Clean up (comment out to debug)
    (gimp-image-undo-group-end timg)

    )) ; end let and define


(script-fu-register-filter
  "plugin-heal-transparency"
  ; No key shortcut _H since already taken by Heal Selection
  _"Heal Transparency..."
  _"Removes alpha channel by synthesis.  Fill outward for edges, inward for holes."
  "Lloyd Konneker"
  "Copyright 2010 Lloyd Konneker"
  "2010"
  "RGBA, GRAYA"  ; !!! Requires an alpha channel to heal
  SF-ONE-OR-MORE-DRAWABLE      ; arity of defined PDB procedure
  ; declare arguments
  SF-ADJUSTMENT _"Context sampling width (pixels)"
    (list 50          ; value
          1           ; lower
          10000       ; upper
          1           ; step_inc
          10          ; page_inc
          0           ; digits
          SF-SPINNER) ; type

  ; Inwards is the default in the original python version.
  ; Here, default is Random.
  ; Practically, the user should assess and make a choice,
  ; since Inwards is better for holes,
  ; but Outwards is better for edges of image.
  SF-OPTION _"Filling order"
    (list _"Random"
          _"Inwards towards center"
          _"Outwards from center")
)


; register the procedure as a menu item
(script-fu-menu-register "plugin-heal-transparency"
			 "<Image>/Filters/Enhance")

