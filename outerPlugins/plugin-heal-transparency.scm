#!/usr/bin/env gimp-script-fu-interpreter-3.0

;; Gimp plugin "Heal transparency"

;; License:
;;
;;   This program is free software; you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation; either version 2 of the License, or
;;   (at your option) any later version.
;;
;;   This program is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;   GNU General Public License for more details.
;;
;;   The GNU Public License is available at
;;   http://www.gnu.org/copyleft/gpl.html

;; plugin-heal-transparency.scm Copyright 2022 itr-tert
;;  Based on plugin-heal-transparency.py Copyright 2010 lloyd konneker (bootch at nc.rr.com)


(define script-fu-heal-transparency (let
()  ; indent keeper

(define (gettext msgid)
  (catch msgid
	 (car (plug-in-resynthesizer-gettext msgid))))
(define (N_ m) m)  ; like gettext-noop
(define (G_ m) (gettext m))
(define (S_ m) (string-append m "​"))  ; Add zero-width spaces to suppress translation.
(define (SG_ m) (S_ (G_ m)))


(define-with-return
  (script-fu-heal-transparency
   timg tdrawable samplingRadiusParam orderParam)
  (let ((org-selection nil))

    ; precondition: an alpha channeld exists
    ; The GUI preflights the precondition and won't allow user to choose
    ; this plugin when precondition is not met.
    ; But the plugin can also be called noninteractive by other plugins.
    ; So check the precondition again.
    (when (= FALSE (car (gimp-drawable-has-alpha tdrawable)))
      (gimp-message (G_"The active layer has no alpha channel to heal."))
      (return))

    (gimp-image-undo-group-start timg)

    ; We play with the selection mask but will leave it in original condition.
    ; Save selection for later restoration.
    (set! org-selection(car(gimp-selection-save timg)))
    ; assert org-selection is a new channel named "selection mask copy"
    ; assert org-selection is the active item i.e. drawable

    ; We want to operate on the drawable, not the selection mask.
    ; Make the single drawable active, i.e. chosen and the target of operations.
    ; since v3, pass vector of layers, size 1
    ; since v3 wording change "active" => "selected" i.e. chosen by user
    (gimp-image-set-selected-layers timg 1 (make-vector 1 tdrawable))

    ; Make a selection mask from the alpha channel.
    ; AKA alpha-to-selection, but that procedure does not exist
    (gimp-image-select-item timg CHANNEL-OP-REPLACE tdrawable)
    ; Assert the selection mask now equals the alpha channel
    ; Note the selection mask is-a channel, but is not visible in GUI.
    ; The selection is all pixels that are not transparent.

    ;; Want the transparent, not the opaque.
    (gimp-selection-invert timg)

    ;; Since transparency was probably anti-aliased (dithered with partial transpancy),
    ;; grow the selection to get past the dithering.
    (gimp-selection-grow timg 1)

    ;; Remove the alpha from this layer.
    ;; IE compose with current background color (often white.)
    ;; Resynthesizer won't heal pixels that are transparent.
    (gimp-layer-flatten tdrawable)

    ;; Call heal selection (not the resynthesizer), which will create a proper corpus.
    ;; 0 = sample from all
    ;; Must pass run-mode, note the name of the constant is not RUNMODE-, i.e. unconventionally named
    (script-fu-heal-selection RUN-NONINTERACTIVE timg tdrawable samplingRadiusParam 0 orderParam)

    ;; Restore image to initial conditions of user, except for later cleanup.

    ;; restore selection mask
    ; The operand is a channel
    (gimp-image-select-item timg CHANNEL-OP-REPLACE org-selection)
    ; Assert the selection mask equals the given channel.

    ; Delete the channel that is selection mask copy, that we created
    ; TODO this fails because channel is not inserted in image?
    ; (gimp-channel-delete org-selection)
    ; but it is visible to user?


    ;; Clean up (comment out to debug)
    (gimp-image-undo-group-end timg)
    ))

(script-fu-register
 ;; func name
 "script-fu-heal-transparency"
 ;; menu label
 (SG_"_Heal transparency(scm)...")
 ;; description
 (string-append
  (SG_"Removes alpha channel by synthesis.  Fill outward for edges, inward for holes.")
  (SG_"Requires separate resynthesizer plugin."))
 ;; author
 "Lloyd Konneker"
 ;; copyright notice
 "Copyright 2010 Lloyd Konneker"
 ;; date created
 "2010"
 ;; image type that the script works on
 "RGBA, GRAYA"  ; !!! Requires an alpha channel to heal
 ;; parameters
 SF-IMAGE "Image" 0
 SF-DRAWABLE "Drawable" 0
 SF-ADJUSTMENT (G_"Context sampling width (pixels)")
 (list 50          ; value
       1           ; lower
       10000       ; upper
       1           ; step_inc
       10          ; page_inc
       0           ; digits
       SF-SPINNER) ; type

 ;; 2 is the default in the original py.
 SF-OPTION (G_"Filling order")
 (list (G_"Random")
       (G_"Inwards towards center")
       (G_"Outwards from center")))

(script-fu-menu-register "script-fu-heal-transparency"
			 "<Image>/Filters/Enhance")

(script-fu-menu-register "script-fu-heal-transparency"
			 (string-append "<Image>/Filters/"
					(SG_"Resynthesizer(scm)")))

script-fu-heal-transparency
))
