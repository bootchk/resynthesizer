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


;; gettext.install("resynthesizer", gimp.locale_directory, unicode=True)

(define (_ m) "stub" m)

(define (N_ m) "stub" m)

(define-with-return
  (script-fu-heal-transparency
   timg tdrawable samplingRadiusParam orderParam)
  (let ((org-selection nil))
    ;; precondition should be enforced by Gimp according to image modes allowed.
    (when (= FALSE (car (gimp-drawable-has-alpha tdrawable)))
      (gimp-message "The active layer has no alpha channel to heal.")
      (return))

    (gimp-image-undo-group-start timg)

    ;; save selection for later restoration.
    ;; Saving selection channel makes it active, so we must save and restore the active layer
    (set! org-selection (gimp-selection-save timg))
    (gimp-image-set-active-layer timg tdrawable)

    ;; alpha to selection
    (gimp-image-select-item timg CHANNEL-OP-REPLACE tdrawable)

    ;; Want the transparent, not the opaque.
    (gimp-selection-invert timg)
    ;; Since transparency was probably anti-aliased (dithered with partial transpancy),
    ;; grow the selection to get past the dithering.
    (gimp-selection-grow timg 1)
    ;; Remove the alpha from this layer. IE compose with current background color (often white.)
    ;; Resynthesizer won't heal transparent.
    (gimp-layer-flatten tdrawable)

    ;; Call heal selection (not the resynthesizer), which will create a proper corpus.
    ;; 0 = sample from all around
    (script-fu-heal-selection timg tdrawable samplingRadiusParam 0 orderParam)

    ;; Restore image to initial conditions of user, except for later cleanup.

    ;; restore selection
    (gimp-image-select-item timg CHANNEL-OP-REPLACE org-selection)

    ;; Clean up (comment out to debug)
    (gimp-image-undo-group-end timg)
    ))

(script-fu-register
 ;; func name
 "script-fu-heal-transparency"
 ;; menu label
 (N_ "Heal transparency(scm)...")
 ;; description
 (string-append
  (N_ "Removes alpha channel by synthesis.  Fill outward for edges, inward for holes.")
  (N_ "Requires separate resynthesizer plugin."))
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

 SF-ADJUSTMENT (_ "Context sampling width (pixels):")
 (list 50          ; value
       1           ; lower
       10000       ; upper
       1           ; step_inc
       10          ; page_inc
       0           ; digits
       SF-SPINNER) ; type

 ;; 2 is the default in the original py.
 SF-OPTION (_ "Filling order:") (list
				 (_ "Random")
				 (_ "Inwards towards center")
				 (_ "Outwards from center"))
 )

(script-fu-menu-register "script-fu-heal-transparency"
			 "<Image>/Filters/Enhance")

(script-fu-menu-register "script-fu-heal-transparency"
                         "<Image>/Filters/Resynthesizer(scm)/")
