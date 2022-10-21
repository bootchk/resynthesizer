;; Gimp plugin "Heal selection"

;; License:

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

;; plugin-heal-selection.scm Copyright 2022 itr-tert
;;  Based on plugin-heal-selection.py Copyright 2009 lloyd konneker (bootch at nc.rr.com)
;;   Based on smart_remove.scm Copyright 2000 by Paul Harrison.


;; gettext.install("resynthesizer", gimp.locale_directory, unicode=True)

(define (_ m) "stub" m)

(define (N_ m) "stub" m)

(define debug FALSE)


(define-with-return (script-fu-heal-selection-test)
  (let* ((image 1)
	 (drawable (car (gimp-image-get-active-drawable image))))
    ;; (set! image (car (gimp-image-list)))
    ;; (set! drawable (gimp-image-get-active-drawable image))
    ;; (gimp-image-delete (gimp-image-duplicate image))
    (script-fu-heal-selection
     image
     drawable
     50
     0
     0
     )))


(define-with-return
  (script-fu-heal-selection
   timg tdrawable samplingRadiusParam directionParam orderParam)
  "Create stencil selection in a temp image to pass as source (corpus) to plugin resynthesizer,
   which does the substantive work."

  (let ((targetBounds nil)
	(tempImage nil)
	(work_drawable nil)
	(orgSelection nil)
	(grownSelection nil)
	(frisketBounds nil)
	(frisketLowerLeftX nil)
	(frisketLowerLeftY nil)
	(frisketUpperRightX nil)
	(frisketUpperRightY nil)
	(targetLowerLeftX nil)
	(targetLowerLeftY nil)
	(targetUpperRightX nil)
	(targetUpperRightY nil)
	(frisketWidth nil)
	(frisketHeight nil)
	(newWidth nil)
	(newHeight nil)
	(newLLX nil)
	(newLLY nil)
	(useBorder nil))

    (set! samplingRadiusParam (floor samplingRadiusParam))

    (when (= TRUE (car (gimp-selection-is-empty timg)))
      (gimp-message (_ "You must first select a region to heal."))
      (return))

    (gimp-image-undo-group-start timg)

    (set! targetBounds (gimp-drawable-mask-bounds tdrawable))

    ;; In duplicate image, create the sample (corpus).
    ;; (I tried to use a temporary layer but found it easier to use duplicate image.)
    (set! tempImage (car (gimp-image-duplicate timg)))
    (when (null? tempImage)
      (throw "Failed duplicate image"))

    ;; !!! The drawable can be a mask (grayscale channel), don't restrict to layer.
    (set! work_drawable (car (gimp-image-get-active-drawable tempImage)))
    (when (null? work_drawable)
      (throw "Failed get active drawable"))

    ;;
    ;; grow and punch hole, making a frisket iow stencil iow donut
    ;;
    (set! orgSelection (car (gimp-selection-save tempImage))) ; save for later use
    (gimp-selection-grow tempImage samplingRadiusParam)

    ;; ??? returns None , docs say it returns SUCCESS

    ;; !!! Note that if selection is a bordering ring already, growing expanded it inwards.
    ;; Which is what we want, to make a corpus inwards.
    (set! grownSelection (car (gimp-selection-save tempImage)))

    ;; Cut hole where the original selection was, so we don't sample from it.
    ;; !!! Note that gimp enums/constants are not prefixed with GIMP_
    (gimp-image-select-item tempImage CHANNEL-OP-SUBTRACT orgSelection)

    ;;
    ;; Selection (to be the corpus) is donut or frisket around the original target T
    ;;   xxx
    ;;   xTx
    ;;   xxx
    ;;

    ;; crop the temp image to size of selection to save memory and for directional healing!!
    (set! frisketBounds (gimp-drawable-mask-bounds grownSelection))
    (set! frisketLowerLeftX  (nth 1 frisketBounds))
    (set! frisketLowerLeftY  (nth 2 frisketBounds))
    (set! frisketUpperRightX (nth 3 frisketBounds))
    (set! frisketUpperRightY (nth 4 frisketBounds))
    (set! targetLowerLeftX   (nth 1 targetBounds ))
    (set! targetLowerLeftY   (nth 2 targetBounds ))
    (set! targetUpperRightX  (nth 3 targetBounds ))
    (set! targetUpperRightY  (nth 4 targetBounds ))

    (set! frisketWidth  (- frisketUpperRightX frisketLowerLeftX))
    (set! frisketHeight (- frisketUpperRightY frisketLowerLeftY))

    ;; User's choice of direction affects the corpus shape, and is also passed to resynthesizer plugin

    (cond
     ( (= directionParam 0)  ; all around
       ;; Crop to the entire frisket
       (set! newWidth  frisketWidth)
       (set! newHeight frisketHeight)
       (set! newLLX    frisketLowerLeftX)
       (set! newLLY    frisketLowerLeftY)
       )
     ( (= directionParam 1)  ; sides
       ;; Crop to target height and frisket width:  XTX
       (set! newWidth  frisketWidth)
       (set! newHeight (- targetUpperRightY targetLowerLeftY))
       (set! newLLX    frisketLowerLeftX)
       (set! newLLY    targetLowerLeftY)
       )
     ( (= directionParam 2)  ; above and below
       ;; X Crop to target width and frisket height
       ;; T
       ;; X
       (set! newWidth  (- targetUpperRightX targetLowerLeftX))
       (set! newHeight frisketHeight)
       (set! newLLX    targetLowerLeftX)
       (set! newLLY    frisketLowerLeftY)
       ))

    ;; Restrict crop to image size (condition of gimp_image_crop) eg when off edge of image
    (set! newWidth  (min newWidth  (- (car (gimp-image-width  tempImage)) newLLX)))
    (set! newHeight (min newHeight (- (car (gimp-image-height tempImage)) newLLY)))
    (gimp-image-crop tempImage newWidth newHeight newLLX newLLY)

    ;; Encode two script params into one resynthesizer param.
    ;; use border 1 means fill target in random order
    ;; use border 0 is for texture mapping operations, not used by this script
    (cond
     ( (= orderParam 0)
       (set! useBorder 1)  ; User wants NO order, ie random filling
       )
     ( (= orderParam 1)  ; Inward to corpus.  2,3,4
       ;; !!! Offset by 2 to get past the original two boolean values
       (set! useBorder (+ directionParam 2))
       )
     ( #t
       ;; Outward from image center.
       ;; 5+0=5 outward concentric
       ;; 5+1=6 outward from sides
       ;; 5+2=7 outward above and below
       (set! useBorder (+ directionParam 5))))

    ;; Note that the old resynthesizer required an inverted selection !!

    (when (= debug TRUE)
      (catch "do nothing when error"
	     (gimp-display-new tempImage)
	     (gimp-displays-flush)
	     ;;; (sleep 2) ;  tiny-scheme can't sleep
	     ))

    ;; Not necessary to restore image to initial condition of selection, activity,
    ;; the original image should not have been changed,
    ;; and the resynthesizer should only heal, not change selection.

    ;; Note that the API hasn't changed but use_border param now has more values.
    (plug-in-resynthesizer
     RUN-NONINTERACTIVE  ; run-mode
     timg                ; image
     tdrawable           ; drawable
     0 0                 ; vtile htile
     useBorder           ; use-context
     work_drawable       ; corpus
     -1                  ; inmask
     -1                  ; outmask
     0.0                 ; map-weight
     0.117               ; autism
     16                  ; neighbourhood
     500                 ; trys
     )

    ;; Clean up
    (when (= debug FALSE)
      (gimp-image-delete tempImage))
    (gimp-image-undo-group-end timg)
    ))

(script-fu-register
 ;; func name
 "script-fu-heal-selection"
 ;; menu label
 "Heal selection(scm)..."
 ;; description
 (string-append
  (N_ "Heal the selection from surroundings as if using the heal tool.")
  "Requires separate resynthesizer plugin.")
 ;; author
 "Lloyd Konneker <>"
 ;; copyright notice
 "2009 Lloyd Konneker"
 ;; date created
 "2009"
 ;; image type that the script works on
 "RGB*, GRAY*"

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

 SF-OPTION (_ "Sample from:") (list
			       (_ "All around")
			       (_ "Sides")
			       (_ "Above and below"))

 SF-OPTION (_ "Filling order:") (list
				 (_ "Random")
				 (_ "Inwards towards center")
				 (_ "Outwards from center"))
 )

(script-fu-menu-register "script-fu-heal-selection"
			 "<Image>/Filters/Enhance")

(script-fu-menu-register "script-fu-heal-selection"
                         "<Image>/Filters/Resynthesizer(scm)/")

;; (script-fu-heal-selection-test)
