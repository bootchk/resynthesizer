#!/usr/bin/env gimp-script-fu-interpreter-3.0
;!# Close comment started on first line. Needed by gettext.

; Gimp plugin "Heal selection"

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

; Copyright 2025 lloyd konneker
; Based on the GIMP 2 version Copyright 2022 itr-tert
;  Based on plugin-heal-selection.py Copyright 2009 lloyd konneker (bootch at nc.rr.com)
;   Based on smart_remove.scm Copyright 2000 by Paul Harrison.


; l.konneker Jan. 2025
;
; Comments about revisions from GIMP 2 to GIMP 3.
; The prior version by itr-tert is for GIMP 2.
; These comments explain changes required for GIMP 3.
; The commit text will not explain.
;
; Same changes are required for all resynthesizer outer plugins in Scheme language.
;
;
; Code changes:
;
; ScriptFu 3 no longer defines for compatibility with SIOD dialect:
;   nil, use '()
;   nth, use Scheme list-ref
;
; Since GIMP 3, PDB calls yielding a C array now yield a Scheme vector,
; no longer prefixed by an integer size.
; Use car instead of cadr.
;
; "define-with-return" is unique to TinyScheme.
; Since ScriptFu 3, use standard Scheme "quit" now to return to GIMP with an error code.
;
; ScriptFu 3 now can independently interpret (alternative to served by extension-script-fu.)
; Use a shebang at the top of the file.
;
; Because block quotes #! !# is not in TinyScheme, 
; but gettext parses for block quotes, need the second line also.
;
; ScriptFu 3 adds script-fu-register-filter.
; Where GIMP implements the dialog and handles settings.
; script-fu-register is deprecated.
; The signature of the registration function changed; omit: image,drawable, add: arity.
; Note the run function still declares image and drawables, now a vector.
;
; Since the multi-select feature of GIMP 3, a filter receives one or many drawables.
; Warn user that the filter only will use one when many are passed.
;
;
; GUI changes:
; 
; The plugins in Scheme replace plugins in Python.
; The plugins appear in menus as before, without "(scm)"
; to denote they are alternatives to Python plugins.
; 
; Capitalize all words in menu items.
; 
;
; Coding style changes:
;
; Prefer whitespace and ";" over ";;"
; Conventional indentation by two spaces.
; No indent keeper for Emacs

; TODO
;   Check that the engine plugin is installed, see code in itr-text repo
;   Eliminate parameter encoding, add separate args to the engine plugin.

; Since now independently interpreted, all definitions
; will go out of scope when plugin yields.
; No worries about polluting the namespace of extension-script-fu.



(define debug #f)


(define (gettext msgid)
  (catch msgid
	 (car (plug-in-resynthesizer-gettext msgid))))
(define (N_ m) m)  ; like gettext-noop
(define (G_ m) (gettext m))
(define (S_ m) (string-append m "​"))  ; Add zero-width spaces to suppress translation.
(define (SG_ m) (S_ (G_ m)))


(define (script-fu-heal-selection-test)
  (let* ((image 1)
	 (drawable (car (gimp-image-get-active-drawable image))))
    ; (set! image (car (gimp-image-list)))
    ; (set! drawable (gimp-image-get-active-drawable image))
    ; (gimp-image-delete (gimp-image-duplicate image))
    (script-fu-heal-selection
     image
     drawable
     50
     0
     0
     )))


(define
  (script-fu-heal-selection
   timg tdrawables samplingRadiusParam directionParam orderParam)
  "Create stencil selection in an image copy to pass as source (corpus) to plugin resynthesizer,
     which does the substantive work."

  (let ((targetBounds '())
	      (corpusImage '())
        (targetDrawable '())
        (corpusDrawable '())
        (orgSelection '())
        (grownSelection '())
        (frisketBounds '())
        (frisketLowerLeftX '())
        (frisketLowerLeftY '())
        (frisketUpperRightX '())
        (frisketUpperRightY '())
        (targetLowerLeftX '())
        (targetLowerLeftY '())
        (targetUpperRightX '())
        (targetUpperRightY '())
        (frisketWidth '())
        (frisketHeight '())
        (newWidth '())
        (newHeight '())
        (newLLX '())
        (newLLY '())
        (useBorder '()))

    (set! samplingRadiusParam (floor samplingRadiusParam))

    ; The filter enabling mechanism in GIMP does not yet
    ; know when a filter requires a selection of a region.
    ; Heal selection requires a selected region to heal.
    ; (When select the whole image, the result is, in practice, usually nonsensical.)
    ; Not user-friendly: yell at user when no selected region.
    (when (= TRUE (car (gimp-selection-is-empty timg)))
      (gimp-message (G_ "You must first select a region to heal."))
      (quit -1))

    (gimp-image-undo-group-start timg)

    ; !!! The user can choose drawable mask (grayscale channel), don't restrict to layer.

    ; Since 3.0 the user can select many drawables, a vector of drawables.
    ; But we only heal one, arbitrarily the first.
    ; Tell user when many drawables are chosen, but proceed.
    (when (> (vector-length tdrawables) 1)
      (gimp-message "Heal Selection will only heal one drawable."))
             

    (set! targetDrawable (vector-ref tdrawables 0))
    (when (null? targetDrawable)
      (throw "Failed get selected drawable"))

    ; Get bounds of user's selection, to be healed.
    (set! targetBounds (gimp-drawable-mask-bounds targetDrawable))



    ; Duplicate image as the sample (corpus).
    ; Easier to use duplicate image than just a duplicate layer.

    (set! corpusImage (car (gimp-image-duplicate timg)))
    (when (null? corpusImage)
      (throw "Failed duplicate image"))

    ; corpusDrawable is the corresponding single, selected drawable from the corpus image
    (set! corpusDrawable (vector-ref 
                            (car (gimp-image-get-selected-drawables corpusImage))
                            0))

    (when (null? corpusDrawable)
      (throw "Failed get selected drawable from corpus"))

    

    
    ; Make new selection in corpus image, the surroundings of the original
    ; selection, less a hole for the target selection.
    ; (We don't want to sample from the target selection.) 
    ; Grow and punch hole, making a frisket iow stencil iow donut
    
    (set! orgSelection (car (gimp-selection-save corpusImage))) ; save for later use
    ; Many PDB calls return void, or an error which terminates this plugin.
    (gimp-selection-grow corpusImage samplingRadiusParam)

    ; !!! Note that if selection is a bordering ring already, growing expanded it inwards.
    ; Which is what we want, to make a corpus inwards.
    (set! grownSelection (car (gimp-selection-save corpusImage)))

    ; Cut hole where the original selection was, so we don't sample from it.
    ; !!! Note that gimp enums/constants are not prefixed with GIMP_
    (gimp-image-select-item corpusImage CHANNEL-OP-SUBTRACT orgSelection)

    ;
    ; Selection (to be the corpus) is donut or frisket around the original target T
    ;   xxx
    ;   xTx
    ;   xxx
    ;

    ; crop corpus image to size of selection to save memory and for directional healing!!
    (set! frisketBounds (gimp-drawable-mask-bounds grownSelection))
    (set! frisketLowerLeftX  (list-ref frisketBounds 1))
    (set! frisketLowerLeftY  (list-ref frisketBounds 2))
    (set! frisketUpperRightX (list-ref frisketBounds 3))
    (set! frisketUpperRightY (list-ref frisketBounds 4))

    (set! targetLowerLeftX   (list-ref targetBounds 1))
    (set! targetLowerLeftY   (list-ref targetBounds 2))
    (set! targetUpperRightX  (list-ref targetBounds 3))
    (set! targetUpperRightY  (list-ref targetBounds 4))

    (set! frisketWidth  (- frisketUpperRightX frisketLowerLeftX))
    (set! frisketHeight (- frisketUpperRightY frisketLowerLeftY))

    ; User's choice of direction affects the corpus shape, and is also passed to resynthesizer plugin

    (cond
     ( (= directionParam 0)  ; all around
       ; Crop to the entire frisket
       (set! newWidth  frisketWidth)
       (set! newHeight frisketHeight)
       (set! newLLX    frisketLowerLeftX)
       (set! newLLY    frisketLowerLeftY)
       )
     ( (= directionParam 1)  ; sides
       ; Crop to target height and frisket width:  XTX
       (set! newWidth  frisketWidth)
       (set! newHeight (- targetUpperRightY targetLowerLeftY))
       (set! newLLX    frisketLowerLeftX)
       (set! newLLY    targetLowerLeftY)
       )
     ( (= directionParam 2)  ; above and below
       ; X Crop to target width and frisket height
       ; T
       ; X
       (set! newWidth  (- targetUpperRightX targetLowerLeftX))
       (set! newHeight frisketHeight)
       (set! newLLX    targetLowerLeftX)
       (set! newLLY    frisketLowerLeftY)
       ))

    ; Restrict crop to image size (condition of gimp_image_crop) eg when off edge of image
    (set! newWidth  (min newWidth  (- (car (gimp-image-get-width  corpusImage)) newLLX)))
    (set! newHeight (min newHeight (- (car (gimp-image-get-height corpusImage)) newLLY)))
    (gimp-image-crop corpusImage newWidth newHeight newLLX newLLY)

    ; Encode two script params into one resynthesizer param.
    ; use border 1 means fill target in random order
    ; use border 0 is for texture mapping operations, not used by this script
    (cond
     ( (= orderParam 0)
       (set! useBorder 1)  ; User wants NO order, ie random filling
       )
     ( (= orderParam 1)  ; Inward to corpus.  2,3,4
       ; !!! Offset by 2 to get past the original two boolean values
       (set! useBorder (+ directionParam 2))
       )
     ( #t
       ; Outward from image center.
       ; 5+0=5 outward concentric
       ; 5+1=6 outward from sides
       ; 5+2=7 outward above and below
       (set! useBorder (+ directionParam 5))))

    ; Note that the old resynthesizer required an inverted selection !!

    (when debug
      (catch "do nothing when error"
	     (gimp-display-new corpusImage)
	     (gimp-displays-flush)
	     ; (sleep 2) ;  tiny-scheme can't sleep
	     ))

    ; Not necessary to restore image to initial condition of selection, activity,
    ; the original image should not have been changed,
    ; and the resynthesizer should only heal, not change selection.

    ; v3 API of the engine plugin changed
    (plug-in-resynthesizer
     targetDrawable
     0 0                 ; vtile htile
     useBorder           ; use-context
     corpusDrawable       ; corpus
     -1                  ; inmask
     -1                  ; outmask
     0.0                 ; map-weight
     0.117               ; autism
     16                  ; neighbourhood
     500                 ; trys
     )

    ; Clean up
    (unless debug
      (gimp-image-delete corpusImage))
    (gimp-image-undo-group-end timg)
    ))

(script-fu-register-filter
  "script-fu-heal-selection"   ; run func name
  (SG_"_Heal Selection...")    ; menu label
  (string-append               ; description
    (SG_"Heal the selection from surroundings as if using the heal tool.")
    (SG_"Requires separate resynthesizer plugin."))
  "Lloyd Konneker"             ; author
  "2009 Lloyd Konneker"        ; copyright notice
  "2025"                       ; date created
  "RGB* GRAY*"                 ; image types handled by script
  SF-ONE-OR-MORE-DRAWABLE      ; arity of defined PDB procedure
  ; Formal declaration of parameters and their widget kinds.
  ; Omitting implicit "image" and "drawables"
  SF-ADJUSTMENT (G_"Context sampling width (pixels)")
    (list 50          ; value
          1           ; lower
          10000       ; upper
          1           ; step_inc
          10          ; page_inc
          0           ; digits
          SF-SPINNER) ; type
  SF-OPTION (G_"Sample from")
    (list (G_"All around")
          (G_"Sides")
          (G_"Above and below"))
  SF-OPTION (G_"Filling order")
    (list (G_"Random")
          (G_"Inwards towards center")
          (G_"Outwards from center")))

(script-fu-menu-register "script-fu-heal-selection"
			  "<Image>/Filters/Enhance")

