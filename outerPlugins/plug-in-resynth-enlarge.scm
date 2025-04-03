#!/usr/bin/env gimp-script-fu-interpreter-3.0

; Gimp plugin "Enlarge and resynthesize"

; License:
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; The GNU Public License is available at
; http://www.gnu.org/copyleft/gpl.html

; Author:
; 2022 itr-tert
;  Based on plugin-resynth-enlarge.py 2010 lloyd konneker
;   Based on smart-enlarge.scm 2000 by Paul Harrison.



; Algorithm:
;
; Scale image up.
; Resynthesize with:
;   corpus = original size image
;   in map = original size image but scaled up and down to blur
;   out map = scaled up image
;
; This restores the detail that scaling up looses.
; It maintains the aspect ratio of all image features.
;
; Unlike the original smart-enlarge.scm, this alters the original image.
;
; original did not accept an alpha channel



(define (get-selected-layer image)
  (vector-ref (gimp-image-get-selected-drawables image) 0))


(define
  (plug-in-enlarge-resynthesized image drawables scale-factor)

  ; Use v3 semantics for binding to PDB: car is not necessary
  ; !!! Note this is called in the run function but has execution scope
  (script-fu-use-v3)

  (gimp-message-set-handler MESSAGE-BOX)

  (let ((drawable    '())
        (temp-image1 '())
        (temp-image2 '())
        (temp-layer1 '())
        (temp-layer2 '())
        (width       '())
        (height      '()))

    ; GIMP 3 allows multi-select
    ; When user selected many, warn.
    (when (> (vector-length drawables) 1)
      (gimp-message "Using only the first selected layer"))

    (set! drawable (get-selected-layer image))

    (gimp-image-undo-group-start image)

    (set! temp-image1 (gimp-image-duplicate image))  ; duplicate for in map
    (set! temp-image2 (gimp-image-duplicate image))  ; duplicate for corpus
    (when (or (null? temp-image1)
              (null? temp-image2))
      (gimp-message "Failed duplicate image")
      (quit))

    (set! temp-layer1 (get-selected-layer temp-image1))
    (set! temp-layer2 (get-selected-layer temp-image2))
    (when (or (null? temp-layer1)
              (null? temp-layer2))
      (gimp-message "Failed get selected layer")
      (quit))

    ; scale input map down and back, to blur
    (set! width  (gimp-drawable-get-width  drawable))
    (set! height (gimp-drawable-get-height drawable))

    (gimp-image-scale temp-image1 (/ width scale-factor) (/ height scale-factor))
    (gimp-image-scale temp-image1    width                  height              )

    ; scale up the image
    (gimp-image-scale image       (* width scale-factor) (* height scale-factor))

    ; Resynthesize entire image to restore details.

    ; Note there should not be a selection, else effect not as desired.
    ; FIXME: ensure there is no selection.

    (plug-in-resynthesizer
     drawable
     0  0  0
     temp-layer2  ; corpus
     temp-layer1  ; input map
     drawable     ; output map is original itself
     1.0  0.117  8  500)

    (gimp-image-delete temp-image1)
    (gimp-image-delete temp-image2)
    (gimp-image-undo-group-end image)
    ))


(script-fu-register-filter
 "plug-in-enlarge-resynthesized"
 
 _"Enlarge and Sharpen by Resynthesis..."      ; menu label
 _"Enlarge image and synthesize to sharpen."  ; tooltip
 ; additional info not implemented
 ; _"Requires separate resynthesizer plugin."
 
 "Lloyd Konneker"   ; author
 "Copyright 2000 Paul Harrison, 2010 Lloyd Konneker"   ; copyright notice
 "2010"             ; date created

 ; script works on any image mode
 "*"

 ; script requires exactly one selected drawable
 SF-ONE-DRAWABLE      ; arity of defined PDB procedure

 ; declare arguments, other than implicit image, drawables
 SF-ADJUSTMENT _"Scale by"  ; scale factor
 (list 2           ; value
       1           ; lower
       32          ; upper
       0.1         ; step inc
       1           ; page inc
       2           ; digits
       SF-SPINNER) ; type
 )

(script-fu-menu-register
  "plug-in-enlarge-resynthesized"
  "<Image>/Filters/Enhance")
