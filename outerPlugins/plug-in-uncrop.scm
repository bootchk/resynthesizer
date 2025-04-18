#!/usr/bin/env gimp-script-fu-interpreter-3.0

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
; lloyd konneker, lkk, itr-tert

; Version:
; 1.0 lkk 2009-5-15 Initial version in scheme, released to Gimp Registry.
; 1.1 lkk 2009-9-21 Translate to python.
; later versions, see git log

; Increase image/canvas size and synthesize outer band from edge of original.

; The effect for users:
; widens the field of view, maintaining perspective of original
; Should be undoable, except for loss of selection.
; Should work on any image type, any count of layers and channels (although only selected layer is affected.)
;
; IN: Nothing special.  The selection is immaterial but is not preserved.
; OUT larger layer and image.  All other layers not enlarged.


(define (uncrop-test)
  (tracing TRUE)
  (let* ((image (vector-ref (car (cdr (gimp-image-list))) 0))
         (drawable (car (gimp-image-get-active-drawable image))))
    (plug-in-uncrop
     image
     drawable
     2
     )))


(define (get-image)
  (vector-ref (car (cdr (gimp-image-list))) 0))


(define (get-drawable image)
  (when (null? image)
    (set! image (get-image)))
  (car (gimp-image-get-active-drawable image)))


(define (get-component-list image)
  (when (null? image) (set! image (get-image)))
  (let ((type (car (gimp-image-get-base-type image))))
    (cond ((= type RGB)
           (list RED-CHANNEL GREEN-CHANNEL BLUE-CHANNEL ALPHA-CHANNEL))
          ((= type GRAY)
           (list GRAY-CHANNEL ALPHA-CHANNEL))
          ((= type INDEXED)
           (list INDEXED-CHANNEL ALPHA-CHANNEL))
          (#t
           (throw "unkwno image base type")))))


(define (get-components-active image)
  (when (null? image) (set! image (get-image)))
  (map (lambda (channel) (car (gimp-image-get-component-active image channel)))
       (get-component-list image)))


(define (set-components-active image active-list)
  (when (null? image) (set! image (get-image)))
  (let* ((components (get-component-list image))
         (components-length (length components))
         (active-list-length (length active-list)))

    (when (<> active-list-length components-length)
      (throw "set-components-active: active-list.length != image.components.length"))

    (do ((i (- active-list-length 1) (- i 1)))
        ((> 0 i))
      (gimp-image-set-component-active image
                                       (nth i components)
                                       (nth i active-list))))
  )

; FIXME
; Has no comments about why this is necessary.
; Not used, for now, until it is ported

(define (drawable-anti-erase-selection drawable)
  (let* ((image          '())
         (ch-active-list '())
         (components     '()))
    (set! image (if (null? drawable)
                    (get-image)
                    (car (gimp-item-get-image drawable))))
    (set! drawable (if (null? drawable)
                       (get-drawable image)
                       drawable))
    (set! ch-active-list (get-components-active image))
    (set! components (get-component-list image))
    (for-each (lambda (ch)
                (gimp-image-set-component-active image ch
                                                 (if (= ch ALPHA-CHANNEL) TRUE FALSE)))
              components)
    (gimp-image-set-component-active image ALPHA-CHANNEL TRUE)
    (gimp-edit-bucket-fill drawable FILL-WHITE LAYER-MODE-NORMAL 100 255 FALSE 0 0)
    (set-components-active image ch-active-list)
    ))


(define (resizeImageCentered image percentEnlarge)
  ; resize and center image by percent (converted to pixel units)
  (let* ((deltaFraction (+ (/ percentEnlarge 100) 1.0))
         (priorWidth  (gimp-image-get-width  image))
         (priorHeight (gimp-image-get-height image))
         (deltaWidth  (* priorWidth  deltaFraction))
         (deltaHeight (* priorHeight deltaFraction))
         (centeredOffX (/ (- deltaWidth  priorWidth)  2))
         (centeredOffY (/ (- deltaHeight priorHeight) 2)))
    (gimp-image-resize image deltaWidth deltaHeight centeredOffX centeredOffY)
    ; if not gimp-image-resize(image, deltaWidth, deltaHeight, centeredOffX, centeredOffY):
    ;     raise RuntimeError, "Failed resize"
    ))


(define (shrinkSelectionByPercent image percent)
  ; shrink selection by percent (converted to pixel units)
  (let* ((deltaFraction (/ percent 100))
         ; convert to pixel dimensions
         (priorWidth  (gimp-image-get-width  image))
         (priorHeight (gimp-image-get-height image))
         (deltaWidth  (* priorWidth  deltaFraction))
         (deltaHeight (* priorHeight deltaFraction))
         ; !!! Note total shrink percentage is halved (width of band is percentage/2)
         (maxDelta (/ (max deltaWidth deltaHeight) 2)))
    (gimp-selection-shrink image maxDelta)
    ; if not gimp-selection-shrink(image, maxDelta):
    ;    raise RuntimeError,  "Failed shrink selection"
    ))

; Yield the first selected layer, or throw
(define (get-selected-layer image)
  (let* ((layers (gimp-image-get-selected-layers image))
         (layer (vector-ref layers 0)))
    (when (null? layer)
      (throw "Failed get selected layer"))
    layer))

; Prepare target: enlarge canvas and select the new, blank outer ring
; Side effects the passed image, the target of resynthesis.
(define (enlarge-image-and-select-ring orgImage orgDrawable percentEnlargeParam)
  (let ((selectAllPrior '()))

    ; Select entire original image. Discard any existing selection.
    (gimp-selection-all orgImage)

    ; Save original bounds to later select outer band
    (set! selectAllPrior (gimp-selection-save orgImage))
    ; selectAllPrior is a channel

    ; Enlarge the canvase
    (resizeImageCentered orgImage percentEnlargeParam)
    ; Resize image alone doesn't resize layer, so resize layer also
    (gimp-layer-resize-to-image-size orgDrawable)

    ; select outer band, the new blank canvas.
    ; First select the original bounds
    (gimp-image-select-item orgImage CHANNEL-OP-REPLACE selectAllPrior)
    ; Then invert
    (gimp-selection-invert orgImage)
    ; Selection is now a ring, the area of enlargement.

    ; Cleanup the saved original bounds
    (gimp-image-remove-channel orgImage selectAllPrior))
)

; Prepare source (corpus) layer, from the corpusImage.
; corpusImage is a copy of the original.
;
; Select a band around the edge.
; Note the width of corpus band is same as width of enlargement band.
;
; Working with the original size.
; Could be alpha channel transparency
;
; Yields a corpus layer
(define (prepare-corpus-layer corpusImage percentEnlargeParam)
  (let ((corpusLayer    '()))
    ; Get the selected (active) layer of the copy image.
    ; The same layer as active in the original.
    (set! corpusLayer (get-selected-layer corpusImage))
    
    ; Select outer band:  select all, shrink
    (gimp-selection-all corpusImage)
    (shrinkSelectionByPercent corpusImage percentEnlargeParam)
    (gimp-selection-invert corpusImage)  ; invert interior selection into a frisket
    ; yield the corpusLayer
    corpusLayer
))




(define (plug-in-uncrop orgImage drawables percentEnlargeParam)
  ; Create frisket stencil selection in a temp image to pass as source (corpus) to plugin resynthesizer,
  ; which does the substantive work.

  (gimp-message-set-handler MESSAGE-BOX)

  (script-fu-use-v3)

  (gimp-image-undo-group-start orgImage)

  (let ((orgDrawable (vector-ref drawables 0))
        (corpusImage    '())
        (corpusLayer    '()))

    (when (not (gimp-item-id-is-layer orgDrawable))
      (gimp-message _"You must select a layer, not a channel.")
      (quit))

    ; copy original for use as corpus
    (set! corpusImage (gimp-image-duplicate orgImage))
    (when (null? corpusImage)
      (throw "Failed duplicate image"))

    (enlarge-image-and-select-ring orgImage orgDrawable percentEnlargeParam)
    ; Assert orgImage, the target, is ready for resynthesis.

    (set! corpusLayer (prepare-corpus-layer corpusImage percentEnlargeParam))
    ; corpusLayer is from copy, same size as original.
    ; corpusLayer has a selection mask that is ring around its edge

    (plug-in-resynthesizer
     orgDrawable        ; target
     0 0                ; vtile htile
     5                  ; use-context ; 5 means inside out direction
     corpusLayer
     -1                 ; inmask
     -1                 ; outmask
     0.0                ; map-weight
     0.117              ; autism
     16                 ; neighbourhood
     500)               ; trys

    ; FIXME added by iter-tert
    ; commented out until ported
    ; (when (= TRUE (car (gimp-drawable-has-alpha orgDrawable)))
    ;  (drawable-anti-erase-selection orgDrawable))

    ; Clean up.
    ; Any errors now are moot.

    (gimp-image-delete corpusImage)

    ; Leave the image without any selection mask that existed prior.
    (gimp-selection-none orgImage)

    ; Make orgDrawable the selected (active) one
    ; Since v3 API, pass a simple vector, without length argument
    (gimp-image-set-selected-layers orgImage (make-vector 1 orgDrawable))

    (gimp-displays-flush)
    (gimp-image-undo-group-end orgImage)
    ))


(script-fu-register-filter
 "plug-in-uncrop"
 _"Uncrop..."
 _"Enlarge image by synthesizing a border that matches the edge, maintaining perspective.  Works best for small, natural edges. Undo a Crop instead, if possible!"
 "Lloyd Konneker"
 "Copyright 2009 Lloyd Konneker"
 "2009"
 ; script works on any image mode and regardless of alpha
 "*"
  SF-ONE-DRAWABLE      ; menu item enabled if exactly one drawable selected
 ; parameters
 
 ; integer valued percent
 SF-ADJUSTMENT _"Percent enlargement"
 (list 10   ; value
       0    ; lower
       100  ; upper
       1    ; step inc
       10   ; page inc
       0    ; integer, zero decimal places
       SF-SLIDER))

(script-fu-menu-register "plug-in-uncrop"
                         "<Image>/Filters/Enhance")