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


; Create new image having texture synthesized from the selection.
; Works best if selection is natural (fractal).
; Can work with man-made regular texture.
; Works worst with man-made, structured but not regular, symbols.
; Sometimes called rendering a texture.
;
; Requires resynthesizer plug-in.

; Authors:
; lloyd konneker
; itr-tert

; Version:
; 1.0 lkk 2010-07-15 Initial version
; 1.1 lkk 2011-04-10 Fixed a bug with layer types impacting greyscale images.
; later versions, see git log

;
; The effect for users:
; Similar to "Fill resynthesized pattern" except:
;   - here the arguments are reversed: you select a texture and create a new image
;     instead of selecting an area and choosing a pattern.
;   - here the result is less random (since Fill resynthesized adds noise.)
; Different from tiling since:
;   - seamless and irregular pattern
;
; The continuum of randomness versus speed:
;   - Filters.Map.Tile is fast but has seams and regular pattern (even if you use "Make Seamless" first.)
;   - Filter.Render.Texture a tile followed by tiling is seamless but still has regularity.
;   - Filte.Render.Texture an entire image is slower but seamless and moderately irregular.
;   - Edit.Fill with resynthesized pattern is slowest but seamless and highly irregular, unpatterned.
;
; This filter is not tiling (instead resynthesizing) but makes
; an image that you can then use to tile with especially if
; you choose the option to make the edges suitable for tiling.
;
; IN: The selection (or the entire active drawable) is the source of texture and is not changed.
; OUT New image, possibly resized canvas, same scale and resolution.
;
; TODO a quality setting
;

(define debug #f)

(define (test)
  (let* ((image (vector-ref (gimp-image-list) 0))
         (drawable (vector-ref (gimp-image-get-selected-drawables image) 0)))
    (script-fu-render-texture image drawable 2 TRUE)))

(define (new-resized-image image resize-ratio)
  ; Create new image resized by a ratio from *selection* in the old image
  (let* ((bounds       (gimp-selection-bounds image))
         (is-selection (list-ref bounds 0))
         (ulx          (list-ref bounds 1))
         (uly          (list-ref bounds 2))
         (lrx          (list-ref bounds 3))
         (lry          (list-ref bounds 4))
         (new-width     '())
         (new-height    '())
         (new-basetype  '())
         (new-layertype '())
         (new-image     '())
         (new-drawable  '()))

    ; for v3 binding, is-selection is type boolean, #t or #f
    (if (not is-selection)
        (begin
          ; Resynthesizer will use the entire source image as corpus.
          ; Resize new image in proportion to entire source image.
          (set! new-width  (floor (* (gimp-image-get-width  image) resize-ratio)))
          (set! new-height (floor (* (gimp-image-get-height image) resize-ratio)))
          (display "Render Texture image from entire image")
          (newline))
        (begin
          ; Resize new image in proportion to selection in source
          (set! new-width  (floor (* (- lrx ulx) resize-ratio)))
          (set! new-height (floor (* (- lry uly) resize-ratio)))
          (display "Render Texture image from selection")))

    (set! new-basetype (gimp-image-get-base-type image))  ; same as source
    (set! new-layertype (gimp-drawable-type (vector-ref (gimp-image-get-selected-layers image) 0)))

    (set! new-image (gimp-image-new new-width new-height new-basetype))

    ; !!! Note that gimp-layer-new wants a layer type, not an image basetype
    (set! new-drawable (gimp-layer-new 
      new-image
      "Texture"  ; name of layer, arg order changed in PDB API v3
      new-width new-height
      new-layertype
      100  ; full opaque
      LAYER-MODE-NORMAL))
      
    ; The new layer has opaque attribute, 
    ; but the new image has transparent pixels (for image modes with alpha)
    ; Resynthesizer will not change the transparency, so make pixels opaque.
    ; Fill with white will make them opaque.
    (gimp-drawable-fill new-drawable FILL-WHITE)

    ; A new layer must be added to an image.
    (gimp-image-insert-layer 
      new-image new-drawable 
      0  ; parent is zero (not a group)
      0) ; position is 0 (top)

    ; See gimp-image-new: if indexed mode, need set palette on new image.
    (when (gimp-drawable-is-indexed new-drawable)
      ; Set palette of new image to palette of in image.
      (gimp-image-set-palette new-image (gimp-image-get-palette image)))

    ; yield a tuple of new image and drawable
    (list new-image new-drawable)))


; copy an image and crop the copy to the selection in the image
(define (copy-and-crop-image-to-selection image)
  (let ((result-image '())
        (bounds       '())
        (is-selection '())
        (ulx        '())
        (uly        '())
        (lrx        '())
        (lry        '()))

    (set! result-image (gimp-image-duplicate image))
    (when (null? result-image)
      (throw "Failed duplicate image"))

    ; Get bounds, offset of selection
    (set! bounds (gimp-selection-bounds result-image))
    (set! is-selection (list-ref bounds 0))
    (set! ulx          (list-ref bounds 1))
    (set! uly          (list-ref bounds 2))
    (set! lrx          (list-ref bounds 3))
    (set! lry          (list-ref bounds 4))

    (display "here")
    ; The value is type boolean, #t or #f
    (when is-selection
      (gimp-image-crop result-image (- lrx ulx) (- lry uly) ulx uly))

    ; yield the cropped image
    result-image))


(define (display-image image)
  (catch "do nothing when error"
         ; If runmode is NONINTERACTIVE, expect gimp-display-new() to fail
         (gimp-display-new image))
  (gimp-displays-flush))


(define (script-fu-render-texture image drawables resize-ratio make-tile)
  
  ; Create a randomized texture image from the selection.
  ; User can choose to make the image be suited for further, seamless tiling.
  ; The image is same scale and resolution but resized from the selection.
  ; Not undoable, no changes to the source (you can just delete the new image.)
  ;
  ; A selection in the source image is optional.
  ; If there is no selection, the resynthesizer will use the entire source image.
  
  ; Its all or nothing, user must delete new image if not happy.

  ; Use v3 binding of return values from PDB.  Has execution scope
  (script-fu-use-v3)

  ; Note argument drawables is a list, but we don't use it.

  (gimp-image-undo-disable image)

  ; Create new image, optionally resized, and display for it.

  (let* ((new-image-new-drawable '())
        (result-image     '())
        (result-drawable '())
        (corpus-image '())
        (corpus-layer '())
        (htile      '())
        (vtile      '()))
    (set! new-image-new-drawable (new-resized-image image resize-ratio))
    (set! result-image    (list-ref new-image-new-drawable 0))
    (set! result-drawable (list-ref new-image-new-drawable 1))

    (gimp-image-undo-disable result-image)
    ; FIXME: when calls to PDB fail, do we really get here with empty list, or with -1?
    (when (null? result-drawable)
      (throw "Failed create new result layer."))

    ; copy original into temp corpus image and crop it to the selection.
    ; To save memory in resynthesizer.
    (set! corpus-image (copy-and-crop-image-to-selection image))

    ; If using new resynthesizer with animation for debugging,
    ; display the result early, and the corpus
    (when debug
      (display-image result-image)
      (display-image corpus-image)
      )

    ; Don't flatten corpus because it turns transparency to background (white usually)

    ; Get the corpus-layer, resynthesizer wants a layer, not an image
    (set! corpus-layer (vector-ref (gimp-image-get-selected-layers corpus-image) 0))
    (when (null? corpus-layer)
      (throw "Failed get corpus layer"))

    ; Insure the selection is all (not necessary, resynthesizer will use all if no selection.)
    (gimp-selection-all corpus-image)

    ; Settings for making edges suitable for seamless tiling afterwards.
    ; That is what these settings mean in the resynthesizer:
    ; wrap context probes in the target so that edges of target will be suitable for seamless tiling.
    ; I.E. treat the target as a sphere when matching.
    ; The GUI control value is 0 or 1, not boolean
    (if (= TRUE make-tile)
        (begin
          (set! htile 1)
          (set! vtile 1))
        (begin
          (set! htile 0)
          (set! vtile 0)))

    ; Call resynthesizer
    ; use-border is moot since there is no context (outside the target) in the newImage.
    ; The target is the entire new image, the source is the cropped copy of the selection.
    ;
    ; 9 neighbors (a 3x3 patch) and 200 tries for speed, since new image is probably large
    ; and source is probably natural (fractal), where quality is not important.

    ; For version of resynthesizer with uninverted selection
    ; !!! Pass -1 for ID of no layer, not None
    (plug-in-resynthesizer
      result-drawable    ; target drawable
      vtile htile        ; whether to make tileable
      0                  ; use-border
      corpus-layer
      -1                 ; inmask
      -1                 ; outmask
      0.0                ; map-weight
      0.117              ; autism
      9                  ; neighbourhood
      200)               ; trys

    (when (not debug)
      (display-image result-image))
    ; else debugging displayed the result previously

    ; Clean up.
    (when (not debug)
      (gimp-image-delete corpus-image))
    (gimp-image-undo-enable image)
    (gimp-image-undo-enable result-image)))



(script-fu-register-filter
  "script-fu-render-texture"
  _"Texture..." ; menu label
  _"Create a new image with texture from the current image or selection. Optionally, create image edges suited for further, seamless tiling. "
  ; "New image is the same scale but seamless and irregular.  Use 'Map>Tile' for less randomness. Use 'Edit>Fill resynthesized pattern' for more randomness. ")
  ; "Requires separate resynthesizer plugin."))
  "Lloyd Konneker"
  "Copyright 2010 Lloyd Konneker"
  "2010"
  ; the script works images of all modes, and regardless of transparency
  "*"
  SF-ONE-DRAWABLE      ; menu item enabled if exactly one drawable selected

  ; parameters
  ; Spinner is digital and linear, slider is analog but exponential
  SF-ADJUSTMENT _"Ratio of size of new image to source selection"
  (list 2   ; value
        0.5 ; lower
        10  ; upper
        0.5 ; step inc
        1   ; page inc
        1   ; float, one decimal place
        SF-SPINNER)
  SF-TOGGLE _"Make new image edges suitable for seamless tiling" FALSE
 )

(script-fu-menu-register "script-fu-render-texture"
                         "<Image>/Filters/Render")