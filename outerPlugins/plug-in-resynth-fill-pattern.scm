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




; Seamless fill the selection, from a pattern.
; This script is a front end to the resynthesizer plugin.
; This uses the GIMP 3.0 API.
; This script is a modification of the original plugin-resynth-fill-pattern.py

; Authors:
;  2025 lloyd konneker 3.0 conversion
;  2022 itr-tert Script-fu 2.0 conversion
;  2011 lloyd konneker plugin-resynth-fill-pattern.py 
;  Original by Rob Antonishen


(define debug #f)


; Create a new image and layer having the same size as a pattern.
; Yields a new image and a new layer.
; The new image is the same type as the source image.
; The new layer is the same type as the passed layer of the source image.
; The new image is same size as the pattern.
(define (layer-from-pattern image drawable pattern)
  
  (let* ((new-basetype   (gimp-image-get-base-type image))  ; same as source
         (new-layertype  (gimp-drawable-type drawable))
         (pattern-info   (gimp-pattern-get-info pattern))
         (pattern-width  (list-ref pattern-info 0))
         (pattern-height (list-ref pattern-info 1))
         (bpp            (list-ref pattern-info 2))
         (new-image      (gimp-image-new pattern-width pattern-height new-basetype))
         ; !!! gimp-layer-new wants a layer type, not an image basetype
         (new-drawable   (gimp-layer-new 
                            new-image
                            "Texture" ; name of layer
                            pattern-width pattern-height
					                  new-layertype  
                            100 ; opacity
                            LAYER-MODE-NORMAL)))

    ; Must add the layer to the image before using it
    (gimp-image-insert-layer 
      new-image 
      new-drawable 
      0 0)  ; parent layer, position

    ; yield the new image and layer
    (list new-image new-drawable)))


; This is the guts of the algorithm.  It creates a new image and
; layer, fills the layer with the pattern, and then uses the
; resynthesizer to fill the selection with the pattern.
; The pattern is an object, named by e.g. "Maple Leaves"
; The image and drawable are the source image and layer.
(define (guts image drawable pattern)
  
  ; Make drawable from pattern
  (let* ((image-layer (layer-from-pattern image drawable pattern))
         (pattern-image (list-ref image-layer 0))
         (pattern-layer (list-ref image-layer 1))
         )

    ; Fill it with pattern
    (gimp-drawable-fill pattern-layer FILL-PATTERN)

    (when debug
      (gimp-display-new pattern-image)
      (gimp-displays-flush))

    ; Resynthesize the selection from the pattern without using context
    ; 0,0,0: Not use-border (context), not tile horiz, not tile vert
    ; -1, -1, 0: No maps and no map weight
    ; Resynthesizer is an engine, never interactive
    (plug-in-resynthesizer
      drawable
      0  0  0
      pattern-layer
      -1  -1 0
      0.05  8  300)

    ; Clean up
    (unless debug
      ; Delete image that is displayed throws RuntimeError
      (gimp-image-delete pattern-image))))


; This is the main plugin function that is called by GIMP.
; It is the entry point for the script.
; It is called by GIMP app when the user selects the script from the menu.
; It is the function that does the work of the script.
(define (plug-in-fill-pattern-resynth image drawable pattern)
  ; The usual user-friendly precondition checking, postcondition cleanup.

  ; User-friendly: if no selection, use entire image.
  ; But the resynthesizer does that for us.

  ; Save/restore the context since we change the pattern

  ; Use v3 binding of return values from calls to GIMP PDB
  (script-fu-use-v3)

  (gimp-message-set-handler MESSAGE-BOX)
  (gimp-context-push)
  (gimp-context-set-pattern pattern)
  (guts image drawable pattern)
  (gimp-context-pop)
  (gimp-displays-flush))


(script-fu-register
 "plug-in-fill-pattern-resynth"
 _"Fill with Pattern Seamless..."
 _"Seamlessly fill with a pattern using synthesis."
 "Lloyd Konneker"
 "Copyright 2011 Lloyd Konneker"
 "2011"
 ; image type that the script works on
 "RGB* GRAY*"
 ; parameters
 SF-IMAGE    "Image" 0
 SF-DRAWABLE "Drawable" 0
 SF-PATTERN  _"Pattern" "Maple Leaves"
 )

(script-fu-menu-register "plug-in-fill-pattern-resynth"
                         "<Image>/Edit")
