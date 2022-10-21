;; Gimp plugin "Fill with pattern seamless..."

;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; The GNU Public License is available at
;; http://www.gnu.org/copyleft/gpl.html

;; Front end to the resynthesizer plugin to make a seamless fill.

;; Author:
;;  itr-tert
;;   Based on plugin-resynth-fill-pattern.py Copyright 2011 lloyd konneker
;;   Idea by Rob Antonishen


;; gettext.install("resynthesizer", gimp.locale_directory, unicode=True);

(define (_ m) "stub" m)

(define (N_ m) "stub" m)

(define debug FALSE)


(define (layer-from-pattern image pattern)
  ;; Create a new image and layer having the same size as a pattern.
  (let* ((new-basetype (car (gimp-image-base-type image)))  ; same as source
         (new-layertype (car (gimp-drawable-type (car (gimp-image-get-active-layer image)))))
         (pattern-info (gimp-pattern-get-info pattern))
         (pattern-width  (nth 0 pattern-info))
         (pattern-height (nth 1 pattern-info))
         (bpp            (nth 2 pattern-info))
         (new-image (car (gimp-image-new pattern-width pattern-height new-basetype)))
         ;; !!! Note that gimp-layer-new wants a layer type, not an image basetype
         (new-drawable (car (gimp-layer-new new-image pattern-width pattern-height
					    new-layertype "Texture" 100 NORMAL-MODE))))
    (gimp-image-add-layer new-image new-drawable 0)
    (list new-image new-drawable)))


(define (guts image drawable pattern)
  ;; Crux of algorithm

  ;; Make drawble from pattern
  (let* ((image-layer (layer-from-pattern image pattern))
         (pattern-image (nth 0 image-layer))
         (pattern-layer (nth 1 image-layer))
         )

    ;; Fill it with pattern
    (gimp-drawable-fill pattern-layer PATTERN-FILL)

    (when (= debug TRUE)
      (gimp-display-new pattern-image)
      (gimp-displays-flush))

    ;; Resynthesize the selection from the pattern without using context
    ;; 0,0,0: Not use_border (context), not tile horiz, not tile vert
    ;; -1, -1, 0: No maps and no map weight
    ;; DO pass pattern_layer.ID !!!
    ;; Resynthesizer is an engine, never interactive
    (plug-in-resynthesizer RUN-NONINTERACTIVE image drawable
                           0  0  0  pattern-layer
                           -1  -1  0  0.05  8  300)
    ;; Clean up
    (when (= debug FALSE)
      ;; Delete image that is displayed throws RuntimeError
      (gimp-image-delete pattern-image))))


(define (script-fu-fill-pattern-resynth image drawable pattern)
  ;; Main: the usual user-friendly precondition checking, postcondition cleanup.
  ;; pattern is a string

  ;; User_friendly: if no selection, use entire image.
  ;; But the resynthesizer does that for us.

  ;; Save/restore the context since we change the pattern
  (gimp-message-set-handler MESSAGE-BOX)
  (gimp-context-push)
  (gimp-context-set-pattern pattern)
  (guts image drawable pattern)
  (gimp-context-pop)
  (gimp-displays-flush))


(script-fu-register
 ;; func name
 "script-fu-fill-pattern-resynth"
 ;; menu label
 (N_ "_Fill with pattern seamless(scm)...")
 ;; description
 (string-append
  (N_ "Seamlessly fill with a pattern using synthesis.")
  (N_ "Requires separate resynthesizer plugin."))
 ;; author
 "Lloyd Konneker"
 ;; copyright notice
 "Copyright 2011 Lloyd Konneker"
 ;; date created
 "2011"
 ;; image type that the script works on
 "RGB*, GRAY*"
 ;; parameters
 SF-IMAGE "Image" 0
 SF-DRAWABLE "Drawable" 0
 SF-PATTERN  (_ "Pattern:") "Maple Leaves"
 )

(script-fu-menu-register "script-fu-fill-pattern-resynth"
                         "<Image>/Edit")

(script-fu-menu-register "script-fu-fill-pattern-resynth"
                         "<Image>/Filters/Resynthesizer(scm)/")
