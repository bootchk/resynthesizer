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


;; Create new image having texture synthesized from the selection.
;; Works best if selection is natural (fractal).
;; Can work with man-made regular texture.
;; Works worst with man-made, structured but not regular, symbols.
;; Sometimes called rendering a texture.
;;
;; Requires resynthesizer plug-in.

;; Author:
;; lloyd konneker, lkk, bootch at nc.rr.com, itr-tert

;; Version:
;; 1.0 lkk 2010-07-15 Initial version
;; 1.1 lkk 2011-04-10 Fixed a bug with layer types impacting greyscale images.
;; later versions, see git log

;;
;; The effect for users:
;; Similar to "Fill resynthesized pattern" except:
;;   - here the arguments are reversed: you select a texture and create a new image
;;     instead of selecting an area and choosing a pattern.
;;   - here the result is less random (since Fill resynthesized adds noise.)
;; Different from tiling since:
;;   - seamless and irregular pattern
;;
;; The continuum of randomness versus speed:
;;   - Filters.Map.Tile is fast but has seams and regular pattern (even if you use "Make Seamless" first.)
;;   - Filter.Render.Texture a tile followed by tiling is seamless but still has regularity.
;;   - Filte.Render.Texture an entire image is slower but seamless and moderately irregular.
;;   - Edit.Fill with resynthesized pattern is slowest but seamless and highly irregular, unpatterned.
;;
;; This filter is not tiling (instead resynthesizing) but makes
;; an image that you can then use to tile with especially if
;; you choose the option to make the edges suitable for tiling.
;;
;; IN: The selection (or the entire active drawable) is the source of texture and is not changed.
;; OUT New image, possibly resized canvas, same scale and resolution.
;;
;; TODO a quality setting
;;


(define script-fu-render-texture (let
()  ; indent keeper

(define debug #f)


(define (gettext msgid)
  (catch msgid
	 (car (plug-in-resynthesizer-gettext msgid))))
(define (N_ m) m)  ; like gettext-noop
(define (G_ m) (gettext m))
(define (S_ m) (string-append m "​"))  ; Add zero-width spaces to suppress translation.
(define (SG_ m) (S_ (G_ m)))


(define (test)
  (let* ((image (vector-ref (car (cdr (gimp-image-list))) 0))
         (drawable (car (gimp-image-get-active-drawable image))))
    (script-fu-render-texture image drawable 2 TRUE)))


(define (new-resized-image image resize-ratio)
  ;; Create new image resized by a ratio from *selection* in the old image
  (let* ((bounds       (gimp-selection-bounds image))
         (is-selection (nth 0 bounds))
         (ulx          (nth 1 bounds))
         (uly          (nth 2 bounds))
         (lrx          (nth 3 bounds))
         (lry          (nth 4 bounds))
         (new-width nil)
         (new-height nil)
         (new-basetype nil)
         (new-layertype nil)
         (new-image nil)
         (new-drawable nil))
    (if (<> TRUE is-selection)
        (begin
          ;; Resynthesizer will use the entire source image as corpus.
          ;; Resize new image in proportion to entire source image.
          (set! new-width  (floor (* (car (gimp-image-width  image)) resize-ratio)))
          (set! new-height (floor (* (car (gimp-image-height image)) resize-ratio))))
        (begin
          ;; Resize new image in proportion to selection in source
          (set! new-width  (floor (* (- lrx ulx) resize-ratio)))
          (set! new-height (floor (* (- lry uly) resize-ratio)))))

    (set! new-basetype (car (gimp-image-base-type image)))  ; same as source
    (set! new-layertype (car (gimp-drawable-type (car (gimp-image-get-active-layer image)))))
    (set! new-image (car (gimp-image-new new-width new-height new-basetype)))
    ;; !!! Note that gimp-layer-new wants a layer type, not an image basetype
    (set! new-drawable (car (gimp-layer-new new-image new-width new-height
                                            new-layertype "Texture" 100 NORMAL-MODE)))
    ;; The new layer is opaque, but the new image has transparent pixels (for case RGBA)
    ;; Resynthesizer will not change the transparency, so make pixels opaque.
    ;; Fill with white will make them opaque.
    (gimp-drawable-fill new-drawable FILL-WHITE)
    ;; A new layer must be added to the image.
    (gimp-image-add-layer new-image new-drawable 0)
    (list new-image new-drawable)))


(define (display-image image)
  (catch "do nothing when error"
         ;; If runmode is NONINTERACTIVE, expect gimp-display-new() to fail
         (gimp-display-new image))
  (gimp-displays-flush))


(define (script-fu-render-texture image drawable resize-ratio make-tile)
  ;;
  ;; Create a randomized texture image from the selection.
  ;; The image can be suited for further, seamless tiling.
  ;; The image is same scale and resolution but resized from the selection.
  ;; Not undoable, no changes to the source (you can just delete the new image.)
  ;;
  ;; A selection in the source image is optional.
  ;; If there is no selection, the resynthesizer will use the entire source image.
  ;;

  ;; Its all or nothing, user must delete new image if not happy.
  (gimp-image-undo-disable image)

  ;;
  ;; Create new image, optionally resized, and display for it.
  ;;
  (let ((new-image-new-drawable nil)
        (new-image nil)
        (new-drawable nil)
        (temp-image nil)
        (bounds nil)
        (is-selection nil)
        (ulx nil)
        (uly nil)
        (lrx nil)
        (lry nil)
        (work-layer nil)
        (htile nil)
        (vtile nil))
    (set! new-image-new-drawable (new-resized-image image resize-ratio))
    (set! new-image    (nth 0 new-image-new-drawable))
    (set! new-drawable (nth 1 new-image-new-drawable))

    (gimp-image-undo-disable new-image)
    (when (null? new-drawable)
      (throw "Failed create layer."))

    ;; If using new resynthesizer with animation for debugging
    (when debug
      (display-image new-image))

    ;;
    ;; copy original into temp and crop it to the selection to save memory in resynthesizer
    ;;
    (set! temp-image (car (gimp-image-duplicate image)))
    (when (null? temp-image)
      (throw "Failed duplicate image"))

    ;; Get bounds, offset of selection
    (set! bounds (gimp-selection-bounds image))
    (set! is-selection (nth 0 bounds))
    (set! ulx          (nth 1 bounds))
    (set! uly          (nth 2 bounds))
    (set! lrx          (nth 3 bounds))
    (set! lry          (nth 4 bounds))


    (when (= TRUE is-selection)
      (gimp-image-crop temp-image (- lrx ulx) (- lry uly) ulx uly))
    ;; Resynthesizer will use all if no selection.

    ;; Don't flatten because it turns transparency to background (white usually)
    (set! work-layer (car (gimp-image-get-active-layer temp-image)))
    (when (null? work-layer)
      (throw "Failed get active layer"))

    ;; Insure the selection is all (not necessary, resynthesizer will use all if no selection.)
    (gimp-selection-all temp-image)

    ;; Settings for making edges suitable for seamless tiling afterwards.
    ;; That is what these settings mean in the resynthesizer:
    ;; wrap context probes in the target so that edges of target will be suitable for seamless tiling.
    ;; I.E. treat the target as a sphere when matching.
    (if (= TRUE make-tile)
        (begin
          (set! htile 1)
          (set! vtile 1))
        (begin
          (set! htile 0)
          (set! vtile 0)))

    ;; Call resynthesizer
    ;; use_border is moot since there is no context (outside the target) in the newImage.
    ;; The target is the entire new image, the source is the cropped copy of the selection.
    ;;
    ;; 9 neighbors (a 3x3 patch) and 200 tries for speed, since new image is probably large
    ;; and source is probably natural (fractal), where quality is not important.

    ;; For version of resynthesizer with uninverted selection
    ;; !!! Pass -1 for ID of no layer, not None
    (plug-in-resynthesizer
     RUN-NONINTERACTIVE ; run-mode
     new-image          ; image
     new-drawable       ; drawable
     vtile htile        ; vtile htile
     0                  ; use-context
     work-layer         ; corpus
     -1                 ; inmask
     -1                 ; outmask
     0.0                ; map-weight
     0.117              ; autism
     9                  ; neighbourhood
     200)               ; trys

    (display-image new-image)

    ;; Clean up.
    (gimp-image-delete temp-image)
    (gimp-image-undo-enable image)
    (gimp-image-undo-enable new-image)

    new-image))


(script-fu-register
 ;; func name
 "script-fu-render-texture"
 ;; menu label
 (SG_"_Texture(scm)...")
 ;; description
 (string-append
  (SG_"Create a new image with texture from the current image or selection. Optionally, create image edges suited for further, seamless tiling. ")
  (SG_"New image is the same scale but seamless and irregular.  Use 'Map>Tile' for less randomness. Use 'Edit>Fill resynthesized pattern' for more randomness. ")
  (SG_"Requires separate resynthesizer plugin."))
 ;; author
 "Lloyd Konneker"
 ;; copyright notice
 "Copyright 2010 Lloyd Konneker"
 ;; date created
 "2010"
 ;; image type that the script works on
 "RGB*, GRAY*"
 ;; parameters
 SF-IMAGE    "Image"    0
 SF-DRAWABLE "Drawable" 0
 ;; Spinner is digital and linear, slider is analog but exponential
 SF-ADJUSTMENT (G_"Ratio of size of new image to source selection")
 (list 2   ; value
       0.5 ; lower
       10  ; upper
       0.5 ; step_inc
       1   ; page_inc
       SF-SPINNER)
 SF-TOGGLE (G_"Make new image edges suitable for seamless tiling") FALSE
 ;; output: [(PF_IMAGE, "new_image", "New, synthesized texture.")]
 )

(script-fu-menu-register "script-fu-render-texture"
                         "<Image>/Filters/Render")

(script-fu-menu-register "script-fu-render-texture"
                         "<Image>/Filters/Resynthesizer(scm)/")

script-fu-render-texture
))