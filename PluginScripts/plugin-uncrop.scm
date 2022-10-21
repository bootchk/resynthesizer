;; Gimp plugin "Uncrop"

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

;; Increase image/canvas size and synthesize outer band from edge of original.

;; Author:
;; lloyd konneker, lkk, itr-tert

;; Version:
;; 1.0 lkk 2009-5-15 Initial version in scheme, released to Gimp Registry.
;; 1.1 lkk 2009-9-21 Translate to python.
;; later versions, see git log

;; The effect for users:
;; widens the field of view, maintaining perspective of original
;; Should be undoable, except for loss of selection.
;; Should work on any image type, any count of layers and channels (although only active layer is affected.)
;;
;; IN: Nothing special.  The selection is immaterial but is not preserved.
;; OUT larger layer and image.  All other layers not enlarged.


;;gettext.install("resynthesizer", gimp.locale_directory, unicode=True)


(define (_ m) "stub" m)

(define (N_ m) "stub" m)


(define-with-return (uncrop-test)
  (tracing TRUE)
  (let* ((image (vector-ref (car (cdr (gimp-image-list))) 0))
         (drawable (car (gimp-image-get-active-drawable image))))
    (script-fu-uncrop
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
  (let ((type (car (gimp-image-base-type image))))
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
      (throw "set-components-active: active_list.length != image.components.length"))

    (do ((i (- active-list-length 1) (- i 1)))
        ((> 0 i))
      (gimp-image-set-component-active image
                                       (nth i components)
                                       (nth i active-list))))
  )


(define (drawable-anti-erase-selection drawable)
  (let* ((image nil)
         (ch-active-list nil)
         (components nil))
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
  ;; resize and center image by percent (converted to pixel units)
  (let* ((deltaFraction (+ (/ percentEnlarge 100) 1.0))
         (priorWidth  (car (gimp-image-width  image)))
         (priorHeight (car (gimp-image-height image)))
         (deltaWidth  (* priorWidth  deltaFraction))
         (deltaHeight (* priorHeight deltaFraction))
         (centeredOffX (/ (- deltaWidth  priorWidth)  2))
         (centeredOffY (/ (- deltaHeight priorHeight) 2)))
    (gimp-image-resize image deltaWidth deltaHeight centeredOffX centeredOffY)
    ;; if not gimp-image-resize(image, deltaWidth, deltaHeight, centeredOffX, centeredOffY):
    ;;     raise RuntimeError, "Failed resize"
    ))


(define (shrinkSelectionByPercent image percent)
  ;; shrink selection by percent (converted to pixel units)
  (let* ((deltaFraction (/ percent 100))
         ;; convert to pixel dimensions
         (priorWidth  (car (gimp-image-width  image)))
         (priorHeight (car (gimp-image-height image)))
         (deltaWidth  (* priorWidth  deltaFraction))
         (deltaHeight (* priorHeight deltaFraction))
         ;; !!! Note total shrink percentage is halved (width of band is percentage/2)
         (maxDelta (/ (max deltaWidth deltaHeight) 2)))
    (gimp-selection-shrink image maxDelta)
    ;; if not gimp-selection-shrink(image, maxDelta):
    ;;    raise RuntimeError,  "Failed shrink selection"
    ))


(define-with-return (script-fu-uncrop orgImage drawable percentEnlargeParam)
  ;; Create frisket stencil selection in a temp image to pass as source (corpus) to plugin resynthesizer,
  ;; which does the substantive work.

  (gimp-message-set-handler MESSAGE-BOX)
  (when (<> TRUE (car (gimp-item-is-layer drawable)))
    (gimp-message (_ "A layer must be active, not a channel."))
    (return))

  (gimp-image-undo-group-start orgImage)

  (let ((tempImage nil)
        (selectAllPrior nil)
        (workLayer nil))
    ;; copy original into temp for later use
    (set! tempImage (car (gimp-image-duplicate orgImage)))
    (when (null? tempImage)
      (throw "Failed duplicate image"))

    ;;
    ;; Prepare target: enlarge canvas and select the new, blank outer ring
    ;;

    ;; Save original bounds to later select outer band
    (gimp-selection-all orgImage)
    (set! selectAllPrior (car (gimp-selection-save orgImage)))
    ;; Resize image alone doesn't resize layer, so resize layer also
    (resizeImageCentered orgImage percentEnlargeParam)
    (gimp-layer-resize-to-image-size drawable)
    (gimp-image-select-item orgImage CHANNEL-OP-REPLACE selectAllPrior)
    ;; select outer band, the new blank canvas.
    (gimp-selection-invert orgImage)
    ;; Assert target image is ready.

    ;;
    ;; Prepare source (corpus) layer, a band at edge of original, in a dupe.
    ;; Note the width of corpus band is same as width of enlargement band.
    ;;

    ;; Working with the original size.
    ;; Could be alpha channel transparency
    (set! workLayer (car (gimp-image-get-active-layer tempImage)))
    (when (null? workLayer)
      (throw "Failed get active layer"))
    ;; Select outer band:  select all, shrink
    (gimp-selection-all tempImage)
    (shrinkSelectionByPercent tempImage percentEnlargeParam)
    (gimp-selection-invert tempImage)  ; invert interior selection into a frisket

    (plug-in-resynthesizer
     RUN-NONINTERACTIVE ; run-mode
     orgImage           ; image
     drawable           ; drawable
     0 0                ; vtile htile
     5                  ; use-context ; 5 means inside out direction
     workLayer          ; corpus
     -1                 ; inmask
     -1                 ; outmask
     0.0                ; map-weight
     0.117              ; autism
     16                 ; neighbourhood
     500)               ; trys

    (when (= TRUE (car (gimp-drawable-has-alpha drawable)))
      (drawable-anti-erase-selection drawable))

    ;; Clean up.
    ;; Any errors now are moot.
    (gimp-selection-none orgImage)
    (gimp-image-remove-channel orgImage selectAllPrior)
    (gimp-image-set-active-layer orgImage drawable)
    (gimp-displays-flush)
    (gimp-image-delete tempImage)
    (gimp-image-undo-group-end orgImage)
    ))

(script-fu-register
 ;; func name
 "script-fu-uncrop"
 ;; menu label
 (N_ "Uncrop(scm)...")
 ;; description
 (string-append
  (N_ "Enlarge image by synthesizing a border that matches the edge, maintaining perspective.  Works best for small enlargement of natural edges. Undo a Crop instead, if possible! ")
  (N_ "Requires separate resynthesizer plugin."))
 ;; author
 "Lloyd Konneker"
 ;; copyright notice
 "Copyright 2009 Lloyd Konneker"
 ;; date created
 "2009"
 ;; image type that the script works on
 "RGB*, GRAY*"
 ;; parameters
 SF-IMAGE    "Image"    0
 SF-DRAWABLE "Drawable" 0
 SF-ADJUSTMENT (_ "Percent enlargement")
 (list 10   ; value
       0    ; lower
       100  ; upper
       1    ; step_inc
       10   ; page_inc
       6    ; digits
       SF-SLIDER))

(script-fu-menu-register "script-fu-uncrop"
                         "<Image>/Filters/Enhance")

(script-fu-menu-register "script-fu-uncrop"
                         "<Image>/Filters/Resynthesizer(scm)/")
