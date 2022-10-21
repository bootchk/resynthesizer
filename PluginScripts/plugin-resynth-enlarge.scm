;; Gimp plugin "Enlarge and resynthesize"

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

;; Author:
;; itr-tert
;;  Based on plugin-resynth-enlarge.py 2010 lloyd konneker
;;   Based on smart_enlarge.scm 2000 by Paul Harrison.


;; gettext.install("resynthesizer", gimp.locale_directory, unicode=True)

(define (_ m) "stub" m)

(define (N_ m) "stub" m)

(define-with-return
  (script-fu-enlarge-resynthesized image drawable scale-factor)
  ;; Algorithm:
  ;;
  ;; Scale image up.
  ;; Resynthesize with:
  ;;   corpus = original size image
  ;;   in map = original size image but scaled up and down to blur
  ;;   out map = scaled up image
  ;;
  ;; This restores the detail that scaling up looses.
  ;; It maintains the aspect ratio of all image features.
  ;;
  ;; Unlike the original smart-enlarge.scm, this alters the original image.
  ;;
  ;; original did not accept an alpha channel

  (gimp-message-set-handler MESSAGE-BOX)

  (let ((temp-image1 nil)
	(temp-image2 nil)
	(temp-layer1 nil)
	(temp-layer2 nil)
	(width nil)
	(height nil))
    (gimp-image-undo-group-start image)

    (set! temp-image1 (car (gimp-image-duplicate image)))  ; duplicate for in map
    (set! temp-image2 (car (gimp-image-duplicate image)))  ; duplicate for corpus
    (when (or (null? temp-image1)
	      (null? temp-image2))
      (gimp-message "Failed duplicate image")
      (return))

    (set! temp-layer1 (car (gimp-image-get-active-layer temp-image1)))
    (set! temp-layer2 (car (gimp-image-get-active-layer temp-image2)))
    (when (or (null? temp-layer1)
	      (null? temp-layer2))
      (gimp-message "Failed get active layer")
      (return))

    ;; scale input map down and back, to blur
    (set! width  (car (gimp-drawable-width  drawable)))
    (set! height (car (gimp-drawable-height drawable)))

    (gimp-image-scale temp-image1 (/ width scale-factor) (/ height scale-factor))
    (gimp-image-scale temp-image1    width                  height              )

    ;; scale up the image
    (gimp-image-scale image       (* width scale-factor) (* height scale-factor))

    ;; Resynthesize to restore details.
    ;; Note there should not be a selection. TODO
    (plug-in-resynthesizer
     RUN-NONINTERACTIVE
     image
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

(script-fu-register
 ;; func name
 "script-fu-enlarge-resynthesized"
 ;; menu label
 (N_ "Enlarge & sharpen(scm)...")
 ;; description
 (string-append
  (N_ "Enlarge image and synthesize to sharpen.")
  (N_ "Requires separate resynthesizer plugin"))
 ;; author
 "Lloyd Konneker"
 ;; copyright notice
 "Copyright 2000 Paul Harrison, 2010 Lloyd Konneker"
 ;; date created
 "2010"
 ;; image type that the script works on
 "RGB*, GRAY*"
 ;; parameters
 SF-IMAGE "Image" 0
 SF-DRAWABLE "Drawable" 0
 SF-ADJUSTMENT (_ "Scale by:")  ; scale_factor
 (list 2           ; value
       1           ; lower
       32          ; upper
       0.1         ; step_inc
       1           ; page_inc
       2           ; digits
       SF-SPINNER) ; type
 )

(script-fu-menu-register "script-fu-enlarge-resynthesized"
			 "<Image>/Filters/Enhance")

(script-fu-menu-register "script-fu-enlarge-resynthesized"
                         "<Image>/Filters/Resynthesizer(scm)/")
