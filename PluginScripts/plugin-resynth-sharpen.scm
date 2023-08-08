;; Gimp plugin "Sharpen by resynthesis"

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
;;  2022 itr-tert
;;  Based on plugin-resynth-sharpen.py 2010 lloyd konneker (bootch at nc.rr.com)
;;   Based on smart_enlarge.scm 2000 by Paul Harrison.


(define script-fu-sharpen-resynthesized (let
()  ; indent keeper

(define (gettext msgid)
  (catch msgid
	 (car (plug-in-resynthesizer-gettext msgid))))
(define (N_ m) m)  ; like gettext-noop
(define (G_ m) (gettext m))
(define (S_ m) (string-append m "​"))  ; Add zero-width spaces to suppress translation.
(define (SG_ m) (S_ (G_ m)))


(define-with-return
  (script-fu-sharpen-resynthesized image drawable scale-factor)

  ;; Algorithm:
  ;;
  ;; Resynthesize with:
  ;;   corpus = smaller image
  ;;   in map = smaller image but scaled up and down to blur
  ;;   out map = original image
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

    (set! width  (car (gimp-drawable-width  drawable)))
    (set! height (car (gimp-drawable-height drawable)))

    ;; scale input image down, for corpus map
    (gimp-image-scale temp-image2
                      (/ width  scale-factor)
                      (/ height scale-factor))
    ;; scale input image way down, then back up to, to blur, for corpus
    ;; Final size is same size as corpus map.
    (gimp-image-scale temp-image1
                      (/ width  (* 2 scale-factor))
                      (/ height (* 2 scale-factor)))
    (gimp-image-scale temp-image1
                      (/ width  scale-factor)
                      (/ height scale-factor))

    ;; Resynthesize to restore details.
    ;; Note there should not be a selection. TODO
    (plug-in-resynthesizer
     RUN-NONINTERACTIVE
     image
     drawable
     0  0  0
     temp-layer2  ; corpus is smaller original
     temp-layer1  ; input map is blurred smaller original
     drawable     ; output map is original itself
     1.0  0.117  8  500)

    (gimp-image-delete temp-image1)
    (gimp-image-delete temp-image2)
    (gimp-image-undo-group-end image)
    ))


(script-fu-register
 ;; func name
 "script-fu-sharpen-resynthesized"
 ;; menu label
 (SG_"_Sharpen by synthesis(scm)...")
 ;; description
 (string-append
  (SG_"Sharpen image by synthesis.")
  (SG_"Requires separate resynthesizer plugin."))
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
 SF-ADJUSTMENT (G_"Sharpening")  ; sharpen_factor
  (list 2           ; value
        1           ; lower
        32          ; upper
        0.1         ; step_inc
        1           ; page_inc
        2           ; digits
        SF-SPINNER) ; type
  )

(script-fu-menu-register "script-fu-sharpen-resynthesized"
                         "<Image>/Filters/Enhance")

(script-fu-menu-register "script-fu-sharpen-resynthesized"
			 (string-append "<Image>/Filters/"
					(SG_"Resynthesizer(scm)")))

script-fu-sharpen-resynthesized
))
