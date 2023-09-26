#!/usr/bin/env gimp-script-fu-interpreter-3.0

;; Gimp plugin.
;; Transfer style (color and surface texture) from a source image to the active, target image.
;; Requires resynthesizer plug-in.

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
;; lloyd konneker, lkk, itr-tert
;;
;; Version:
;; 1.0 lkk 2010-07-15 Initial version.  Released to Gimp Registry.
;; 1.1 lkk 2010-08-01 Unreleased
;; 1.2 lkk 2010-08-10
;; later versions, see git log

;; Change log:
;; _________________
;; 1.1
;;   Bug: Fixed test of mode variable, since it is a string, needs explicit test for == 1
;;   Bug: Added remove Selection Mask copy channel in make_grayscale_map
;; 1.2
;;   Changes for new resynthesizer: no need to synchronize, remove alphas
;;   Fixed improper adjustment of contrast of source: only adjust source map.
;; later versions, see git log

;; TODO
;; a quality setting that changes the parameters to resynth


;; Users Guide
;; ___________
;;
;; What this plugin does:
;;
;; Transfers artistic style from one image to another.  Often the source is an artistic image and the target is a realistic, photo image.  But you can also transfer between artistic images or between realistic images.
;;
;; An artist might say this plugin "renders in the media and style from another image."  A computer user might say it "renders in the theme of another image."
;;
;; Transferring style means transferring small scale features (color and texture) to an image while retaining large scale features (objects.)
;;
;; Map can mean "transfer and transform".  This plugin gives limited control of the transform.  That is, colors are usually mapped to similar colors (hues.)  This plugin is not intended to do "false color" (but it might have that effect.)
;;
;; Style can mean "color and surface."  Texture mapping usually means just surface (pattern of brightness, e.g. a weave or grain.)  This plugin can transfer both color and surface.
;;
;; This plugin has more effect than just an overlay or screen or a map.  A screen usually applies a texture uniformly across an image.  This plugin transfers style in patches.  The style in a region can come from any patch of the source, or be synthesized (mixed) from many patches of the source.
;;
;; The transfer is not exactly a copy, again because of optional synthesis or mixing.
;;
;; About the selection:
;;
;; Usually you transfer between separate images, the target and source images.  You can make a selection in either image, or both.  If there is no selection, the plugin uses the entire layer.
;;
;; The target is the active LAYER and you can choose the source LAYER.  Note that the plugin doesn't use everything visible in an image, just one layer.
;;
;; SPECIAL CASE: If the target and source layers are in the same image, the source style comes from the inverse of the selection in the source layer.  Similarly, if the target and source layers are the same layer, the target is the selection and the style comes from the inverse of the selection, i.e. outside the selection.  In this case, the effect is little if there is no difference in texture between the inside and outside of the selection, or a distort, if there is a difference.
;;
;; About the settings:
;;
;; "Percent transfer:" how much style to transfer.  Less transfer means the effect retains the large scale objects of the original, but gives the image a grainy surface.  More transfer means the effect leaves only a ghost of the large scale objects, and almost fully copies the style image (with less synthesis or mixing.)
;;
;; "Map by:" whether color affects the style transfer, when both target and source are in color.  If you choose "color and brightness", style colors are more apt to be transferred to areas with same colors.  However, it is still possible that colors are radically transformed, if the surface (brightness pattern) is a better match.  If you choose "brightness only", style colors are more apt to be radically transformed.
;;
;; This setting has less effect if there are no color matches between source and target (e.g. one is all red and the other is all green) or if the target image is GRAY.  This setting has NO effect if the source image or both images are GRAY.
;;
;; About image modes:
;;
;; You can transfer style between any combination of RGB and GRAY images.   The plugin changes the mode of the target to the mode of the source as necessary.
;;
;; Why this plugin:
;;
;; This plugin is a front-end to the separate resynthesizer plugin.  This plugin simplifies using the resynthesizer plugin.  It automates many steps.  It hides several complexities of the resynthesizer plugin:  selection, modes, alpha channels, and settings.


;; Programming notes:
;; _________________
;;
;; IN: The active image and layer.
;;     The selection in the active image.
;;     The selection in any layers chosen for source.
;; OUT: The active image, altered.  The source is unaltered.
;;   Target mode can be altered, but with the implied consent of the user.
;;
;; The print stmts go to the console, info to advanced users and debuggers.
;;
;; This plugin is mostly about UI and simplifications for user (the engine does the image processing):
;; making maps automatically
;; synchronization of alphas (note the new resynthesizer ignores alphas.)
;; synchronization of modes
;; abstracting the settings
;; contrast adjustment


(define script-fu-map-style (let
()  ; indent keeper

(define debug #f)  ; #t if you want to display and retain working, temporary images


(define (gettext msgid)
  (catch msgid
	 (car (plug-in-resynthesizer-gettext msgid))))
(define (N_ m) m)  ; like gettext-noop
(define (G_ m) (gettext m))
(define (S_ m) (string-append m "​"))  ; Add zero-width spaces to suppress translation.
(define (SG_ m) (S_ (G_ m)))


(define (display-debug-image image)
  (when debug
    (catch "do nothing when error"
           ;; if run-mode not interactive, Gimp throws
           (gimp-display-new image)
           (gimp-displays-flush))))


(define-with-return (make-grayscale-map image drawable)
  ;; Make a grayscale copy for a map.
  ;; Maps must be same size as their parent image.
  ;; If image is already grayscale, return it without copying.
  ;; Maps don't need a selection, since the resynthesizer looks at parent drawables for the selection.

  ; v3 PDB name change
  (when (= GRAY (car (gimp-image-get-base-type image)))
    (return (list image drawable)))

  ;; Save selection, copy entire image, and restore
  (let ((original-selection (car (gimp-selection-save image)))
        (temp-image nil)
        (temp-drawable nil)
        )
    (gimp-selection-all image)  ; copy requires selection
    ; to the clipboard, one drawable
    (gimp-edit-copy 1 (make-vector 1 drawable))

    ;; restore selection in image
    (gimp-image-select-item image CHANNEL-OP-REPLACE original-selection)
    ;; cleanup the copied selection mask
    (gimp-image-remove-channel image original-selection)
    ;; !!! Note remove-channel not drawable-delete

    ;; Make a copy, greyscale
    (set! temp-image (car (gimp-edit-paste-as-new-image)))
    (gimp-image-convert-grayscale temp-image)
    (display-debug-image temp-image)
    ; The image could have many drawables.
    ; One was chosen by the user when this script started.
    ; But since v3 Gimp supports many active (selected) layers or channels.
    ; And the active drawable may not be the one the plugin user chose.
    ; We want the drawable in the copy that corresponds to one chosen.
    ; Here we punt, assume the user chose a layer (versus channel)
    ; and it was the first.
    ; FIXME a better way of doing this
    (set! temp-drawable
      (vector-ref
        ; car is a count, cadr is a vector
        (cadr (gimp-image-get-layers temp-image))
        0))
    (return (list temp-image temp-drawable))))


(define (synchronize-modes target-image source-image)
  ;; User-friendliness:
  ;; If mode of target is not equal to mode of source source, change modes.
  ;; Resynthesizer requires target and source to be same mode.
  ;; Assert target is RGB or GRAY (since is precondition of plugin.)
  ;; UI decision: make this quiet, presume user intends mode change.
  ;; But don't permanently change mode of source.
  ;; Always upgrade GRAY to RGB, not downgrade RGB to GRAY.
  (let ((target-mode (car (gimp-image-get-base-type target-image)))
        (source-mode (car (gimp-image-get-base-type source-image))))
    (when (<> target-mode source-mode)
      (when debug
        (display "Map style: converted mode\n."))
      (if (= target-mode GRAY)
          (begin (gimp-image-convert-rgb target-image))
          (begin
            ;; target is RGB and source is GRAY
            ;; Assert only convert a copy of source,
            ;; user NEVER intends original source be altered.
            (gimp-image-convert-rgb source-image))))))


(define (copy-selection-to-image drawable)
  ;; If image has a selection, copy selection to new image, and prepare it for resynthesizer,
  ;; else return a copy of the entire source image.
  ;; This is called for the source image, where it helps performance to reduce size and flatten.
  (let ((image (car (gimp-item-get-image drawable)))
        (image-copy nil)
        (layer-copy nil))
    ;; copy selection or whole image to clipboard, from one drawable
    (gimp-edit-copy 1 (make-vector 1 drawable))
    (set! image-copy (car (gimp-edit-paste-as-new-image)))
    ;; Activate layer, and remove alpha channel
    (gimp-image-flatten image-copy)
    (set! layer-copy (car (gimp-image-get-active-layer image-copy)))
    ;; In earlier version, futzed with selection to deal with transparencey
    (display-debug-image image-copy)
    (list image-copy layer-copy)))


(define (synchronize-contrast ref-drawable source-drawable percent-transfer)
  ;; Adjust contrast of source, to match target.
  ;; Adjustment depends inversely on percent-transfer.
  ;; Very crude histogram matching.

  ; v3 values are floats in range [0,1.0]
  ; v3 procedure names changed

  ;; histogram upper half: typical mean is 0.75. Mean approaching 1.0 means high contrast.
  (let* ((ref-mean    (car (gimp-drawable-histogram ref-drawable    HISTOGRAM-VALUE 0.5 1.0)))
         (source-mean (car (gimp-drawable-histogram source-drawable HISTOGRAM-VALUE 0.5 1.0)))
         ;; Adjust contrast of source.
         ;; Inversely proportional to percent transfer.
         ;; 2.5 is from experimentation with gimp-brightness-contrast which seems linear in its effect.
         (contrast-control (* (- ref-mean source-mean) 2.5 (- 1 (/ percent-transfer 100)))))
    (when (and debug (> ref-mean source-mean))
      (display "synchronize-contrast: target has more contrast than source\n"))
    ;; clamp to valid range (above formula is lazy, ad hoc)
    (cond ((> -1 contrast-control) (set! contrast-control -1))
          ((<  1 contrast-control) (set! contrast-control  1)))

    (gimp-drawable-brightness-contrast source-drawable 0 contrast-control)

    (when debug
      ;; For experimentation, print new values
      (set! source-mean (car (gimp-drawable-histogram source-drawable HISTOGRAM-VALUE 128 255)))
      (display (list "Map style: Source contrast changed by " contrast-control))
      (display (list "Map style: Target and source upper half histogram means" ref-mean source-mean))
    )))


(define (calculate-map-weight percent-transfer)
  ;; This is a GUI design discussion.
  ;; Transform percent-transfer to map-weight parameter to resynthesizer.
  ;; For resynthesizer:
  ;; map weight 0 means copy source to target, meaning ALL style.
  ;; map weight 0.5 means just a grainy transfer of style (as little as is possible.)
  ;; Transform from a linear percent GUI, because user more comfortable than with a ratio [.5, 0]
  ;; which is backwards to the usual *less on the left*.
  ;; By experiment, a sinusoid gives good results for linearizing the non-linear map-weight control.
  (/ (acos (+ -1 (* (/ percent-transfer 100) 2)))
     (* 2 3.1415926535)))


(define-with-return (script-fu-map-style
                     image drawable source-drawable percent-transfer map-mode)
  ;; Main body of plugin to transfer style from one image to another.
  ;;
  ;; !!! Note map-mode is type string, "if map-mode:" will not work.

  (gimp-image-undo-group-start image)
  (gimp-message-set-handler MESSAGE-BOX)

  ;; Get image of source drawable
  (let* ((source-image (car (gimp-item-get-image source-drawable)))
         ;;
         ;; User-friendliness.
         ;; Note the drawable chooser widget in Pygimp does not allow us to prefilter INDEXED mode.
         ;; So check here and give a warning.
         ;;
         ;; These are the originals base types, and this plugin might change the base types
         (original-source-base-type (car (gimp-image-get-base-type source-image)))
         (original-target-base-type (car (gimp-image-get-base-type image)))
         (is-source-copy nil)
         (temp-result nil)
         (target-map nil)
         (target-map-image nil)
         (target-map-drawable nil)
         (source-map nil)
         (source-map-image nil)
         (source-map-drawable nil)
         (map-weight nil)
         )
    (when (= INDEXED original-source-base-type)
      (gimp-message (G_"The style source cannot be of mode INDEXED"))
      (return))

    (if (and (= image source-image)
             (= drawable source-drawable))
        (set! is-source-copy #f)
        ;;
        ;; If source is same as target,
        ;; then the old resynthesizer required a selection (engine used inverse selection for corpus).
        ;; New resynthesizer doesn't need a selection.
        ;; If source same as target, effect is similar to a blur.
        ;;
        ;; assert modes and alphas are same (since they are same layer!)
        (begin  ; target layer is not the source layer (source could be a copy of target, but effect is none)
          ;; Copy source always, for performance, and for possible mode change.
          (set! is-source-copy #t)
          (set! temp-result (copy-selection-to-image source-drawable))
          (set! source-image    (nth 0 temp-result))
          (set! source-drawable (nth 1 temp-result))
          ;; Futz with modes if necessary.
          (synchronize-modes image source-image)
          ))

    ;;
    ;; TODO For performance, if there is a selection in target, it would be better to copy
    ;; selection to a new layer, and later merge it back (since resynthesizer engine reads
    ;; entire target into memory.  Low priority since rarely does user make a selection in target.
    ;;

    ;;
    ;; !!! Note this plugin always sends maps to the resynthesizer,
    ;; and the "percent transfer" setting is always effective.
    ;; However, maps may not be separate,copied images unless converted to grayscale.
    ;;

    ;; Copy and reduce maps to grayscale: at the option of the user
    ;; !!! Or if the target was GRAY and source is RGB, in which case maps give a better result.
    ;; Note that if the target was GRAY, we already upgraded it to RGB.
    (if (or (= map-mode 1)
            (and (= original-source-base-type RGB)
                 (= original-target-base-type GRAY)))
        (begin
          ;; print "Map style: source mode: ", original-source-base-type, " target mode: ", original-target-base-type
          ;; print "Map style: Converting maps to grayscale"
          ;; Convert mode, but in new temp image and drawable
          (set! temp-result (make-grayscale-map image drawable))
          (set! target-map-image    (nth 0 temp-result))
          (set! target-map-drawable (nth 1 temp-result))
          (set! temp-result (make-grayscale-map source-image source-drawable))
          (set! source-map-image    (nth 0 temp-result))
          (set! source-map-drawable (nth 1 temp-result))

          (set! target-map target-map-drawable)
          (set! source-map source-map-drawable)
          ;; later, delete temp images

          ;; User control: adjust contrast of source-map as a function of percent transfer
          ;; Hard to explain why, but experimentation shows result more like user expectation.
          ;; TODO This could be improved.
          ;; !!! Don't change the original source, only a temporary map we created
          (synchronize-contrast drawable source-map percent-transfer)
          )
        (begin
          ;; !!! Maps ARE the target and source, not copies
          (set! source-map source-drawable)
          (set! target-map drawable)))

    (set! map-weight (calculate-map-weight percent-transfer))

    ;; !!! This is for version of resynthesizer, with an uninverted selection
    (plug-in-resynthesizer
      drawable           ; drawable
      1 1                ; vtile htile 1 since it reduces artifacts around edge
      ;; use-context ; 1 since there might be a selection and context (outside target).
      1
      source-drawable    ; corpus
      source-map         ; inmask
      target-map         ; outmask
      map-weight         ; map-weight I linearize since easier on users than an exponential
      0.117              ; autism
      9                  ; neighbourhood ; 9 (a 3x3 patch) and
      200)               ; trys          ; 200 for speed

    ;; Clean up.
    ;; Delete working images: separate map images and copy of source image
    (unless debug
      (when (= map-mode 1)  ; if made working map images
        (gimp-image-delete target-map-image)
        (gimp-image-delete source-map-image)
        )
      (when is-source-copy  ; if created a copy earlier
        (gimp-image-delete source-image)))
    (gimp-image-undo-group-end image)
    (gimp-displays-flush)))


(define (test)
  (define (get-image)
    (vector-ref (car (cdr (gimp-image-list))) 0))
  (define (get-second-image)
    (vector-ref (car (cdr (gimp-image-list))) 1))
  (define (get-drawable image)
    (when (null? image)
      (set! image (get-image)))
    (car (gimp-image-get-active-drawable image)))
  (script-fu-map-style (get-image)
                       (get-drawable nil)
                       (get-drawable (get-second-image))
                       10
                       1))

(script-fu-register
 ;; func name
 "script-fu-map-style"
 ;; menu label
 (SG_"_Style(scm)...")
 ;; description
 (string-append
  (SG_"Transfer style (color and surface) from a chosen source to the active layer. ")
  (SG_"Transforms image using art media and style from another image. Maps or synthesizes texture or theme from one image onto another. ")
  (SG_"Requires separate resynthesizer plugin."))
 ;; author
 "Lloyd Konneker (bootch nc.rr.com)"
 ;; copyright notice
 "Copyright 2010 Lloyd Konneker"
 ;; date created
 "2010"
 ;; image type that the script works on
 "RGB*, GRAY*"
 ;; parameters
 SF-IMAGE    "Image"    0
 SF-DRAWABLE "Drawable" 0
 SF-DRAWABLE   (G_"Source of style") -1
 ; Integer valued percent from 10 to 90
 ; 0 and 100 make no sense
 SF-ADJUSTMENT (G_"Percent transfer")
 (list 10    ; default value
       10   ; lower
       90   ; upper
       10   ; step_inc
       20   ; page_inc
       0    ; digits 0 means integer valued
       SF-SLIDER)
 SF-OPTION (G_"Map by")
 (list (G_"Color and brightness")
       (G_"Brightness only")))

(script-fu-menu-register "script-fu-map-style"
                         "<Image>/Filters/Map")

(script-fu-menu-register "script-fu-map-style"
			 (string-append "<Image>/Filters/"
					(SG_"Resynthesizer(scm)")))

script-fu-map-style
))
