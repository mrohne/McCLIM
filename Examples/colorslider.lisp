;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-DEMO; Base: 10; Lowercase: Yes -*-

;; $fiHeader: calcuator.lisp,v 1.0 22/08/200 $

;;;  (c) copyright 2000 by 
;;;           Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the 
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;; Boston, MA  02111-1307  USA.

(in-package :clim-internals)

;; example gadget definition
(defclass slider-test-pane (standard-gadget) ())

(defmethod repaint-sheet ((pane slider-test-pane) region)
  (declare (ignore region))
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* (sheet-region pane))
    (display-gadget-background pane (gadget-current-color pane) 0 0 (- x2 x1) (- y2 y1))))

(defmethod handle-event ((pane slider-test-pane) (event window-repaint-event))
  (declare (ignorable event))
  (dispatch-repaint pane (sheet-region pane)))

(in-package :clim-demo)

;; slider callback and macro

(defvar rgb '(0 0 0))

;; Macro defining all the slider-call-back

(defmacro define-slider-callback (name position)
  `(defun ,(make-symbol name) (gadget value)
     (let ((colored (third (sheet-siblings gadget))))
       (setf ,(case position (1 `(car rgb)) (2 `(cadr rgb)) (3 `(caddr rgb)))
	     (/ value 10000)
	     (clim-internals::gadget-current-color colored)
	       (apply #'clim-internals::make-named-color "our-color"
		      (mapcar #'(lambda (color) (coerce color 'single-float)) rgb))))))

(defvar callback-red (define-slider-callback "SLIDER-R" 1))
(defvar callback-green (define-slider-callback "SLIDER-G" 2))
(defvar callback-blue (define-slider-callback "SLIDER-B" 3))

;; test functions

(defun colorslider ()
  (loop for port in climi::*all-ports*
      do (destroy-port port))
  (setq climi::*all-ports* nil)
  (setq frame (make-application-frame 'colorslider))
  (setq fm (frame-manager frame))
  (setq port (climi::frame-manager-port fm))
  (setq pane (first (frame-panes frame)))
  (setq medium (sheet-medium pane))
  (setq graft (graft frame))
  (setq vbox (climi::frame-pane frame))
  (run-frame-top-level frame))

(defmethod slidertest-frame-top-level ((frame application-frame)
				       &key (command-parser 'command-line-command-parser)
				       (command-unparser 'command-line-command-unparser)
				       (partial-command-parser
					'command-line-read-remaining-arguments-for-partial-command)
				       (prompt "Command: "))
  (declare (ignore command-parser command-unparser partial-command-parser prompt))
  (loop (event-read (climi::frame-pane frame))))

(define-application-frame colorslider () ()
  (:panes
   (text    :text-field
	    :value "Pick a color"
	    :height 50
            :width 100)
   (slider-r  :slider
	      :drag-callback callback-red
	      :value-changed-callback callback-red
	      :min-value 0
	      :max-value 9999
	      :value 0
	      :show-value-p t
	      ;;:orientation :horizontal
	      :width 120)
   (slider-g  :slider
	      :drag-callback callback-green
	      :value-changed-callback callback-green
	      :min-value 0
	      :max-value 9999
	      :value 0
	      :width 120)
   (slider-b  :slider
	      :drag-callback callback-blue
	      :value-changed-callback callback-blue
	      :min-value 0
	      :max-value 9999
	      :value 0
	      :width 120)
   (colored :slider-test
            :normal +black+
            :width 200 :height 90))
  (:layouts
   (default (vertically ()
                        text
                        (horizontally ()
                                      slider-r
                                      slider-g
                                      slider-b
                                      colored))))
  (:top-level (slidertest-frame-top-level . nil)))
