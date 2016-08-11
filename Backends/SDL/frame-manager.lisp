;;; -*- Mode: Lisp; Package: CLIM-SDL -*-

;;; (c) 2005 Christophe Rhodes (c.rhodes@gold.ac.uk)

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

(in-package :clim-sdl)

(defclass popup-sheet-pane (climi::composite-pane)
  ()
  (:documentation "Popup sheet pane"))

(defmethod compose-space ((pane popup-sheet-pane) &key width height)
  (declare (ignore width height))
  (make-space-requirement))

(defmethod allocate-space ((pane popup-sheet-pane) width height)
  (declare (ignore width height))
  (values))

(defclass menu-popup-sheet-pane (popup-sheet-pane)
  ())

(climi::define-abstract-pane-mapping 
    'climi::menu-unmanaged-top-level-sheet-pane
    'menu-popup-sheet-pane)

(defclass sdl-frame-manager (frame-manager)
  ())

;;; FIXME: maybe this or something like it belongs in CLIMI?
(defun generic-concrete-pane-class (name)
  (let* ((concrete-name (get name 'climi::concrete-pane-class-name))
         (maybe-name (concatenate 'string (symbol-name name) 
                                  (symbol-name '#:-pane)))
         (maybe-symbol (find-symbol maybe-name :climi))
         (maybe-class (find-class maybe-symbol nil)))
    (or maybe-class
        (find-class concrete-name nil)
        (find-class (if (keywordp name) 
                        (intern (symbol-name name) :climi)
                        name) nil))))

(defmethod make-pane-1
    ((fm sdl-frame-manager) (frame application-frame) type &rest initargs)
  (apply #'make-instance (generic-concrete-pane-class type)
	 :frame frame :manager fm :port (port frame)
	 initargs))

(in-package :clim-internals)

;;; must override core method in order to control the parent

(defmethod adopt-frame :before ((fm clim-sdl::sdl-frame-manager) (frame menu-frame))
  ;; Temporary kludge.
  (with-slots (left top) frame
    (multiple-value-bind (x y) 
	(pointer-position (port-pointer (port fm)))
      (when (null left) (setf left (+ x 10)))
      (when (null top) (setf top y)))))

(defmethod adopt-frame :after ((fm clim-sdl::sdl-frame-manager) (frame menu-frame))
  (with-slots (top-level-sheet) frame
    (setf (pointer-sheet (port-pointer (port fm))) top-level-sheet)))

(defmethod adopt-frame ((fm clim-sdl::sdl-frame-manager) (frame menu-frame))
  (setf (slot-value fm 'frames) (cons frame (slot-value fm 'frames)))
  (setf (slot-value frame 'manager) fm)
  (setf (slot-value frame 'state) :disabled)
  (setf (slot-value frame 'port) (port fm))
  (setf (slot-value frame 'graft) (find-graft :port (port frame)))
  (setf (slot-value frame 'top-level-sheet) (make-pane-1 fm *application-frame*
							 'menu-unmanaged-top-level-sheet-pane
							 :name 'top-level-sheet))
  (multiple-value-bind (width height left top) 
      (frame-geometry* frame)
    (layout-frame frame width height)
    (multiple-value-bind (left top)
	(untransform-position (sheet-delta-transformation (frame-top-level-sheet *application-frame*) nil)
			      left top)
      (move-sheet (frame-top-level-sheet frame) left top)))
  (sheet-adopt-child (frame-top-level-sheet frame) (frame-panes frame))
  (sheet-adopt-child (frame-top-level-sheet *application-frame*) (frame-top-level-sheet frame)))

(defmethod disown-frame ((fm frame-manager) (frame menu-frame))
  (setf (slot-value fm 'frames) (remove frame (slot-value fm 'frames)))
  (sheet-disown-child (frame-top-level-sheet *application-frame*) (frame-top-level-sheet frame))
  (setf (frame-manager frame) nil))
