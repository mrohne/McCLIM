;;; -*- Mode: Lisp; Package: CLIM-SDL; -*-

;;;  (c) copyright 2005 by Christophe Rhodes (c.rhodes@gold.ac.uk)
;;;  (c) copyright 2014 by Robert Strandh (robert.strandh@gmail.com)

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

(defclass sdl-pointer (standard-pointer)
  ((cursor :accessor pointer-cursor :initform :upper-left)
   (x :initform 0)
   (y :initform 0)))

(defclass sdl-port (basic-port)
  ((id)
   (pointer :accessor port-pointer :initform (make-instance 'sdl-pointer))
   (window :initform nil :accessor sdl-port-window)))

(defun parse-sdl-server-path (path)
  path)

;;; FIXME: if :port-type and :server-path-parser aren't CLIM-specified
;;; keywords, they should be altered to be in some mcclim-internal
;;; package instead.
(setf (get :sdl :port-type) 'sdl-port)
(setf (get :sdl :server-path-parser) 'parse-sdl-server-path)

(defmethod initialize-instance :after ((port sdl-port) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value port 'id) (sdl2:init :everything))
  (push (make-instance 'sdl-frame-manager :port port)
	(slot-value port 'climi::frame-managers))
  (setf (slot-value port 'pointer)
	(make-instance 'sdl-pointer :port port))
  )

(defmethod print-object ((object sdl-port) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream "~S ~S" :id (slot-value object 'id))))

(defmethod port-set-mirror-region ((port sdl-port) mirror mirror-region)
  ())
                                   
(defmethod port-set-mirror-transformation
    ((port sdl-port) mirror mirror-transformation)
  ())

(defmethod realize-mirror ((port sdl-port) (sheet mirrored-sheet-mixin))
  nil)

(defmethod destroy-mirror ((port sdl-port) (sheet mirrored-sheet-mixin))
  ())

(defmethod mirror-transformation ((port sdl-port) mirror)
  ())


(defmethod port-set-sheet-region ((port sdl-port) (graft graft) region)
  ())

;; these don't exist
;;;(defmethod port-set-sheet-transformation
;;;    ((port sdl-port) (graft graft) transformation)
;;;  ())
;;;
;;;(defmethod port-set-sheet-transformation
;;;    ((port sdl-port) (sheet mirrored-sheet-mixin) transformation)
;;;  ())

(defmethod port-set-sheet-region
    ((port sdl-port) (sheet mirrored-sheet-mixin) region)
  (declare (ignore region))
  nil)

(defmethod port-enable-sheet ((port sdl-port) (mirror mirrored-sheet-mixin))
  nil)

(defmethod port-disable-sheet ((port sdl-port) (mirror mirrored-sheet-mixin))
  nil)

(defmethod destroy-port :before ((port sdl-port))
  nil)

(defmethod port-motion-hints ((port sdl-port) (mirror mirrored-sheet-mixin))
  nil)

(defmethod (setf port-motion-hints)
    (value (port sdl-port) (sheet mirrored-sheet-mixin))
  value)

(defmethod get-next-event
    ((port sdl-port) &key wait-function (timeout nil))
  (declare (ignore wait-function timeout))
  nil)

(defmethod make-graft
    ((port sdl-port) &key (orientation :default) (units :device))
  (make-instance 'sdl-graft
                 :port port :mirror (gensym)
                 :orientation orientation :units units))

(defmethod make-medium ((port sdl-port) sheet)
  (make-instance 'sdl-medium :sheet sheet))

(defmethod text-style-mapping
    ((port sdl-port) text-style &optional character-set)
  (declare (ignore text-style character-set))
  nil)

(defmethod (setf text-style-mapping)
    (font-name (port sdl-port)
    (text-style text-style) &optional character-set)
  (declare (ignore font-name text-style character-set))
  nil)

;;; The generic function PORT-CHARACTER-WIDTH might be intended to be
;;; common for all ports, but in fact, that symbol is in the CLIM-SDL
;;; package, so it is only defined here, and nowhere used. 
(defgeneric port-character-width (port text-style char))

(defmethod port-character-width ((port sdl-port) text-style char)
  (declare (ignore text-style char))
  nil)

;;; The generic function PORT-STRING-WIDTH might be intended to be
;;; common for all ports, but in fact, that symbol is in the CLIM-SDL
;;; package, so it is only defined here, and nowhere used. 
(defgeneric port-string-width (port text-style string &key start end))

(defmethod port-string-width ((port sdl-port) text-style string &key (start 0) end)
  (declare (ignore text-style string start end))
  nil)

(defmethod port-mirror-width ((port sdl-port) sheet)
  (declare (ignore sheet))
  nil)

(defmethod port-mirror-height ((port sdl-port) sheet)
  (declare (ignore sheet))
  nil)

(defmethod graft ((port sdl-port))
  (first (climi::port-grafts port)))

(defmethod port-allocate-pixmap ((port sdl-port) sheet width height)
  (declare (ignore sheet width height))
  ;; FIXME: this isn't actually good enough; it leads to errors in
  ;; WITH-OUTPUT-TO-PIXMAP
  nil)

(defmethod port-deallocate-pixmap ((port sdl-port) pixmap)
  #+nil
  (when (port-lookup-mirror port pixmap)
    (destroy-mirror port pixmap)))

(defmethod pointer-position ((pointer sdl-pointer))
  (values (slot-value pointer 'x) (slot-value pointer 'y)))

(defmethod pointer-button-state ((pointer sdl-pointer))
  nil)

(defmethod port-modifier-state ((port sdl-port))
  nil)

(defmethod synthesize-pointer-motion-event ((pointer sdl-pointer))
  nil)

(defmethod port-frame-keyboard-input-focus ((port sdl-port) frame)
  (frame-properties frame 'focus))

(defmethod (setf port-frame-keyboard-input-focus) 
    (focus (port sdl-port) frame)
  (setf (frame-properties frame 'focus) focus))

(defmethod (setf port-keyboard-input-focus) (focus (port sdl-port))
  focus)

(defmethod port-keyboard-input-focus ((port sdl-port))
  nil)

(defmethod port-force-output ((port sdl-port))
  nil)

;; FIXME: What happens when CLIM code calls tracking-pointer recursively?
(defmethod port-grab-pointer ((port sdl-port) pointer sheet)
  (declare (ignore pointer sheet))
  nil)

(defmethod port-ungrab-pointer ((port sdl-port) pointer sheet)
  (declare (ignore pointer sheet))
  nil)

(defmethod distribute-event :around ((port sdl-port) event)
  (declare (ignore event))
  nil)

(defmethod set-sheet-pointer-cursor ((port sdl-port) sheet cursor)
  (declare (ignore sheet cursor))
  nil)        

(defmethod bind-selection ((port sdl-port) window &optional time)
  (declare (ignore window time))
  nil)

(defmethod release-selection ((port sdl-port) &optional time)
  (declare (ignore time))
  nil)

(defmethod request-selection ((port sdl-port) requestor time)
  (declare (ignore requestor time))
  nil)

(defmethod get-selection-from-event ((port sdl-port) event)
  (declare (ignore event))
  nil)

(defmethod send-selection ((port sdl-port) event string)
  (declare (ignore event string))
  nil)
