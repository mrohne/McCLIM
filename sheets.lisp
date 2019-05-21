;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com),
;;;  (c) copyright 2000 by
;;;           Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;  (c) copyright 2000, 2014 by
;;;           Robert Strandh (robert.strandh@gmail.com)
;;;  (c) copyright 2001 by
;;;           Arnaud Rouanet (rouanet@emi.u-bordeaux.fr)
;;;           Lionel Salabartan (salabart@emi.u-bordeaux.fr)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The sheet protocol

(in-package :clim-internals)

(defgeneric raise-sheet-internal (sheet parent))
(defgeneric bury-sheet-internal (sheet parent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; input protocol

(defgeneric dispatch-event (client event))
(defgeneric queue-event (client event))
(defgeneric schedule-event (client event delay))
(defgeneric handle-event (client event))
(defgeneric event-read (client))
(defgeneric event-read-no-hang (client))
(defgeneric event-peek (client &optional event-type))
(defgeneric event-unread (client event))
(defgeneric event-listen (client))

;;; These DEFGENERIC forms are commented out because they appear
;;; in decls.lisp.
;(defgeneric sheet-direct-mirror (sheet))
;(defgeneric sheet-mirrored-ancestor (sheet))
;(defgeneric sheet-mirror (sheet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; repaint protocol

(defgeneric dispatch-repaint (sheet region))
;(defgeneric queue-repaint (sheet region))
;(defgeneric handle-repaint (sheet region))
;(defgeneric repaint-sheet (sheet region))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; notification protocol

(defgeneric note-sheet-grafted (sheet))
(defgeneric note-sheet-degrafted (sheet))
(defgeneric note-sheet-adopted (sheet))
(defgeneric note-sheet-disowned (sheet))
(defgeneric note-sheet-enabled (sheet))
(defgeneric note-sheet-disabled (sheet))
(defgeneric note-sheet-region-changed (sheet))
(defgeneric note-sheet-transformation-changed (sheet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; sheet protocol class

(defclass basic-sheet (sheet)
  ((region :type region
	   :initarg :region
	   :initform (make-rectangle* 0 0 100 100)
	   :accessor sheet-region)
   (mirror-transformation :type (or null transformation) :initform nil)
   (mirror-region :type (or null region) :initform nil)
   (native-transformation :type (or null transformation) :initform nil)
   (native-region :type (or null region) :initform nil)
   (device-transformation :type (or null transformation) :initform nil)
   (device-region :type (or null region) :initform nil)
   (pointer-cursor :accessor sheet-pointer-cursor :initarg  :pointer-cursor :initform :default)
   (enabled-p :type boolean :initarg :enabled-p :initform t :accessor sheet-enabled-p)))

;;; Native region is volatile, and is only computed at the first
;;; request when it's equal to nil.
;;;
;;; Invalidate-cached-region method sets the native-region to nil.

(defmethod sheet-parent ((sheet basic-sheet))
  nil)

(defmethod sheet-children ((sheet basic-sheet))
  nil)

(defmethod sheet-adopt-child ((sheet basic-sheet) (child sheet))
  (error "~S attempting to adopt ~S" sheet child))

(defmethod sheet-adopt-child :after ((sheet basic-sheet) (child sheet))
  (note-sheet-adopted child)
  (when (sheet-grafted-p sheet)
    (labels ((graft-sheet (sheet)
	       (note-sheet-grafted sheet)
	       (mapc #'graft-sheet (sheet-children sheet))))
      (graft-sheet child))))

(define-condition sheet-is-not-child (error) ())

(defmethod sheet-disown-child :before
    ((sheet basic-sheet) (child sheet) &key (errorp t))
  (when (and (not (member child (sheet-children sheet))) errorp)
    (error 'sheet-is-not-child))
  (note-sheet-disowned child)
  (when (sheet-grafted-p sheet)
    (labels ((degraft-sheet (sheet)
	       (mapc #'degraft-sheet (sheet-children sheet))
	       (note-sheet-degrafted sheet)))
      (degraft-sheet child))))

(defmethod sheet-disown-child :after
    ((sheet basic-sheet) (child sheet) &key (errorp t))
  (declare (ignore errorp)))

(defmethod sheet-siblings ((sheet basic-sheet))
  (when (not (sheet-parent sheet))
    (error 'sheet-is-not-child))
  (remove sheet (sheet-children (sheet-parent sheet))))

(defmethod sheet-enabled-children ((sheet basic-sheet))
  (delete-if-not #'sheet-enabled-p (copy-list (sheet-children sheet))))

(defmethod sheet-ancestor-p ((sheet basic-sheet)
			     (putative-ancestor sheet))
  (or (eq sheet putative-ancestor)
      (and (sheet-parent sheet)
	   (sheet-ancestor-p (sheet-parent sheet) putative-ancestor))))

(defmethod raise-sheet ((sheet basic-sheet))
  (error 'sheet-is-not-child))

(defmethod bury-sheet ((sheet basic-sheet))
  (error 'sheet-is-not-child))

(define-condition sheet-ordering-underspecified (error) ())

(defmethod reorder-sheets ((sheet basic-sheet) new-ordering)
  (when (set-difference (sheet-children sheet) new-ordering)
    (error 'sheet-ordering-underspecified))
  (when (set-difference new-ordering (sheet-children sheet))
    (error 'sheet-is-not-child))
  (setf (sheet-children sheet) new-ordering)
  sheet)

(defmethod sheet-viewable-p ((sheet basic-sheet))
  (and (sheet-parent sheet)
       (sheet-viewable-p (sheet-parent sheet))
       (sheet-enabled-p sheet)))

(defmethod sheet-occluding-sheets ((sheet basic-sheet) (child sheet))
  (labels ((fun (l)
		(cond ((eq (car l) child) '())
		      ((and (sheet-enabled-p (car l))
                            (region-intersects-region-p
                             (sheet-region (car l)) (sheet-region child)))
		       (cons (car l) (fun (cdr l))))
		      (t (fun (cdr l))))))
    (fun (sheet-children sheet))))

(defmethod map-over-sheets (function (sheet basic-sheet))
  (funcall function sheet)
  (mapc #'(lambda (child) (map-over-sheets function child))
        (sheet-children sheet))
  nil)

(defmethod (setf sheet-enabled-p) :after (enabled-p (sheet basic-sheet))
  (if enabled-p
      (note-sheet-enabled sheet)
      (note-sheet-disabled sheet)))

(defmethod sheet-transformation ((sheet basic-sheet))
  +identity-transformation+)

(defmethod (setf sheet-transformation) (transformation (sheet basic-sheet))
  (declare (ignore transformation))
  (error "Attempting to set the TRANSFORMATION of a SHEET that doesn't contain one"))

(defmethod move-sheet ((sheet basic-sheet) x y)
  (let ((transform (sheet-transformation sheet)))
    (multiple-value-bind (old-x old-y)
        (transform-position transform 0 0)
      (setf (sheet-transformation sheet)
            (compose-translation-with-transformation
             transform (- x old-x) (- y old-y))))))

(defmethod resize-sheet ((sheet basic-sheet) width height)
  (setf (sheet-region sheet)
        (make-bounding-rectangle 0 0 width height)))

(defmethod move-and-resize-sheet ((sheet basic-sheet) x y width height)
  (move-sheet sheet x y)
  (resize-sheet sheet width height))

(defmethod map-sheet-position-to-parent ((sheet basic-sheet) x y)
  (declare (ignore x y))
  (error "Sheet has no parent"))

(defmethod map-sheet-position-to-child ((sheet basic-sheet) x y)
  (declare (ignore x y))
  (error "Sheet has no parent"))

(defmethod map-sheet-rectangle*-to-parent ((sheet basic-sheet) x1 y1 x2 y2)
  (declare (ignore x1 y1 x2 y2))
  (error "Sheet has no parent"))

(defmethod map-sheet-rectangle*-to-child ((sheet basic-sheet) x1 y1 x2 y2)
  (declare (ignore x1 y1 x2 y2))
  (error "Sheet has no parent"))

(defmethod map-over-sheets-containing-position (function (sheet basic-sheet) x y)
  (mapc
   #'(lambda (child)
       (multiple-value-bind (x y) (map-sheet-position-to-child child x y)
	 (when (region-contains-position-p (sheet-region child) x y)
	   (funcall function child))))
   (sheet-children sheet)))

(defmethod map-over-sheets-overlapping-region (function (sheet basic-sheet) region)
  (mapc
   #'(lambda (child)
       (let ((region (untransform-region (sheet-transformation child) region)))
	 (when (region-intersects-region-p region (sheet-region child))
	   (funcall function child))))
   (sheet-children sheet)))

(defmethod child-containing-position ((sheet basic-sheet) x y)
  (loop for child in (sheet-children sheet)
	do (multiple-value-bind (tx ty) (map-sheet-position-to-child child x y)
	     (if (and (sheet-enabled-p child)
		      (region-contains-position-p (sheet-region child) tx ty))
		 (return child)))))

(defmethod children-overlapping-region ((sheet basic-sheet) (region region))
  (loop for child in (sheet-children sheet)
	if (and (sheet-enabled-p child)
		(region-intersects-region-p
		 region
		 (transform-region (sheet-transformation child)
				   (sheet-region child))))
	  collect child))

(defmethod children-overlapping-rectangle* ((sheet basic-sheet) x1 y1 x2 y2)
  (children-overlapping-region sheet (make-rectangle* x1 y1 x2 y2)))

(defmethod sheet-delta-transformation ((sheet basic-sheet) (ancestor (eql nil)))
  (cond ((sheet-parent sheet)
	 (compose-transformations (sheet-transformation sheet)
				  (sheet-delta-transformation
				   (sheet-parent sheet) ancestor)))
	(t +identity-transformation+)))

(define-condition sheet-is-not-ancestor (error) ())

(defmethod sheet-delta-transformation ((sheet basic-sheet) (ancestor sheet))
  (cond ((eq sheet ancestor) +identity-transformation+)
	((sheet-parent sheet)
	 (compose-transformations (sheet-delta-transformation
				   (sheet-parent sheet) ancestor)
				  (sheet-transformation sheet)))
	(t (error 'sheet-is-not-ancestor))))

(defmethod sheet-allocated-region ((sheet basic-sheet) (child sheet))
  (reduce #'region-difference
	  (mapcar #'(lambda (child)
                      (transform-region (sheet-transformation child)
                                        (sheet-region child)))
                  (cons child (sheet-occluding-sheets sheet child)))))

(defmethod sheet-direct-mirror ((sheet basic-sheet))
  nil)

(defmethod sheet-mirrored-ancestor ((sheet basic-sheet))
  (let ((parent (sheet-parent sheet)))
    (when parent
      (if (sheet-direct-mirror parent)
	  parent
	  (sheet-mirrored-ancestor parent)))))

(defmethod sheet-mirror ((sheet basic-sheet))
  (or (sheet-direct-mirror sheet)
      (let ((parent (sheet-parent sheet)))
	(when parent 
	  (sheet-mirror parent)))))

(defmethod graft ((sheet basic-sheet))
  nil)

(defmethod note-sheet-grafted ((sheet basic-sheet)))

(defmethod note-sheet-degrafted ((sheet basic-sheet))
  nil)

(defmethod note-sheet-adopted ((sheet basic-sheet))
  (declare (ignorable sheet))
  nil)

(defmethod note-sheet-disowned ((sheet basic-sheet))
  (declare (ignorable sheet))
  nil)

(defmethod note-sheet-enabled ((sheet basic-sheet))
  (declare (ignorable sheet))
  nil)

(defmethod note-sheet-disabled ((sheet basic-sheet))
  (declare (ignorable sheet))
  nil)

(defmethod note-sheet-region-changed ((sheet basic-sheet))
  nil) ;have to change

(defmethod note-sheet-transformation-changed ((sheet basic-sheet))
  nil)

(defmethod port-mirror-transformation (port (sheet basic-sheet))
  (let ((parent (sheet-parent sheet)))
    (if parent
	(compose-transformations
	 (sheet-native-transformation parent)
	 (sheet-transformation sheet))
	(sheet-transformation sheet))))

(defmethod port-mirror-region (port (sheet basic-sheet))
  (let ((parent (sheet-parent sheet)))
    (if parent
	(transform-region
	 (sheet-native-transformation parent)
	 (region-intersection
	  (sheet-region parent)
	  (transform-region
	   (sheet-transformation sheet)
	   (sheet-region sheet))))
	(transform-region
	 (sheet-transformation sheet)
	 (sheet-region sheet)))))
  
(defmethod sheet-mirror-transformation ((sheet basic-sheet))
  (with-slots (mirror-transformation) sheet
    (unless mirror-transformation
      (setf mirror-transformation (port-mirror-transformation (port sheet) sheet)))
    mirror-transformation))

(defmethod sheet-mirror-region ((sheet basic-sheet))
  (with-slots (mirror-region) sheet
    (unless mirror-region
      (setf mirror-region (port-mirror-region (port sheet) sheet)))
    mirror-region))

(defmethod port-native-transformation (port (sheet basic-sheet))
  (sheet-mirror-transformation sheet))

(defmethod port-native-region (port (sheet basic-sheet))
  (sheet-mirror-region sheet))
  
(defmethod sheet-native-transformation ((sheet basic-sheet))
  (with-slots (native-transformation) sheet
    (unless native-transformation
      (setf native-transformation (port-native-transformation (port sheet) sheet)))
    native-transformation))

(defmethod sheet-native-region ((sheet basic-sheet))
  (with-slots (native-region) sheet
    (unless native-region
      (setf native-region (port-native-region (port sheet) sheet)))
    native-region))

(defmethod port-device-transformation (port (sheet basic-sheet))
  (let ((medium (sheet-medium sheet)))
    (if medium
	(compose-transformations
	 (sheet-native-transformation sheet)
	 (medium-transformation medium))
	(sheet-native-transformation sheet))))

(defmethod port-device-region (port (sheet basic-sheet))
  (let ((medium (sheet-medium sheet)))
    (if medium
	(region-intersection
	 (transform-region
	  (sheet-device-transformation sheet)
	  (sheet-region sheet))
	 (medium-clipping-region medium))
	(transform-region
	 (sheet-device-transformation sheet)
	 (sheet-region sheet)))))
  
(defmethod sheet-device-transformation ((sheet basic-sheet))
  (with-slots (device-transformation) sheet
    (unless device-transformation
      (setf device-transformation (port-device-transformation (port sheet) sheet)))
    device-transformation))

(defmethod sheet-device-region ((sheet basic-sheet))
  (with-slots (device-region) sheet
    (unless device-region
      (setf device-region (port-device-region (port sheet) sheet)))
    device-region))

(defmethod invalidate-cached-transformations ((sheet basic-sheet))
  (with-slots (mirror-transformation native-transformation device-transformation) sheet
    (setf mirror-transformation nil
          native-transformation nil
          device-transformation nil))
  (loop for child in (sheet-children sheet)
     do (invalidate-cached-transformations child)))

(defmethod invalidate-cached-regions ((sheet basic-sheet))
  (with-slots (mirror-region native-region device-region) sheet
    (setf mirror-region nil
          native-region nil
          device-region nil))
  (loop for child in (sheet-children sheet)
     do (invalidate-cached-regions child)))

(defmethod (setf sheet-transformation) :after
    (transformation (sheet basic-sheet))
  (declare (ignore transformation))
  (invalidate-cached-transformations sheet)
  (note-sheet-transformation-changed sheet))

(defmethod (setf sheet-region) :after (region (sheet basic-sheet))
  (declare (ignore region))
  (invalidate-cached-regions sheet)
  (note-sheet-region-changed sheet))

(defmethod (setf sheet-pointer-cursor) :after (cursor (sheet basic-sheet))
  (set-sheet-pointer-cursor (port sheet) sheet cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; sheet parent mixin

(defclass sheet-parent-mixin ()
  ((parent :initform nil :accessor sheet-parent)))

(define-condition sheet-already-has-parent (error) ())
(define-condition sheet-is-ancestor (error) ())

(defmethod sheet-adopt-child :before (sheet (child sheet-parent-mixin))
  (when (and (sheet-parent child) (not (eq sheet (sheet-parent child))))
    (error 'sheet-already-has-parent))
  (when (sheet-ancestor-p sheet child)
    (error 'sheet-is-ancestor)))

(defmethod sheet-adopt-child :after (sheet (child sheet-parent-mixin))
  (setf (sheet-parent child) sheet))

(defmethod sheet-disown-child :after (sheet
				      (child sheet-parent-mixin)
				      &key (errorp t))
  (declare (ignore sheet errorp))
  (setf (sheet-parent child) nil))

(defmethod raise-sheet ((sheet sheet-parent-mixin))
  (when (sheet-parent sheet)
    (raise-sheet-internal sheet (sheet-parent sheet)))
  (when (sheet-direct-mirror sheet)
    (raise-mirror (port sheet) sheet)))

(defmethod bury-sheet ((sheet sheet-parent-mixin))
  (when (sheet-parent sheet)
    (bury-sheet-internal sheet (sheet-parent sheet)))
  (when (sheet-direct-mirror sheet)
    (bury-mirror (port sheet) sheet)))

(defmethod graft ((sheet sheet-parent-mixin))
  (and (sheet-parent sheet) (graft (sheet-parent sheet))))

(defmethod map-sheet-position-to-parent ((sheet sheet-parent-mixin) x y)
  (transform-position (sheet-transformation sheet) x y))

(defmethod map-sheet-position-to-child ((sheet sheet-parent-mixin) x y)
  (untransform-position (sheet-transformation sheet) x y))

(defmethod map-sheet-rectangle*-to-parent
    ((sheet sheet-parent-mixin) x1 y1 x2 y2)
  (transform-rectangle* (sheet-transformation sheet) x1 y1 x2 y2))

(defmethod map-sheet-rectangle*-to-child
    ((sheet sheet-parent-mixin) x1 y1 x2 y2)
  (untransform-rectangle* (sheet-transformation sheet) x1 y1 x2 y2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; sheet leaf mixin

(defclass sheet-leaf-mixin () ())

(defmethod sheet-children ((sheet sheet-leaf-mixin))
  nil)

(defmethod sheet-adopt-child ((sheet sheet-leaf-mixin) (child sheet))
  (error "Leaf sheet attempting to adopt a child"))

(defmethod sheet-disown-child
    ((sheet sheet-leaf-mixin) (child sheet) &key (errorp t))
  (declare (ignorable errorp))
  (error "Leaf sheet attempting to disown a child"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; sheet single child mixin

(defclass sheet-single-child-mixin ()
  ((child :initform nil :accessor sheet-child)))

(defmethod sheet-children ((sheet sheet-single-child-mixin))
  (and (sheet-child sheet) (list (sheet-child sheet))))

(define-condition sheet-supports-only-one-child (error) ())

(defmethod sheet-adopt-child :before ((sheet sheet-single-child-mixin)
				      (child sheet-parent-mixin))
  (when (sheet-child sheet)
    (error 'sheet-supports-only-one-child)))

(defmethod sheet-adopt-child ((sheet sheet-single-child-mixin)
			      (child sheet-parent-mixin))
  (setf (sheet-child sheet) child))

(defmethod sheet-disown-child ((sheet sheet-single-child-mixin)
			       (child sheet-parent-mixin)
			       &key (errorp t))
  (declare (ignore errorp))
  (setf (sheet-child sheet) nil))

(defmethod raise-sheet-internal (sheet (parent sheet-single-child-mixin))
  (declare (ignorable sheet parent))
  (values))

(defmethod bury-sheet-internal (sheet (parent sheet-single-child-mixin))
  (declare (ignorable sheet parent))
  (values))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; sheet multiple child mixin

(defclass sheet-multiple-child-mixin ()
  ((children :initform nil :initarg :children :accessor sheet-children)))

(defmethod sheet-adopt-child ((sheet sheet-multiple-child-mixin)
			      (child sheet-parent-mixin))
  (push child (sheet-children sheet)))

(defmethod sheet-disown-child ((sheet sheet-multiple-child-mixin)
			       (child sheet-parent-mixin)
			       &key (errorp t))
  (declare (ignore errorp))
  (setf (sheet-children sheet) (delete child (sheet-children sheet))))

(defmethod raise-sheet-internal (sheet (parent sheet-multiple-child-mixin))
  (setf (sheet-children parent)
	(cons sheet (delete sheet (sheet-children parent)))))

(defmethod bury-sheet-internal (sheet (parent sheet-multiple-child-mixin))
  (setf (sheet-children parent)
	(nconc (delete sheet (sheet-children parent)) (list  sheet))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; sheet geometry classes

(defclass sheet-identity-transformation-mixin ()
  ())

(defmethod sheet-transformation ((sheet sheet-identity-transformation-mixin))
  +identity-transformation+)

(defclass sheet-transformation-mixin ()
  ((transformation :initform +identity-transformation+
		   :initarg :transformation
		   :accessor sheet-transformation)))

(defclass sheet-translation-transformation-mixin (sheet-transformation-mixin)
  ())

(defmethod (setf sheet-transformation) :before
    ((transformation transformation)
     (sheet sheet-translation-transformation-mixin))
  (if (not (translation-transformation-p transformation))
      (error "Attempting to set the SHEET-TRANSFORMATION of a SHEET-TRANSLATION-TRANSFORMATION-MIXIN to a non translation transformation")))

(defclass sheet-y-inverting-transformation-mixin (sheet-transformation-mixin)
  ()
  (:default-initargs :transformation (make-transformation 1 0 0 -1 0 0)))

(defmethod (setf sheet-transformation) :before
    ((transformation transformation)
     (sheet sheet-y-inverting-transformation-mixin))
  (if (not (y-inverting-transformation-p transformation))
      (error "Attempting to set the SHEET-TRANSFORMATION of a SHEET-Y-INVERTING-TRANSFORMATION-MIXIN to a non Y inverting transformation")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mirrored sheet

;;; We assume the following limitations of the host window systems:
;;;
;;;  mirror transformations:
;;;   . can only be translations
;;;   . are limited to 16-bit signed integer deltas
;;;
;;;  mirror regions:
;;;   . can only be axis-aligend rectangles
;;;   . min-x = min-y = 0
;;;   . max-x, max-y < 2^16
;;;
;;; These are the limitations of the X Window System.
;;;

(defclass mirrored-sheet-mixin ()
  ((port :initform nil :initarg :port :accessor port)))

(defmethod sheet-direct-mirror ((sheet mirrored-sheet-mixin))
  (port-lookup-mirror (port sheet) sheet))

;;; The generic function (SETF SHEET-DIRECT-MIRROR) is not part of the
;;; CLIM II specification.  For that reason, there is no DEFGENERIC
;;; form for it in decls.lisp.  Since some Common Lisp compilers emit
;;; a warning if there is no explicit DEFGENERIC form, and in order to
;;; get a clean build, we include the DEFGENERIC form here.
(defgeneric (setf sheet-direct-mirror) (mirror sheet))

(defmethod (setf sheet-direct-mirror) (mirror (sheet mirrored-sheet-mixin))
  (port-register-mirror (port sheet) sheet mirror))

(defmethod note-sheet-grafted :before ((sheet mirrored-sheet-mixin))
  (unless (port sheet)
    (error "~S called on sheet ~S, which has no port?!" '
	   note-sheet-grafted sheet))
  (realize-mirror (port sheet) sheet))

(defmethod note-sheet-degrafted :after ((sheet mirrored-sheet-mixin))
  (destroy-mirror (port sheet) sheet))

(defmethod port-sheet-region-changed (port (sheet mirrored-sheet-mixin))
  )

(defmethod port-sheet-transformation-changed (port (sheet mirrored-sheet-mixin))
  )

(defmethod note-sheet-region-changed ((sheet mirrored-sheet-mixin))
  (port-sheet-region-changed (port sheet) sheet))

(defmethod note-sheet-transformation-changed ((sheet mirrored-sheet-mixin))
  (port-sheet-transformation-changed (port sheet) sheet))

(defmethod (setf sheet-enabled-p) :after
    (new-value (sheet mirrored-sheet-mixin))
  (when (sheet-direct-mirror sheet)
    ;; We do this only if the sheet actually has a mirror.
    (if new-value
        (port-enable-sheet (port sheet) sheet)
        (port-disable-sheet (port sheet) sheet))))

(defgeneric sheet-motion-hints (sheet)
  (:documentation "Returns t if motion hints are enabled for this sheet"))

(defmethod sheet-motion-hints ((sheet mirrored-sheet-mixin))
  (when (sheet-direct-mirror sheet)
    (port-motion-hints (port sheet) sheet)))

(defgeneric (setf sheet-motion-hints) (val sheet))

(defmethod (setf sheet-motion-hints) (val (sheet mirrored-sheet-mixin))
  (when (sheet-direct-mirror sheet)
    (setf (port-motion-hints (port sheet) sheet) val)))

;;;; Coordinate Swizzling

;;; This implements what I call "coordinate swizzling", the illusion that
;;; sheets can be arbitrary large. The key idea here is that there is a
;;; certain kind freedom in choosing the native transformation. A little
;;; diagram to illustrate the involved transformations:

;;;
;;;                  NT                           NT = native transformation
;;;    sheet  ---------------->  mirror          PNT = parent's NT
;;;      |                         |              MT = mirror transformation
;;;      |                         |               T = sheet transformation
;;;      |                         |
;;;    T |                         | MT
;;;      |                         |
;;;      |                         |
;;;      |                         |
;;;      v          PNT            v
;;;    parent ----------------> parent
;;;                             mirror
;;;

;;; To setup both the mirror transformation (MR) and the mirror region (MR),
;;; we start with the mirror region. The window systems limitations are here:
;;; We can only have a certain size and its upper-left corner must be at the
;;; origin.

;;; Now the parent already has a mirror region (PMR) assigned, which obeys to
;;; the very same size restrictions. Since every part of MR outside of (PMR o
;;; MT^1) is not visible, the first idea is to just clip it by the visible
;;; part:

;;;  MR_1 = intersection (SR o NT, PMR o MT^-1)             [mirror space]

;;; Since both NT and MT^-1 are not yet known let us reformulate that region
;;; in the parent mirror space:

;;;  MR_2 = MR_1 o MT                                       [parent mirror space]
;;;       = intersection (SR o NT, PMR o MT^-1) o MT
;;;       = intersection (SR o NT o MT, PMR o MT^-1 o MT)
;;;       = intersection (SR o (T o PNT o MT^-1) o MT, PMR)
;;;       = intersection (SR o T o PNT, PMR)

;;; MR_2 now is a good candidate for a mirror region. Unfortunately it is
;;; still in parent mirror space, so we transform it back, yielding MR_3:

;;;  MR_3 = MR_2 o MT^-1
;;;       = intersection (SR o T o PNT, PMR) o MT^-1

;;; Here the only unknown is the mirror transformation MT, we can still
;;; choose any as long as the window system limitations are met for both MR
;;; and MT.

;;; 1. MT should be a translation, whose delta x and y components are within
;;;    limits.

;;; 2. The size limitation of MR is already met, since MR_3's size is no
;;;    larger than PMR's size (which mets the limitations). [Remember that MT
;;;    was defined to be some translation].

;;; 3. MR_3's upper left corner should also be at the origin which nicely
;;;    defines MT^-1: Just choose this upper left corner coordinates as MT's x
;;;    and y deltas.

;;; So we can meet all criteria. The NT can easily be set up by the identity:

;;;    NT = T o PNT o MT^-1

;;; Notes

;;; . when the native transformation changes, we need to:

;;;  a. Redraw the mirror's contents since the mapping from the sheet space
;;;     to the mirror space (that is the native transformation) just changed. 
;;;     Translational changes in the native transformation can be catered by
;;;     blittering, but then have a nice synchronization problem: Suppose
;;;     a repaint event is underway as we blitter from some region R_1 to
;;;     region R_2. Say the repaint event's region intersects with R_1. In
;;;     this case we just blittered pixels which were considered dirty into
;;;     R_2. Redrawing R_1 now does not repair the defect, since R_2 now also
;;;     contains dirty pixels. => oops, redraw error.
;;
;;;  b. Since the above above calculation took the parent's native
;;;     transformation into account, (and even the naively wanted mirror
;;;     region depends on the parent's native transformation), we need to
;;;     redo mirror geometry calculation for any child.
;;
;;;  c. I imagine more aggressive output records which remember the actual
;;;     octets which need to be send to the X server. These would contain
;;;     mirror coordinates and will need to be recalculated, when the native
;;;     transformation changes.

;;; => Changing the native transformation can be expensive, so we want a way
;;;    to minimize changes to the native transformation.

;;; What did we do? We clipped the wanted mirror region, SR o NT, inside the
;;; parent's mirror region to meet the window system limitations. We can make
;;; this clip region larger as long as we still come up with an mirror
;;; region, which meets the limits.

;;; Sheets as bounding rectangles

;;; Somewhat hidden in the spec, we read (section 4.1.1 "The Bounding
;;; Rectangle Protocol")
;;;

;;; | bounding-rectangle* region [Generic Function]
;;; |
;;; |      [...] The argument region must be either a bounded region [...] or
;;; |      some other object that obeys the bounding rectangle protocol, such
;;; |      as a sheet or an output record. [...]

(defmethod bounding-rectangle* ((sheet sheet))
  (bounding-rectangle* (sheet-region sheet)))

;;; The null sheet

(defclass null-sheet (basic-sheet) ())
