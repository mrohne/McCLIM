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

(declaim (optimize (debug 3) (safety 3)))

(defclass sdl-medium (basic-medium)
  ((%port :initarg :port :reader port)
   (%font :initform nil)
   (%render :initform nil :accessor medium-render)
   (%design :initform nil :accessor medium-design)
   (%buffer :initform nil)))

;; The standard is (perhaps deliberately) vague about where in the
;; call chain below WITH-DRAWING-OPTIONS the transition happens from
;; an abstract "medium" (STREAM/SHEET/T) to a physcial MEDIUM

(defmethod color-rgb ((ink t))
  (values 0 0 0))

(defmethod opacity-value ((ink t))
  (values 1))

(defmethod climi::do-graphics-with-options-internal :around
    ((medium sdl-medium) orig-medium func &rest options &key ink &allow-other-keys)
  (declare (ignorable options))
  (let ((sheet (medium-sheet medium)))
    (when sheet
      (let ((render (sheet-render sheet)))
	(when render
	  (let ((render (shiftf (medium-render medium) render))
		(ink (shiftf (medium-ink medium) ink)))
	    (unwind-protect
		 (with-port-locked ((port medium))
		   (call-next-method))
	      (setf (medium-render medium) render)
	      (setf (medium-ink medium) ink))))))))

(defmethod (setf medium-ink) ((ink color) (medium sdl-medium))
  (with-slots (%port %render %design) medium
    (when %render
      (with-port-locked (%port)
	(multiple-value-bind (r g b) (color-rgb ink)
	  (let ((r8 (min (floor r 1/256) 255))
		(g8 (min (floor g 1/256) 255))
		(b8 (min (floor b 1/256) 255))
		(a8 (min (floor 1 1/256) 255)))
	    ;; install color and design
	    (sdl2::check-rc (sdl-set-render-draw-color %render r8 g8 b8 a8))
	    (setf %design nil)))))))

(defmethod (setf medium-ink) ((design design) (medium sdl-medium))
  (with-slots (%port %render %design) medium
    (when %render
      (with-port-locked (%port)
	(multiple-value-bind (r g b a) (values 0.0 0.5 0.5 0.5)
	  (let ((r8 (min (floor r 1/256) 255))
		(g8 (min (floor g 1/256) 255))
		(b8 (min (floor b 1/256) 255))
		(a8 (min (floor a 1/256) 255)))
	    ;; install color and design
	    (sdl2::check-rc (sdl-set-render-draw-color %render r8 g8 b8 a8))
	    (setf %design nil)))))))

(defmethod (setf medium-ink) ((design climi::transformed-design) (medium sdl-medium))
  (format *trace-output* ";; ~a ignoring ~a~%" design (climi::transformed-design-transformation design))
  (setf (medium-ink medium) (climi::transformed-design-design design)))

(defmethod (setf medium-ink) ((pattern climi::indexed-pattern) (medium sdl-medium))
  (with-slots (%port %render %design) medium
    (when %render
      (with-port-locked (%port)
	(multiple-value-bind (r g b a) (values 0.0 0.5 0.5 0.5)
	  (let ((r8 (min (floor r 1/256) 255))
		(g8 (min (floor g 1/256) 255))
		(b8 (min (floor b 1/256) 255))
		(a8 (min (floor a 1/256) 255)))
	    (sdl2::check-rc (sdl-set-render-draw-color %render r8 g8 b8 a8))))
	(let ((pixels (pattern-array pattern))
	      (colors (make-array 256 :initial-element '(0 0 0 0))))
	  (loop
	     for jj from 0
	     for color across (pattern-designs pattern) do
	       (multiple-value-bind (r g b) (color-rgb color)
		 (let ((a (opacity-value color)))
		   (setf (aref colors jj) (list r g b a)))))
	  (setf %design (sdl-create-indexed-surface pixels colors)))))))

(defmethod (setf medium-ink) ((new-value (eql +background-ink+)) (medium sdl-medium))
  (setf (medium-ink medium) (medium-background medium)))

(defmethod (setf medium-ink) ((new-value (eql +foreground-ink+)) (medium sdl-medium))
  (setf (medium-ink medium) (medium-foreground medium)))

(defmethod (setf medium-line-style) :before (line-style (medium sdl-medium))
  (declare (ignore line-style))
  nil)

(defmethod (setf medium-clipping-region) :after (region (medium sdl-medium))
  (declare (ignore region))
  nil)

(defmethod medium-copy-area ((from-drawable sdl-medium)
			     from-x from-y width height
                             (to-drawable sdl-medium)
			     to-x to-y)
  (declare (ignore from-x from-y width height to-x to-y))
  nil)

(defmethod medium-copy-area ((from-drawable sdl-medium)
			     from-x from-y width height
			     (to-drawable pixmap)
			     to-x to-y)
  (declare (ignore from-x from-y width height to-x to-y))
  nil)

(defmethod medium-copy-area ((from-drawable pixmap)
			     from-x from-y width height
			     (to-drawable sdl-medium)
			     to-x to-y)
  (with-slots (%port %render) to-drawable
    (when %render
      (with-port-locked (%port)
	(let ((px (pixmap-mirror from-drawable)))
	  (let ((tx (sdl2::check-null (sdl-create-texture-from-surface %render px))))
	    (unwind-protect
		 ;; texture -> screen
		 (c-with ((sr sdl-rect)
			  (dr sdl-rect))
		   (setf (sr :x) (snap from-x))
		   (setf (sr :y) (snap from-y))
		   (setf (sr :w) (snap width))
		   (setf (sr :h) (snap height))
		   (setf (dr :x) (snap to-x))
		   (setf (dr :y) (snap to-y))
		   (setf (dr :w) (snap width))
		   (setf (dr :h) (snap height))
		   (sdl2::check-rc (sdl-set-texture-blend-mode tx +sdl-blendmode-none+))
		   (sdl2::check-rc (sdl-render-copy %render tx sr dr)))
	      (sdl-destroy-texture tx))))))))

(defmethod medium-copy-area ((from-drawable pixmap)
			     from-x from-y width height
			     (to-drawable pixmap)
			     to-x to-y)
  (declare (ignore from-x from-y width height to-x to-y))
  nil)

(defmethod medium-draw-point* ((medium sdl-medium) x y)
  (with-slots (%port %render) medium
    (when %render
      (with-port-locked (%port)
	(let ((tr (sheet-device-transformation (medium-sheet medium))))
	  (with-transformed-position (tr x y)
	    (sdl2::check-rc (sdl-render-draw-point %render (snap x) (snap y)))))))))

(defmethod medium-draw-points* ((medium sdl-medium) coord-seq)
  (with-slots (%port %render) medium
    (when %render
      (with-port-locked (%port)
	(let ((ii 0)
	      (nn 256)
	      (tr (sheet-device-transformation (medium-sheet medium))))
	  (c-with ((ps sdl-point :count nn))
	    (let (x0 y0 x1 y1)
	      (map nil
		   (lambda (cc)
		     (cond ((null x0)
			    (setf x0 cc))
			   ((null y0)
			    (setf y0 cc)))
		     (cond ((null x1)
			    (setf x1 cc))
			   ((null y1)
			    (setf y1 cc)))
		     (when (and x1 y1)
		       (with-transformed-position (tr x1 y1)
			 (setf (ps ii :x) (snap x1))
			 (setf (ps ii :y) (snap y1))
			 (setf ii (1+ ii)))
		       (setf x1 nil)
		       (setf y1 nil))
		     (when (= ii nn)
		       (sdl2::check-rc (sdl-render-draw-points %render ps ii))
		       (setf ii 0)))
		   coord-seq)
	      (sdl2::check-rc (sdl-render-draw-points %render ps ii)))))))))

(defmethod medium-draw-line* ((medium sdl-medium) x1 y1 x2 y2)
  (with-slots (%port %render) medium
    (when %render
      (with-port-locked (%port)
	(let ((tr (sheet-device-transformation (medium-sheet medium))))
	  (with-transformed-position (tr x1 y1)
	    (with-transformed-position (tr x2 y2)
	      (sdl2::check-rc (sdl-render-draw-line %render (snap x1) (snap y1) (snap x2) (snap y2))))))))))

(defmethod medium-draw-lines* ((medium sdl-medium) coord-seq)
  (with-slots (%port %render) medium
    (when %render
      (with-port-locked (%port)
	(let ((tr (sheet-device-transformation (medium-sheet medium))))
	  (let (x1 y1 x2 y2)
	    (map nil
		 (lambda (cc)
		   (cond ((null x1)
			  (setf x1 cc))
			 ((null y1)
			  (setf y1 cc))
			 ((null x2)
			  (setf x2 cc))
			 ((null y2)
			  (setf y2 cc)))
		   (when (and x1 y1 x2 y2)
		     (with-transformed-position (tr x1 y1)
		       (with-transformed-position (tr x2 y2)
			 (sdl2::check-rc (sdl-render-draw-line %render (snap x1) (snap y1) (snap x2) (snap y2)))))
		     (setf x1 nil)
		     (setf y1 nil)
		     (setf x2 nil)
		     (setf y2 nil)))
		 coord-seq)))))))

(defmethod medium-draw-polygon* ((medium sdl-medium) coord-seq closed filled)
  (declare (ignore filled))
  (with-slots (%port %render) medium
    (when %render
      (with-port-locked (%port)
	(let ((ii 0)
	      (jj 255)
	      (nn 256)
	      (tr (sheet-device-transformation (medium-sheet medium))))
	  (c-with ((ps sdl-point :count nn))
	    (let (x0 y0 x1 y1)
	      (map nil
		   (lambda (cc)
		     (cond ((null x0)
			    (setf x0 cc))
			   ((null y0)
			    (setf y0 cc)))
		     (cond ((null x1)
			    (setf x1 cc))
			   ((null y1)
			    (setf y1 cc)))
		     (when (and x1 y1)
		       (with-transformed-position (tr x1 y1)
			 (setf (ps ii :x) (snap x1))
			 (setf (ps ii :y) (snap y1))
			 (setf jj ii ii (1+ ii)))
		       (setf x1 nil)
		       (setf y1 nil))
		     (when (= ii nn)
		       (sdl2::check-rc (sdl-render-draw-lines %render ps ii))
		       (setf ii 0)
		       (setf (ps ii :x) (ps jj :x))
		       (setf (ps ii :y) (ps jj :y))
		       (setf jj ii ii (1+ ii))))
		   coord-seq)
	      (when closed
		(when (and x0 y0)
		  (with-transformed-position (tr x0 y0)
		    (setf (ps ii :x) (snap x0))
		    (setf (ps ii :y) (snap y0))
		    (setf jj ii ii (1+ ii)))
		  (setf x1 nil)
		  (setf y1 nil)))
	      (sdl2::check-rc (sdl-render-draw-lines %render ps ii)))))))))

(defmethod medium-draw-rectangle* ((medium sdl-medium) left top right bottom filled)
  (with-slots (%port %render %design) medium
    (when %render
      (with-port-locked (%port)
	(let ((tr (sheet-device-transformation (medium-sheet medium))))
	  (with-transformed-position (tr left top)
	    (with-transformed-position (tr right bottom)
	      (c-with ((dr sdl-rect))
		(setf (dr :x) (snap left))
		(setf (dr :y) (snap top))
		(setf (dr :w) (snap (- right left)))
		(setf (dr :h) (snap (- bottom top)))
		(cond
		  ((not filled)
		   (sdl2::check-rc (sdl-render-draw-rect %render dr)))
		  ((null %design)
		   (sdl2::check-rc (sdl-render-fill-rect %render dr)))
		  (t
		   (let ((tx (sdl2::check-null (sdl-create-texture-from-surface %render %design))))		 
		     (unwind-protect
			  (c-with ((sr sdl-rect))
			    (setf (sr :x) 0)
			    (setf (sr :y) 0)
			    (setf (sr :w) (c-ref %design sdl-surface :w))
			    (setf (sr :h) (c-ref %design sdl-surface :w))
			    (sdl2::check-rc (sdl-set-texture-blend-mode tx +sdl-blendmode-blend+))
			    (sdl2::check-rc (sdl-render-copy %render tx sr dr)))
		       (sdl-destroy-texture tx)))))))))))))

(defmethod medium-draw-rectangles* ((medium sdl-medium) position-seq filled)
  (declare (ignore position-seq filled))
  nil)

(defmethod medium-draw-ellipse* ((medium sdl-medium) center-x center-y
				 radius-1-dx radius-1-dy
				 radius-2-dx radius-2-dy
				 start-angle end-angle filled)
  (declare (ignore center-x center-y
		   radius-1-dx radius-1-dy
		   radius-2-dx radius-2-dy
		   start-angle end-angle filled))
  nil)

(defmethod medium-draw-circle* ((medium sdl-medium)
				center-x center-y radius start-angle end-angle
				filled)
  (declare (ignore center-x center-y radius
		   start-angle end-angle filled))
  nil)

(defmethod invoke-with-text-style :before ((medium sdl-medium) thunk text-style)
  (let ((font (port-find-font (port medium) text-style)))
    (with-slots (%font) medium
      (setf %font font))))

(defmethod text-style-ascent (text-style (medium sdl-medium))
  (let ((font (port-find-font (port medium) text-style)))
    (unsnap (ttf-font-ascent font))))

(defmethod text-style-descent (text-style (medium sdl-medium))
  (let ((font (port-find-font (port medium) text-style)))
    (unsnap (ttf-font-descent font))))

(defmethod text-style-height (text-style (medium sdl-medium))
  (- (text-style-ascent text-style medium) (text-style-descent text-style medium)))

(defmethod text-style-character-width (text-style (medium sdl-medium) char)
  (let ((font (port-find-font (port medium) text-style)))
    (c-with ((minx :int) (maxx :int) (miny :int) (maxy :int) (advx :int))
      (ttf-glyph-metrics font (char-code #\m) (minx &) (maxx &) (miny &) (maxy &) (advx &))
      (unsnap advx))))

;;; FIXME: this one is nominally backend-independent
(defmethod text-style-width (text-style (medium sdl-medium))
  (with-text-style (medium text-style)
    (text-style-character-width text-style medium #\m)))

(defmethod text-size ((medium sdl-medium) string &key text-style (start 0) end)
  (with-text-style (medium text-style)
    (with-slots (%font) medium
      (when (null %font)
	(cerror "Continue" "No font"))
      (when %font
	(setf string (etypecase string
		       (character (string string))
		       (string string)))
	(c-with ((ww :int) (hh :int))
	  (loop
	     with bl = (text-style-ascent text-style medium)
	     for beg = start then (1+ pos)
	     for pos = (position #\Newline string :start beg :end end)
	     for (ww hh) = (multiple-value-list (string-size medium string :text-style text-style :start beg :end pos))
	     maximize ww into wd
	     summing hh into ht
	     when (null pos) do (return (values wd ht ww dy bl))
	     summing hh into dy))))))
(defmethod string-size ((medium sdl-medium) string &key text-style (start 0) end)
  (with-text-style (medium text-style)
    (with-slots (%font) medium
      (when (null %font)
	(cerror "Continue" "No font"))
      (when %font
	(c-with ((ww :int) (hh :int))
	  (ttf-size-text %font (subseq string start end) (ww &) (hh &))
	  (values (unsnap ww) (unsnap hh)))))))
(defmethod climi::text-bounding-rectangle*
    ((medium sdl-medium) string &key text-style (start 0) end)
  (multiple-value-bind (dx dy cx cy bl)
      (text-size medium string :text-style text-style :start start :end end)
    (declare (ignorable dx dy cx cy bl))
    (values 0 0 dx dy)))

(defmethod medium-draw-text* ((medium sdl-medium) string x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs)
  (declare (ignore toward-x toward-y transform-glyphs))
  (when (characterp string)
    (setq string (make-string 1 :initial-element string)))
  (multiple-value-bind (ww hh dx dy bl) 
      (text-size medium string :start start :end end)
    (declare (ignore dx dy))
    (setq x (- x (ecase align-x
		   ((nil :left) 0)
		   (:center (round ww 2))
		   (:right ww))))
    (setq y (- y (ecase align-y
		   (:top 0)
		   (:center (floor hh 2))
		   ((nil :baseline) bl)
		   (:bottom hh)))))
  (with-slots (%port %font %render) medium
    (when %render
      (when %font
	(with-port-locked (%port)
	  (let ((tr (sheet-device-transformation (medium-sheet medium))))
	    (with-transformed-position (tr x y)
	      (c-with ((cc sdl-color)
		       (sr sdl-rect)
		       (dr sdl-rect))
		;; extract color from current %render
		(sdl2::check-rc (sdl-get-render-draw-color %render (cc :r &) (cc :g &) (cc :b &) (cc :a &)))
		;; render text -> surface
		(let ((px (sdl2::check-null (ttf-render-text-blended %font string (cc :r) (cc :g) (cc :b) (cc :a)))))
		  (unwind-protect
		       ;; surface -> texture
		       (let ((tx (sdl2::check-null (sdl-create-texture-from-surface %render px))))		 
			 (unwind-protect
			      ;; texture -> screen
			      (let ((sx 0)
				    (sy 0)
				    (dx (snap x))
				    (dy (snap y))
				    (ww (c-ref px sdl-surface :w))
				    (hh (c-ref px sdl-surface :h)))
				(setf (sr :x) sx (sr :y) sy (sr :w) ww (sr :h) hh)
				(setf (dr :x) dx (dr :y) dy (dr :w) ww (dr :h) hh)
				(sdl2::check-rc (sdl-set-texture-blend-mode tx +sdl-blendmode-blend+))
				(sdl2::check-rc (sdl-render-copy %render tx sr dr)))
			   (sdl-destroy-texture tx)))
		    (sdl-free-surface px)))))))))))

(defmethod medium-draw-glyph ((medium sdl-medium) element x y
			      align-x align-y toward-x toward-y
			      transform-glyphs)
  (medium-draw-text* medium (string element) x y 0 1 align-x align-y toward-x toward-y transform-glyphs))

(defmethod medium-finish-output ((medium sdl-medium))
  )

(defmethod medium-force-output ((medium sdl-medium))
  )

(defmethod medium-clear-area ((medium sdl-medium) left top right bottom)
  (declare (ignore left top right bottom))
  nil)

(defmethod medium-beep ((medium sdl-medium))
  nil)

(defmethod invoke-with-special-choices (continuation (medium sdl-medium))
  (let ((sheet (medium-sheet medium)))
    (funcall continuation (sheet-medium sheet))))

(defmethod medium-miter-limit ((medium sdl-medium))
  0)

;;; FIXME: need these to stop the default method attempting to do
;;; pixmaps, which it appears the sdl backend doesn't support yet.
(defmethod climi::medium-draw-bezier-design* 
    ((medium sdl-medium) (design climi::bezier-area))
  nil)
(defmethod climi::medium-draw-bezier-design* 
    ((medium sdl-medium) (design climi::bezier-union))
  nil)
(defmethod climi::medium-draw-bezier-design* 
    ((medium sdl-medium) (design climi::bezier-difference))
  nil)
