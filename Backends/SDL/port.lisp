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
(declaim (optimize (debug 3) (safety 3)))

;;; Hook into the backend selection mechanism
(setf (get :sdl :port-type) 'sdl-port)
(setf (get :sdl :server-path-parser) 'values)

(defclass sdl-pointer (standard-pointer)
  ((cursor :accessor pointer-cursor :initform :upper-left)
   (x :initform 0)
   (y :initform 0)))

(defclass sdl-port (basic-port)
  ((id :initform (format nil "SDL ~{~d~^.~}" (multiple-value-list (sdl2:version))))
   (pointer :initarg :pointer :reader port-pointer)
   ;; semaphore and mailboxes for main loop
   (%sema :initform (make-semaphore))
   (%send :initform (make-mailbox))
   (%recv :initform (make-mailbox))
   ;; one-off foreign allocations
   (%mode :initform nil)
   (%wake :initform nil)
   (%wait :initform nil)
   ;; map on window-id
   (%id->window :initform (make-hash-table))
   (%id->render :initform (make-hash-table))
   ;; loaded fonts
   (%ts->font :initform (make-hash-table :test 'equalp) :reader ts->font)   
   ;; event filtering
   (%sheet->mask :initform (make-hash-table))
   (%sheet->grab :initform (make-hash-table))
   (%grab-sheet :initform nil :accessor pointer-grab-sheet)
   ;; mediums that need refresh
   (%dirty-mediums :initform (make-hash-table) :reader dirty-mediums)))

;; TODO: Clean up & improve on the SDL2 event loop
(defmethod initialize-instance :after ((port sdl-port) &rest initargs)
  (declare (ignore initargs))
  ;; install hook
  (bt:interrupt-thread (main-thread) (make-sdl-hook port))
  ;; wait for handshake
  (exec-sdl-port port (make-sdl-init port))
  ;; install CLIM support
  (push (make-graft port) (port-grafts port))
  (setf (slot-value port 'pointer) (make-instance 'sdl-pointer :port port))
  (push (make-instance 'sdl-frame-manager :port port) (frame-managers port)))

(defmethod print-object ((object sdl-port) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream "~S ~S" :id (slot-value object 'id))))

(defmethod destroy-port :after ((port sdl-port))
  ;; wake up and quit sdl
  (wake-sdl-port port (make-sdl-fini port))
  (wake-sdl-port port (make-sdl-quit port)))

;; client/server interactions
(defmethod exec-sdl-port ((port sdl-port) func)
  (with-slots (%sema %send %recv) port
    (%exec-sdl-port func %sema %send %recv)))
(defmethod wake-sdl-port ((port sdl-port) func)
  (with-slots (%sema %send %recv %wake) port
    (%wake-sdl-port func %sema %send %recv %wake)))
(defmethod wait-sdl-port ((port sdl-port))
  (with-slots (%send %recv) port
    (%wait-sdl-port %send %recv)))
(defmethod poll-sdl-port ((port sdl-port))
  (with-slots (%send %recv) port
    (%poll-sdl-port %send %recv)))
(defmethod invoke-with-port-locked ((port sdl-port) func)
  (if (eql (current-thread) (main-thread))
      (funcall func)
      (let ((standard-input *standard-input*)
	    (standard-output *standard-output*)
	    (error-output *error-output*)
	    (package *package*))
	(flet ((thunk ()
		   (let ((*standard-input* standard-input)
			 (*standard-output* standard-output)
			 (*error-output* error-output)
			 (*package* package))
		     (funcall func))))
	  (wake-sdl-port port #'thunk)))))

;; Thunks for the main loop
(defmethod make-sdl-hook ((port sdl-port))
  (lambda ()
    (format *trace-output* "~&")
    (format *trace-output* ";; Starting TTF~%")
    (sdl2::check-rc (ttf-init))
    (format *trace-output* ";; Starting SDL~%")
    (sdl2::check-rc (sdl-init *sdl-init-flags*))
    (format *trace-output* ";; Handshake SDL~%")
    (wait-sdl-port port)
    (format *trace-output* ";; Serving SDL~%")
    (with-simple-restart
	(sdl-quit "Quit main loop")
      (loop
	 (with-simple-restart
	     (sdl-continue "Continue main loop")
	   (process-next-event port :timeout nil))))))
(defmethod make-sdl-init ((port sdl-port))
  (with-slots (%mode %wait %wake) port
    (lambda ()
      (setf %mode (alloc-collected sdl-display-mode))
      (sdl2::check-rc (sdl-get-desktop-display-mode 0 %mode))
      (format *trace-output* ";; Display 0: ~t~dx~d~%"
	      (c-ref %mode sdl-display-mode :w)
	      (c-ref %mode sdl-display-mode :h))
      (setf %wait (alloc-collected sdl-event))
      (setf %wake (alloc-collected sdl-event))
      (setf (c-ref %wake sdl-event :type) (sdl2::check-rc (sdl-register-events 1)))
      (format *trace-output* ";; Allocated SDL user event: ~t~d~%" (c-ref %wake sdl-event :type)))))
(defmethod make-sdl-fini ((port sdl-port))
  (with-slots (%wait %wake) port
    (lambda ()
      (format *trace-output* ";; Closing SDL fonts~%")
      (loop 
	 for style being each hash-key of (ts->font port) 
	 using (hash-value font) do
	   (handler-case
	       (unwind-protect
		    (without-exceptions (ttf-close-font font))
		 (remhash style (ts->font port)))
	     (error (e) (format *trace-output* ";; Error closing SDL fonts ~a~%" e)))))))
(defmethod make-sdl-quit ((port sdl-port))
  (lambda ()
    (with-slots (%wait %wake) port
      (format *trace-output* ";; Freeing SDL event structures~%")
      (setf (c-ref %wake sdl-event :type) #xdeadbeef)
      (setf %wake nil)
      (setf %wait nil)
      (format *trace-output* ";; Shutting down SDL~%")
      (without-exceptions (sdl-quit))
      (format *trace-output* ";; Shutting down TTF~%")
      (without-exceptions (ttf-quit))
      (invoke-restart 'abort))))

;; TTF fonts
(defparameter *ttf-font-path* #p"/System/Library/Fonts/Monaco.dfont" #+nil #p"/Library/Fonts/Skia.ttf")
(defparameter *ttf-font-size* 12)
(defgeneric port-find-font (port style))
(defmethod port-find-font ((port sdl-port) style)
  (with-slots (%ts->font) port
    (or (gethash style %ts->font)
	(setf (gethash style %ts->font)
	      (sdl2::check-null 
	       (ttf-open-font 
		(namestring *ttf-font-path*) 
		(* 2 *ttf-font-size*)))))))

(defmethod port-register-window ((port sdl-port) mirror window)
  (with-slots (%id->window) port
    (setf (gethash mirror %id->window) window)))

(defmethod port-unregister-window ((port sdl-port) mirror window)
  (with-slots (%id->window) port
    (remhash mirror %id->window)))

(defmethod port-lookup-window ((port sdl-port) mirror)
  (with-slots (%id->window) port
    (gethash mirror %id->window)))

(defmethod port-register-render ((port sdl-port) mirror render)
  (with-slots (%id->render) port
    (setf (gethash mirror %id->render) render)))

(defmethod port-unregister-render ((port sdl-port) mirror render)
  (with-slots (%id->render) port
    (remhash mirror %id->render)))

(defmethod port-lookup-render ((port sdl-port) mirror)
  (with-slots (%id->render) port
    (gethash mirror %id->render)))

(defmethod realize-mirror ((port sdl-port) (sheet mirrored-sheet-mixin))
  (dispatch-repaint (sheet-mirrored-ancestor sheet) +everywhere+))

(defmethod destroy-mirror ((port sdl-port) (sheet mirrored-sheet-mixin))
  (dispatch-repaint (sheet-mirrored-ancestor sheet) +everywhere+))

(defmethod port-native-transformation (port (sheet mirrored-sheet-mixin))
  (sheet-mirror-transformation sheet))

(defmethod realize-mirror ((port sdl-port) (sheet top-level-sheet-pane))
  (unless (port-lookup-mirror port sheet)
    (with-port-locked (port)
      (with-bounding-rectangle* (x0 y0 x1 y1) (sheet-mirror-region sheet)
	(let* ((ww (floor (+ (- x1 x0) 0.5)))
	       (hh (floor (+ (- y1 y0) 0.5)))
	       (xx (floor (+ x0 0.5)))
	       (yy (floor (+ y0 0.5)))
	       (window (sdl2::check-null 
			(without-exceptions 
			  (sdl-create-window "SDL" xx yy ww hh *sdl-window-flags*))))
	       (format (sdl2::check-rc 
			(sdl-get-window-pixel-format window)))
	       (render (sdl2::check-null 
			(without-exceptions 
			  (sdl-create-renderer window -1 *sdl-renderer-flags*))))
	       (mirror (sdl-get-window-id window)))
	  (format *trace-output* ";; Window ~a: ~dx~d+~d+~d~%" window xx yy ww hh)
	  (format *trace-output* ";; Render ~a: ~a~%" render (sdl-get-pixel-format-name format))
	  (port-register-window port mirror window)
	  (port-register-render port mirror render)	
	  (port-register-mirror port sheet mirror)))))
  (port-lookup-mirror port sheet))

(defmethod destroy-mirror ((port sdl-port) (sheet top-level-sheet-pane))
  (let* ((mirror (port-lookup-mirror port sheet)))
    (when mirror
      (with-port-locked (port)
	(let ((render (port-lookup-render port mirror)))
	  (when render
	    (unwind-protect
		 (handler-case 
		     (without-exceptions (sdl-destroy-renderer render))
		   (condition (e) (format *trace-output* ";; sdl-destroy-renderer: ~a~%" e)))
	      (port-unregister-render port mirror render))))
	(let ((window (port-lookup-window port mirror)))
	  (when window 
	    (unwind-protect
		 (handler-case 
		     (without-exceptions (sdl-destroy-window window))
		   (condition (e) (format *trace-output* ";; sdl-destroy-window: ~a~%" e)))
	      (port-unregister-window port mirror window))))
	(port-unregister-mirror port sheet mirror)))))

(defmethod port-native-transformation ((port sdl-port) (sheet top-level-sheet-pane))
  +identity-transformation+)

(defmethod port-motion-hints ((port sdl-port) (sheet top-level-sheet-pane))
  nil)

(defmethod (setf port-motion-hints)
    (value (port sdl-port) (sheet top-level-sheet-pane))
  value)

(defmethod realize-mirror ((port sdl-port) (sheet unmanaged-top-level-sheet-pane))
  (unless (port-lookup-mirror port sheet)
    (with-port-locked (port)
      (with-bounding-rectangle* (x0 y0 x1 y1) (sheet-mirror-region sheet)
	(let* ((ww (floor (+ (- x1 x0) 0.5)))
	       (hh (floor (+ (- y1 y0) 0.5)))
	       (xx (floor (+ x0 0.5)))
	       (yy (floor (+ y0 0.5)))
	       (window (sdl2::check-null (without-exceptions (sdl-create-window "SDL" xx yy ww hh *sdl-popup-flags*))))
	       (render (sdl2::check-null (without-exceptions (sdl-create-renderer window -1 *sdl-renderer-flags*))))
	       (format (sdl2::check-rc (sdl-get-window-pixel-format window)))
	       (mirror (sdl-get-window-id window)))
	  (format *trace-output* ";; Window ~a: ~dx~d+~d+~d~%" window xx yy ww hh)
	  (format *trace-output* ";; Render ~a: ~a~%" render (sdl-get-pixel-format-name format))
	  (port-register-window port mirror window)
	  (port-register-render port mirror render)	
	  (port-register-mirror port sheet mirror)))))
  (port-lookup-mirror port sheet))

(defmethod sheet-window ((sheet basic-sheet))
  (let ((mirror (sheet-mirror sheet)))
    (when mirror
      (port-lookup-window (port sheet) mirror))))

(defmethod sheet-render ((sheet basic-sheet))
  (let ((mirror (sheet-mirror sheet)))
    (when mirror
      (port-lookup-render (port sheet) mirror))))

(defmethod realize-mirror ((port sdl-port) (pixmap pixmap))
  (when (null (port-lookup-mirror port pixmap))
    (with-port-locked (port)
      (let* ((ww (snap (pixmap-width pixmap)))
	     (hh (snap (pixmap-height pixmap)))
	     (sheet (pixmap-sheet pixmap))
	     (window (sheet-mirror sheet))
	     (format (sdl2::check-rc (sdl-get-window-pixel-format window)))
	     (sf *sdl-surface-flags*)
	     (pf *sdl-pixel-format*))
	(c-with ((bpp :int)
		 (rm uint32)
		 (gm uint32)
		 (bm uint32)
		 (am uint32))
	  (sdl2::check-true (sdl-pixel-format-enum-to-masks pf (bpp &) (rm &) (gm &) (bm &) (am &)))
	  (let* ((buffer (sdl2::check-null (sdl-create-rgb-surface sf ww hh bpp rm gm bm am)))
		 (render (sdl2::check-null (sdl-create-software-renderer buffer))))
	    (sdl2::check-rc (sdl-set-render-draw-blend-mode render +sdl-blendmode-none+))
	    (sdl2::check-rc (sdl-set-render-draw-color render 128 0 0 255))
	    (sdl2::check-rc (sdl-render-clear render))
	    (sdl-render-present render)
	    (format *trace-output* ";; Window ~a: ~dx~d@~a~%" buffer ww hh (sdl-get-pixel-format-name format))
	    (port-register-mirror port pixmap buffer))))))
  (port-lookup-mirror port pixmap))

(defmethod destroy-mirror ((port sdl-port) (pixmap pixmap))
  (let ((buffer (port-lookup-mirror port pixmap)))
    (when buffer
      (with-port-locked (port)
	(handler-case 
	    (unwind-protect 
		 (let ((render (sdl2::check-null (sdl-get-renderer buffer))))
		   (sdl-destroy-renderer render))
	      (sdl-free-surface buffer))
	  (error (e) (format *trace-output* ";; ~a~%" e))))
      (port-unregister-mirror port pixmap buffer))))

(defmethod port-allocate-pixmap ((port sdl-port) sheet width height)
  (let ((pixmap (make-instance 'mirrored-pixmap
			       :sheet sheet
			       :width width
			       :height height
			       :port port)))
    (when (sheet-grafted-p sheet)
      (realize-mirror port pixmap)
      (values pixmap))))

(defmethod port-deallocate-pixmap ((port sdl-port) pixmap)
  (when (port-lookup-mirror port pixmap)
    (destroy-mirror port pixmap)))

(defmethod copy-from-pixmap ((pixmap pixmap) buffer-x buffer-y width height (sheet mirrored-sheet-mixin) medium-x medium-y)
  (let ((buffer (sheet-mirror pixmap))
	(render (sheet-render sheet)))
    ;; surface -> texture
    (let* ((tx (sdl2::check-null (sdl-create-texture-from-surface render buffer))))
      (unwind-protect
	   ;; texture -> screen
	   (let ((sx (snap buffer-x))
		 (sy (snap buffer-y))
		 (ww (snap width))
		 (hh (snap height))
		 (dx (snap medium-x))
		 (dy (snap medium-y)))
	     (c-with ((sr sdl-rect)
		      (dr sdl-rect))
	       (setf (sr :x) sx (sr :y) sy (sr :w) ww (sr :h) hh)
	       (setf (dr :x) dx (dr :y) dy (dr :w) ww (dr :h) hh)
	       (sdl2::check-rc (sdl-set-texture-blend-mode tx +sdl-blendmode-none+))
	       (sdl2::check-rc (sdl-render-copy render tx sr dr))
	       (sdl2::check-rc (sdl-render-draw-rect render dr))))
	(sdl-destroy-texture tx)))))

(defmethod port-enable-sheet ((port sdl-port) (sheet mirrored-sheet-mixin))
  (let* ((port (port sheet))
	 (mirror (port-lookup-mirror port sheet)))
    (unless (null mirror)
      (let ((window (port-lookup-window port mirror)))
	(sdl-show-window window)))))

(defmethod port-disable-sheet ((port sdl-port) (sheet mirrored-sheet-mixin))
  (let* ((port (port sheet))
	 (mirror (port-lookup-mirror port sheet)))
    (unless (null mirror)
      (let ((window (port-lookup-window port mirror)))
	(sdl-hide-window window)))))

(defmethod port-create-buffer ((port sdl-port) mirror)
  (let* ((render (port-lookup-render port mirror))
	 (window (port-lookup-window port mirror))
	 (format (sdl-get-window-pixel-format window)))
    (c-with ((cr sdl-rect))
      (sdl-get-window-size window (cr :w &) (cr :h &))
      (let* ((ww (snap (cr :w)))
	     (hh (snap (cr :h)))
	     (tf *sdl-texture-flags*))
	(sdl2::check-null (sdl-create-texture render format tf ww hh))))))

(defmethod port-ensure-buffer ((port sdl-port) mirror)
  (let* ((render (port-lookup-render port mirror))
	 (buffer (sdl-get-render-target render)))
    (when (autowrap:wrapper-null-p buffer)
      (setf buffer (port-create-buffer port mirror))
      (sdl2::check-rc (sdl-set-render-target render buffer)))
    (values buffer)))

(defmethod port-destroy-buffer ((port sdl-port) mirror)
  (let* ((render (port-lookup-render port mirror))
	 (buffer (sdl-get-render-target render)))
    (unless (autowrap:wrapper-null-p buffer)
      (setf buffer (sdl-destroy-texture buffer))
      (sdl2::check-rc (sdl-set-render-target render buffer)))
    (values buffer)))

(defmethod port-sheet-region-changed ((port sdl-port) (sheet mirrored-sheet-mixin))
  (let ((mirror (port-lookup-mirror port sheet)))
    (when mirror
      (port-destroy-buffer port mirror))))

(defmethod dispatch-repaint :around ((sheet mirrored-sheet-mixin) region)
  (let* ((port (port sheet)))
    (cond ((port-lookup-mirror port sheet)
	   (call-next-method))
	  ((sheet-parent sheet)
	   (dispatch-repaint (sheet-parent sheet) +everywhere+))
	  (t
	   (call-next-method)))))

(defmethod handle-repaint :around ((sheet mirrored-sheet-mixin) region)
  (let* ((port (port sheet))
	 (mirror (port-lookup-mirror port sheet))
	 (render (port-lookup-render port mirror)))
    (if (null mirror)
	(call-next-method)
	(with-port-locked (port)
	  (sdl2::check-rc (sdl-set-render-draw-blend-mode render +sdl-blendmode-blend+))
	  (sdl2::check-rc (sdl-set-render-draw-color render 128 0 0 255))
	  (sdl2::check-rc (sdl-render-clear render))
	  (call-next-method)
	  (sdl-render-present render)))))

(defmethod repaint-sheet :around ((sheet mirrored-sheet-mixin) region)
  (let* ((port (port sheet))
	 (mirror (sheet-mirror sheet))
	 (render (port-lookup-render port mirror)))
    (if (null mirror)
	(call-next-method)
	(with-port-locked (port)
	  (c-with ((cr sdl-rect))
	    (with-bounding-rectangle* (x1 y1 x2 y2) 
		(region-intersection 
		 (sheet-native-region sheet)
		 (transform-region (sheet-native-transformation sheet) region))
	      (setf (cr :x) (snap x1))
	      (setf (cr :y) (snap y1))
	      (setf (cr :w) (snap (- x2 x1)))
	      (setf (cr :h) (snap (- y2 y1))))
	    (sdl2::check-rc (sdl-render-set-clip-rect render cr))
	    (call-next-method)
	    (sdl2::check-rc (sdl-render-set-clip-rect render nil)))))))

(defmethod make-graft
    ((port sdl-port) &key (orientation :default) (units :device))
  (with-slots (%mode) port
    (c-with ((mode sdl-display-mode :from %mode))
      (make-instance 'sdl-graft
		     :port port :mirror (gensym)
		     :region (make-bounding-rectangle 0 0 (mode :w) (mode :h))
		     :orientation orientation :units units))))

(defmethod make-medium ((port sdl-port) sheet)
  (make-instance 'sdl-medium :sheet sheet :port port))

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

(defmethod port-native-transformation ((port sdl-port) (sheet graft))
  +identity-transformation+)

(defmethod graft ((port sdl-port))
  (first (port-grafts port)))

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
  (loop
     for medium being each hash-key in (dirty-mediums port)
     do (medium-force-output medium)))

;; FIXME: What happens when CLIM code calls tracking-pointer recursively?

(defmethod port-grab-pointer ((port sdl-port) pointer sheet)
  (declare (ignore pointer))
  (setf (pointer-grab-sheet port) sheet))

(defmethod port-ungrab-pointer ((port sdl-port) pointer sheet)
  (declare (ignore pointer))
  (when (eq (pointer-grab-sheet port) sheet)
    (setf (pointer-grab-sheet port) nil)))

(defmethod distribute-event :around ((port sdl-port) event)
  (let ((sheet (pointer-grab-sheet port)))
    (if sheet
	(dispatch-event sheet event)
	(call-next-method))))

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

