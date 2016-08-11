(in-package :clim-sdl)
(declaim (optimize (debug 3) (safety 3)))

(defmethod mirrored-at-position ((sheet null) x y)
  "Depth-first search for child overlapping position"
  (values sheet x y))

(defmethod mirrored-at-position ((sheet basic-sheet) x y)
  "Depth-first search for child overlapping position"
  (loop
     for child in (sheet-children sheet)
     when (region-contains-position-p (sheet-mirror-region child) x y) do
       (return (mirrored-at-position child x y))))

(defmethod mirrored-at-position ((sheet mirrored-sheet-mixin) x y)
  "Depth-first search for child overlapping position"
  (or (call-next-method)
      (values sheet x y)))

(defmethod make-event (class sheet x y &rest rest)
  "Depth-first search for child overlapping position"
  (let ((sheet (mirrored-at-position sheet x y)))
    ;; TODO: search upward for matching event mask
    (apply #'make-instance class :sheet sheet :x x :y y rest)))
    
(defmethod sdl-keyboard ((port sdl-port))
  (with-slots (%wait) port
    (c-let ((wait sdl-event :from %wait))
      (let* ((mirror (wait :key :window-id))
	     (sheet (port-lookup-sheet port mirror))
	     (focus (port-keyboard-input-focus port))
	     (target (or focus sheet)))
	(if (null target)
	    (format *trace-output* ";; No target for keyboard event: #x~8,'0x ~1x~%"
		    (wait :key :window-id) (wait :key :keysym))
	    (let* ((event (cond ((eql (wait :type) +sdl-keydown+) 'key-press-event)
				((eql (wait :type) +sdl-keyup+) 'key-release-event)))
		   (keysym (wait :key :keysym))
		   (scancode (sdl2:scancode keysym))
		   (keycode (sdl2:sym-value keysym))
		   (keyname (sdl2:get-key-name keycode)))
	      (make-event event target 0 0 
			  :key-name scancode :key-character keyname)))))))
(defmethod process-sdl-mousemotion ((port sdl-port) event)
  (c-let ((event sdl-event :from event))
    (let* ((xx (event :motion :x))
	   (yy (event :motion :y))
	   (mirror (event :motion :window-id))
	   (sheet (port-lookup-sheet port mirror))
	   (child (mirrored-at-position sheet xx yy))
	   (pointer (port-pointer port)))
      (with-transformed-position 
	  ((if sheet (sheet-mirror-transformation sheet) +identity-transformation+) xx yy)
	(setf (slot-value pointer 'x) xx)
	(setf (slot-value pointer 'y) yy))
      (if (eql child (port-pointer-sheet port))
	  (when (pointer-grab-sheet port)
	    (distribute-event port (make-instance 'pointer-motion-event
						  :sheet (pointer-grab-sheet port)
						  :x xx :y yy :pointer pointer
						  :modifier-state 0)))
	  (progn
	    (when (port-pointer-sheet port)
	      (distribute-event port (make-instance 'pointer-exit-event
						    :sheet (port-pointer-sheet port)
						    :x xx :y yy 
						    :modifier-state 0)))
	    (setf (port-pointer-sheet port) child)
	    (when (port-pointer-sheet port)
	      (distribute-event port (make-instance 'pointer-enter-event
						    :sheet (port-pointer-sheet port)
						    :x xx :y yy 
						    :modifier-state 0))))))))

(defmethod process-sdl-buttonevent ((port sdl-port) event)
  (c-let ((event sdl-event :from event))
    (let* ((xx (event :button :x))
	   (yy (event :button :y))
	   (button (cond ((eql (event :button :button) 1) +pointer-left-button+)
			 ((eql (event :button :button) 2) +pointer-middle-button+)
			 ((eql (event :button :button) 3) +pointer-right-button+)))
	   (state 0)
	   (mirror (event :motion :window-id))
	   (sheet (port-lookup-sheet port mirror))
	   (child (mirrored-at-position sheet xx yy))
	   (pointer (port-pointer port)))
      (with-transformed-position 
	  ((if sheet (sheet-mirror-transformation sheet) +identity-transformation+) xx yy)
	(setf (slot-value pointer 'x) xx)
	(setf (slot-value pointer 'y) yy))
      (setf (port-pointer-sheet port) child)
      (cond ((eql (event :type) +sdl-mousebuttondown+)
	     (unless (pointer-grab-sheet port)
	       (setf (pointer-grab-sheet port) (port-pointer-sheet port)))
	     (when (port-pointer-sheet port)
	       (distribute-event port (make-instance 'pointer-button-press-event
						     :sheet (port-pointer-sheet port)
						     :x xx :y yy :pointer pointer
						     :button button
						     :modifier-state state))))
	    ((eql (event :type) +sdl-mousebuttonup+)
	     (when (port-pointer-sheet port)
	       (distribute-event port (make-instance 'pointer-button-release-event
						     :sheet (port-pointer-sheet port)
						     :x xx :y yy :pointer pointer
						     :button button
						     :modifier-state state)))
	     (when (pointer-grab-sheet port)
	       (distribute-event port (make-instance 'pointer-ungrab-event
						     :sheet (pointer-grab-sheet port)
						     :x xx :y yy :pointer pointer
						     :button button
						     :modifier-state state))
	       (setf (pointer-grab-sheet port) nil)))))))

(defmethod process-sdl-windowevent ((port sdl-port) event)
  (c-let ((event sdl-event :from event))
    (let* ((mirror (event :window :window-id))
	   (sheet (port-lookup-sheet port mirror))
	   (window (port-lookup-window port mirror)))
      (cond
	( ;; shown
	 (eql (event :window :event) +sdl-windowevent-shown+)
	 (distribute-event port (make-instance 'window-repaint-event :sheet sheet :region +everywhere+)))
	( ;; hidden
	 (eql (event :window :event) +sdl-windowevent-hidden+)
	 (distribute-event port (make-instance 'window-unmap-event :sheet sheet :region +everywhere+)))
	( ;; exposed
	 (eql (event :window :event) +sdl-windowevent-exposed+)
	 (distribute-event port (make-instance 'window-repaint-event :sheet sheet :region +everywhere+)))
	( ;; moved
	 (eql (event :window :event) +sdl-windowevent-moved+)
	 (multiple-value-bind (xx yy) (sdl2:get-window-position window)
	   (multiple-value-bind (ww hh) (sdl2:get-window-size window)
	     (distribute-event port (make-instance 'window-configuration-event :sheet sheet 
						   :x xx :y yy :width ww :height hh :region +everywhere+)))))
	( ;; resized
	 (eql (event :window :event) +sdl-windowevent-resized+)
	 (multiple-value-bind (xx yy) (sdl2:get-window-position window)
	   (multiple-value-bind (ww hh) (sdl2:get-window-size window)
	     (distribute-event port (make-instance 'window-configuration-event :sheet sheet 
						   :x xx :y yy :width ww :height hh :region +everywhere+)))))
	( ;; size-changed
	 (eql (event :window :event) +sdl-windowevent-size-changed+)
	 (multiple-value-bind (ww hh) (sdl2:get-window-size window)
	   (multiple-value-bind (xx yy) (sdl2:get-window-position window)
	     (distribute-event port (make-instance 'window-configuration-event :sheet sheet 
						   :x xx :y yy :width ww :height hh :region +everywhere+)))))
	( ;; minimized
	 (eql (event :window :event) +sdl-windowevent-minimized+)
	 (distribute-event port (make-instance 'window-unmap-event :sheet sheet)))
	( ;; restored
	 (eql (event :window :event) +sdl-windowevent-restored+)
	 (distribute-event port (make-instance 'window-repaint-event :sheet sheet :region +everywhere+)))
	( ;; enter
	 (eql (event :window :event) +sdl-windowevent-enter+)
	 (setf (port-pointer-sheet port) sheet)
	 (when (port-pointer-sheet port)
	   (let ((xx (event :window :data1))
		 (yy (event :window :data2)))
	     (distribute-event port (make-instance 'pointer-enter-event
						   :sheet (port-pointer-sheet port) :x xx :y yy
						   :modifier-state 0)))))
	( ;; leave
	 (eql (event :window :event) +sdl-windowevent-leave+)
	 (when (port-pointer-sheet port)
	   (let* ((pointer (port-pointer port))
		  (xx (or (slot-value pointer 'x) (event :window :data1)))
		  (yy (or (slot-value pointer 'y) (event :window :data2))))
	     (distribute-event port (make-instance 'pointer-exit-event
						   :sheet (port-pointer-sheet port) :x xx :y yy
						   :modifier-state 0)))
	 (setf (port-pointer-sheet port) nil)))
	( ;; focus-gained
	 (eql (event :window :event) +sdl-windowevent-focus-gained+)
	 (setf (port-keyboard-input-focus port) sheet)
	 (values nil))
	( ;; focus-lost
	 (eql (event :window :event) +sdl-windowevent-focus-lost+)
	 (setf (port-keyboard-input-focus port) nil)
	 (values nil))
	( ;; close
	 (eql (event :window :event) +sdl-windowevent-close+)
	 (distribute-event port (make-instance 'window-destroy-event :sheet sheet)))
	( ;; unknown
	 (format *trace-output* ";; Discarding window event: #x~8,'0x ~1x~%" 
		 (event :window :window-id) (event :window :event)))))))

(defmethod sdl-quitevent ((port sdl-port))
  (loop for graft in (port-grafts port) do
       (loop for sheet in (sheet-children graft) do
	    (return-from sdl-quitevent
	      (make-instance 'window-manager-delete-event :sheet sheet)))))

(defmethod process-next-event ((port sdl-port) &key wait-function (timeout nil))
  (declare (ignore wait-function))
  (with-slots (%send %recv %wake %wait) port
    (c-let ((wake sdl-event :from %wake)
	    (wait sdl-event :from %wait))
      (loop
	 (let ((rc (sdl-next-event %wait timeout)))
	   (when (zerop rc)
	     (return-from process-next-event (values nil :timeout)))
	   #+debug(format *trace-output* ";; Received event: ~a~%" (sdl2:get-event-type %wait))
	   (cond 
	     ( ;; finger events not implemented
	      (or (eql (wait :type) +sdl-fingerdown+)
		  (eql (wait :type) +sdl-fingerup+)
		  (eql (wait :type) +sdl-fingermotion+))
	      (return-from process-next-event (values nil :finger)))
	     ( ;; gesture events not implemented
	      (or (eql (wait :type) +sdl-dollargesture+)
		  (eql (wait :type) +sdl-dollarrecord+)
		  (eql (wait :type) +sdl-multigesture+))
	      (return-from process-next-event (values nil :gesture)))
	     ( ;; keyboard event
	      (or (eql (wait :type) +sdl-keydown+) (eql (wait :type) +sdl-keyup+))
	      (let ((event (sdl-keyboard port)))
		(unless (null event)
		  (distribute-event port event))
		(return-from process-next-event (values event :keyboard))))
	     ( ;; pointer moved
	      (eql (wait :type) +sdl-mousemotion+)
	      (return-from process-next-event (process-sdl-mousemotion port %wait)))
	     ( ;; button event
	      (or (eql (wait :type) +sdl-mousebuttondown+) (eql (wait :type) +sdl-mousebuttonup+))
	      (return-from process-next-event (process-sdl-buttonevent port %wait)))
	     ( ;; window event
	      (eql (wait :type) +sdl-windowevent+)
	      (return-from process-next-event (process-sdl-windowevent port %wait)))
	     ( ;; quit event
	      (eql (wait :type) +sdl-quit+)
	      (let ((event (sdl-quitevent port)))
		(unless (null event)
		  (distribute-event port event))
		(return-from process-next-event (values event :quit))))
	     ( ;; lisp event
	      (eql (wait :type) (wake :type))
	      (return-from process-next-event (poll-sdl-port port)))
	     (t
	      (format *trace-output* ";; Discarding event: #x~4,'0x~%" (wait :type))
	      (return-from process-next-event (values nil :discard)))))))))

(defmethod distribute-event :after ((port sdl-port) (event window-configuration-event))
  (climi::dispatch-repaint (event-sheet event) (window-event-region event)))



