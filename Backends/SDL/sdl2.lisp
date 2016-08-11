(in-package :clim-sdl)

;; Round and scale coordinate
(declaim (inline snap unsnap))
(defun snap (x)
  (floor (+ (* 2.0 x) .5)))
(defun unsnap (x)
  (* 0.5 x))

;; Convenience parameters
(defparameter *default-server-path* (list :sdl))
(defparameter *sdl-init-flags* 
  (logior +sdl-init-events+ +sdl-init-video+))
(defparameter *sdl-window-flags* 
  (logior +sdl-window-hidden+ 
	  +sdl-window-opengl+
	  +sdl-window-resizable+
	  +sdl-window-allow-highdpi+))
(defparameter *sdl-popup-flags* 
  (logior +sdl-window-shown+ 
	  +sdl-window-opengl+
	  +sdl-window-borderless+
	  +sdl-window-mouse-focus+
	  +sdl-window-allow-highdpi+))
(defparameter *sdl-renderer-flags* 
  (logior +sdl-renderer-accelerated+
	  +sdl-renderer-targettexture+
	  +sdl-renderer-presentvsync+))
(defparameter *sdl-texture-flags* 
  (logior +sdl-textureaccess-target+))
(defparameter *sdl-surface-flags*
  (logior +sdl-swsurface+))
(defparameter *sdl-pixel-format*
  +sdl-pixelformat-argb8888+)

;; SEMAPHORE - serialized access to the main thread
(defstruct semaphore
  (count 1 :type fixnum)
  (%cond (bt:make-condition-variable))
  (%lock (bt:make-lock)))
(defmacro with-semaphore-down (semaphore &body body)
  `(progn 
     (bt:with-lock-held ((semaphore-%lock ,semaphore))
       (loop 
	  while (< (semaphore-count ,semaphore) 1) do
	    (bt:condition-wait (semaphore-%cond ,semaphore) 
			       (semaphore-%lock ,semaphore)))
       (decf (semaphore-count ,semaphore) 1))
     (unwind-protect
	  (progn ,@body)
       (bt:with-lock-held ((semaphore-%lock ,semaphore))
	 (incf (semaphore-count ,semaphore) 1)
	 (bt:condition-notify (semaphore-%cond ,semaphore))))))

;; MAILBOX - aka one-slot message queue
(defstruct mailbox
  (empty t)
  (message nil)
  (%cond (bt:make-condition-variable))
  (%lock (bt:make-lock)))
(defun push-message (mailbox message)
  (bt:with-lock-held ((mailbox-%lock mailbox))
    (loop 
       until (mailbox-empty mailbox) do
	 (bt:condition-wait (mailbox-%cond mailbox) 
			    (mailbox-%lock mailbox)))
    (setf (mailbox-message mailbox) message)
    (setf (mailbox-empty mailbox) nil)
    (bt:condition-notify (mailbox-%cond mailbox))))
(defun wait-message (mailbox)
  (bt:with-lock-held ((mailbox-%lock mailbox))
    (loop 
       while (mailbox-empty mailbox) do
	 (bt:condition-wait (mailbox-%cond mailbox) 
			    (mailbox-%lock mailbox)))
    (shiftf (mailbox-empty mailbox) t)
    (shiftf (mailbox-message mailbox) nil)))
(defun poll-message (mailbox)
  (bt:with-lock-held ((mailbox-%lock mailbox))
    (unless (mailbox-empty mailbox)
      (shiftf (mailbox-empty mailbox) t)
      (shiftf (mailbox-message mailbox) nil))))

;; Threads and FFI helpers
(declaim (inline main-thread current-thread))
(defun main-thread ()
  "Return the initial thread"
  #+(or sbcl)
  (sb-thread:main-thread)
  #+(not (or sbcl))
  (find "main thread" (bt:all-threads) :key #'bt:thread-name :test #'string=))
(defun current-thread ()
  "Return the initial thread"
  #+(or sbcl)
  sb-thread:*current-thread*
  #+(not (or sbcl))
  (bt:current-thread))
;; Shamelessly stolen from cl-glut
(defmacro without-interrupts (&body body)
  "Execute BODY with interrupts disabled"
  #+(and sbcl)
  `(sb-sys:without-interrupts ,@body)
  #-(and sbcl)
  `(progn ,@body))
(defmacro without-fp-traps (&body body)
  "Execute BODY with floating point traps disabled"
  #+(and sbcl (or x86 x86-64))
  `(sb-int:with-float-traps-masked (:invalid :inexact :divide-by-zero :overflow) ,@body)
  #-(and sbcl (or x86 x86-64))
  `(progn ,@body))
(defmacro without-exceptions (&body body)
  "Execute BODY without any exceptions"
  `(without-interrupts (without-fp-traps ,@body)))
(defmacro alloc-collected (foreign-type)
  "Allocate foreign memory with finalization"
  `(autowrap:autocollect (ptr) 
       (autowrap:alloc ',foreign-type)
     (autowrap:free ptr)))

;; indexed surface
(defun sdl-create-indexed-surface (pixels colors)
  (destructuring-bind (hh ww) (array-dimensions pixels)
    #+trace(format *trace-output* ";; create surface...~%")
    (let ((surface 
	   (autowrap:autocollect (ptr)
	       (c-with ((dp :int) (rm uint32) (gm uint32) (bm uint32) (am uint32))
		 (sdl2::check-rc (sdl-pixel-format-enum-to-masks +sdl-pixelformat-index8+ (dp &) (rm &) (gm &) (bm &) (am &)))
		 (sdl2::check-null (sdl-create-rgb-surface 0 ww hh dp rm gm bm am)))
	     (sdl-free-surface ptr))))
      (c-with ((cs uint8 :count (* 256 4) :free t))
	#+trace(format *trace-output* ";; copy colors to ~a...~%" (cs &))
	(loop 
	   for jj below 256
	   for (r g b a) across colors do
	     (setf (c-aref (cs &) (+ (* jj 4) 0) :unsigned-char) (min (floor r 1/256) 255))
	     (setf (c-aref (cs &) (+ (* jj 4) 1) :unsigned-char) (min (floor g 1/256) 255))
	     (setf (c-aref (cs &) (+ (* jj 4) 2) :unsigned-char) (min (floor b 1/256) 255))
	     (setf (c-aref (cs &) (+ (* jj 4) 3) :unsigned-char) (min (floor a 1/256) 255)))
	#+trace(format *trace-output* ";; install palette...~%")
	(let ((palette (sdl2::check-null (sdl-alloc-palette 256))))
	  (unwind-protect
	       (progn
		 (sdl2::check-rc (sdl-set-palette-colors palette (cs &) 0 256))
		 (sdl2::check-rc (sdl-set-surface-palette surface palette)))
	    (sdl-free-palette palette))))
      (c-with ((px uint8 :count (* hh ww) :free nil :from (c-ref surface sdl-surface :pixels)))
	#+trace(format *trace-output* ";; copy pixels to ~a...~%" (px &))
	(loop
	   with dd = (c-ref surface sdl-surface :pitch)
	   for jj below hh do
	     (loop for ii below ww do
		  (setf (c-aref (px &) (+ (* jj dd) ii) :unsigned-char) 
			(aref pixels jj ii)))))
      (values surface))))

;; event queue
(defun sdl-next-event (wait timeout)
  (cond 
    ((null timeout)
     (without-exceptions (sdl-wait-event-timeout wait (floor 0.01 0.001))))
    ((zerop timeout)
     (without-exceptions (sdl-poll-event wait)))
    ((plusp timeout)
     (without-exceptions (sdl-wait-event-timeout wait (floor timeout 0.001))))
    (t
     (error "Bad timeout value: ~a" timeout))))

;; client/server support
(defun %exec-sdl-port (func sema send recv)
  (with-semaphore-down sema
    (push-message send func)
    (%recv-sdl-vals (wait-message recv))))
(defun %wake-sdl-port (func sema send recv wake)
  (with-semaphore-down sema
    (push-message send func)
    (sdl2::check-rc (sdl-push-event wake))
    (%recv-sdl-vals (wait-message recv))))
(defun %wait-sdl-port (send recv)
  (let ((func (wait-message send)))
    (unless (null func)
      (%call-sdl-func func recv))))
(defun %poll-sdl-port (send recv)
  (let ((func (poll-message send)))
    (unless (null func)
      (%call-sdl-func func recv))))
(defun %call-sdl-func (func recv)
  "Call FUNC, send multiplexed values/conditions to RECV"
  (let ((vals nil))
    (unwind-protect
	 (handler-case
	     (setf vals (multiple-value-list (funcall func)))
	   (error (state)
	     (format *trace-output* ";; SDL error: ~a~%" state)
	     (sb-debug:backtrace)
	     (setf vals state))
	   (condition (state) 
	     (format *trace-output* ";; SDL condition: ~a~%" state)
	     (sb-debug:backtrace)
	     (setf vals state)))
      (push-message recv vals))))
(defun %recv-sdl-vals (vals)
  "Receive VALS, demultiplex values and conditions"
  (etypecase vals
    (null (values-list vals))
    (cons (values-list vals))
    (error (cerror "Ignore message" "SDL error: ~a" vals))
    (condition (format *trace-output* ";; SDL condition: ~a~%" vals))))
