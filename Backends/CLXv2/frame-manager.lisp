(in-package :clim-clxv2)



(defclass clxv2-frame-manager (clim-clx::clx-frame-manager)
  ())


;; the panes to be mirrored
(defun get-mirroring-fn (port)
  (cond ((or (null (clxv2-port-mirroring port))
	     (eq (clxv2-port-mirroring port) :all-basic-panes))
	 #'(lambda (pane-class)
	     (subtypep pane-class 'basic-pane)))
	((eq (clxv2-port-mirroring port) :none)
	 #'(lambda (pane-class)
	     (subtypep pane-class 'top-level-sheet-pane)))
	((eq (clxv2-port-mirroring port) :gadgets)
	 #'(lambda (pane-class)
	     (or (subtypep pane-class 'top-level-sheet-pane)
		 (subtypep pane-class 'gadget))))
	(t
	 #'(lambda (pane-class)
	     (declare (ignore pane-class))
	     nil))))

;;; if the pane is a subclass of basic-pane and it is not mirrored we create a new class.
(defun maybe-mirroring (port concrete-pane-class)
  (when (and (not (subtypep concrete-pane-class 'mirrored-sheet-mixin))
	     (funcall (get-mirroring-fn port) concrete-pane-class))
    (let* ((concrete-pane-class-symbol (if (typep concrete-pane-class 'class)
                                          (class-name concrete-pane-class)
                                          concrete-pane-class))
	   (concrete-mirrored-pane-class (concatenate 'string
						      "CLXv2-"
						      (symbol-name concrete-pane-class-symbol)
						      "-DUMMY"))
	   (concrete-mirrored-pane-class-symbol (find-symbol concrete-mirrored-pane-class
							     :clim-clxv2))
	   (superclasses (if (subtypep concrete-pane-class 'sheet-with-medium-mixin)
			     (list 'clxv2-mirrored-sheet-mixin
				   concrete-pane-class-symbol)
			     (list 'clxv2-mirrored-sheet-mixin
				   ;;'temporary-medium-sheet-output-mixin
				   'permanent-medium-sheet-output-mixin
				   concrete-pane-class-symbol))))
      (format *debug-io* "use dummy mirrored class ~A ~A~%" (clxv2-port-mirroring port) concrete-mirrored-pane-class)
      (unless concrete-mirrored-pane-class-symbol
	(setf concrete-mirrored-pane-class-symbol
	      (intern concrete-mirrored-pane-class :clim-clxv2))
	(eval
	 `(defclass ,concrete-mirrored-pane-class-symbol
	      ,superclasses
	    ()
	    (:metaclass ,(type-of (find-class concrete-pane-class-symbol)))))
	(format *debug-io* "create class ~A~%" concrete-mirrored-pane-class-symbol))
      (setf concrete-pane-class (find-class concrete-mirrored-pane-class-symbol))))
  concrete-pane-class)

(defmethod make-pane-1 ((fm clxv2-frame-manager) (frame application-frame) type &rest args)
  (apply #'make-instance
	 (maybe-mirroring (port fm) (clim-clx::find-concrete-pane-class type))
	 :frame frame
	 :manager fm
	 :port (port frame)
	 args))
