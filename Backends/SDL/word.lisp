(in-package :clim-user)
;; lifted from <http://www.kantz.com/clim-primer/drawing.htm>
(define-application-frame word		;name
     ()					;superclasses
  ((doc-title				;slots
    :accessor doc-title
    :initarg :doc-title))
   ;; options
   (:panes
    (title				;pane name 
     :application			;pane type
     ;; pane options
     :display-function #'display-doc-title
     :foreground +blue+
     :background +pink+
     :initial-cursor-visibility nil)
    (document				;pane name 
     :application			;pane type
     ;; pane options
     :display-function #'display-doc-body
     ))
   (:layouts
    (default				;name of the layout
	(vertically ()			;layout macros
	  (1/4 title)
	  (3/4 document)))))
(defmethod display-doc-title ((frame word) stream)
  (format stream "Document: ")
  (if (slot-boundp frame 'doc-title)
      (format stream (doc-title frame))
      (format stream "Untitled")))
(defmethod display-doc-body ((frame word) stream)
  (draw-line* stream 0 0 100 100 :ink +green+)
  (draw-line* stream 1 0 100  99 :ink +green+)
  (draw-line* stream 0 1  99 100 :ink +green+)
  (draw-text* stream "Hello" 10 180 :ink +blue+)
  (format stream "Howdy?")
  (terpri stream)
  (format stream "Now what?")) 
(defun word ()
  (run-frame-top-level
   (make-application-frame 'word :height 500 :width 500
			   :doc-title "Microcrock's Business Plan")))
