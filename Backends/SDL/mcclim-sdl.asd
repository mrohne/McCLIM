
(defsystem #:mcclim-sdl
  :depends-on (#:clim
	       #:trivial-channels 
	       #:bordeaux-threads 
	       #:sdl2)
  :serial t
  :components
  ((:file "package")
   (:file "sdl2" :depends-on ("package"))
   (:file "medium" :depends-on ("package"))
   (:file "port" :depends-on ("package"))
   (:file "event" :depends-on ("package"))
   (:file "graft" :depends-on ("package"))
   (:file "frame-manager" :depends-on ("package"))))
