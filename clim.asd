;;; -*- Mode: Lisp -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000, 2014 by 
;;;           Robert Strandh (robert.strandh@gmail.com)
;;;  (c) copyright 2005 by
;;;           Andreas Fuchs (asf@boinkor.net)
;;;
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


;;; Really, I wouldn't bother with anything but ASDF. Almost every lisp
;;; ships with it, and it has the added benefit of ASDF-INSTALL.
;;; Get ASDF, and be welcome to the 21st century. -- [2005-01-31:asf]

(defpackage :clim.system
  (:use :asdf :cl))
(in-package :clim.system)

(defparameter *clim-directory* (directory-namestring *load-truename*))
(proclaim '(optimize (debug 3) (safety 3) (space 0) (speed 0)))

;;; Load SWANK
#+common-lisp#.
(ignore-errors 
  (require 'swank))

;;; Make CLX asdf-loadable on Allegro 6.2
#+allegro#.
(progn
  (defsystem :clx
    :components ((:file "require-clx"))))

;;; Clozure CL native GUI stuff
#+ccl
(progn
  #+darwin
  (ignore-errors 
    (require 'cocoa)
    (pushnew :clim-beagle *features*)))

;;; SBCL on Darwin - use SDL
#+sbcl
(progn
  #+darwin
  (ignore-errors
    (pushnew :clim-sdl *features*)))

(defsystem :clim-system
  :components 
  ((:module "System"
	    :components
	    ((:file "patch")
	     (:module "Fix"
		      :components
		      ((:file   #+cmu       "fix-cmu"
				#+scl       "fix-scl"
				#+excl      "fix-acl"
				#+sbcl      "fix-sbcl"
				#+openmcl   "fix-openmcl"
				#+lispworks "fix-lispworks"
				#+clisp     "fix-clisp")))
	     (:file "package")
	     (:module "MP"
		      :components
		      ((:file #.(first
				 (list
				  #+cmu                     "mp-cmu"
				  #+scl                     "mp-scl"
				  #+sb-thread               "mp-sbcl"
				  #+excl                    "mp-acl"
				  #+openmcl                 "mp-openmcl"
				  #+lispworks               "mp-lw"
				  #| fall back |#           "mp-nil")))))))))

(defsystem :clim-base
  :depends-on (:clim-system :spatial-trees (:version "flexichain" "1.5.1"))
  :serial t
  :components ((:file "decls")
	       (:file "protocol-classes")
	       (:module "CLIM/Base"
			:components
			((:file "utils")
			 (:file "coordinates")
			 (:file "setf-star")
			 (:file "transforms")
			 (:file "dead-keys")
			 (:file "design")
			 (:file "regions")
			 (:file "X11-colors")
			 (:file "sheets")
			 (:file "pixmap")
			 (:file "events")
			 (:file "ports")
			 (:file "grafts")
			 (:file "medium")
			 (:file "output")
			 (:file "input")
			 (:file "repaint")
			 (:file "graphics")
			 (:file "views")
			 (:file "stream-output")
			 (:file "recording")
			 (:file "encapsulate")
			 (:file "stream-input")
			 (:file "text-selection")
			 (:file "bezier")))))

(defsystem :goatee-core
  :depends-on (:clim-base)
  :components
  ((:module "Goatee"
	    :components
	    ((:file "conditions")
	     (:file "dbl-list")
	     (:file "flexivector")
	     (:file "buffer")
	     (:file "editable-buffer")
	     (:file "editable-area")
	     (:file "clim-area")
	     (:file "kill-ring")
	     (:file "goatee-command")
	     (:file "editing-stream")
	     (:file "presentation-history")))))

(defsystem :clim-postscript
  :depends-on (:clim-base)
  :components
  ((:module "Backends/PostScript"
            :components
            ((:file "package")
             (:file "encoding")
             (:file "paper")
             (:file "class")
             (:file "font")
             (:file "graphics")
             (:file "sheet")
             (:file "afm")
             (:file "standard-metrics")))))

(defsystem :clim-core
  :depends-on (:clim-base :goatee-core :clim-postscript)
  :components 
  ((:module "CLIM/Core"
	    :components 
	    ((:file "text-formatting")
	     (:file "defresource")
	     (:file "presentations")
	     (:file "xpm")
	     (:file "bordered-output")
	     (:file "table-formatting")
	     (:file "input-editing")
	     (:file "pointer-tracking")
	     (:file "graph-formatting")
	     (:file "frames")
	     (:file "presentation-defs")
	     (:file "dialog-views")
	     (:file "describe")
	     (:file "commands")
	     (:file "incremental-redisplay")
	     (:file "panes")
	     (:file "gadgets")
	     (:file "menu-choose")
	     (:file "menu")
	     (:file "dialog")
	     (:file "builtin-commands")))))

(defsystem :esa-mcclim
  :depends-on (:clim-core)
  :components 
  ((:module "ESA"
	    :components 
	    ((:file "packages")
	     (:file "utils")
	     (:file "colors")
	     (:file "esa")
	     (:file "esa-buffer")
	     (:file "esa-io")
	     (:file "esa-command-parser")))))



(defsystem :drei-mcclim
  :depends-on ((:version "flexichain" "1.5.1") :esa-mcclim :clim-core)
  :components
  ((:module "Drei"
	    :components
	    ((:module "cl-automaton"
		      :components ((:file "automaton-package")
				   (:file "eqv-hash")
				   (:file "state-and-transition")
				   (:file "automaton")
				   (:file "regexp")))
	     (:module "Persistent"
		      :components ((:file "binseq-package")
				   (:file "binseq")
				   (:file "obinseq")
				   (:file "binseq2")))
	     (:file "packages")
	     (:file "buffer")
	     (:file "delegating-buffer")
	     (:file "Persistent/persistent-buffer")
	     (:file "motion")
	     (:file "editing")
	     (:file "base")
	     (:file "syntax")
	     (:file "modes")
	     (:file "views")
	     (:file "drei")
	     (:file "drei-clim")
	     (:file "drei-redisplay")
	     (:file "drawing-options")
	     (:file "input-editor")
	     (:file "abbrev")
	     (:file "kill-ring")
	     (:file "undo")
	     (:file "Persistent/persistent-undo")
	     (:file "basic-commands")
	     (:file "core")
	     (:file "fundamental-syntax")
	     (:file "buffer-streams")
	     (:file "rectangle")
	     (:file "targets")
	     (:file "core-commands")
	     (:file "misc-commands")
	     (:file "search-commands")
	     (:file "lr-syntax")
	     (:file "lisp-syntax")
	     (:file "lisp-syntax-swine")
	     (:file "lisp-syntax-commands")
	     #+swank (:file "lisp-syntax-swank")))))

(defsystem :drei-tests
  :depends-on (:drei-mcclim :fiveam)
  :components
  ((:module "Drei/Tests"
            :components 
            ((:module
              "cl-automaton"
             
              :components
              ((:file "automaton-tests")
               (:file "state-and-transition-tests")
               (:file "eqv-hash-tests")
               (:file "regexp-tests")))
             (:file "packages")
             (:file "testing")
             (:file "buffer-tests")
             (:file "base-tests")
             (:file "kill-ring-tests")
             (:file "motion-tests")
             (:file "editing-tests")
             (:file "core-tests")
             (:file "buffer-streams-tests")
             (:file "rectangle-tests")
             (:file "undo-tests")
             (:file "lisp-syntax-tests")
             (:file "lisp-syntax-swine-tests")))))

(defsystem :clim-input
  :depends-on (:clim-core :goatee-core :esa-mcclim :drei-mcclim)
  :components
  ((:file "input-editing-goatee")
   (:file "input-editing-drei")
   (:file "text-editor-gadget")
   (:file "Extensions/tab-layout")))

(defsystem :clim-sdl
    :depends-on (:clim-core :sdl2)
    :components
    ((:module "Backends/SDL"
              :components
              ((:file "package")
               (:file "port")
               (:file "medium")
               (:file "graft")
               (:file "frame-manager")))))

(defsystem :clim-clx
  :depends-on (:clim-core :clx)
  :components
  ((:module "Backends/CLX"
    :components
    ((:file "package")
     (:file "image")
     (:file "keysyms-common")
     (:file "keysyms")
     (:file "keysymdef")
     (:file "port")
     (:file "medium")
     (:file "graft")
     (:file "frame-manager")))))

(defsystem :clim-beagle
  :depends-on (:clim-core)
  :components
  ((:module "Backends"
            :components
            ((:module "beagle"
              :serial t
              :components
              ((:file "package")
               (:file "cocoa-util")
               (:module "native"
                :components ((:file "lisp-bezier-path")
                             (:file "lisp-window")
                             (:file "lisp-window-delegate")
                             (:file "lisp-view")
                             (:file "lisp-view-additional"
                             )
                             (:file "lisp-scroller")
                             (:file "lisp-slider")
                             (:file "lisp-button")
                             (:file "lisp-image")))
               (:module "windowing"
               
                :components ((:file "port")
                             (:file "frame-manager")
                             (:file "mirror")
                             (:file "graft")))
               (:module "native-panes"
                :components ((:file "beagle-scroll-bar-pane")
                             (:file "beagle-slider-pane")
                             ;; Basic buttons - not collections of buttons
                             (:file "beagle-fundamental-button-pane")
                             ;; Button collections (radio + checkbox)
                             ;; (:file "beagle-button-collection-pane")
                             (:file "scroller-pane-fix")))
               (:module "output"
               
                :components ((:file "medium")
                             (:file "fonts")))
               (:module "input"
               
                :components ((:file "events")
                             (:file "keysymdef")))
               (:module "glimpse"
                :components ((:file "glimpse")
                             (:file "glimpse-support")
                             (:file "glimpse-command-tables")
                             (:file "glimpse-present-process"
                             )
                             (:file "glimpse-present-window"
                             )
                             (:file "glimpse-modeless-commands"
                             )
                             (:file "glimpse-process-commands"
                             )
                             (:file "glimpse-window-commands"
                             )))
               (:module "profile"
                :components ((:file "profile")))
               (:module "tests"
                :components ((:file "drawing-tests")
                             (:file "graft-tests")))))))))

(defsystem :clim-gtkairo
    :depends-on (:clim-core :cffi)
    :components
    ((:module "Backends/gtkairo"
              :serial t
              :components
              ((:file "clim-fix")
               (:file "package")
               (:file "gtk-ffi")
               (:file "cairo-ffi")
               (:file "ffi")
               (:file "graft")
               (:file "port")
               (:file "event")
               (:file "keys")
               (:file "medium")
               (:file "pango")
               (:file "cairo")
               (:file "gdk")
               (:file "pixmap")
               (:file "frame-manager")
               (:file "gadgets")))))

(defsystem :clim-graphic-forms
  :depends-on (:clim-core :graphic-forms-uitoolkit)
  :components
  ((:module "Backends/GFW"
	    :components
	    ((:file "package")
	     (:file "utils")
	     (:file "graft")
	     (:file "port")
	     (:file "medium")
	     (:file "pixmap")
	     (:file "frame-manager")
	     (:file "gadgets")))))

(defsystem :clim-null
    :depends-on (:clim-core)
    :components
    ((:module "Backends/Null"
              :components
              ((:file "package")
               (:file "port")
               (:file "medium")
               (:file "graft")
               (:file "frame-manager")))))

;;; A system that loads the appropriate backend for the current
;;; platform.
(defsystem :clim-looks
  :depends-on (:clim-core 
	       #+clim-sdl            :clim-sdl
	       #+clim-clx            :clim-clx
	       #+clim-graphic-forms  :clim-graphic-forms
	       #+clim-opengl         :clim-opengl 
	       #+clim-beagle         :clim-beagle
	       #+clim-gtkairo        :clim-gtkairo
	       #+clim-null           :clim-null
	       )
  :components ((:file "Looks/pixie")))

;;; The actual CLIM system that people should to use in their ASDF
;;; package dependency lists.
(defsystem :clim
  :version "0.0.0"
  :depends-on (:clim-core :clim-input :clim-looks))

(defmethod perform :after ((op load-op) (c (eql (find-system :clim))))
  (pushnew :clim *features*)) ;; The fact that CLIM itself is available is true when all is loaded.

;; The fact that our CLIM implementation is McCLIM is already true now.
;; This feature is notably used by ESA and DREI, in cases where they need to
;; know whether they are compiled with McCLIM or another CLIM implementation.
(pushnew :mcclim *features*) 
