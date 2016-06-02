;;;; qt-fft-viz.asd
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(asdf:defsystem #:qt-fft-viz
  :description "Interactive FFT explorer."
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC (BSD-like)"
  :depends-on (#:qtools
               #:qtgui
               #:qtcore
               #:anim-utils
               #:mixalot-mp3)
  :serial t
  :components ((:file "package")
               (:file "qt-fft-viz")))

