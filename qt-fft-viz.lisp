;;;; qt-fft-viz.lisp
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:qt-fft-viz)
(named-readtables:in-readtable :qtools)

(define-widget main-window (QMainWindow)
  ())

(define-menu (main-window File)
  (:item ("Open" (ctrl o))
         (open-file main-window))
  (:separator)
  (:item ("Quit" (ctrl alt q))
         (q+:close main-window)))

(define-menu (main-window Help)
  (:item "About"
         (q+:qmessagebox-information
          main-window "About"
          "Interactively view and manipulate FFT data.")))


(define-widget fft-drawer (QWidget)
               ((mp3-file :initform nil)
                (current-location :initform 0)
                (song-duration :initform 0)
                (total-frames :initform 0))
  (:documentation "Draw "))

(defun generate-frame (left-fft-data right-fft-data)
  "Create a data structure that represents one frame of the animation.
   Later, the frame data is used to generate the actual image."
  (loop for lft across left-fft-data for rgt across right-fft-data for cnt below 128
     collect (cons (abs lft) (abs rgt))))
  
(define-override (fft-drawer paint-event paint) (ev)
  "Handle paint events."
  (declare (ignore ev))
  ;; (format t "Got a paint event!~%")
  (with-finalizing 
      ;; Create a painter object to draw on
      ((painter (q+:make-qpainter fft-drawer))
       (pen (q+:make-qpen )))
    ;; (format t "Clearing background~%")
    ;; Clear the background
    (q+:fill-rect painter (q+:rect fft-drawer) (q+:qt.black))
    (q+:set-color pen (q+:make-qcolor 0 205 0))
    (q+:set-pen painter pen)
    ;; (format t "File: ~a~%" mp3-file)
    (when mp3-file
      ;; (format t "Calculating fft~%")
      (let* ((height (q+:height fft-drawer))
             (width (q+:width fft-drawer))

             (x-aspect-ratio (if (< height width)
                                 (/ height width 1.0)
                                 1.0))
             (y-aspect-ratio (if (< height width)
                                 1.0
                                 (/ width height 1.0)))
             
             (win-center (ceiling (* 44100 (interpolate 0.0 song-duration
                                                        current-location total-frames))))
             
             (left-fft-data (bordeaux-fft:windowed-fft (mp3-file-left-channel mp3-file) win-center fft-window-size))
             (right-fft-data (bordeaux-fft:windowed-fft (mp3-file-right-channel mp3-file) win-center fft-window-size))
             (frame (generate-frame left-fft-data right-fft-data)))
        
        ;; Local functions for mapping logical coordinates to physical coordinates
        (flet (
               ;; xmapper maps logical x coordinates in the range x-min to x-max to
               ;; screen coordinates in the range 0 to width
               (xmapper (x) (map-val (* x-aspect-ratio x) -200.0 200.0 0 width))

               ;; ymapper does the same thing, but for y coordinates
               (ymapper (y) (map-val (* y-aspect-ratio y) 0.0 200.0 0 height)))

          ;; Actual drawing goes here.  In this case, just a line.
          (loop for vals in frame
             for idx from 0
             do
               (q+:draw-line painter
                             (xmapper (- (car vals))) (ymapper (* 1.25 idx))
                             (xmapper (cdr vals)) (ymapper (* 1.25 idx)))))))))


;; Create all of the controls

(define-subwidget (main-window viz-widget) (make-instance 'fft-drawer)
  "The fft-drawer itself.")

(define-subwidget (main-window timer) (q+:make-qtimer main-window)
  (setf (q+:single-shot timer) nil))

(define-slot (main-window tick) ()
  (declare (connected timer (timeout)))
  (incf (slot-value viz-widget 'current-location))
  (format t "Drawing frame: ~a~%" (slot-value viz-widget 'current-location))
  (q+:repaint viz-widget)
  ;; (q+:start timer (round 1000/30))
  )

(define-slot (main-window open open-file) ()
  (q+:stop timer)
  (let* ((filename (q+:qfiledialog-get-open-file-name main-window "Select File"
                                                      (q+:qdesktopservices-storage-location (q+:qdesktopservices.music-location))))
         (mp3-file (read-mp3-file filename))
         (sduration (mp3-file-duration-in-seconds mp3-file))
         (tframes (ceiling (* sduration 30))))
    (setf (slot-value viz-widget 'mp3-file) (copy-mp3-file mp3-file))
    (setf (slot-value viz-widget 'current-location) 0)
    (setf (slot-value viz-widget 'song-duration) sduration)
    (setf (slot-value viz-widget 'total-frames) tframes))
  (q+:start timer (round (/ 1000 30))))

(define-subwidget (main-window controls) (make-instance 'fft-drawer )
  "The main-window's fft-controls widget."
  )

(define-initializer (main-window setup)
  "Set the window title and set the fft-controls to be the central widget."
  (setf (q+:window-title main-window) "Interactive FFT Explorer")
  (setf (q+:central-widget main-window) controls))

(defun main ()
  "Create the main window."
  (with-main-window (window (make-instance 'main-window))))


