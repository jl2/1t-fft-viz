;;;; qt-fft-viz.lisp
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:qt-fft-viz)
(named-readtables:in-readtable :qtools)

(define-widget main-window (QMainWindow)
  ())

(define-menu (main-window File)
  (:item ("Open" (ctrl o))
         (q+:close main-window))
  (:separator)
  (:item ("Quit" (ctrl alt q))
         (q+:close main-window)))

(define-menu (main-window Help)
  (:item "About"
         (q+:qmessagebox-information
          main-window "About"
          "Interactively view and manipulate FFT data.")))


(define-widget fft-drawer (QWidget)
  ((left-fft-data :initform #())
   (right-fft-data :initform #()))
  (:documentation "Draw "))

(defun generate-frame (left-fft-data right-fft-data)
  "Create a data structure that represents one frame of the animation.
   Later, the frame data is used to generate the actual image."
  (loop for lft across left-fft-data for rgt across right-fft-data for cnt below 128
     collect (cons (abs lft) (abs rgt))))
  
(define-override (fft-drawer paint-event paint) (ev)
  "Handle pain events."
  (declare (ignore ev))

  (with-finalizing 
      ;; Create a painter object to draw on
      ((painter (q+:make-qpainter fft-drawer))
       (pen (q+:make-qpen )))
    ;; Clear the background
    (q+:fill-rect painter (q+:rect fft-drawer) (q+:qt.black))
    (q+:set-color pen (q+:make-qcolor 0 205 0))
    (q+:set-pen painter pen)
    
    (let* ((height (q+:height fft-drawer))
           (width (q+:width fft-drawer))
           (frame (generate-frame left-fft-data right-fft-data))
           (x-aspect-ratio (if (< height width)
                               (/ height width 1.0)
                               1.0))
           (y-aspect-ratio (if (< height width)
                               1.0
                               (/ width height 1.0))))
      
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
                           (xmapper (cdr vals)) (ymapper (* 1.25 idx))))))))

(define-widget fft-controls (QWidget)
  ()
  (:documentation "The fft-controls widget contails input controls for all of the data fields
                   in a fft-viewer object."))


;; Create all of the controls

(define-subwidget (fft-controls sviewer) (make-instance 'fft-drawer)
  "The fft-drawer itself.")

;; (define-subwidget (fft-controls aval-spin) (q+:make-qdoublespinbox fft-controls)
;;   "The 'a' value spinbox."
;;   (q+:set-decimals aval-spin 2)
;;   (q+:set-single-step aval-spin 0.01)
;;   (q+:set-maximum aval-spin 1000.0)
;;   (q+:set-minimum aval-spin 0.01)
;;   (q+:set-value aval-spin (slot-value sviewer 'a-val)))

;; (define-subwidget (fft-controls bval-spin) (q+:make-qdoublespinbox fft-controls)
;;   "The 'b' value spinbox."
;;   (q+:set-decimals bval-spin 2)
;;   (q+:set-single-step bval-spin 0.01)
;;   (q+:set-maximum bval-spin 1000.0)
;;   (q+:set-minimum bval-spin 0.01)
;;   (q+:set-value bval-spin (slot-value sviewer 'b-val)))

;; (define-subwidget (fft-controls hval-spin) (q+:make-qdoublespinbox fft-controls)
;;   "The 'h' value spinbox."
;;   (q+:set-decimals hval-spin 2)
;;   (q+:set-single-step hval-spin 0.01)
;;   (q+:set-maximum hval-spin 1000.0)
;;   (q+:set-minimum hval-spin 0.01)
;;   (q+:set-value hval-spin (slot-value sviewer 'h-val)))

;; (define-subwidget (fft-controls steps-spin) (q+:make-qspinbox fft-controls)
;;   "The spinbox for the number of steps."
;;   (q+:set-maximum steps-spin 10000000)
;;   (q+:set-minimum steps-spin 4)
;;   (q+:set-value steps-spin (slot-value sviewer 'steps)))

;; (define-subwidget (fft-controls dt-spin) (q+:make-qdoublespinbox fft-controls)
;;   "The 'h' value spinbox."
;;   (q+:set-decimals dt-spin 3)
;;   (q+:set-single-step dt-spin 0.001)
;;   (q+:set-minimum dt-spin 0.001)
;;   (q+:set-maximum dt-spin (* pi 1000))
;;   (q+:set-value dt-spin (slot-value sviewer 'dt)))

;; (define-subwidget (fft-controls epitrochoid-button) (q+:make-qradiobutton "Epitrochoid")
;;   "Epitrochoid radio button."
;;   (q+:set-checked epitrochoid-button t))

;; (define-subwidget (fft-controls hypotrochoid-button) (q+:make-qradiobutton "Hypotrochoid")
;;   "Hypotrochoid radio button.")

;; (define-subwidget (fft-controls button-group) (q+:make-qbuttongroup fft-controls)
;;   "Button group to ensure radio buttons are exclusive."
;;   (q+:set-exclusive button-group t)
;;   (q+:add-button button-group epitrochoid-button)
;;   (q+:add-button button-group hypotrochoid-button))

;; (define-slot (fft-controls type-changed) ()
;;   "Handle radio button changes that hcange the curve type."
;;   (declare (connected epitrochoid-button (released)))
;;   (declare (connected hypotrochoid-button (released)))
;;   (cond 
;;     ;; Epitrochoid selected
;;     ((q+:is-checked epitrochoid-button)
;;      (setf (slot-value sviewer 'x-function) #'epitrochoid-x)
;;      (setf (slot-value sviewer 'y-function) #'epitrochoid-y))

;;     ;; Hypotrochoid selected
;;     ((q+:is-checked hypotrochoid-button)
;;      (setf (slot-value sviewer 'x-function) #'hypotrochoid-x)
;;      (setf (slot-value sviewer 'y-function) #'hypotrochoid-y))
    
;;     ;; One of the above should always be true, but just in case...
;;     ;; Print a warning and toggle the  epitrochoid-button
;;     (t
;;      (format t "Warning: No type selected, defaulting to epitrochoid.~%")
;;      (q+:set-checked epitrochoid-button t)
;;      (setf (slot-value sviewer 'x-function) #'epitrochoid-x)
;;      (setf (slot-value sviewer 'y-function) #'epitrochoid-y)))

;;   ;; Repaint to reflect the changes
;;   (q+:repaint sviewer))


;; (define-subwidget (fft-controls use-dt-button) (q+:make-qradiobutton "dt = dt")
;;   "dt = dt"
;;   (q+:set-checked use-dt-button t))

;; (define-subwidget (fft-controls pi-over-dt-button) (q+:make-qradiobutton "dt = π/dt (Symetric)")
;;   "dt = π/dt")

;; (define-subwidget (fft-controls dt-button-group) (q+:make-qbuttongroup fft-controls)
;;   "Button group to ensure radio buttons are exclusive."
;;   (q+:set-exclusive dt-button-group t)
;;   (q+:add-button dt-button-group use-dt-button)
;;   (q+:add-button dt-button-group pi-over-dt-button))

(define-subwidget (fft-controls animate-button) (q+:make-qpushbutton "Animate" fft-controls)
  (q+:set-checkable animate-button t)
  (q+:set-checked animate-button nil))

;; (define-slot (fft-controls dt-changed) ()
;;   "Handle radio button changes that hcange the curve type."
;;   (declare (connected pi-over-dt-button (released)))
;;   (declare (connected use-dt-button (released)))
;;   (cond 
;;     ;; Epitrochoid selected
;;     ((q+:is-checked use-dt-button)
;;      (setf (slot-value sviewer 'dt-type) :normal))

;;     ;; Hypotrochoid selected
;;     ((q+:is-checked pi-over-dt-button)
;;      (setf (slot-value sviewer 'dt-type) :over-pi))
    
;;     ;; One of the above should always be true, but just in case...
;;     ;; Print a warning and toggle the  epitrochoid-button
;;     (t
;;      (format t "Warning: No dt type selected, defaulting to normal.~%")
;;      (setf (slot-value sviewer 'dt-type) :normal))
;;      (q+:set-checked use-dt-button t))

;;   ;; Repaint to reflect the changes
;;   (q+:repaint sviewer))

;; It would be nice to handle all spin box changes in one slot, but I don't know 
;; how to ignore the value type.
;; (define-slot (fft-controls steps-changed) ((value int))
;;   "Handle changes to the steps-spin box."
;;   (declare (connected steps-spin (value-changed int)))
;;   (setf (slot-value sviewer 'steps) (q+:value steps-spin))
;;   (q+:repaint sviewer))

;; (define-slot (fft-controls values-changed) ((value double))
;;   "Handle changes to all of the spin boxes except steps."
;;   (declare (connected aval-spin (value-changed double)))
;;   (declare (connected bval-spin (value-changed double)))
;;   (declare (connected hval-spin (value-changed double)))
;;   (declare (connected dt-spin (value-changed double)))
;;   (setf (slot-value sviewer 'a-val) (q+:value aval-spin))
;;   (setf (slot-value sviewer 'b-val) (q+:value bval-spin))
;;   (setf (slot-value sviewer 'h-val) (q+:value hval-spin))
;;   (setf (slot-value sviewer 'dt) (q+:value dt-spin))
;;   (q+:repaint sviewer))

(define-subwidget (fft-controls timer) (q+:make-qtimer fft-controls)
  (setf (q+:single-shot timer) nil))

(define-slot (fft-controls tick) ()
  (declare (connected timer (timeout)))
  (setf (slot-value sviewer 'dt) (+ (slot-value sviewer 'dt) 0.0001))
  (setf (q+:value fft-data) (slot-value sviewer 'dt))
  (q+:repaint sviewer)
  ;; (q+:start timer (round 1000/30))
  )

(define-slot (fft-controls animate-changed) ()
  "Handle changes to the steps-spin box."
  (declare (connected animate-button (released)))
  (if (q+:is-checked animate-button)
      (q+:start timer (round (/ 1000 60)))
      (q+:stop timer))
  (q+:repaint sviewer))

  
(define-subwidget (fft-controls control-layout) (q+:make-qvboxlayout fft-controls)
  "Layout all of the control widgets in a vertical box layout."

  ;; ;; Create horizontal layouts to hold the labels and spinboxes
  ;; (let ((a-inner (q+:make-qhboxlayout))
  ;;       (b-inner (q+:make-qhboxlayout))
  ;;       (h-inner (q+:make-qhboxlayout))
  ;;       (steps-inner (q+:make-qhboxlayout))
  ;;       (dt-inner (q+:make-qhboxlayout))
  ;;       (type-inner (q+:make-qhboxlayout)))
    
  ;;   ;; Populate the horizontal layouts and add them to the top level vertical layout
  ;;   (q+:add-widget a-inner (q+:make-qlabel "A: " fft-controls))
  ;;   (q+:add-widget a-inner aval-spin)
  ;;   (q+:add-layout control-layout a-inner)

  ;;   (q+:add-widget b-inner (q+:make-qlabel "B: " fft-controls))
  ;;   (q+:add-widget b-inner bval-spin)
  ;;   (q+:add-layout control-layout b-inner)

  ;;   (q+:add-widget h-inner (q+:make-qlabel "H: " fft-controls))
  ;;   (q+:add-widget h-inner hval-spin)
  ;;   (q+:add-layout control-layout h-inner)

  ;;   (q+:add-widget steps-inner (q+:make-qlabel "Steps: " fft-controls))
  ;;   (q+:add-widget steps-inner steps-spin)
  ;;   (q+:add-layout control-layout steps-inner)

  ;;   (q+:add-widget dt-inner animate-button)
  ;;   (q+:add-widget dt-inner (q+:make-qlabel "dt: " fft-controls))
  ;;   (q+:add-widget dt-inner dt-spin)
  ;;   (q+:add-widget dt-inner use-dt-button)
  ;;   (q+:add-widget dt-inner pi-over-dt-button)
  ;;   (q+:add-layout control-layout dt-inner)

  ;;   ;; Add the radio buttons directly to the vertical layout
  ;;   (q+:add-widget type-inner epitrochoid-button)
  ;;   (q+:add-widget type-inner hypotrochoid-button)
  ;;   (q+:add-layout control-layout type-inner)

    ;; Finally add the fft viewer directly to the vertical layout
    (q+:add-widget control-layout sviewer))


(define-subwidget (main-window controls) (make-instance 'fft-controls )
  "The main-window's fft-controls widget."
  )

(define-initializer (main-window setup)
  "Set the window title and set the fft-controls to be the central widget."
  (setf (q+:window-title main-window) "Interactive FFT Explorer")
  (setf (q+:central-widget main-window) controls))

(defun main ()
  "Create the main window."
  (with-main-window (window (make-instance 'main-window))))


