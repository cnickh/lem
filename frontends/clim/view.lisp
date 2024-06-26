(defpackage :lem-clim/view
  (:use :clim-lisp :clim :lem-clim/utils)
  (:local-nicknames
   (:obj :lem-clim/object))
  (:export
   :view
   :set-cursor-position
   :last-cursor-x
   :last-cursor-y
   :view-width
   :view-height
   :view-modeline
   :create-view
   :resize
   :draw-view
   :update-line
   :move-position))

(in-package :lem-clim/view)

(defclass buffer-view ()
  ((%window
    :initarg :window
    :reader view-window)
   (%x
    :initarg :x
    :accessor view-x)
   (%y
    :initarg :y
    :accessor view-y)
   (%width
    :initarg :width
    :accessor view-width)
   (%height
    :initarg :height
    :accessor view-height)
   (%use-modeline
    :initarg :use-modeline
    :reader view-use-modeline)
   (%lines
    :initform '()
    :accessor view-lines)
   (%modeline
    :initform nil
    :accessor view-modeline)
   (%last-cursor-x
    :initform nil
    :accessor view-last-cursor-x)
   (%last-cursor-y
    :initform nil
    :accessor view-last-cursor-y)))

(defmethod set-cursor-position (buffer-view x y)
  (setf (view-last-cursor-x buffer-view) x
        (view-last-cursor-y buffer-view) y))

(defmethod last-cursor-x (buffer-view  medium)
  (or (view-last-cursor-x buffer-view)
      (* (lem:last-print-cursor-x (view-window buffer-view))
         (text-width medium))))

(defmethod last-cursor-y (buffer-view medium)
  (or (view-last-cursor-y buffer-view)
      (* (lem:last-print-cursor-y (view-window buffer-view))
         (text-height medium))))

(defun create-view (window x y width height use-modeline)
  (when use-modeline (incf height))
  (let ((new-view (make-instance 'buffer-view
                 :window window
                 :x x
                 :y y
                 :width width
                 :height height
                 :use-modeline use-modeline)))

;;    (log:info "new-view ~a have lines ~a" new-view (view-lines new-view))
    new-view))

(defmethod resize (buffer-view width height)
  (when (view-use-modeline buffer-view) (incf height))
  (setf (view-width buffer-view) width)
  (setf (view-height buffer-view) height))

(defmethod move-position (buffer-view x y)
  (setf (view-x buffer-view) x
        (view-y buffer-view) y))

(defmethod update-line (buffer-view x y objects height)
  (push (list x y objects height) (view-lines buffer-view))

  (log:info "(update-line ~a)"
	    (loop
	      for line in (view-lines buffer-view)
	      collect (loop for object in (nth 2 line)
			    collect (obj:object-text object))
	   ;;(log:info "update-line view now has (~a)"
	   ;;(view-lines buffer-view))
				     )))


(defmethod color-line (buffer-view pane x y height)
;;  (log:info "Have attrib color ~a ~%"
;;	    (lem-core:attribute-background-color default-attribute))
  (draw-rectangle
   pane
   (make-point x y)
   (make-point
    (- (lem-if:view-width (lem-core:implementation) buffer-view) x)
    height)
   :filled t))

(defvar +mode-ratio+ .5)

(defun draw-modeline (buffer-view modeline pane)

  (declare (ignore buffer-view))
  
  (let ((x0 0)
	(x1 0)
	(x2 0)
	(y0 0)
	(y1 0))

    (draw-rectangle
     pane
     (make-point x0 y0)
     (make-point x1 y1))

    (loop for object in (car modeline) ;;draw left objects
	  )
    
    (draw-rectangle
     pane
     (make-point x1 y0)
     (make-point x2 y1))

    (loop for object in (cadr modeline) ;;draw right objects
	  )
    
    ))

(defun _draw-lines (buffer-view pane)
  (loop
    for line in (view-lines buffer-view)
    do (progn
	 (color-line buffer-view pane (car line) (nth 1 line) (nth 3 line))
	(loop
	 :with current-x := (car line)
	 :with y := (nth 1 line)
	 :with height := (nth 3 line)
	 :for object :in (nth 2 line)
	 :do (progn
	       (log:info
		"(draw-object ~a ~a ~a) ~%"
		object current-x (+ y height))
		   
	       (incf
		current-x
		1
		;;(obj:draw-object object current-x (+ y height) pane buffer-view)
		))))))

(defmethod draw-view (buffer-view pane)
  (log:info "Drawing view[~a] x: ~a y: ~a ~% height: ~a width: ~a ~%~%" 
	    buffer-view
	    (view-x buffer-view) 
            (view-y buffer-view)
            (view-height buffer-view)
            (view-width buffer-view))
    
  (let* ((x0 (* (view-x buffer-view) (text-width pane)))
         (y0 (* (view-y buffer-view) (text-height pane)))
         (x1 (* (+ x0 (view-width buffer-view)) (text-width pane)))
         (y1 (* (+ y0 (view-height buffer-view)) (text-height pane)))
	 (modeline (view-modeline buffer-view)))

    (declare (ignore modeline))
;;    (if modeline
;;	)
    
    (draw-rectangle 
     pane
     (make-point x0 y0)
     (make-point x1 y1)
     :filled nil
     :line-thickness 3)
    
    (log:info "have line :: ~a ~%" (view-lines buffer-view))
    (_draw-lines buffer-view pane)))
