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

(defmethod last-cursor-x (buffer-view medium)
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

(defmacro replace-add-if (val pred list)
  `(if (find-if ,pred ,list)
      (nsubstitute-if ,val ,pred ,list)
      (push ,val ,list)))

(defun view-lines-list (buffer-view)
  (when (view-lines buffer-view)
    (loop for line in (view-lines buffer-view)
          collect (cons (cadr line) (loop for object in (nth 2 line)
                                          collect (obj:object-text object))))))

(defmethod update-line (buffer-view x y objects height)
  (replace-add-if 
   (list x y objects height)
   (lambda (line) 
     (= (cadr line) y))
   (view-lines buffer-view))

  ;;(log:info "update-line on ~a  @ x:~a y:~a  height:~a with ~a ~% ~a"
  ;;          buffer-view x y height
  ;;          (loop for object in objects
  ;;                collect (obj:object-text object))
  ;;          (view-lines-list buffer-view))
  )


(defmethod color-line (buffer-view pane x y height)
;;  (log:info "Have attrib color ~a ~%"
;;	    (lem-core:attribute-background-color default-attribute))
  (draw-rectangle
   pane
   (make-point x y)
   (make-point
    (- (view-width buffer-view) x)
    height)
   :filled t))

(defun draw-modeline (buffer-view modeline pane)
  (let ((x0 (* (view-x buffer-view) (text-width pane)))
	(x1 (* (+ (view-x buffer-view) (view-width buffer-view)) (text-width pane)))
        (y (* (+ (view-y buffer-view) (view-height buffer-view)) (text-height pane))))

    (loop for object in (car modeline) ;;draw left objects
             do (incf x0
                      (obj:draw-object object x0 y pane buffer-view)))

    (loop for object in (cadr modeline) ;;draw right objects
	  do (incf x1
                   (- (obj:draw-object object (- x1 (obj:object-width object pane)) y pane buffer-view))))))

(defmethod draw-view (buffer-view pane frame)

  (let* ((x0 (* (view-x buffer-view) (text-width pane)))
         (y0 (* (view-y buffer-view) (text-height pane)))
         (x1 (+ x0 (* (view-width buffer-view) (text-width pane))))
         (y1 (+ y0 (* (view-height buffer-view) (text-height pane))))
	 (modeline (view-modeline buffer-view)))

    (log:info "view @ ~a computed dimen x:~a y:~a width:~a height:~a" 
              buffer-view x0 y0 x1 y1)
    
    (if modeline
        (draw-modeline buffer-view modeline pane))
    
    (draw-rectangle 
     pane (make-point x0 y0) (make-point x1 y1))
    ;;draw lines
    (loop
      for line in (view-lines buffer-view)
      do (progn
           (log:info "drawing line: ~a" (loop for object in (nth 2 line)
                                              collect (obj:object-text object)))
           (loop
           :with current-x := (+ (car line) (* (view-x buffer-view) (text-width pane)))
           :with y := (+ (cadr line) (* (view-y buffer-view) (text-height pane)))
           :with height := (nth 3 line)
           :for object :in (nth 2 line)
           :do (incf
                current-x
                (obj:draw-object object current-x (+ y height) pane buffer-view)))))))




