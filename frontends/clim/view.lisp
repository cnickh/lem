(defpackage :lem-clim/view
  (:use :clim-lisp :clim :lem-clim/utils)
  (:local-nicknames
   (:obj :lem-clim/object))
  (:export
   :view-window
   :view
   :view-width
   :view-height
   :view-modeline
   :view-x
   :create-view
   :resize
   :draw-view
   :update-line
   :view-lines
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

(defun create-view (window x y width height use-modeline)
  (when use-modeline (incf height))
  (make-instance 'buffer-view
                 :window window
                 :x x
                 :y y
                 :width width
                 :height height
                 :use-modeline use-modeline))

(defmethod resize (buffer-view medium width height)
  (setf (view-lines buffer-view) 
        (delete-if 
         (lambda (line) 
           (>= (cadr line) (* height (text-height medium)))) 
         (view-lines buffer-view)))

  (log:info "have lines: ~a" (view-lines buffer-view))
  
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
   (view-lines buffer-view)))

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
        (y (* (+ (view-y buffer-view) (view-height buffer-view)) (text-height pane)))
	(background (lem-core:attribute-background (nth 2 modeline))))

    ;;(log:info "draw-modeline left:~a right:~a" (car modeline) (cadr modeline))
    (draw-rectangle
     pane
     (make-point x0 y)
     (make-point x1 (- y (text-height pane)))
     :ink (parse-color background)
     :filled t)
    
    (loop for object in (car modeline) ;;draw left objects
          do (incf x0
                      (obj:draw-object object x0 y pane buffer-view)))
;;
    (loop for object in (cadr modeline) ;;draw right objects
          do (when (< x0 (- x1 (obj:object-width object pane))) (incf x1
                   (- (obj:draw-object object (- x1 (obj:object-width object pane)) y pane buffer-view)))))))

(defun line-cmp (a b)
  (if (= (length a) (length b))
      (loop for obj-a in a
            for obj-b in b
            do (when 
                   (not (string= (obj:object-text obj-a) (obj:object-text obj-b))) 
                 (return-from line-cmp NIL))) NIL) T)

(defun modeline-cmp (a b)
  (let ((res
          (and (line-cmp (car a) (car b)) (line-cmp (cadr a) (cadr b)))))

;;    (log:info 
;;     "modeline-cmp result: ~a ~% mode-a: (left:~a right:~a) ~% mode-b:(left:~a right:~a) "
;;     res (car a) (cadr a) (car b) (cadr b))
    
    res))


(defmethod draw-window-bg (pane x0 y0 x1 y1 (window lem:floating-window))
  (when (and (lem:floating-window-border window)
             (< 0 (lem:floating-window-border window)))
    (let ((x2 (- x0 (floor (text-width pane) 2)))
           (y2 (- y0 (floor (text-height pane) 2)))
           (x3 (+ x1 (floor (text-width pane) 2)))
           (y3 (+ y1 (floor (text-height pane) 2))))     
    (draw-rectangle pane (make-point x2 y2) (make-point x3 y3) :ink (medium-background pane))
    (draw-rectangle pane (make-point x2 y2) (make-point x3 y3) :filled nil :ink (medium-foreground pane)))))

(defmethod draw-window-bg (pane x0 y0 x1 y1 (window lem:window))
  (when (< 0 (lem:window-x window))
    (let* ((attrib (lem:ensure-attribute 'lem:modeline-inactive))
           (width (text-width pane))
           (x1 (- x0 (floor width 2))))
      (draw-line
       pane (make-point x1 y0) (make-point x1 y1)
       :line-thickness width
       :ink (parse-raw-color (lem-core:attribute-background-color attrib)))
      (draw-line
       pane (make-point x1 y0) (make-point x1 y1)
       :line-thickness (floor width 3)
       :ink (parse-raw-color (lem-core:attribute-foreground-color attrib))))))

(defmethod draw-view (buffer-view pane frame)
  (let* ((modeline (view-modeline buffer-view))
         (x0 (* (view-x buffer-view) (text-width pane)))
         (y0 (* (view-y buffer-view) (text-height pane)))
         (x1 (+ x0 (* (view-width buffer-view) (text-width pane))))
         (y1 (+ y0 (* (view-height buffer-view) (text-height pane)))))
    
    (log:info "view x:~a y:~a width:~a height:~a ~%"
              x0
              y0
              x1
              y1)
    
  (draw-rectangle pane (make-point x0 y0) (make-point x1 (- y1 (text-height pane))) 
                  :ink (medium-background pane))
  (draw-window-bg pane x0 y0 x1 y1 (view-window buffer-view))
  
    ;;draw lines
    (loop
      for line in (view-lines buffer-view)
      do (progn
           (log:info "have ~a" line)
           ;;(log:Info "[~a ~a] drawing line: ~a"
           ;;          x0
           ;;          (+ (cadr line) y0)
           ;;          (loop for object in (nth 2 line)
           ;;                collect (obj:object-text object)))
         (loop
           :with current-x := x0
           :with y := (+ (cadr line) y0)
           :with height := (nth 3 line)
           :for object :in (nth 2 line)
           :do (incf 
                current-x
                (obj:draw-object object current-x (+ y height) pane buffer-view)))))
    (if modeline
        (draw-modeline buffer-view modeline pane))))
