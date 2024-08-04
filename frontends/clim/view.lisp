(defpackage :lem-clim/view
  (:use :clim-lisp :clim :lem-clim/image-buffer :lem-clim/utils :lem-core/display)
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
   :move-position
   :redisplayp
   :modeline-cmp
   :view-id
   :update-modeline
   :clear-after))

(in-package :lem-clim/view)

(defvar *view-count* 0)

(defvar view-lock (bt:make-lock))

(defclass buffer-view ()
  ((%window
    :initarg :window
    :reader view-window)
   (%lock
    :initform (bt:make-lock)
    :accessor view-lock)
   (%view-id
    :initform (incf *view-count*)
    :reader view-id)
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
   (%redisplay
    :initform 0
    :accessor redisplayp)
   (%size-cache
    :initform 0
    :accessor size-cache)
   (%last-cursor-x
    :initform nil
    :accessor view-last-cursor-x)
   (%last-cursor-y
    :initform nil
    :accessor view-last-cursor-y)))

(defun enable-redisplay (view)
  ;;(log:info "~a" (sb-debug:list-backtrace))
  (incf (redisplayp view)))

(defun update-size (view)
  (when (view-modeline view)
    (incf (nth 4 (view-modeline view))))
  (incf (size-cache view)))

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
  ;;(bt:with-lock-held ((view-lock buffer-view))
    (setf (view-lines buffer-view) 
          (delete-if 
           (lambda (line) 
             (>= (cadr line) (* height (text-height medium))))
           (view-lines buffer-view)))
  
  (when (view-use-modeline buffer-view) (incf height))
  (setf (view-width buffer-view) width)
  (setf (view-height buffer-view) height)
  (update-size buffer-view)
  (enable-redisplay buffer-view));;)

(defmethod move-position (buffer-view x y)
  ;;(bt:with-lock-held ((view-lock buffer-view))
  (setf (view-x buffer-view) x
        (view-y buffer-view) y)
  (update-size buffer-view)
  (enable-redisplay buffer-view));;)

(defmacro replace-add-if (val pred list)
  `(let* ((new_item ,val)
          (line (find-if ,pred ,list))
          (res (if line
                  ;;(if (line-cmp (nth 2 line) (nth 2 ,val)) nil
                   (progn 
                     (setf (nth 4 new_item) (incf (nth 4 line)))
                     (nsubstitute-if new_item ,pred ,list));;)
                   (push new_item ,list))))
     ;;(when res (log:info "~a ~% new ~a ~% old ~a ~% res ~a" (bt:current-thread) ,val line res))
     res))

(defvar temp-height nil)

(defmethod update-line (buffer-view x y objects height)
  ;;(bt:with-lock-held ((view-lock buffer-view))
  (unless temp-height
    (log:info "setting height ~a" height)
    (setf temp-height height))

  (unless (= temp-height height)
    (log:info "diff height ~a" height))
    
    (when (replace-add-if
           (list x y objects height 0)
           (lambda (line) 
             (= 
              (round (cadr line) height) 
              (round y height)))
           (view-lines buffer-view))
      ;;(log:info "line added/changed ~a " (view-lines buffer-view))
      (enable-redisplay buffer-view)));;)

(defmethod update-modeline (buffer-view left-objects right-objects default-attribute height)
  ;;(bt:with-lock-held ((view-lock buffer-view))
    (let ((modeline (list left-objects right-objects default-attribute height 0)))
      ;;(when (not (modeline-cmp modeline (view-modeline buffer-view)))
        ;;(log:info "modeline set")
        (when (view-modeline buffer-view)
          (setf (nth 4 modeline) (incf (nth 4 (view-modeline buffer-view)))))
        (setf (view-modeline buffer-view) modeline)
        (enable-redisplay buffer-view)));;);;)

(defmethod clear-after (buffer-view y)
  ;;(bt:with-lock-held ((view-lock buffer-view))
    (let ((view-lines (view-lines buffer-view)))
      (when (find-if (lambda (line) (>= (cadr line) y)) view-lines)
        (setf (view-lines buffer-view)
              (delete-if (lambda (line) (>= (cadr line) y)) view-lines))
        ;;(log:info "pumping view")
        (enable-redisplay buffer-view))));;)

;;(defmethod color-line (buffer-view pane x y height)
;;;;  (log:info "Have attrib color ~a ~%"
;;;;	    (lem-core:attribute-background-color default-attribute))
;;  (draw-rectangle
;;   pane
;;   (make-point x y)
;;   (make-point
;;    (- (view-width buffer-view) x)
;;    height)
;;   :filled t))

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

(defmethod draw-view (buffer-view pane display-width display-height)
  (handler-case
      ;;(bt:with-lock-held ((view-lock buffer-view))
        ;;(updating-output (pane :unique-id (view-id buffer-view) :cache-value (redisplayp buffer-view))
          (let* ((modeline (view-modeline buffer-view))
                 (x0 (* (view-x buffer-view) (text-width pane)))
                 (y0 (* (view-y buffer-view) (text-height pane)))
                 (x1 (+ x0 (* (view-width buffer-view) (text-width pane))))
                 (y1 (+ y0 (* (view-height buffer-view) (text-height pane))))
                 (buffer (lem:window-buffer (view-window buffer-view))))

        (log:info "draw-view ~% id:~a redisplay val:~a x0 ~a y0 ~a x1 ~a y1 ~a ~% lines::~a" 
                  (view-id buffer-view) (redisplayp buffer-view) x0 y0 x1 y1 (view-lines buffer-view))

            ;;(updating-output (pane :cache-value (size-cache buffer-view))
              (draw-rectangle pane (make-point x0 y0) (make-point x1 (- y1 (text-height pane))) 
                              :ink (medium-background pane))
    
              (draw-window-bg pane x0 y0 x1 y1 (view-window buffer-view));;)
  
    ;;draw lines
            (handler-case 
                (if (typep buffer 'image-buffer)
                    (render buffer pane x0 y0 display-width display-height)
                    (loop
                      for line in (view-lines buffer-view)
                      do (progn ;;(log:info "line ~a ~a" (cadr line) (nth 4 line))
                           ;;(updating-output (pane :unique-id (cons (cadr line) (view-id buffer-view)) 
                           ;;                       :id-test 'equal 
                           ;;                       :cache-value (nth 4 line))
                           (log:info "view ~a line ~a ~a ~%" 
                                     (view-id buffer-view) (floor (cadr line) (text-height pane)) line)
                             (loop
                               :with current-x := x0
                               :with y := (+ (cadr line) y0)
                               :with height := (nth 3 line)
                               :for object :in (nth 2 line)
                               :do (incf current-x
                                         (obj:draw-object object current-x (+ y height) pane buffer-view))))));;)
              (error (e)
                (log:info "ERR draw-line got error ~a" e)))
            
            (if modeline
                ;;(updating-output (pane :cache-value (nth 4 modeline))
                  (draw-modeline buffer-view modeline pane)));;));;)
    
    (error (e)
      (log:info "ERR caught in draw-view ~a" e))))
