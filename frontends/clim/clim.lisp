(defpackage :lem-clim/clim
  (:use :clim-lisp :clim :lem-clim/utils)
  (:local-nicknames (:frame :lem-clim/frame)
                   (:view :lem-clim/view)
                   (:obj :lem-clim/object)))

(in-package :lem-clim/clim)

(defclass clim (lem:implementation)
  ()
  (:default-initargs
   :name :clim
   :redraw-after-modifying-floating-window nil))

(defvar +app-frame+)

(defvar compose-lock (bt:make-lock)) 

(defmacro current-views()
  `(frame:views +app-frame+))

(defun display-pane ()
  (car (frame-current-panes +app-frame+)))

(defmethod lem-if:invoke ((implementation clim) function)
  (log:info "threads ~a" (bt:all-threads))
  (setq +app-frame+ (make-application-frame 'frame:lem))
  (setq *default-text-style* (make-text-style :fix :roman :normal))
  (setf (frame:editor-thread +app-frame+) function)
  (run-frame-top-level +app-frame+))

(defmethod lem-if:get-background-color ((implementation clim))
  ;;(log:info "bg-col")
  ;;(bt:with-recursive-lock-held (frame:frame-lock)
    (frame:background +app-frame+));;)

(defmethod lem-if:get-foreground-color ((implementation clim))
  ;;(log:info "fg-col")
  ;;(bt:with-recursive-lock-held (frame:frame-lock)
    (frame:foreground +app-frame+));;)

(defmethod lem-if:update-foreground ((implementation clim) color-name)
  ;;(log:info "update-foreground")
  ;;(bt:with-recursive-lock-held (frame:frame-lock)
    (setf (frame:foreground +app-frame+) (lem:parse-color color-name)));;)

(defmethod lem-if:update-background ((implementation clim) color-name)
  ;;(log:info "update-background")
  ;;(bt:with-recursive-lock-held (frame:frame-lock)
    (setf (frame:background +app-frame+) (lem:parse-color color-name)));;)

(defmethod lem-if:display-width ((implementation clim))
  ;;(let ((bck-trc (sb-debug:list-backtrace)))
    ;;(log:info "display-width:~a ~a " (frame:display-char-width +app-frame+) (bt:current-thread)))
  ;;(bt:with-recursive-lock-held (frame:frame-lock)
    (frame:display-char-width +app-frame+));;);;)
  
(defmethod lem-if:display-height ((implementation clim))
  ;;(log:info "display-height:~a ~a" (frame:display-char-height +app-frame+) (bt:current-thread))
  ;;(bt:with-recursive-lock-held (frame:frame-lock)
    (frame:display-char-height +app-frame+));;)


(defun floating-p (view)
  (and (typep (view:view-window view) 'lem:floating-window)
       (not (typep (view:view-window view) 'lem:side-window))))

(defmethod lem-if:make-view 
    ((implementation clim) window x y width height use-modeline)
  (log:info "create called on ~a" window)
  ;;(setq needs-redraw t)
  ;;(bt:with-lock-held (frame:frame-lock)
  (let ((new-view (view:create-view window x y width height use-modeline)))
    (push new-view (current-views))
    (setf (current-views) (sort (current-views)
          (lambda (a b)
           (cond
             ((and (floating-p a) (not (floating-p b))) nil)
             ((and (not (floating-p a)) (floating-p b)) t)
             ((< (view:view-x a) (view:view-x b)) t)))))
    ;;(log:info "returning ~a" new-view)
    new-view));;)
  
(defmethod lem-if:delete-view ((implementation clim) view)
  (log:info "delete ~a from ~a" view (current-views))
  ;;(bt:with-lock-held (frame:frame-lock)
  (setf (current-views) (delete-if (lambda (v) (equal view v)) (current-views)))
  (when (= (length (current-views)) 0) 
    (destroy-frame +app-frame+))
  ;;(setq needs-redraw t)
  );;)

(defmethod lem-if:clear ((implementation clim) view)
  nil)

(defmethod lem-if:set-view-size ((implementation clim) view width height)
  ;;(log:info "@~a (set-view-size ~a ~a)"  view width height)
  ;;(bt:with-lock-held (frame:frame-lock)
    (view:resize view (display-pane) width height));;)

(defmethod lem-if:set-view-pos ((implementation clim) view x y)
  ;;(log:info "@~a (set-view-pos ~a ~a)"  view x y)
  ;;(bt:with-lock-held (frame:frame-lock)
    (view:move-position view x y));;)

(defmethod lem-if:update-display ((implementation clim))
  (execute-frame-command +app-frame+ '(lem-clim/frame:com-redisplay)))

(defmethod lem-if:view-width ((implementation clim) view)
  (* (view:view-width view) (text-width (display-pane))))

(defmethod lem-if:view-height ((implementation clim) view)
  ;;(+ (view:view-height view) 1)
  (* (- (view:view-height view) 1) (text-height (display-pane))))

(defmethod lem-if:object-width ((implementation clim) object)
  (obj:object-width object (display-pane)))

(defmethod lem-if:object-height ((implementation clim) object)
  (obj:object-height object (display-pane)))
  ;;1)

(defun adjust (y)
  (+ (floor y (text-height (display-pane))) 1))

(defmethod lem-if:render-line ((implementation clim) view x y objects height)
  (let ((count 0))
    (flet ((counter (item) (when (= (adjust (cadr item)) (adjust y)) (incf count))))
      
      (mapcar #'counter (view:view-lines view))
      
      (log:info "(render-line y:~a) before-count: ~a" (+ (floor y (text-height (display-pane))) 1) count)
      (view:update-line view x y objects height)

      (setf count 0)
      (mapcar #'counter (view:view-lines view))
      (log:info "(render-line y:~a) after-count: ~a" (+ (floor y (text-height (display-pane))) 1) count)
  )))

(defun collect-cadr (list)
  (loop for item in list
        collect (cadr item)))

(defmethod lem-if:clear-to-end-of-window ((implementation clim) view y)
  
  (log:info "clear to end of window ~a" (floor y (text-height (display-pane))))
  (view:clear-after view y)
  
  )
  
(defmethod lem-if:get-char-width ((implementation clim))
  (text-width (display-pane)))

(defmethod lem-if:get-char-height ((implementation clim))
  (text-height (display-pane)))

(defun read-text-objects (objects)
  (loop for object in objects
	collect (obj:object-text object)))

(defmethod lem-if:render-line-on-modeline
    ((implementation clim) view left-objects right-objects default-attribute height)
  ;;(log:info "render-modeline")
  (view:update-modeline view left-objects right-objects default-attribute height))

(defmacro with-clim (() &body body)
  `(with-implementation (make-instance 'clim)
     (setup-first-frame)
     ,@body))
