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

(defmacro current-views()
  `(frame:views +app-frame+))

(defun display-pane ()
  (car (frame-current-panes +app-frame+)))

(defun join-ui-thread()
  (bt:join-thread
   (find-if
    (lambda (th)
      (search "ui-thread" (bt:thread-name th)))
    (bt:all-threads))))

(defun kill-editor-thread ()
  (bt:destroy-thread
   (find-if
    (lambda (th)
      (search "editor" (bt:thread-name th)))
    (bt:all-threads))))

(defmethod lem-if:invoke ((implementation clim) function)
  
  (setq +app-frame+ (make-application-frame 'frame:lem))
  (setq *default-text-style* (make-text-style :fix :roman :normal))
  
  (bt:make-thread
   (lambda () 
     (run-frame-top-level +app-frame+)) 
   :name "ui-thread")

  (sleep .5)

  (setf (frame:display-char-width +app-frame+)
	(floor (frame:display-width +app-frame+) (text-width (display-pane))))
  (setf (frame:display-char-height +app-frame+)
	(floor (frame:display-height +app-frame+) (text-height (display-pane))))
  
  (log:info 
   "text-style: ~a pane-style: ~a text-height == ~a ~% text-width == ~a"
   *default-text-style*
   (medium-text-style (display-pane))
   (text-height (display-pane))
   (text-width (display-pane)))
  
  (funcall function)

  (frame:enable-resize))
  ;;(join-ui-thread)
  ;;(frame:disable-resize)
  ;;(kill-editor-thread))


(defmethod lem-if:get-background-color ((implementation clim))
  (frame:background +app-frame+))

(defmethod lem-if:get-foreground-color ((implementation clim))
  (frame:foreground +app-frame+))

(defmethod lem-if:update-foreground ((implementation clim) color-name)
  (setf (frame:foreground +app-frame+) (lem:parse-color color-name)))

(defmethod lem-if:update-background ((implementation clim) color-name)
  (setf (frame:background +app-frame+) (lem:parse-color color-name)))

(defmethod lem-if:display-width ((implementation clim))
  ;;(log:info "display-width:~a" (frame:display-char-width +app-frame+))
  (frame:display-char-width +app-frame+))
  
(defmethod lem-if:display-height ((implementation clim))
  ;;(log:info "display-height:~a" (frame:display-char-height +app-frame+))
  (frame:display-char-height +app-frame+))


(defun floating-p (view)
  (and (typep (view:view-window view) 'lem:floating-window)
       (not (typep (view:view-window view) 'lem:side-window))))

(defmethod lem-if:make-view 
    ((implementation clim) window x y width height use-modeline)
  ;;(log:info "create called on ~a" window)
  (let ((new-view (view:create-view window x y width height use-modeline)))
    (push new-view (current-views))
    (setf (current-views) (sort (current-views)
          (lambda (a b)
           (cond
             ((and (floating-p a) (not (floating-p b))) nil)
             ((and (not (floating-p a)) (floating-p b)) t)
             ((< (view:view-x a) (view:view-x b)) t)))))
    ;;(log:info "returning ~a" new-view)
    new-view))
  
(defmethod lem-if:delete-view ((implementation clim) view)
  ;;(log:info "delete ~a from ~a ~%" view (current-views))
  (setf (current-views) (delete-if (lambda (v) (equal view v)) (current-views)))
  (when (= (length (current-views)) 0) 
    (destroy-frame +app-frame+)))

(defmethod lem-if:clear ((implementation clim) view)
  nil)

(defmethod lem-if:set-view-size ((implementation clim) view width height)
  (log:info "@~a (set-view-size ~a ~a) ~%"  view width height)
  (view:resize view (display-pane) width height))

(defmethod lem-if:set-view-pos ((implementation clim) view x y)
  ;;(log:info "@~a (set-view-pos ~a ~a) ~%"  view x y)
  (view:move-position view x y))

(defmethod lem-if:update-display ((implementation clim))
  (redisplay-frame-panes +app-frame+ :force-p t))

(defmethod lem-if:view-width ((implementation clim) view)
  (* (view:view-width view) (text-width (display-pane))))

(defmethod lem-if:view-height ((implementation clim) view)
  (* (- (view:view-height view) 1) (text-height (display-pane))))

(defmethod lem-if:object-width ((implementation clim) object)
  (obj:object-width object (display-pane)))

(defmethod lem-if:object-height ((implementation clim) object)
  (obj:object-height object (display-pane)))

(defmethod lem-if:render-line ((implementation clim) view x y objects height)
  (log:info "(render-line y:~a) on @~a" y view)
  (view:update-line view x y objects height))

(defmethod lem-if:clear-to-end-of-window ((implementation clim) view y)
  (setf (view:view-lines view) 
        (delete-if (lambda (line) (>= (cadr line) y)) (view:view-lines view)))
  ;;(log:info "(clear-to-end-of-window view:~a y:~a ~% lines after::~a)" view y (view:view-lines view))
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
  (setf (view:view-modeline view)
	(list left-objects right-objects default-attribute height)))

(defmacro with-clim (() &body body)
  `(with-implementation (make-instance 'clim)
     (setup-first-frame)
     ,@body))
