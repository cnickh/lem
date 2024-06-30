(defpackage :lem-clim/clim
  (:use :clim-lisp :clim :lem-clim/utils)
  (:local-nicknames ;;(:display :lem-clim/display)
                   (:frame :lem-clim/frame)
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
  '(frame:views +app-frame+))

(defun display-pane ()
  (car (frame-current-panes +app-frame+)))

(defun join-ui-thread()
  (handler-case
      (bt:join-thread
       (find-if
	(lambda (th)
          (search "ui-thread" (bt:thread-name th)))
        (bt:all-threads)))

    ;; Catch a user's C-c
    (#+sbcl sb-sys:interactive-interrupt
      () (progn
           (format *error-output* "Aborting.~&")
            (uiop:quit)))
    (error (c)
      (format t "unknown error occured:~&~a~&" c))))

(defun kill-editor-thread ()
  (bt:destroy-thread
   (find-if
    (lambda (th)
      (search "editor" (bt:thread-name th)))
    (bt:all-threads))))

(defmethod lem-if:invoke ((implementation clim) function)
  (format t "invoke called... :) ~%")
  (setq +app-frame+ (make-application-frame 'frame:lem))
  
  (bt:make-thread 
   (lambda () 
     (run-frame-top-level +app-frame+)) 
   :name "ui-thread")

  (sleep 1)
  ;;(format t "Have pane: ~a ~%" (frame-current-panes +app-frame+))
  ;;(loop while (not (display-pane)) 
  ;;      do (format t "Have pane: ~a ~%" (display-pane)))

  (log:info "have text-height == ~a ~%" (text-height (display-pane)))
  (setf (frame:display-char-width +app-frame+)
	(floor (frame:display-width +app-frame+) (text-width (display-pane))))
  (setf (frame:display-char-height +app-frame+)
	(floor (frame:display-height +app-frame+) (text-height (display-pane))))
;  (let ((height
;	  (* (lem-if:get-char-height (lem-core:implementation))
;	     (display-char-height +app-frame+)))
;	(width
;	  (* (lem-if:get-char-width (lem-core:implementation))
;	     (display-char-width +app-frame+))))
;    (layout-frame +app-frame+ height width))

  
  (log:info "calling editor thread")
  (funcall function)
  (join-ui-thread)
  (kill-editor-thread))

(defun parse-color (color)
  (let ((pallet (lem:parse-color color)))
    (make-rgb-color
     (lem:color-red pallet)
     (lem:color-green pallet)
     (lem:color-blue pallet))))

(defmethod lem-if:get-background-color ((implementation clim))
  (frame:background +app-frame+))

(defmethod lem-if:get-foreground-color ((implementation clim))
  (frame:foreground +app-frame+))

(defmethod lem-if:update-foreground ((implementation clim) color-name)
  (log:info "update-foreground called with ~a~%" color-name)
  (setf (frame:foreground +app-frame+) (lem:parse-color color-name))
  (setf (medium-foreground (display-pane)) (parse-color color-name)))

(defmethod lem-if:update-background ((implementation clim) color-name)
  (log:info "update-bg called wtih ~a~%" color-name)
  (setf (frame:background +app-frame+) (lem:parse-color color-name))
  (setf (medium-background (display-pane)) (parse-color color-name)))


(defmethod lem-if:display-width ((implementation clim))
;;  (log:info"display-width :/ ~%")
  (frame:display-char-width +app-frame+))
;;  (let ((pane (car (frame-current-panes +app-frame+))))
;;    (if pane (bounding-rectangle-width (sheet-region pane)) 10)))
  
(defmethod lem-if:display-height ((implementation clim))
  ;;  (log:info "display-height :/ ~%")
  (frame:display-char-height +app-frame+))
;;  (let ((pane (car (frame-current-panes +app-frame+))))
;;    (if pane (bounding-rectangle-height (sheet-region pane)) 10)))
  
(defmethod lem-if:make-view 
    ((implementation clim) window x y width height use-modeline)

 (let ((new-view
	(view:create-view window x y width height use-modeline)))
    (push new-view (current-views))
   (log:info "(create-view ~a ~a ~a ~a ~a ~a) for ~a ~% Have views: [~a]"
             new-view x y width height use-modeline window (current-views))
    new-view))

(defmethod lem-if:delete-view ((implementation clim) view)
  (log:info "delete ~a from ~a ~%" view (current-views))
  (delete-if (lambda (v) (equal view v)) (current-views)))

(defmethod lem-if:clear ((implementation clim) view)
  nil)

(defmethod lem-if:set-view-size ((implementation clim) view width height)
  (log:info "@~a (set-view-size ~a ~a) ~%"  view width height)
  (view:resize view width height))

(defmethod lem-if:set-view-pos ((implementation clim) view x y)
  (log:info "@~a (set-view-pos ~a ~a) ~%"  view x y)
  (view:move-position view x y))

(defmethod lem-if:update-display ((implementation clim))
  ;;(log:info "update-display called ~%")
  (redisplay-frame-panes +app-frame+ :force-p t)
  t)

(defmethod lem-if:view-width ((implementation clim) view)
  (view:view-width view))

(defmethod lem-if:view-height ((implementation clim) view)
  (view:view-height view))

(defmethod lem-if:object-width ((implementation clim) object)
  (obj:object-width object (display-pane)))
  ;;1)

(defmethod lem-if:object-height ((implementation clim) object)
  (obj:object-height object (display-pane)))
  ;;1)

(defmethod lem-if:render-line ((implementation clim) view x y objects height)
  ;;(log:info "(render-line ~a ~a ~a ~a ~a) ~%" view x y objects height)
  (view:update-line view x y objects height))

(defmethod lem-if:clear-to-end-of-window ((implementation clim) view y)
  nil)

(defmethod lem-if:get-char-width ((implementation clim))
  (text-width (display-pane)))

(defmethod lem-if:get-char-height ((implementation clim))
  (text-height (display-pane)))

(defun read-text-objects (objects)
  (loop for object in objects
	collect (obj:object-text object)))

(defun display-modeline-helper
    (view left-objects right-objects default-attribute height)
  ;;(log:info
  ;; "(display-modeline ~a ~a ~a) right ~a left ~a"
  ;; view
  ;; default-attribute height
  ;; (read-text-objects right-objects)
  ;; (read-text-objects left-objects))
  )

(defmethod lem-if:render-line-on-modeline
    ((implementation clim) view left-objects right-objects default-attribute
     height)
  (display-modeline-helper
   view
   left-objects
   right-objects
   default-attribute
   height)

  (setf (view:view-modeline view)
	(list left-objects right-objects default-attribute height)))


(defmacro with-clim (() &body body)
  `(with-implementation (make-instance 'clim)
     (setup-first-frame)
     ,@body))
