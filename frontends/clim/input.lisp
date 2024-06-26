(defpackage :lem-clim/input
  (:use :clim-lisp :clim :lem-clim/utils)
  (:export
   :send-lem))

(in-package :lem-clim/input)

(defstruct modifier
  shift
  ctrl
  meta
  super)

(defvar *modifier* (make-modifier))

(defvar *mouse-button* nil)

(defun set-modifier (modifier val)
  (case modifier
    (:shift (setf (modifier-shift *modifier*) val))
    (:ctrl (setf (modifier-ctrl *modifier*) val))
    (:meta (setf (modifier-meta *modifier*) val))
    (:super (setf (modifier-super *modifier*) val))))

(defmacro with-pointer-event-bound ((event medium) &rest body)
  `(let*  ((x (pointer-event-x ,event))
          (y (pointer-event-y ,event))
          (char-x (floor x (text-width ,medium)))
          (char-y (floor y (text-height ,medium))))
     ,@body))

(defmacro with-button-event-bound (event &rest body)
  `(let ((button (case (pointer-event-button ,event)
                  (+pointer-left-button+ :button-1)
                  (+pointer-middle-button+ :button-2)
                  (+pointer-right-button+ :button-3))))
     ,@body))

(defun pointer-scroll (event medium)
  (with-pointer-event-bound (event medium)
    (declare
     (ignore char-x)
     (ignore char-y))))
 
(defun pointer-exit (event medium)
  (declare (ignore event)
           (ignore medium)))

(defun pointer-motion (event medium)
  (with-pointer-event-bound (event medium)
    (lem:send-event
     (lambda ()
       ();;(lem:receive-mouse-motion char-x char-y x y nil)
       ))))
  
(defun pointer-release (event medium)
  (with-pointer-event-bound (event medium)
    (with-button-event-bound event
      (lem:send-event
       (lambda ()
         ();;(lem:receive-mouse-button-up char-x char-y x y button)
	 )))))
  
(defun pointer-press (event medium)
  (with-pointer-event-bound (event medium)
    (with-button-event-bound event
      (lem:send-event
       (lambda ()
         ();;(lem:receive-mouse-button-down char-x char-y x y button 1)
	 )))))
  

(defun key-release (event) 
  (let ((key (keyboard-event-key-name event)))
    (set-modifier key nil)))
        
(defun key-press (event) 
  (let ((key (keyboard-event-key-name event))
        (char (string (keyboard-event-character event))))
    (set-modifier key t)
    ;;(when char
     ;; (lem:send-event 
     ;;  (lem:make-key :shift (modifier-shift *modifier*)
     ;;                :ctrl (modifier-ctrl *modifier*)
     ;;                :meta (modifier-meta *modifier*)
     ;;                :super (modifier-super *modifier*)
     ;;                :sym char)))
    ))

(defun repaint (event)
  (declare (ignore event))
  (let ((height (lem-if:display-height (lem-core:implementation)))
	(width (lem-if:display-width (lem-core:implementation))))
    (log:info "display-height ~a ~% display-width ~a ~%" height width)))

;;(defun key-press (event)
;;  (let* ((char (keyboard-event-key-name event))
;;         (curse (curse *application-frame*))
;;         (next-x (+ (x curse) 10)))
;;    (if (> next-x 
;;           (bounding-rectangle-width 
;;            (sheet-region 
;;             (car
;;              (frame-current-panes *application-frame*)))))
;;        (progn
;;          (setf (x curse) 0)
;;          (setf (y curse) (+ (y curse) 10)))
;;        (setf (x curse) next-x))
;;    (setf (last-char curse) char)))

(defun send-lem (event medium)
  (log:info "event ~a ~%" event)
  (unless (typep event 'character)
    (case (event-type event)
      (:window-repaint (repaint event))
      (:pointer-scroll (pointer-scroll event medium))
      (:pointer-exit (pointer-exit event medium))
      (:pointer-motion (pointer-motion event medium))
      (:pointer-button-press (pointer-press event medium))
      (:pointer-button-release (pointer-release event medium))
      (:key-release (key-release event))
      (:key-press (key-press event))))
  ;;(execute-frame-command *application-frame* (cons 'com-redisplay ()))
  )
