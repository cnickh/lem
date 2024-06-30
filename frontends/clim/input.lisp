(defpackage :lem-clim/input
  (:use :clim-lisp :clim :lem-clim/utils)
  (:export
   :send-lem))

(in-package :lem-clim/input)

(defvar *mouse-button* nil)

(defparameter *code-name-table*
  '(("BACKSPACE" "Backspace" nil)
    ("TAB" "Tab" nil)
    ("RETURN" "Return" nil)
    ("INSERT" "Insert" nil)
    ("DELETE" "Delete" nil)
    ("SPACE" "Space" t)
    ("HOME" "Home" nil)
    ("END" "End" nil)
    ("PAGEUP" "PageUp" nil)
    ("PAGEDOWN" "PageDown" nil)
    ("ESCAPE" "Escape" nil)
    ("LEFT" "Left" nil)
    ("RIGHT" "Right" nil)
    ("UP" "Up" nil)
    ("DOWN" "Down" nil)
    ("F1" "F1" nil)
    ("F2" "F2" nil)
    ("F3" "F3" nil)
    ("F4" "F4" nil)
    ("F5" "F5" nil)
    ("F6" "F6" nil)
    ("F7" "F7" nil)
    ("F8" "F8" nil)
    ("F9" "F9" nil)
    ("F10" "F10" nil)
    ("F11" "F11" nil)
    ("F12" "F12" nil)))

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
  
;;(defun key-release (event) 
;;  (let ((key (keyboard-event-key-name event)))
;;    (set-modifier key nil)))

(defun convert-to-sym (event)
  (let* ((key-code (keyboard-event-key-name event))
        (keyinfo (assoc key-code *code-name-table* :test #'string=)) 
        (key (keyboard-event-character event)))

    ;;(log:info "have name ~a ~%" key-code)
    (if keyinfo
        (cadr keyinfo)
        (when key 
          (string key)))))
        
(defmacro bool-and (modifier code)
  `(not (eq (logand ,modifier ,code) 0)))

(defun key-press (event)
  (let ((sym (convert-to-sym event))
        (modifier (event-modifier-state event)))
    (log:info "have sym: ~a ~%" sym)
    (when sym 
      (lem:send-event 
       (lem:make-key :shift (bool-and modifier 256) 
                     :ctrl (bool-and modifier 512) 
                     :meta (bool-and modifier 1024)
                     :super (bool-and modifier 128)
                     :sym sym)))))

(defun repaint (event)
  (declare (ignore event))
  (let ((height (lem-if:display-height (lem-core:implementation)))
	(width (lem-if:display-width (lem-core:implementation))))
    ;;(log:info "display-height ~a ~% display-width ~a ~%" height width)
    ))

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
  ;;(log:info "event ~a ~%" event)
  (unless (typep event 'character)
    (case (event-type event)
      ;;(:window-repaint (repaint event))
      ;;(:pointer-scroll (pointer-scroll event medium))
      ;;(:pointer-exit (pointer-exit event medium))
      ;;(:pointer-motion (pointer-motion event medium))
      ;;(:pointer-button-press (pointer-press event medium))
      ;;(:pointer-button-release (pointer-release event medium))
      ;;(:key-release (key-release event))
      (:key-press (key-press event))))
  ;;(execute-frame-command *application-frame* (cons 'com-redisplay ()))
  )
