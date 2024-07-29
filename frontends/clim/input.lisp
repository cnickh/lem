(defpackage :lem-clim/input
  (:use :clim-lisp :clim :lem-clim/utils)
  (:export
   :key-press
   :pointer-scroll
   :pointer-exit
   :pointer-motion
   :pointer-press
   :pointer-release
   ))

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
                  (,+pointer-left-button+ :button-1)
                  (,+pointer-middle-button+ :button-2)
                  (,+pointer-right-button+ :button-3))))
     ,@body))

(defmacro with-scroll-event-bound (event &rest body)
  `(multiple-value-bind 
         (wheel-x wheel-y)
       (case (pointer-event-button ,event)
         (,+pointer-wheel-up+ (values 0 1))
         (,+pointer-wheel-down+ (values 0 -1))
         (,+pointer-wheel-left+ (values 1 0))
         (,+pointer-wheel-right+ (values -1 0)))
       ,@body))

(defun pointer-scroll (event medium)
  (with-pointer-event-bound (event medium)
    (with-scroll-event-bound event
      ;;(log:info "Have release char-x:~a char-y:~a x:~a y:~a wheel-x:~a wheel-y:~a"
      ;;          char-x char-y x y wheel-x wheel-y)
      (lem:send-event
       (lambda ()
         (lem:receive-mouse-wheel char-x char-y x y wheel-x wheel-y))))))
 
(defun pointer-exit (event medium)
  (declare (ignore event)
           (ignore medium)))

(defun pointer-motion (event medium)
  (with-pointer-event-bound (event medium)
;;    (log:info "Have motion char-x:~a char-y:~a x:~a y:~a"
;;              char-x char-y x y)
   (lem:send-event
      (lambda ()
        (lem:receive-mouse-motion char-x char-y x y *mouse-button*)))))
  
(defun pointer-release (event medium)
  (with-pointer-event-bound (event medium)
    (with-button-event-bound event
      ;;(log:info "Have release char-x:~a char-y:~a x:~a y:~a button: ~a"
      ;;          char-x char-y x y button)
      (setf *mouse-button* nil)
      (lem:send-event
       (lambda ()
         (lem:receive-mouse-button-up char-x char-y x y button))))))
  
(defun pointer-press (event medium)
  (with-pointer-event-bound (event medium)
    (with-button-event-bound event
      ;;(log:info "Have press char-x:~a char-y:~a x:~a y:~a button~a"
      ;;          char-x char-y x y button)
      (setf *mouse-button* button)
      (lem:send-event
       (lambda ()
         (lem:receive-mouse-button-down char-x char-y x y button 1))))))

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
    ;;(log:info "have sym: ~a ~%" sym)
    (when sym 
      (lem:send-event 
       (lem:make-key :shift (bool-and modifier 256) 
                     :ctrl (bool-and modifier 512) 
                     :meta (bool-and modifier 1024)
                     :super (bool-and modifier 128)
                     :sym sym)))))
