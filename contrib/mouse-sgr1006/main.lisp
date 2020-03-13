(uiop/package:define-package :lem-mouse-sgr1006/main
  (:nicknames :lem-mouse-sgr1006) (:use :cl :lem)
  (:shadow) (:export :parse-mouse-event) (:intern))
(in-package :lem-mouse-sgr1006/main)
;;;don't edit above
(defparameter *message-on-mouse-event* nil)

(defvar *dragging-window* ())
(defvar *min-cols*  5)
(defvar *min-lines* 1)
(defvar *wheel-scroll-size* 3)

(defun get-window-rect (window)
  (values (lem:window-x     window)
          (lem:window-y     window)
          (lem:window-width window)
          (- (lem:window-height window)
             (if (lem::window-use-modeline-p window) 1 0))))

(defun move-to-cursor (window x y)
  (lem:move-point (lem:current-point) (lem::window-view-point window))
  (lem:move-to-next-virtual-line (lem:current-point) y)
  (lem:move-to-virtual-line-column (lem:current-point) x))

(defun parse-mouse-event (getch-fn)
  (let ((msg (loop :for c := (code-char (funcall getch-fn))
                   :with result
                   :with part
                   :until (or (char= c #\m)
                              (char= c #\M))
                   :when (char= c #\;)
                   :do (setq result #1=(cons (parse-integer (format nil "~{~A~}"
                                                                    (reverse part)))
                                             result)
                             part nil)
                   :else
                   :do (push c part)
                   :finally (return (cons c (reverse #1#))))))
    (lambda ()
      (multiple-value-bind (bstate bno x1 y1)
          (apply #'values msg)
        ;; convert mouse position from 1-origin to 0-origin
        (decf x1)
        (decf y1)
        ;; check mouse status
        (when (or (and (not lem::*floating-windows*)
                       (eql bno 0)      ; button-1
                       (or (eql bstate #\m)
                           (eql bstate #\M)))
                  (and (or (eql bno 64) ; wheel
                           (eql bno 65))
                       (eql bstate #\M)))
          ;; send a dummy key event to exit isearch-mode
          (lem:send-event (lem:make-key :sym "NopKey")))
        ;; send actual mouse event
        (lem:send-event (parse-mouse-event-sub bstate bno x1 y1))))))

(defun parse-mouse-event-sub (bstate bno x1 y1)
  (lambda ()
    ;; process mouse event
    (cond
      ;; button-1 down
      ((and (not lem::*floating-windows*)
            (eql bno 0)
            (eql bstate #\M))
       (find-if
        (lambda(o)
          (multiple-value-bind (x y w h) (get-window-rect o)
            (cond
              ;; vertical dragging window
              ((and (= y1 (- y 1)) (<= x x1 (+ x w -1)))
               (setf *dragging-window* (list o 'y))
               t)
              ;; horizontal dragging window
              ((and (= x1 (- x 1)) (<= y y1 (+ y h -1)))
               (setf *dragging-window* (list o 'x))
               t)
              ;; move cursor
              ((and (<= x x1 (+ x w -1)) (<= y y1 (+ y h -1)))
               (setf (lem:current-window) o)
               (move-to-cursor o (- x1 x) (- y1 y))
               (lem:redraw-display)
               t)
              (t nil))))
        ;; include active minibuffer window
        (if (lem::active-minibuffer-window)
            (cons (lem::active-minibuffer-window) (lem:window-list))
            (lem:window-list))))
      ;; button-1 up
      ((and (eql bno 0)
            (eql bstate #\m))
       (let ((o-orig (lem:current-window))
             (o (first *dragging-window*)))
         (when (windowp o)
           (multiple-value-bind (x y w h) (get-window-rect o)
             (declare (ignore x y))
             (cond
               ;; vertical dragging window
               ((eq (second *dragging-window*) 'y)
                (let ((vy (- (- (lem:window-y o) 1) y1)))
                  ;; this check is incomplete if 3 or more divisions exist
                  (when (and (not lem::*floating-windows*)
                             (>= y1       *min-lines*)
                             (>= (+ h vy) *min-lines*))
                    (setf (lem:current-window) o)
                    (lem:grow-window vy)
                    (setf (lem:current-window) o-orig)
                    (lem:redraw-display))))
               ;; horizontal dragging window
               (t
                (let ((vx (- (- (lem:window-x o) 1) x1)))
                  ;; this check is incomplete if 3 or more divisions exist
                  (when (and (not lem::*floating-windows*)
                             (>= x1       *min-cols*)
                             (>= (+ w vx) *min-cols*))
                    (setf (lem:current-window) o)
                    (lem:grow-window-horizontally vx)
                    (setf (lem:current-window) o-orig)
                    (lem:redraw-display))))
               )))
         (when o
           (setf *dragging-window*
                 (list nil (list x1 y1) *dragging-window*)))))
      ;; wheel up
      ((and (eql bno 64)
            (eql bstate #\M))
       (lem:scroll-up *wheel-scroll-size*)
       (lem:redraw-display))
      ;; wheel down
      ((and (eql bno 65)
            (eql bstate #\M))
       (lem:scroll-down *wheel-scroll-size*)
       (lem:redraw-display))
      )
    (when *message-on-mouse-event*
      (lem:message "mouse:~s ~s ~s ~s" bstate bno x1 y1)
      (lem:redraw-display))))

(defvar *enable-hook* '())
(defvar *disable-hook* '())

(defun enable-hook ()
  (format lem::*terminal-io-saved* "~A[?1000h~A[?1002h~A[?1006h~%" #\esc #\esc #\esc)
  (ignore-errors
   (dolist (window (lem:window-list))
     (lem::screen-clear (lem::window-screen window)))
   (lem:redraw-display))
  (run-hooks *enable-hook*))

(defun disable-hook ()
  (format lem::*terminal-io-saved* "~A[?1006l~A[?1002l~A[?1000l~%" #\esc #\esc #\esc)
  (ignore-errors
   (dolist (window (lem:window-list))
     (lem::screen-clear (lem::window-screen window)))
   (lem:redraw-display))
  (run-hooks *disable-hook*))

(define-minor-mode mouse-sgr-1006-mode
  (:global t
   :enable-hook #'enable-hook
   :disable-hook #'disable-hook))

(defun enable-mouse-sgr-1006-mode ()
  (mouse-sgr-1006-mode t))

(defun disable-mouse-sgr-1006-mode ()
  (mouse-sgr-1006-mode nil))

(add-hook *after-init-hook* 'enable-mouse-sgr-1006-mode)
(add-hook *exit-editor-hook* 'disable-mouse-sgr-1006-mode)

