;;check add underline

(when (and attrib
           (lem:attribute-underline attrib))
  (draw-line 
   medium
   (make-point x bottom-y)
   (make-point (+ x width) bottom-y)
   :ink (let ((underline (lem:attribute-underline attrib)))
          (if (eq underline t)
              (lem-core:attribute-foreground-color attrib)
              (or (lem:parse-color underline)
                  (lem-core:attribute-foreground-color attrib))))))


  `(multiple-value-bind
          (width height final-x final-y baseline) 
        (text-size 
         ,medium
         "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM1234567890-=[]\;'./!@#$%^&*()_+{}|:<>?")
      (declare (ignore final-x)
               (ignore final-y)
               (ignore baseline)
               (ignore width)) height))

(defun draw-view-lines (buffer-view pane)
  (loop
    for line in (view-lines buffer-view)
    do (loop
         :with current-x := (+ (car line) (* (view-x buffer-view) (text-width pane)))
         :with y := (+ (cadr line) (* (view-y buffer-view) (text-height pane)))
         :with height := (nth 3 line)
         :for object :in (nth 2 line)
         :do (progn
               ;;(log:info "drawing obj: ~a == ~a" object (obj:object-text object))
               (incf
                current-x
                (obj:draw-object object current-x (+ y height) pane buffer-view))))))