(defpackage :lem-clim/object
  (:use :clim-lisp :clim :lem-core/display :lem-clim/utils :lem-clim/resource)
  (:export
   :+open-folder+
   :object-width
   :object-height
   :object-text
   :draw-object))

(in-package :lem-clim/object)
;;Object Types
;; text-object
;; control-character-object
;; Icon-object
;; folder-object
;; emoji-object
;; eol-cursor-object
;; extend-to-eol-object
;; line-end-object
;; void-object

;;(declaim (ftype function view:set-cursor-position))

(defvar +open-folder+ nil)

(defgeneric draw-object (drawing-object x bottom-y medium view))

(defgeneric object-width (drawing-object medium))

(defgeneric object-height (drawing-object medium))

(defgeneric object-text (drawing-object))

(defmethod draw-object :around (drawing-object x bottom-y medium view)
  (handler-case
      (call-next-method)
      (error (e)
        (log:info "drawing ~a have ERR ~a" drawing-object e)
        0)))

(defmethod object-text ((drawing-object text-object))
  (text-object-string drawing-object))

(defmethod object-text ((drawing-object void-object)) "_")

(defmethod object-text ((drawing-object eol-cursor-object)) "@")

(defmethod object-text ((drawing-object extend-to-eol-object)) "eol")

;;Object Heights

(defmethod object-height ((drawing-object void-object) medium)
  (declare (ignore drawing-object))
  (text-height medium))

(defmethod object-height ((drawing-object text-object) medium)
  (declare (ignore drawing-object))
  (text-height medium))

(defmethod object-height ((drawing-object control-character-object) medium)
  (declare (ignore drawing-object))
  (text-height medium))

(defmethod object-height ((drawing-object icon-object) medium)
  (declare (ignore drawing-object))
  (text-height medium))

(defmethod object-height ((drawing-object folder-object) medium)
  (declare (ignore drawing-object))
  (text-height medium))

(defmethod object-height ((drawing-object eol-cursor-object) medium)
  (declare (ignore drawing-object))
  (text-height medium))

(defmethod object-height ((drawing-object extend-to-eol-object) medium)
  (declare (ignore drawing-object))
  (text-height medium))

(defmethod object-height ((drawing-object emoji-object) medium)
  (declare (ignore drawing-object))
  (text-height medium))
  
(defmethod object-height ((drawing-object line-end-object) medium)
  (declare (ignore drawing-object))
  (text-height medium))

(defmethod object-height ((drawing-object image-object) medium)
  0) ;;Surface Height ::TODO

;;Object Widths
(defmethod object-width ((drawing-object void-object) medium)
  (declare (ignore drawing-object medium))
  0)

(defmethod object-width ((drawing-object control-character-object) medium) 
  (* 2 (text-width medium)))

(defmethod object-width ((drawing-object text-object) medium)
  ;;(text-width medium))
  (multiple-value-bind
         (width height final-x final-y baseline) 
      (text-size 
       medium
       (text-object-string drawing-object))
    (declare (ignore final-y)
             (ignore baseline)
             (ignore width)
             (ignore height)) final-x))

(defmethod object-width ((drawing-object icon-object)  medium)
  (declare (ignore drawing-object medium))
  0) ;;Surface width ::TODO

(defmethod object-width ((drawing-object folder-object) medium)
  (declare (ignore drawing-object))
  (* 2  (text-width medium)))

(defmethod object-width ((drawing-object eol-cursor-object) medium)
  (declare (ignore drawing-object medium))
  0)

(defmethod object-width ((drawing-object extend-to-eol-object) medium)
  (declare (ignore drawing-object medium))
  0)

(defmethod object-width ((drawing-object line-end-object) medium)
  (declare (ignore drawing-object medium))
  0) ;;Surface width

(defmethod object-width ((drawing-object image-object) medium)
  (declare (ignore drawing-object medium))
  0) ;;Surface width

;;Object Draw Methods

(defmethod draw-object ((drawing-object void-object) x bottom-y medium view)
  (declare (ignore drawing-object medium))
  0)

(defmethod draw-object ((drawing-object text-object) x bottom-y medium view)
  ;;(log:info "txt:~a attrib:~a" (object-text drawing-object) (text-object-attribute drawing-object))
  
  (let* ((text (text-object-string drawing-object))
         (attrib (text-object-attribute drawing-object))
         (width (object-width drawing-object medium))	 
         (bg nil) 
	 (uline nil)
	 (fg (medium-foreground medium))
         (text-style *default-text-style*))
    ;;    (log:info "width:~a bg:~a fg:~a tStyle:~a" width bg fg text-style)

    (when attrib
      (setq bg (parse-color (lem-core:attribute-background attrib)))
      (setq uline (lem-core:attribute-underline attrib))
      (when (lem-core:attribute-foreground attrib) (setq fg (parse-color (lem-core:attribute-foreground attrib))))
      (when (lem-core:attribute-bold attrib) 
        (setq text-style (make-text-style (text-style-family *default-text-style*) :bold (text-style-size *default-text-style*))))
          
      (when bg
        (draw-rectangle
         medium
         (make-point x bottom-y)
         (make-point (+ x width)
                     (- bottom-y (text-height medium)))
         :ink bg))
      (when uline nil))
    
    (draw-text medium text (make-point x bottom-y) :align-y :bottom :text-style text-style :ink fg)
    width))

(defmethod draw-object ((drawing-object icon-object) x bottom-y medium view)
  (log:info "draw icon-object stirng:~a attrib:~a " (text-object-string drawing-object) (text-object-atrribute drawing-object))
  0)

(defmethod draw-object
    ((drawing-object folder-object) x bottom-y medium view)

  (unless +open-folder+
    (setq +open-folder+
          (make-pattern-from-bitmap-file
           (get-resource-pathname "resources/open-folder.png") :format :png)))

  (draw-design medium +open-folder+
   :transformation (compose-transformations
		    (make-translation-transformation
		     x (- bottom-y (text-height medium)))
		    (make-scaling-transformation
   		     (/ (object-width drawing-object medium)
			(pattern-width +open-folder+))
   		     (/ (text-height medium)
			(pattern-height +open-folder+)))))
  (object-width drawing-object medium))

(defmethod draw-object ((drawing-object eol-cursor-object) x bottom-y medium view)
  (draw-rectangle 
   medium 
   (make-point x bottom-y)
   (make-point (+ x (text-width medium)) 
               (- bottom-y (object-height drawing-object medium)))
   :ink (parse-raw-color (lem-core/display:eol-cursor-object-color drawing-object))
   :fill t) 0)

(defmethod draw-object
    ((drawing-object extend-to-eol-object) x bottom-y medium view)
;;  (log:info "painting ex-eol ~a x:~a next-x:~a" 
;;            (extend-to-eol-object-color drawing-object) x
;;            (+ (lem-if:view-width (lem-core:implementation) view) x)
;;            (lem-if:view-width (lem-core:implementation) view))
;;  (draw-rectangle
;;   medium
;;   (make-point x bottom-y)
;;   (make-point (floor (+ (lem-if:view-width (lem-core:implementation) view) x) 2)
;;               (- bottom-y (text-height medium)))
;;               
;;   :filled t
;;   :ink (extend-to-eol-object-color drawing-object))
;;  (object-width drawing-object medium))
  0)

(defmethod draw-object
    ((drawing-object line-end-object) x bottom-y medium view)
  (call-next-method drawing-object
                    (+ x 
                       (* (line-end-object-offset drawing-object)
                          (text-width medium)))
                    bottom-y
                    medium
                    view))

(defmethod draw-object ((drawing-object image-object) x bottom-y medium view)
  ;;(log:info "draw image-object ~a" (image-object-image drawing-object))
  0)
