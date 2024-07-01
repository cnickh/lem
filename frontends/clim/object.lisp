(defpackage :lem-clim/object
  (:use :clim-lisp :clim :lem-core/display :lem-clim/utils)
;;  (:local-nicknames 
;;                    (:view :lem-clim/view))
  (:export
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

(defgeneric draw-object (drawing-object x bottom-y medium view))

(defgeneric object-width (drawing-object medium))

(defgeneric object-height (drawing-object medium))

(defgeneric object-text (drawing-object))

(defmethod object-text ((drawing-object text-object))
  (text-object-string drawing-object))

(defmethod object-text ((drawing-object void-object)) 0)

(defmethod object-text ((drawing-object eol-cursor-object)) "@")
;;Object Heights

(defmethod object-height ((drawing-object void-object) medium)
  (text-height medium))

(defmethod object-height ((drawing-object text-object) medium)
  (multiple-value-bind
          (width height final-x final-y baseline) 
        (text-size medium (text-object-string drawing-object))
      (declare (ignore final-x)
               (ignore final-y)
               (ignore baseline)
               (ignore width))
      height)) ;;Surface Height

(defmethod object-height ((drawing-object control-character-object) medium) 
  (text-height medium))

(defmethod object-height ((drawing-object icon-object) medium)
  (text-height medium))

(defmethod object-height ((drawing-object folder-object) medium)
  (text-height medium))

(defmethod object-height ((drawing-object eol-cursor-object) medium)
  (text-height medium))

(defmethod object-height ((drawing-object extend-to-eol-object) medium)
  (text-height medium))

(defmethod object-height ((drawing-object emoji-object) medium) 
  (text-height medium))
  
(defmethod object-height ((drawing-object line-end-object) medium)
  (text-height medium))

(defmethod object-height ((drawing-object image-object) medium)
  0) ;;Surface Height

;;Object Widths

(defmethod object-width ((drawing-object void-object) medium)
  0)

(defmethod object-width ((drawing-object control-character-object) medium) 
  (* 2 (text-width medium)))

(defmethod object-width ((drawing-object text-object) medium)
  (let ((width (text-size medium (text-object-string drawing-object))))
    (if (> 0 width) 0 width)))

(defmethod object-width ((drawing-object icon-object)  medium)
  0) ;;Surface width

(defmethod object-width ((drawing-object folder-object) medium)
  (* 2  (text-width medium)))

(defmethod object-width ((drawing-object eol-cursor-object) medium)
  0)

(defmethod object-width ((drawing-object extend-to-eol-object) medium)
  0)

(defmethod object-width ((drawing-object line-end-object) medium)
  ()) ;;Surface width

(defmethod object-width ((drawing-object image-object) medium)
  0) ;;Surface width

;;Object Draw Methods

(defmethod draw-object ((drawing-object void-object) x bottom-y medium view)
  0)

(defmethod draw-object ((drawing-object text-object) x bottom-y medium view)
  (let* ((text (text-object-string drawing-object))
        (attrib (text-object-attribute drawing-object))
        (width (object-width drawing-object medium)))
    
    (log:info "obj-text:~a have attrib ~a " (object-text drawing-object) attrib)
    (when (lem-core:cursor-attribute-p attrib)
      (draw-rectangle
       medium
       (make-point x bottom-y)
       (make-point (+ x (text-width medium))
                   (- bottom-y (text-height medium)))
       :ink (parse-color (lem-core:attribute-background attrib))))
    
    (if attrib
      (draw-text medium text (make-point x bottom-y) :align-y :bottom
                 :ink (parse-color (lem-core:attribute-foreground attrib)))
      (draw-text medium text (make-point x bottom-y)  :align-y :bottom))
    width))

(defmethod draw-object ((drawing-object icon-object) x bottom-y medium view)
  ())

(defmethod draw-object ((drawing-object folder-object) x bottom-y medium view)
  ())

(defmethod draw-object ((drawing-object eol-cursor-object) x bottom-y medium view)
  ;;add display arg?? view arg??
  (draw-rectangle 
   medium 
   (make-point x bottom-y)
   (make-point (+ x (text-width medium)) 
               (- bottom-y (object-height drawing-object medium)))
   :ink (parse-raw-color (lem-core/display:eol-cursor-object-color drawing-object))
   :fill t) 
  0)

(defmethod draw-object
    ((drawing-object extend-to-eol-object) x bottom-y medium view)
;;  (with-drawing-options (medium :ink (eol-cursor-object-color drawing-object))
;;    (draw-rectangle
;;     medium
;;     (make-point x (- bottom-y (text-height medium)))
;;     (make-point (- (lem-if:view-width (lem-core:implementation) view) x)
;;                 (text-height medium))
;;     :fill t))
;;  (object-width drawing-object medium)
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
  0)
