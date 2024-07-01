(defpackage :lem-clim/utils
  (:use :clim-lisp :clim)
  (:export
   :text-width
   :text-height
   :parse-color
   :parse-raw-color))

(in-package :lem-clim/utils)

(defmacro text-width (medium)
  `(text-style-width (medium-text-style ,medium) ,medium))

(defmacro text-height (medium)
;;  `(text-style-height (medium-text-style ,medium) ,medium))

  `(multiple-value-bind
          (width height final-x final-y baseline) 
        (text-size 
         ,medium
         "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM1234567890-=[]\;'./!@#$%^&*()_+{}|:<>?")
      (declare (ignore final-x)
               (ignore final-y)
               (ignore baseline)
               (ignore width)) height))

(defun parse-color (color)
  (let ((pallet (lem:parse-color color)))
    (make-rgb-color
     (/ (lem:color-red pallet) 255)
     (/ (lem:color-green pallet) 255)
     (/ (lem:color-blue pallet) 255))))

(defun parse-raw-color (color)
  (make-rgb-color
     (/ (lem:color-red color) 255)
     (/ (lem:color-green color) 255)
     (/ (lem:color-blue color) 255)))

