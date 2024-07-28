(defpackage :lem-clim/utils
  (:use :clim-lisp :clim)
  (:export
   :text-width
   :text-height
   :parse-color
   :parse-raw-color
   :log-init
   :log-to-file))

(in-package :lem-clim/utils)

(defmacro text-width (medium)
  `(text-style-width *default-text-style* ,medium))

(defmacro text-height (medium)
  `(text-style-height *default-text-style* ,medium))

(defun parse-color (color)
  (when color
    (let ((pallet (lem:parse-color color)))
      (make-rgb-color
       (/ (lem:color-red pallet) 255)
       (/ (lem:color-green pallet) 255)
       (/ (lem:color-blue pallet) 255)))))

(defun parse-raw-color (color)
  (make-rgb-color
     (/ (lem:color-red color) 255)
     (/ (lem:color-green color) 255)
     (/ (lem:color-blue color) 255)))
