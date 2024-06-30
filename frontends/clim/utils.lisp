(defpackage :lem-clim/utils
  (:use :clim-lisp :clim)
  (:export
   :text-width
   :text-height))

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

