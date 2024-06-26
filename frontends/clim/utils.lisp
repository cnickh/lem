(defpackage :lem-clim/utils
  (:use :clim-lisp :clim)
  (:export
   :text-width
   :text-height))

(in-package :lem-clim/utils)

(defmacro text-width (medium)
  `(text-style-width (medium-text-style ,medium) ,medium))

(defmacro text-height (medium)
  `(text-style-height (medium-text-style ,medium) ,medium))
