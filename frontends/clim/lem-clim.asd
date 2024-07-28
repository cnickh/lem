(defsystem "lem-clim"
  :depends-on ("mcclim"
               "lem"
               "lem/extensions")
  :serial t
  :components (
               (:file "resource")
               (:file "utils")
               (:file "object")
               (:file "view")
               (:file "input")
               (:file "frame")
               (:file "clim")
               ))

(defsystem "lem-clim/executable"
  :build-operation program-op
  :build-pathname "../../lem"
  :entry-point "lem:main"
  :depends-on ("lem-clim"))