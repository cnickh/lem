(defsystem "lem-legit"
  :serial t
  :depends-on ("lem" "lem-patch-mode" "lem-yaml-mode" "lem-markdown-mode")
  :components ((:module "./"
                :components ((:file "porcelain")
                             (:file "legit-common")
                             (:file "peek-legit")
                             (:file "legit")
                             (:file "legit-rebase")
                             (:file "legit-commit")))
               (:module "scripts"
                :components ((:static-file "dumbrebaseeditor.sh")))))
