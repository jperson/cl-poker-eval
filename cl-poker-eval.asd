;;;; cl-poker-eval.asd

(asdf:defsystem #:cl-poker-eval
  :author "JRP"
  :version "0.1"
  :components ((:file "package")
               (:file "cl-poker-eval"   :depends-on ("package" "poker-defs" "straight-tbl"))
               (:file "top-5-cards-tbl" :depends-on ("package" "poker-defs"))
               (:file "straight-tbl"    :depends-on ("package" "poker-defs"))
               (:file "top-card-tbl"    :depends-on ("package" "poker-defs"))
               (:file "poker-defs"      :depends-on ("package")) 
               (:file "nbits-tbl"       :depends-on ("package"))))

