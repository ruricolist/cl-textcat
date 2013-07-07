;;;; cl-textcat.asd

(asdf:defsystem #:cl-textcat
  :serial t
  :description "Language detector"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on (#:alexandria
               #:cl-fad
               #:optima
               #:optima.ppcre
               #:anaphora
               #:split-sequence)
  :components ((:file "package")
               (:file "util")
               (:file "cl-textcat")
               (:file "code-list")))
