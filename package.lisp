;;;; package.lisp

(defpackage #:cl-textcat
  (:use #:cl #:alexandria #:serapeum
        #:optima #:optima.ppcre
        #:split-sequence)
  (:export #:classify)
  (:shadow #:tokens)
  (:nicknames #:textcat))
