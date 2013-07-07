;;;; package.lisp

(defpackage #:cl-textcat
  (:use #:cl #:alexandria #:optima #:optima.ppcre #:split-sequence)
  (:export #:classify)
  (:nicknames #:textcat))
