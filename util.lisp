(in-package #:cl-textcat)

(defmacro eval-and-compile (&body body)
  "Emacs's `eval-and-compile'.

Shorthand for
        (eval-when (:compile-toplevel :load-toplevel :execute) ...)"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(eval-and-compile
  (defun unsplice (form)
    "If FORM is non-nil, wrap it in a list.
This is useful with ,@ in macros, and with MAPCAN."
    (if form
        (list form)
        nil)))

(defmacro defsubst (name params &body body)
  "Define NAME as an inline function."
  `(progn
     (declaim (inline ,name))
     (defun ,name ,params
       ,@body)))

(defmacro defconst (symbol initvalue &optional docstring)
  "If SYMBOL is not already bound, bind it as a constant."
  `(defconstant ,symbol
     (if (boundp ',symbol)
         (symbol-value ',symbol)
         ,initvalue)
     ,@(unsplice docstring)))

(defsubst make (class &rest initargs)
  "Shorthand for MAKE-INSTANCE, after Eulisp."
  (declare (dynamic-extent initargs))
  (apply #'make-instance class initargs))

(define-compiler-macro make (class &rest initargs)
  `(make-instance ,class ,@initargs))

(defun dict (&rest keys-and-values &key &allow-other-keys)
  "Return an 'equal hash table containing each successive pair of keys
and values from KEYS-AND-VALUES."
  (plist-hash-table keys-and-values :test 'equal))

(macrolet ((defdict (name test)
             `(define-compiler-macro ,name (&rest keys-and-values &key &allow-other-keys)
                (with-gensyms (ht)
                  `(let ((,ht (make-hash-table :test ',',test)))
                     ,@(unsplice
                        (when keys-and-values
                          `(setf ,@(loop for (key value . rest) on keys-and-values by #'cddr
                                         append `((gethash ,key ,ht) ,value)))))
                     ,ht)))))
  (defdict dict equal))

(defun firstn (n list)
  "Maclisp's FIRSTN: return the first N elements from LIST."
  (loop repeat n for elt in list collect elt))

(defsubst whitespacep (char)
  "Is CHAR whitespace?"
  (case char
    ((#\Space #\Tab #\Linefeed #\Return #\Newline #\Page #\No-break_space) t)))

(defsubst blankp (seq)
  "Is SEQ all whitespace?"
  (every #'whitespacep seq))

(defsubst upcase (x)
  "Upcase a string or character."
  (etypecase x
    (string (string-upcase x))
    (character (char-upcase x))))

(defun lines (string)
  "A list of lines in STRING."
  (declare (string string))
  (split-sequence-if
   (lambda (c)
     (case c
       ((#\Newline #\Return #\Linefeed) t)))
   string :remove-empty-subseqs t))

(defsubst nsubseq (seq start &optional (end (length seq)))
  (make-array (- end start)
              :element-type (array-element-type seq)
              :displaced-to seq
              :displaced-index-offset start))

(defun truncate-list (n list)
  "The destructive equivalent of FIRSTN."
  (check-type n (integer 0 *))
  (cond ((zerop n) nil)
        ((atom list) list)
        ((= n 1) (rplacd list nil))
        (t (rplacd list (truncate-list (1- n) (cdr list))))))
