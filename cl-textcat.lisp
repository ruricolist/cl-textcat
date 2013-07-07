;;;; cl-textcat.lisp

(in-package #:cl-textcat)

;;; "cl-textcat" goes here. Hacks and glory await!

(eval-and-compile
  (defvar *base*
    #.(fad:pathname-directory-pathname (or *compile-file-truename* *load-truename*))))

(defvar *language-models*
  (let ((models (fad:list-directory (merge-pathnames "lm/" *base*))))
    (format t "~&Loading language models...")
    (loop for model in models
          for language = (make-keyword (upcase (pathname-name model)))
          do (format t " ~(~a~)" language)
          collect (cons language
                        (loop with ngram = (dict)
                              with rang = 1
                              for line in (lines (read-file-into-string model :external-format :latin-1))
                              do (match line
                                   ((ppcre "^([^0-9\\s]+)" ng)
                                    (setf (gethash ng ngram)
                                          (incf rang))))
                              finally (return ngram))))))

(defun classify (input &key (ngram-limit 400)
                       (languages *language-models*)
                       remove-singletons
                       (cutoff 1.05))
  "Return the language of INPUT.

LANGUAGES is the alist of language models to use; default is
*LANGUAGE-MODELS*.

REMOVE-SINGLETONS is either NIL (don't), T (always), or a
number (above this length). It provides a slight increase in speed for
long texts, but should not be used with short texts.

If a guess is unlikelier than than CUTOFF times the previous guess, it
is discarded."
  (let ((unknown
          (mapcar #'car (create-lm input
                                   :remove-singletons
                                   (if (numberp remove-singletons)
                                       (> (length input) remove-singletons)
                                       remove-singletons)))))
    (let ((distances
            (loop for (language . model) in languages
                  collect (cons language
                                (loop for i from 0
                                      for ngram in unknown
                                      for match = (gethash ngram model)
                                      if match
                                        sum (abs (- match i))
                                      else sum ngram-limit)))))
      (let ((results (sort distances #'< :key #'cdr)))
        (let ((results
                (destructuring-bind ((best . score) . rest) results
                  (cons best
                        (let ((base (* score cutoff)))
                          (loop for (lang . score) in rest
                                while (< score base)
                                collect lang))))))
          (values-list (map-into results #'alpha-2 results)))))))

(defun update-lm (lm input)
  (prog1 lm
    (loop for each in (ppcre:split "[0-9\\s]+" input)
          for word = (let (*print-pretty*)
                       (format nil "_~a_" each))
          do (let ((len (length word)))
               (loop for i from 0 below (length word) do
                 (flet ((get-ngram (j)
                          (let ((ngram (subseq word i (+ i j))))
                            (incf (gethash ngram lm 0)))))
                   (when (> len 4)
                     (get-ngram 5))
                   (when (> len 3)
                     (get-ngram 4))
                   (when (> len 2)
                     (get-ngram 3))
                   (when (> len 1)
                     (get-ngram 2))
                   (get-ngram 1)
                   (decf len)))))))

(defun finalize-lm (lm &key (ngram-limit 400) remove-singletons)
  (firstn ngram-limit
          (delete-if
           (lambda (pair)
             (when remove-singletons
               (= (cdr pair) 1)))
           (sort (hash-table-alist lm) #'> :key #'cdr))))

(defun create-lm (input &key remove-singletons (ngram-limit 400))
  (finalize-lm (update-lm (dict) input)
               :remove-singletons remove-singletons
               :ngram-limit ngram-limit))

(defun dump-language-model (model file)
  (with-open-file (f file :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede
                          :external-format :utf-8)
    (let (*print-pretty*)
      (loop for ((ngram . count) . rest) on model
            do (write-string ngram f)
               (write-char #\Tab f)
               (write-char #\Space f)
               (format f "~D" count)
               (when rest
                 (terpri f))))))
