;;;; cl-textcat.lisp

(in-package #:cl-textcat)

;;; "cl-textcat" goes here. Hacks and glory await!

(eval-and-compile
  (defvar *base*
    #.(fad:pathname-directory-pathname (or *compile-file-truename* *load-truename*))))

(defun load-model (file)
  (let ((lines
          (lines
           (read-file-into-string
            file :external-format :utf-8))))
    (loop with ngrams = (dict)
          with rang = 1
          for line in lines
          do (match line
               ((ppcre "^([^0-9\\s]+)" ng)
                (setf (gethash ng ngrams) (incf rang))))
          finally (return ngrams))))

(defvar *language-models*
  (load-time-value
   (let ((models (fad:list-directory (merge-pathnames "lm/" *base*))))
     (format t "~&Loading language models...")
     (loop for model in models
           for language = (make-keyword (upcase (pathname-name model)))
           do (format t " ~(~a~)" language)
           collect (cons language (load-model model))))
   t))

(defun classify (input &key (ngram-limit 400)
                            (languages *language-models*)
                            remove-singletons
                            (cutoff 1.05)
                            (sample-size 20000))
  "Return the language of INPUT.

LANGUAGES is the alist of language models to use; default is
*LANGUAGE-MODELS*.

SAMPLE-SIZE limits the portion of INPUT used to generate the ngram
profile to a certain number of characters (default is 20,000, which is
plenty). NIL means no limit.

REMOVE-SINGLETONS is either NIL (don't), T (always), or a
number (above this length). It provides a slight increase in speed for
long texts, but should not be used with short texts.

If a guess is unlikelier than than CUTOFF times the previous guess, it
is discarded."
  (let* ((sample
           (if (or (not sample-size)
                   (> sample-size (length input)))
               input
               (nsubseq input 0 sample-size)))
         (unknown
           (let ((lm (create-lm sample
                                :remove-singletons
                                (if (numberp remove-singletons)
                                    (> (length sample) remove-singletons)
                                    remove-singletons))))
             (map-into lm #'car lm)))
         (distances
           (loop for (language . model) in languages
                 collect (cons language
                               (loop for i from 0
                                     for ngram in unknown
                                     for match = (gethash ngram model)
                                     if match
                                       sum (abs (- match i))
                                     else sum ngram-limit))))
         (results (sort distances #'< :key #'cdr)))
    (let ((results
            (destructuring-bind ((best . score) . rest) results
              (cons (alpha-2 best)
                    (let ((base (* score cutoff)))
                      (loop for (lang . score) in rest
                            while (< score base)
                            collect (alpha-2 lang)))))))
      (values (first results) (rest results)))))

(defun update-lm (lm input)
  (prog1 lm
    (dolist (token (tokens input))
      (map-ngrams
       (lambda (ngram)
         (incf (gethash ngram lm 0)))
       token))))

(defun map-ngrams (fn token)
  "Call FN on each ngram of TOKEN.
This implements the reduced n-gram approach from Hornik et al."
  (declare (optimize (speed 3) (safety 0) (debug 0)
                     (compilation-speed 0))
           (string token))
  (let ((word (concatenate 'string "_" token "_"))
        (fn (ensure-function fn)))
    (declare ((simple-array character (*)) word))
    ;; Words of length 1 yield only one trigram.
    (if (= 1 (length token))
        (funcall fn word)
        (let ((len (length word)))
          (declare (array-length len))
          (loop for i of-type array-length from 0 below len do
            (flet ((get-ngram (j)
                     (declare ((integer 1 5) j))
                     ;; Ignore ngrams that line up with either end of
                     ;; the word, but don't have the space attached.
                     (unless (or (= i 1) (= j (1- len)))
                       (let ((ngram (subseq word i (+ i j))))
                         ;; Ignore the trivial ngram "_".
                         (unless (equal ngram "_")
                           (prog1 nil
                             (funcall fn ngram)))))))
              (declare (dynamic-extent (function get-ngram)))
              (tagbody (case len
                         (1 (go :1))
                         (2 (go :2))
                         (3 (go :3))
                         (4 (go :4))
                         (t (go :5)))
               :5 (get-ngram 5)
               :4 (get-ngram 4)
               :3 (get-ngram 3)
               :2 (get-ngram 2)
               :1 (get-ngram 1)
                 (decf len))))))))

(assert
 (set-equal
  (let ((ngrams '()))
    (map-ngrams (lambda (ngram) (push ngram ngrams))
                "corpus")
    ngrams)
  (split-sequence #\Space "_c _co _cor _corp o or orp orpu r rp rpu rpus_ p pu pus_ u us_ s_")
  :test 'equal))

(defun tokens (input)
  (declare (optimize speed))
  (split-sequence:split-sequence-if
   (lambda (c)
     (declare (character c))
     (case c
       (#.(coerce "0123456789" 'list) t)
       ((#\Space #\Tab #\Linefeed #\Return #\Page) t)
       (t nil)))
   input
   :remove-empty-subseqs t))

(defun finalize-lm (lm &key (ngram-limit 400) remove-singletons)
  (when remove-singletons
    (maphash (lambda (k v)
               (when (= v 1)
                 (remhash k lm)))
             lm))
  (let ((alist (hash-table-alist lm)))
    (firstn ngram-limit (sort alist #'> :key #'cdr))))

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
