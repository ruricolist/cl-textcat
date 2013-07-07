(in-package :cl-textcat)

(ql:quickload '(:drakma :cxml :cxml-stp :closure-html :css-selectors-stp))

(defclass bible-parser (sax:default-handler)
  ((lm :initform (make-hash-table :test 'equal :size #.(1- (expt 2 9))))
   alpha-3))

(defmethod sax:characters ((handler bible-parser) data)
  (with-slots (lm) handler
    (update-lm lm data)))

(defmethod sax:start-element ((handler bible-parser) ns local qname attrs)
  (with-slots (alpha-3) handler
    (when (equal qname "language")
      (when-let (attr (sax:find-attribute "iso639" attrs))
        (setf alpha-3 (sax:attribute-value attr))))))

(defun parse-bible (file)
  (let ((parser (make 'bible-parser)))
    (with-slots (lm alpha-3) parser
      (cxml:parse-file file parser)
      (values (finalize-lm lm) alpha-3))))

(defun bibles->language-models (bible-dir models-dir)
  "BIBLE-DIR is the source of the XML-formatted Bibles; MODELS-DIR is
where the language models should be saved."
  (let ((bibles (directory (merge-pathnames (make-pathname :name :wild :type "xml")
                                            (fad:pathname-as-directory bible-dir))))
        (models-dir (ensure-directories-exist (fad:pathname-as-directory models-dir))))
    (loop for bible in bibles do
      (multiple-value-bind (lm alpha-3)
          (parse-bible bible)
        (format t "~&Parsed ~a" alpha-3)
        (dump-language-model lm (merge-pathnames (format nil "~a.lm" alpha-3) models-dir))))))

(defparameter *code-list*
  "http://www.loc.gov/standards/iso639-2/php/code_list.php"
  "Official list of ISO639-2 language codes at the LOC.")

(defun node-text (node)
  (typecase node
    (string node)
    (stp:text (stp:data node))
    ((or stp:element stp:document)
     (when-let (text (stp:filter-recursively (of-type 'stp:text) node))
       (let ((data
               (with-output-to-string (s)
                 (loop for (node . more) on text
                       do (write-string (stp:data node) s)
                          (when more
                            (write-char #\Space s))))))
         data)))))

(defun update-code-list ()
  (let ((page (chtml:parse (drakma:http-request *code-list*)
                           (stp:make-builder))))
    (with-open-file (s (merge-pathnames "code-list.lisp" *base*)
                      :direction :output
                      :if-exists :supersede
                      :if-does-not-exist :create)
      (prin1 `(in-package #:cl-textcat) s)
      (terpri s)
      (prin1 `(defun alpha-2 (alpha-3)
                (case alpha-3
                  ,@(remove-duplicates
                     (loop for row in (butlast (cdr (css:query "table table tr" page)))
                           for (codes alpha-2 en fr) = (mapcar #'node-text (css:query "td, th" row))
                           unless (blankp alpha-2)
                             collect `((,@(loop for code in (ppcre:split "\\s+" codes)
                                                unless (string= "(" (aref code 0))
                                                  collect (make-keyword (upcase code))))
                                       ,(make-keyword (upcase alpha-2))))
                     :test #'equal)
                  (t alpha-3)))
             s))))
