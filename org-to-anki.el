(defconst +sep+ "	")


(defun zds/org-element-to-text-in-buffer (org-element)
  (replace-regexp-in-string
   "\n$"
   ""
   (buffer-substring-no-properties
    (org-element-property :begin
                          org-element)
    (org-element-property :end
                          org-element))))

(defun zds/strip-properties (prop)
  (set-text-properties 0 (length prop) nil prop)
  prop)

(defun zds/org-element-tags (org-element)
  (let ((tags (org-element-property :tags org-element)))
    (mapcar
     #'(lambda (tag)
         (set-text-properties 0 (length tag) nil tag)
         tag)
     tags)))


(defun zds/make-card (front back)
  (cons front back))

(defun zds/org-parents-helper (org-element)
  (if (null org-element) '()
    (let* ((parent (org-element-property :parent org-element))
           (text-alone (zds/strip-properties
                    (org-element-property :raw-value parent)))
           (str (format "%s" text-alone)))
      (if (null text-alone)
          (zds/org-parents-helper parent)
        (cons str (zds/org-parents-helper parent))))))

(defun zds/org-parents (org-element)
  (mapconcat #'identity
             (reverse (zds/org-parents-helper org-element))
             " § "))

(defun zds/parse-org-ast (tree)
  (cond ((null tree) '())

        ;; Add captured headers
        ((and (eq (org-element-type tree) 'headline) ;
              (member "cap" (zds/org-element-tags tree)))
         (zds/make-card
          (org-element-property :raw-value tree)
          "HAHA"))

        ;; Add this as a card
        ((member (org-element-type tree)
                 '(paragraph section))
         (zds/make-card
          (zds/org-parents tree)
          (zds/org-element-to-text-in-buffer tree)))
        
        ((eq (org-element-type tree)
             'headline)
         (seq-mapcat
          #'zds/parse-org-ast
          (org-element-contents tree)))
        
        (t (seq-mapcat
            #'zds/parse-org-ast
            (org-element-contents tree)))))

(defun zds/parse-org-sections (tree)
  (org-element-map
      tree
      'section
    (lambda (section)
      (zds/make-card
       (zds/org-parents section)
       (zds/org-element-to-text-in-buffer section)))))

(defun zds/parse-org-captured-headlines-as-list (tree)
  (org-element-map
   tree
   'headline
   (lambda (headline)
     (and (eq (org-element-property :parent headline) tree) ;; only direct descendants
          (member "cap" (zds/org-element-tags headline)) ;; with the capture tag
          (org-element-property :raw-value headline)))))

(defun zds/parse-org-captured-headlines (tree)
  (cl-remove-duplicates
   (org-element-map
       tree
       'headline
     (lambda (headline)
       (let* ((parent (org-element-property :parent headline))
              (parents-string (zds/org-parents headline))
              (children (zds/parse-org-captured-headlines-as-list parent)))
         (unless (or (null parent) (null children))
           (zds/make-card
            (or (zds/org-parents headline) "None")
            (concat "+ " (mapconcat #'identity children "\n+ ")))))))
   :test #'equal))

(defun zds/org-to-anki-csv (dest-file)
  (interactive "FOutput CSV to: ")
  (let* ((tree (org-element-parse-buffer))
         (parsed (concatenate 'list
                              (zds/parse-org-captured-headlines tree)
                              (zds/parse-org-sections tree)))
         (csv (loop for (front . back) in parsed
                    with s = ""
                    do (setf s
                             (concat
                              s
                              front
                              +sep+
                              (replace-regexp-in-string "\n\?$" "<br>" back)
                              "\n"))
                    finally return s)))
    (with-temp-file dest-file
      (insert (concat +sep+ "\n")) ;; first line is separator
      (insert csv)
      )))
