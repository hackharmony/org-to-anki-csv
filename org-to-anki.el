(defconst +sep+ "	")

(defcustom captured-tag-name "cap"
  "org-mode tag name for sub-headlines that should be captured as list items with its parent headlines as the front of the flashcard")


(defun zds/org-element-to-text-in-buffer (org-element)
  (replace-regexp-in-string
   ;;;;;;;;; converts/"escapes"
   "	" ;; tab literal ->
   "    " ;; four spaces
   ;; because we're using TABs to represent the CSV separator character
   (replace-regexp-in-string
    "\n$"
    ""
    (buffer-substring-no-properties
     (org-element-property :begin
                           org-element)
     (org-element-property :end
                           org-element)))))

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

(defun zds/flatten-org-parents (org-element)
  (loop for oe = org-element then (org-element-property :parent oe)
        until (null oe)
        collect oe))

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

;; (defun zds/parse-org-ast (tree)
;;   (cond ((null tree) '())

;;         ;; Add captured headers
;;         ((and (eq (org-element-type tree) 'headline) ;
;;               (member captured-tag-name (zds/org-element-tags tree)))
;;          (zds/make-card
;;           (org-element-property :raw-value tree)
;;           "HAHA"))

;;         ;; Add this as a card
;;         ((member (org-element-type tree)
;;                  '(paragraph section))
;;          (zds/make-card
;;           (zds/org-parents tree)
;;           (zds/org-element-to-text-in-buffer tree)))
        
;;         ((eq (org-element-type tree)
;;              'headline)
;;          (seq-mapcat
;;           #'zds/parse-org-ast
;;           (org-element-contents tree)))
        
;;         (t (seq-mapcat
;;             #'zds/parse-org-ast
;;             (org-element-contents tree)))))

(defun zds/any-parents-marked-ignore? (org-element)
  (cl-some #'(lambda (x) (member "ignore" (zds/org-element-tags x)))
           (zds/flatten-org-parents org-element)))

(defun zds/parse-org-sections (tree)
  (org-element-map
      tree
      'section
      (lambda (section)
        (unless (zds/any-parents-marked-ignore? section)
          (zds/make-card
           (zds/org-parents section)
           (zds/org-element-to-text-in-buffer (first (org-element-contents section))))))))

(defun zds/parse-org-captured-headlines-as-list (tree)
  (org-element-map
   tree
   'headline
   (lambda (headline)
     (and (eq (org-element-property :parent headline) tree) ;; only direct descendants
          (member captured-tag-name (zds/org-element-tags headline)) ;; with the capture tag
          (not (zds/any-parents-marked-ignore? headline)) ;; not part of an :ignore:d subtree
          (org-element-property :raw-value headline)))))

(defun zds/make-list (lines &optional bullet)
  (cond ((zerop (length lines)) "")
        ((< (length lines) 2) (car lines))
        (t (let ((bullet (concat (or bullet "+") " ")))
             (concat bullet
                     (mapconcat #'identity lines
                                (concat "\n" bullet)))))))

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
            (zds/make-list children))))))
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


(defun zds/org-set-tag-to-cap (point mark)
  (interactive "r")
  (if (use-region-p)
      (save-excursion
        (progn
          ;; because ORG-CHANGE-TAG-IN-REGION doesn't seem to affect the last line selected in the region, I have to nudge the region one line down
          (goto-char mark)
          (move-end-of-line nil)
          (org-change-tag-in-region point (1+ (point)) captured-tag-name nil)))
    (org-set-tags-to (list captured-tag-name))))

