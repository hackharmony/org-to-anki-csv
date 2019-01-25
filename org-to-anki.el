(defconst +sep+ "	")

(defcustom captured-tag-name "cap"
  "org-mode tag name for sub-headlines that should be captured as list items with its parent headlines as the front of the flashcard")

(defcustom aslist-tag-name "l"
  "the name of the tag on headlines to indicate that the subheadlines under this headline are to be captured as a list to be on the other side of the flashcard")

(defun org->anki/org-element-to-text-in-buffer (org-element)
  (replace-regexp-in-string
   "\n$"
   ""
   (buffer-substring-no-properties
    (org-element-property :begin
                          org-element)
    (org-element-property :end
                          org-element))))

(defun org->anki/strip-properties (prop)
  (set-text-properties 0 (length prop) nil prop)
  prop)

(defun org->anki/org-element-tags (org-element)
  (let ((tags (org-element-property :tags org-element)))
    (mapcar
     #'(lambda (tag)
         (set-text-properties 0 (length tag) nil tag)
         tag)
     tags)))

(defun org->anki/make-card (front back)
  (cl-flet ((clean-string (s)
                          (replace-regexp-in-string ;; converts/"escapes"
                           "	" ;; tab literal ->
                           "    " ;; four spaces
                           ;; because we're using TABs to represent the CSV separator characters
                           s)))
    (cons (clean-string front) (clean-string back))))

(defun org->anki/flatten-org-parents (org-element)
  (loop for oe = org-element then (org-element-property :parent oe)
        until (null oe)
        collect oe))

(defun org->anki/org-parents-helper (org-element)
  (if (null org-element) '()
    (let* ((parent (org-element-property :parent org-element))
           (text-alone (org->anki/strip-properties
                    (org-element-property :raw-value parent)))
           (str (format "%s" text-alone)))
      (if (null text-alone)
          (org->anki/org-parents-helper parent)
        (cons str (org->anki/org-parents-helper parent))))))

(defun org->anki/org-parents (org-element)
  (mapconcat #'identity
             (reverse (org->anki/org-parents-helper org-element))
             " § "))

;; (defun org->anki/parse-org-ast (tree)
;;   (cond ((null tree) '())

;;         ;; Add captured headers
;;         ((and (eq (org-element-type tree) 'headline) ;
;;               (member captured-tag-name (org->anki/org-element-tags tree)))
;;          (org->anki/make-card
;;           (org-element-property :raw-value tree)
;;           "HAHA"))

;;         ;; Add this as a card
;;         ((member (org-element-type tree)
;;                  '(paragraph section))
;;          (org->anki/make-card
;;           (org->anki/org-parents tree)
;;           (org->anki/org-element-to-text-in-buffer tree)))
        
;;         ((eq (org-element-type tree)
;;              'headline)
;;          (seq-mapcat
;;           #'org->anki/parse-org-ast
;;           (org-element-contents tree)))
        
;;         (t (seq-mapcat
;;             #'org->anki/parse-org-ast
;;             (org-element-contents tree)))))

(defun org->anki/any-parents-marked-ignore? (org-element)
  (cl-some #'(lambda (x) (member "ignore" (org->anki/org-element-tags x)))
           (org->anki/flatten-org-parents org-element)))

(defun org->anki/parse-org-sections (tree)
  (org-element-map
      tree
      'section
      (lambda (section)
        (unless (org->anki/any-parents-marked-ignore? section)
          (org->anki/make-card
           (org->anki/org-parents section)
           (org->anki/org-element-to-text-in-buffer (first (org-element-contents section))))))))

(defun org->anki/parse-org-captured-headlines-as-list (tree)
  (org-element-map
   tree
   'headline
   (lambda (headline)
     (and (eq (org-element-property :parent headline) tree) ;; only direct descendants
          (member captured-tag-name (org->anki/org-element-tags headline)) ;; with the capture tag
          (not (org->anki/any-parents-marked-ignore? headline)) ;; not part of an :ignore:d subtree
          (org-element-property :raw-value headline)))))

(defun org->anki/make-list (lines &optional bullet)
  (cond ((zerop (length lines)) "")
        ((< (length lines) 2) (car lines))
        (t (let ((bullet (concat (or bullet "+") " ")))
             (concat bullet
                     (mapconcat #'identity lines
                                (concat "\n" bullet)))))))

(defun org->anki/parse-org-captured-headlines (tree)
  (cl-remove-duplicates
   (org-element-map
       tree
       'headline
     (lambda (headline)
       (let* ((parent (org-element-property :parent headline))
              (parents-string (org->anki/org-parents headline))
              (children (org->anki/parse-org-captured-headlines-as-list parent)))
         (unless (or (null parent) (null children))
           (org->anki/make-card
            (or (org->anki/org-parents headline) "None")
            (org->anki/make-list children))))))
   :test #'equal))

(defun org->anki/parse-org-aslist-headlines (tree)
  (let ((cards))
    (org-element-map tree
                     'headline
                     #'(lambda (headline)
                         (when (member aslist-tag-name (org->anki/org-element-tags headline))
                           (push (org->anki/make-card
                                  (concat (org->anki/org-parents headline)
                                          " § "
                                          (org-element-property :raw-value headline))
                                  (org->anki/make-list
                                   (org-element-map headline
                                       'headline
                                     #'(lambda (subheadline)
                                         (and (eq (org-element-property :parent subheadline)
                                                  headline)
                                              (org-element-property :raw-value subheadline))))))
                                 cards))))
    cards))
                           

(defun org->anki/org-to-anki-csv (dest-file)
  (interactive "FOutput CSV to: ")
  (let* ((tree (org-element-parse-buffer))
         (parsed (concatenate 'list
                              ;; headlines that were individually captured as list items representing side B of a flashcard with its parent headline as side A
                              (org->anki/parse-org-captured-headlines tree)
                              ;; an "aslist" headline captures all its subheadlines as a list for side B of the card
                              (org->anki/parse-org-aslist-headlines tree)
                              ;; all the text under headlines are captured as cards where: side A is the full sequence of its parent headlines, and side B is the actual text in the section
                              (org->anki/parse-org-sections tree)))
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


(defun org->anki/org-set-tag-to (point mark tag)
  (if (use-region-p)
      (save-excursion
        (progn
          ;; because ORG-CHANGE-TAG-IN-REGION doesn't seem to affect the last line selected in the region, I have to nudge the region one line down
          (goto-char mark)
          (move-end-of-line nil)
          (org-change-tag-in-region point (1+ (point)) captured-tag-name nil)))
    (org-set-tags-to (list tag))))

(defun org->anki/org-set-tag-to-cap (point mark)
  (interactive "r")
  (org->anki/org-set-tag-to point mark captured-tag-name))

(defun org->anki/org-set-tag-to-cap-on-all-same-level-headlines ()
  "Indicate capturing the headline at current point and all its sibling headlines"
  (interactive)
  (let ((element-at-point (org-element-at-point))
        (ast (org-element-parse-buffer))
        (old-point (point))
        (tag-inserts-count 0))
    (when (eq (org-element-type element-at-point) 'headline)
      (org-element-map ast 'headline
        (lambda (headline-at-point)
          (when (equal (org-element-property :raw-value element-at-point)
                       (org-element-property :raw-value headline-at-point))
            (let ((headline-at-point-parent (org-element-property :parent headline-at-point)))
              (org-element-map
                  headline-at-point-parent
                  'headline
                (lambda (sibling-of-target-headline)
                  (and (equal
                        (org-element-property :raw-value headline-at-point-parent)
                        (org-element-property :raw-value (org-element-property :parent sibling-of-target-headline)))
                       (org-element-set-element
                        sibling-of-target-headline
                        (progn (org-element-put-property sibling-of-target-headline
                                                         :tags
                                                         (list captured-tag-name))
                               (incf tag-inserts-count)
                               sibling-of-target-headline)
                        ;; (org-change-tag-in-region
                        ;;  (org-element-property :begin sibling-of-target-headline)
                        ;;  (org-element-property :end sibling-of-target-headline)
                        ;;  captured-tag-name nil)
                        )))))))))
    ;; write the new AST to the buffer
    (erase-buffer)
    (insert (org-element-interpret-data ast))
    (goto-char (+ old-point (* (+ 3 (length captured-tag-name)) tag-inserts-count))) ;; this tries to restore the point and compensate for the newly inserted text
    ;; TODO need to adjust TAG-INSERTS-COUNT to how many inserts were /before/ OLD-POINT
    )
    )

(provide 'org->anki)
