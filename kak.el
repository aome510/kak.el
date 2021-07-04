;;; kak.el -*- lexical-binding: t; -*-
;;;
;;;
;;;

;;; overidding `evil-mc-get-default-cursor' to use
;;; `evil-mc-from-real-cursor' that creates a fake cursor
;;; based on the real cursor.

(defun evil-mc-from-real-cursor ()
  "Return a cursor based on the real cursor"
  (let (cursor
        (state (evil-mc-read-cursor-state))
        (names (evil-mc-get-cursor-variables)))
    (dolist (name names)
      (setq cursor (evil-mc-put-cursor-property
                    cursor
                    name
                    (copy-tree (evil-mc-get-cursor-property state name)))))
    cursor))

(advice-add 'evil-mc-get-default-cursor :override
            #'evil-mc-from-real-cursor)

;;; variables

(defvar kak-current-buffer nil)
(defvar kak-region-beg nil)
(defvar kak-region-end nil)
(defvar kak-matches nil)
(defvar kak-invert-search nil)
(defvar kak-match-overlays nil)
(defvar kak-filter-keep nil)

;;; hooks

(defun kak-add-search-highlight (_beg _end _range)
  (progn
    (mapc #'delete-overlay kak-match-overlays)
    (condition-case err
        (let* ((pattern (evil-ex-make-search-pattern (minibuffer-contents)))
               (regex (evil-ex-pattern-regex pattern))
               (case-fold-search (evil-ex-pattern-ignore-case pattern)))
          (with-current-buffer kak-current-buffer
            (progn
              (kak-get-matches-in-region regex)
              (kak-get-match-overlays))))
      (error
       (evil-ex-echo "%s" (cdr err))))))

(defun kak-add-filter-highlight (_beg _end _range)
  (progn
    (mapc #'delete-overlay kak-match-overlays)
    (condition-case err
        (let* ((pattern (evil-ex-make-search-pattern (minibuffer-contents)))
               (regex (evil-ex-pattern-regex pattern))
               (case-fold-search (evil-ex-pattern-ignore-case pattern)))
          (with-current-buffer kak-current-buffer
            (progn
              (setq kak-matches (seq-filter
                                 (lambda (match)
                                   (xor kak-filter-keep
                                        (not (string-match-p regex
                                                             (buffer-substring-no-properties
                                                              (cl-first match)
                                                              (cl-second match))))))
                                 (kak-get-matches)))
              (kak-get-match-overlays))))
      (error
       (evil-ex-echo "%s" (cdr err))))))

(defun kak-start-filter-session()
  (add-hook 'after-change-functions #'kak-add-filter-highlight nil t))

(defun kak-end-filter-session ()
  (mapc #'delete-overlay kak-match-overlays)
  (remove-hook 'minibuffer-setup-hook #'kak-start-filter-session)
  (remove-hook 'minibuffer-exit-hook #'kak-end-filter-session)
  (remove-hook 'after-change-functions #'kak-add-filter-highlight))

(defun kak-start-search-session()
  (add-hook 'after-change-functions #'kak-add-search-highlight nil t))

(defun kak-end-search-session ()
  (mapc #'delete-overlay kak-match-overlays)
  (remove-hook 'minibuffer-setup-hook #'kak-start-search-session)
  (remove-hook 'minibuffer-exit-hook #'kak-end-search-session)
  (remove-hook 'after-change-functions #'kak-add-search-highlight))

;;; functions

(defun kak-get-match-overlays ()
  (setq kak-match-overlays (mapcar
                            (lambda (match) (make-overlay (cl-first match) (cl-second match)))
                            kak-matches))
  (mapcar (lambda (overlay)
            (overlay-put overlay 'priority 1000)
            (overlay-put overlay 'face 'evil-ex-lazy-highlight))
          kak-match-overlays))

(defun kak-get-invert-matches (matches beg end)
  (if (> beg end) nil
    (pcase matches
      ('nil `((,beg ,end)))
      (`(,match . ,matches)
       (if (eq beg (cl-first match))
           (kak-get-invert-matches matches (cl-second match) end)
         (cons `(,beg ,(cl-first match))
               (kak-get-invert-matches matches (cl-second match) end)))))))

(defun kak-get-matches-in-region (regex)
  (let ((match))
    (setq! kak-matches nil
           kak-match-overlays nil)
    (goto-char kak-region-beg)
    (if (search-forward-regexp regex nil kak-region-end)
        (setq match (match-data 0)) (setq match nil))
    (while (and match (<= (cl-second match) kak-region-end))
      (message "%s" match)
      (push match kak-matches)
      (goto-char (cl-second match))
      ;; handle eof and eol cases
      (when (= (point) (line-end-position))
        (forward-char 1))
      (when (= (point) (point-max))
        (forward-char -1))
      (if (search-forward-regexp regex nil kak-region-end)
          (setq match (match-data 0)) (setq match nil)))
    (setq kak-matches (reverse kak-matches))
    (when kak-invert-search
      (setq kak-matches (kak-get-invert-matches kak-matches kak-region-beg kak-region-end)))))

(defun kak-make-cursors-for-matches (matches)
  (pcase matches
    (`(,match . nil) (evil-visual-make-selection (cl-first match) (1- (cl-second match))))
    (`(,match . ,matches) (progn
                            (let ((cursor) (region)
                                  (pos (1- (cl-second match))))
                              (goto-char pos)
                              (setq region (evil-mc-create-region (cl-first match) pos 'char))
                              (setq cursor (evil-mc-put-cursor-property
                                            (evil-mc-read-cursor-state)
                                            'last-position pos
                                            'order (if (null evil-mc-cursor-list) 1
                                                     (1+ (apply #'max
                                                                (mapcar (lambda (cursor)
                                                                          (evil-mc-get-cursor-property cursor 'order))
                                                                        evil-mc-cursor-list))))
                                            'temporary-goal-column (evil-mc-column-number pos)
                                            'overlay (evil-mc-cursor-overlay-at-pos pos)
                                            'region region))
                              (evil-mc-run-cursors-before)
                              (evil-mc-insert-cursor cursor))
                            (kak-make-cursors-for-matches matches)))))

(defun kak-init-search (beg end &optional invert)
  (progn
    (setq! kak-current-buffer (current-buffer)
           kak-region-beg beg
           kak-region-end end
           kak-invert-search invert
           kak-match-overlays nil)
    (add-hook 'minibuffer-setup-hook #'kak-start-search-session)
    (add-hook 'minibuffer-exit-hook #'kak-end-search-session)))

(defun kak-get-matches ()
  (let ((matches
         (cons `(,(region-beginning) ,(region-end))
               (mapcar
                (lambda (cursor)
                  (let ((region (evil-mc-get-cursor-region cursor)))
                    `(,(evil-mc-get-region-start region)
                      ,(evil-mc-get-region-end region))))
                evil-mc-cursor-list))))
    (cl-sort matches #'< :key #'car)))

(defun kak-init-filter (&optional keep)
  (progn
    (setq! kak-current-buffer (current-buffer)
           kak-filter-keep keep
           kak-match-overlays nil)
    (add-hook 'minibuffer-setup-hook #'kak-start-filter-session)
    (add-hook 'minibuffer-exit-hook #'kak-end-filter-session)))

;;; interactive functions

(defun kak-restore-last-region ()
  (interactive)
  (if (and kak-region-beg kak-region-end)
      (evil-visual-make-region kak-region-beg (1- kak-region-end))))

(defun kak-select (beg end &optional invert)
  (interactive "r")
  (if (evil-visual-state-p)
      (progn
        (kak-init-search beg end invert)
        (read-regexp "pattern: ")
        (progn
          (goto-char beg)
          (if kak-matches
              (progn (evil-exit-visual-state) (kak-make-cursors-for-matches kak-matches))
            (user-error "no match"))))
    (user-error "must be in visual state")))

(defun kak-filter (&optional keep)
  (interactive)
  (if (evil-visual-state-p)
      (progn
        (kak-init-filter keep)
        (read-regexp "pattern: ")
        (if kak-matches
            (progn
              (evil-mc-undo-all-cursors)
              (kak-make-cursors-for-matches kak-matches))
          (user-error "no matches remaining")))
    (user-error "must be in visual state")))

(defun kak-exec-shell-command (command)
  (interactive (list (read-shell-command "Shell command: ")))
  (if (evil-visual-state-p)
      (evil-mc-execute-for-all-cursors
       (lambda (cursor)
         (let (region beg end (index (evil-mc-get-cursor-property cursor :index)))
           (if (eq index 0) ;; real cursor
               (progn
                 (setq beg (region-beginning))
                 (setq end (region-end))
                 (evil-shell-command-on-region beg end command)
                 (evil-exit-visual-state))
             (progn
               (setq region (evil-mc-get-cursor-region cursor))
               (setq beg (evil-mc-get-region-start region))
               (setq end (evil-mc-get-region-end region))
               (evil-shell-command-on-region beg end command))))))
    (user-error "must be in visual state")))

(defun kak-insert-index (base)
  (interactive "nbase index: ")
  (setq base (1- base))
  (evil-mc-execute-for-all-cursors
   (lambda (cursor)
     (let ((index (evil-mc-get-cursor-property cursor :index)))
       (if (eq index 0) ;; real cursor, assume that it's always the last cursor
           (progn
             (insert (number-to-string (+ index base (evil-mc-get-cursor-count))))
             (evil-exit-visual-state))
         (progn
           (goto-char (evil-mc-get-cursor-end cursor))
           (insert (number-to-string (+ index base)))
           (backward-char 1)))))))

(map!
 :v ". #" #'kak-insert-index
 :v ". |" #'kak-exec-shell-command
 :v ". s" #'(lambda (beg end) (interactive "r") (kak-select beg end nil))
 :v ". S" #'(lambda (beg end) (interactive "r") (kak-select beg end t))
 :v ". k" (lambda () (interactive) (kak-filter t))
 :v ". K" (lambda () (interactive) (kak-filter nil))
 )

(provide 'kak)
