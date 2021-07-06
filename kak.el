;;; kak.el --- An attempt to port Kakoune's interactive multiple cursors to Emacs -*- lexical-binding: t; -*-

;; Author: Thang Pham <phamducthang1234@gmail.com>
;; Version: 0.1
;; MIT License

;;; Commentary:
;;; This package provides functions that simulate Kakoune's multi-selection commands.
;;; It is built on top of `evil-mc''s multiple cursors and `evil''s functions.

;;; In this package, a matching region and a cursor's region are used interchangably.
;;; A matching region indicates the region of a real cursor and `evil-mc''s fake cursors.

;;; Code:
(require 'evil)
(require 'evil-mc)
(require 'core-lib)

(defun evil-mc-create-fake-cursor-from-real-cursor ()
  "Create an `evil-mc' fake cursor from the real cursor state."
  (let (cursor
        (state (evil-mc-read-cursor-state))
        (names (evil-mc-get-cursor-variables)))
    (dolist (name names)
      (setq cursor (evil-mc-put-cursor-property
                    cursor
                    name
                    (copy-tree (evil-mc-get-cursor-property state name)))))
    cursor))

;; overidding `evil-mc-get-default-cursor' to use `evil-mc-create-fake-cursor-from-real-cursor' for creating a fake cursor
(advice-add 'evil-mc-get-default-cursor :override
            #'evil-mc-create-fake-cursor-from-real-cursor)

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
  "Update highlight for all search matching regions."
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
  "Update highlight for all filter matching regions."
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
  "Start a filter session."
  (add-hook 'after-change-functions #'kak-add-filter-highlight nil t))

(defun kak-end-filter-session ()
  "End the current filter session."
  (mapc #'delete-overlay kak-match-overlays)
  (remove-hook 'minibuffer-setup-hook #'kak-start-filter-session)
  (remove-hook 'minibuffer-exit-hook #'kak-end-filter-session)
  (remove-hook 'after-change-functions #'kak-add-filter-highlight))

(defun kak-start-search-session()
  "Start a search session."
  (add-hook 'after-change-functions #'kak-add-search-highlight nil t))

(defun kak-end-search-session ()
  "End the current search session."
  (mapc #'delete-overlay kak-match-overlays)
  (remove-hook 'minibuffer-setup-hook #'kak-start-search-session)
  (remove-hook 'minibuffer-exit-hook #'kak-end-search-session)
  (remove-hook 'after-change-functions #'kak-add-search-highlight))

;;; functions

(defun kak-get-match-overlays ()
  "Get overlays from matching regions."
  (setq kak-match-overlays (mapcar
                            (lambda (match) (make-overlay (cl-first match) (cl-second match)))
                            kak-matches))
  (mapcar (lambda (overlay)
            (overlay-put overlay 'priority 1000)
            (overlay-put overlay 'face 'evil-ex-lazy-highlight))
          kak-match-overlays))

(defun kak-get-invert-matches (matches beg end)
  "Invert matching regions (MATCHES) in a given region (BEG to END).
Matching regions become spliting regions."
  (if (> beg end) nil
    (pcase matches
      ('nil `((,beg ,end)))
      (`(,match . ,matches)
       (if (eq beg (cl-first match))
           (kak-get-invert-matches matches (cl-second match) end)
         (cons `(,beg ,(cl-first match))
               (kak-get-invert-matches matches (cl-second match) end)))))))

(defun kak-get-matches-in-region (regex)
  "Get all regions matching a REGEX string."
  (let ((match))
    (setq! kak-matches nil
           kak-match-overlays nil)
    (goto-char kak-region-beg)
    (if (search-forward-regexp regex nil kak-region-end)
        (setq match (match-data 0)) (setq match nil))
    (while (and match (<= (cl-second match) kak-region-end))
      (when (< (cl-first match) (cl-second match))
          (push match kak-matches))
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
  "Create a fake cursor for each matching regions in MATCHES."
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
  "Initialize a search session in a given region (BEG to END).
If INVERT is nil, the search regex is used to select matching regions.
If INVERT is t, the search regex is used to split matching regions."
  (progn
    (setq! kak-current-buffer (current-buffer)
           kak-region-beg beg
           kak-region-end end
           kak-invert-search invert
           kak-match-overlays nil)
    (add-hook 'minibuffer-setup-hook #'kak-start-search-session)
    (add-hook 'minibuffer-exit-hook #'kak-end-search-session)))

(defun kak-get-matches ()
  "Get all the matching regions based on the fake cursors (`evil-mc' cursors) and the real cursors."
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
  "Initialize a filter session.
If KEEP is nil, cursors matching a regex string will be filtered.
If KEEP is true, cursors matching a regex string will be kept."
  (progn
    (setq! kak-current-buffer (current-buffer)
           kak-filter-keep keep
           kak-match-overlays nil)
    (add-hook 'minibuffer-setup-hook #'kak-start-filter-session)
    (add-hook 'minibuffer-exit-hook #'kak-end-filter-session)))

;;; interactive functions

(defun kak-restore-last-region ()
  "Restore the previous region used for selecting/spliting."
  (interactive)
  (if (and kak-region-beg kak-region-end)
      (evil-visual-make-region kak-region-beg (1- kak-region-end))))

(defun kak-select (beg end &optional invert)
  "Select/split a region (BEG to END) into multiple matching regions.
If INVERT is nil, the search regex is used to select matching regions.
If INVERT is t, the search regex is used to split matching regions."
  (interactive "r")
  (if (evil-visual-state-p)
      (progn
        (kak-init-search beg end invert)
        (read-regexp "pattern: ")
        (progn
          (goto-char beg)
          (if kak-matches
              (progn (evil-exit-visual-state) (kak-make-cursors-for-matches kak-matches))
            (user-error "No match"))))
    (user-error "Must be in visual state")))

(defun kak-split-lines (beg end)
  "Split a region (BEG to END) into multiple lines. Each line will be a cursor region."
  (interactive "r")
  (if (evil-visual-state-p)
  (progn
    (setq! kak-current-buffer (current-buffer)
           kak-region-beg beg
           kak-region-end end
           kak-match-overlays nil)
    (kak-get-matches-in-region ".*")
    (kak-make-cursors-for-matches kak-matches)))
  (user-error "Must be in visual state"))

(defun kak-filter (&optional keep)
  "Filter/keep all cursors matching a regex string.
If KEEP is nil, cursors matching a regex string will be filtered.
If KEEP is true, cursors matching a regex string will be kept."
  (interactive)
  (if (evil-visual-state-p)
      (progn
        (kak-init-filter keep)
        (read-regexp "pattern: ")
        (if kak-matches
            (progn
              (evil-mc-undo-all-cursors)
              (kak-make-cursors-for-matches kak-matches))
          (user-error "No matches remaining")))
    (user-error "Must be in visual state")))

(evil-define-command evil-shell-command-on-region (beg end command)
  "Execute a shell COMMAND on a given region (from BEG to END).
The region's content will be used as the COMMAND's standard input and it will be replace by the output of the COMMAND."
  (interactive (let (cmd)
                 (unless (mark)
                   (user-error "No active region"))
                 (setq cmd (read-shell-command "Shell command on region: "))
                 (list (region-beginning) (region-end)
                       cmd)))
  (shell-command-on-region beg end command nil t shell-command-default-error-buffer t (region-noncontiguous-p)))

(defun kak-exec-shell-command (command)
  "Execute a shell COMMAND at each matching region (cursor).
The region's content will be used as the COMMAND's standard input and it will be replace by the output of the COMMAND."
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
    (user-error "Must be in visual state")))

(defun kak-insert-index (base)
  "Insert a index after every matching region (cursor) based on the cursor's position and the BASE index."
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

(provide 'kak)
;;; kak.el ends here
