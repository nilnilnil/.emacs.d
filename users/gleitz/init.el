;; Install the following
;; brew install pylint
;; npm install -g jshint

;; Default indentation levels
(setq js-indent-level 2)
(setq ruby-indent-level 2)

;; Font size
(define-key global-map (kbd "M-s +") 'zoom-in)
(define-key global-map (kbd "M-s -") 'zoom-out)

;; Keyboard modifications
(global-set-key (kbd "M-4") 'ispell-word)
(global-set-key (kbd "M-r") 'replace-string)
(global-set-key (kbd "M-R") 'replace-regexp)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "M-u") 'revert-buffer)
(global-set-key (kbd "C-c d") 'desktop-change-dir)
(global-set-key (kbd "C-x g") 'find-grep-dired)
(global-set-key (kbd "C-x n") 'find-name-dired)
(global-set-key (kbd "<S-down>") 'ff/comment-and-go-down)
(global-set-key (kbd "<S-up>") 'ff/uncomment-and-go-up)

;; Flycheck
(add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'js2-mode-hook 'flycheck-mode)
(setq flycheck-error-display-timer nil)

;; Infinite pop mark
(setq set-mark-command-repeat-pop t)

;; No backups, please
(setq auto-save-default nil)

;; Edit the buffer as super user
(defun sudo ()
  (interactive)
  (let ((fname (or buffer-file-name
                   dired-directory)))
    (when fname
      (if (string-match "^/sudo:root@localhost:" fname)
          (setq fname (replace-regexp-in-string
                       "^/sudo:root@localhost:" ""
                       fname))
        (setq fname (concat "/sudo:root@localhost:" fname)))
      (find-alternate-file fname))))

;; Remove console.log statements from the current buffer
(defun console-remove ()
  (interactive)
  (kmacro-exec-ring-item (quote ("\274console.log" 0 "%d")) 100))

;; Remove debugger; statements from the current buffer
(defun debugger-remove ()
  (interactive)
  (kmacro-exec-ring-item (quote ("\274debugger;" 0 "%d")) 100))

;; Remove alert() statements from the current buffer
(defun alert-remove ()
  (interactive)
  (kmacro-exec-ring-item (quote ("\274alert(" 0 "%d")) 100))

;; Collapse whitespace between two lines
(fset 'collapse-whitespace
      [left ?\C-\M-s ?\[ ?\[ ?: ?g ?r ?a ?p ?h ?: ?\] ?\] ?\C-m left ?\C-  ?\S-\C-\M-r ?\[ ?\[ ?: ?g ?r ?a ?p ?h ?: ?\] ?\] ?\C-m right ?\C-x ?\C-k])
(global-set-key "\C-x\C-j" 'collapse-whitespace)

(defun tidy ()
  "Tidy up a buffer by replacing all special unicode characters
   (smart quotes, etc.) with their more sane cousins"
  (interactive)
  (let ((unicode-map '(("[\u2018\|\u2019\|\u2032|\u201A\|\uFFFD]" . "'")
                       ("[\u201c\|\u201d\|\u201e]" . "\"")
                       ("[\u2013\|\u2014]" . "-")
                       ("\u2026" . "...")
                       ("\u00A9" . "(c)")
                       ("\u00AE" . "(r)")
                       ("\u2122" . "TM")
                       ("[\u02DC\|\u00A0]" . " "))))
    (save-excursion
      (loop for (key . value) in unicode-map
            do
            (goto-char (point-min))
            (replace-regexp key value)))))

;; Comments and goes down ARG lines
(defun ff/comment-and-go-down (arg)
  (interactive "p")
  (condition-case nil
      (comment-region (point-at-bol) (point-at-eol)) (error nil))
  (next-line 1)
  (if (> arg 1) (ff/comment-and-go-down (1- arg))))

;; Uncomments and goes up ARG lines
(defun ff/uncomment-and-go-up (arg)
  (interactive "p")
  (condition-case nil
      (uncomment-region (point-at-bol) (point-at-eol)) (error nil))
  (next-line -1)
  (if (> arg 1) (ff/uncomment-and-go-up (1- arg))))
