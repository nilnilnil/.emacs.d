(require 'magit-svn)

;; Subtler highlight
(set-face-background 'magit-item-highlight "#121212")
(set-face-foreground 'diff-context "#666666")
(set-face-foreground 'diff-added "#00cc33")
(set-face-foreground 'diff-removed "#ff0000")

;; Load git configurations
;; For instance, to run magit-svn-mode in a project, do:
;;
;;     git config --add magit.extension svn
;;
(add-hook 'magit-mode-hook 'magit-load-config-extensions)

;; C-x C-k to kill file on line

(defun magit-kill-file-on-line ()
  "Show file on current magit line and prompt for deletion."
  (interactive)
  (magit-visit-item)
  (delete-current-buffer-file)
  (magit-refresh))

(define-key magit-status-mode-map (kbd "C-x C-k") 'magit-kill-file-on-line)

;; full screen magit-status

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

;; full screen vc-annotate

(defun vc-annotate-quit ()
  "Restores the previous window configuration and kills the vc-annotate buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :vc-annotate-fullscreen))

(eval-after-load "vc-annotate"
  '(progn
     (defadvice vc-annotate (around fullscreen activate)
       (window-configuration-to-register :vc-annotate-fullscreen)
       ad-do-it
       (delete-other-windows))

     (define-key vc-annotate-mode-map (kbd "q") 'vc-annotate-quit)))

;; ignore whitespace

(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)


;; Rebase until it hurts
;; http://jrb.tumblr.com/post/49248876242/git-pull-rebase-until-it-hurts
(defun magit-pull ()
  (interactive)
  (magit-run-git-async "pull" "--rebase" "-v"))

(defun magit-abort-rebase-and-pull ()
  (interactive)
  (magit-run-git-async "rebase" "--abort" "&&" "git" "pull"))

(defun magit-pull-no-rebase ()
  (interactive)
  (magit-run-git-async "pull"))

(provide 'setup-magit)
