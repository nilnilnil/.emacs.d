;; Seed the random-number generator
(random t)

;; Keep region when undoing in region
(defadvice undo-tree-undo (around keep-region activate)
  (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
            (p (set-marker (make-marker) (point))))
        ad-do-it
        (goto-char p)
        (set-mark m)
        (set-marker p nil)
        (set-marker m nil))
    ad-do-it))

;; Whitespace-style
(setq whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 100)

;; Add Urban Dictionary to webjump (C-x g)
(eval-after-load "webjump"
  '(add-to-list 'webjump-sites '("Urban Dictionary" .
                             [simple-query
                              "www.urbandictionary.com"
                              "http://www.urbandictionary.com/define.php?term="
                              ""])))

;; Various superfluous white-space. Just say no.
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'makefile-mode-hook 'indent-tabs-mode)

;; Newline after inserting closing tag in html-mode
(defadvice sgml-close-tag (after close-tag-then-newline activate)
  (newline-and-indent))

;; Trademark on C-x 8 t m

(global-set-key (kbd "C-x 8 t m") (λ (insert "™")))

;; Add JSP expansions to html-mode
(eval-after-load "sgml-mode" '(require 'jsp-expansions))

;; A bit of misc cargo culting in misc.el
(setq xterm-mouse-mode t)

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

;; Open github pages from files
(defun browse-on-github ()
  "Show the current file on github"
  (interactive)
  (browse-url
   (trim-string
    (shell-command-to-string
     (format "python %s %s %d %d"
             (expand-file-name "~/.emacs.d/lib/python/gleitzpy/gitopener.py")
             (buffer-file-name)
             (if mark-active (line-number-at-pos (region-beginning))
               (string-to-number (replace-regexp-in-string "Line " "" (what-line))))
             (if mark-active (line-number-at-pos (region-end))
               0)
)))))

;; I know that string is in my buffer somewhere
(defun my-multi-occur-in-matching-buffers (regexp &optional allbufs)
  "Show all lines matching REGEXP in all buffers."
  (interactive (occur-read-primary-args))
  (multi-occur-in-matching-buffers ".*" regexp))
(global-set-key (kbd "M-s /") 'my-multi-occur-in-matching-buffers)

(defun my-isearch-buffers ()
  "isearch multiple buffers."
  (interactive)
  (multi-isearch-buffers
   (delq nil (mapcar (lambda (buf)
                       (set-buffer buf)
                       (and (not (equal major-mode 'dired-mode))
                            (not (string-match "^[ *]" (buffer-name buf)))
                            buf))
                     (buffer-list)))))
(global-set-key (kbd "M-s \\") 'my-isearch-buffers)

(provide 'my-misc)
