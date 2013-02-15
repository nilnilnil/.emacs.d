;; Malabar mode
(load-file (expand-file-name "~/.emacs.d/site-lisp/cedet/common/cedet.el"))
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode t)
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode t)
;; Enable EDE (Project Management) features
(global-ede-mode 1)
(setq malabar-extra-source-locations (quote ("~/projects/java/" "~/projects/ebay/feedhome/feedhome/src/main/java/")))
(setq malabar-groovy-lib-dir (expand-file-name "~/.emacs.d/site-lisp/malabar/lib"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/malabar/lisp"))
(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-mru-bookmark-mode))
(require 'cedet)
(semantic-load-enable-minimum-features) ;; or enable more if you wish
(require 'malabar-mode)

(provide 'setup-malabar)
