
;; Default configs are under here(sorted by usefulness); the next section shows package-related configs---------

;; System locale to use for formatting time values.
(setq system-time-locale "C")         ; Make sure that the weekdays in the
					; time stamps of your Org mode files and
					; in the agenda appear in English.

;;Put all backups in one directory so emacs doesn't strew them 
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;;turn all "yes" and "no" prompts into "y" and "n" prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Tab width of 2 is compact and readable
(setq default-tab-width 2)
(blink-cursor-mode -1)

(show-paren-mode 1)
(setq show-paren-delay 0)


;;increase garbage collection limit to 100MiB
(setq gc-cons-threshold (* 100 1024 1024))
(setq large-file-warning-threshold (* 100 1024 1024))


;;enable these commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; scroll bar not useful as its behaviour is weird(too lazy to learn), and there's a percentage to show vertical position so...
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(display-time-mode 1)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(menu-bar-mode -1)

(toggle-frame-fullscreen)

;; set buffer to auto-update when the associated file is written to externally, and set it to update in 1s
(global-auto-revert-mode 1)
(setq auto-revert-interval 1)

;; speed up unicode loading, but uses more memory
(setq inhibit-compacting-font-caches t)

(global-visual-line-mode 1)

(set-language-environment 'utf-8) ; fixes the "haskell process has died" error somehow

;; All package-related stuff goes under here---------------------------------

;; Straight.el bootstrap code
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; makes each use-package form also invoke straight.el to install the package, unless otherwise specified
(setq straight-use-package-by-default t)

;; install use-package
(straight-use-package 'use-package)


;; This use-package.el code is kept to enable browsing of MELPA packages.
;; (package-initialize)
(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "https://melpa.org/packages/")
 t)

(defun system-name? (name-string)
  (string-equal system-name name-string))

(require 'recentf);recent files browsing feature
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode 1)


(use-package xah-fly-keys
  :config ; set-layout required before enabling
  (cond 
   ((or (system-name? "ASSES-UX310UQK") (system-name? "DURIAN") (system-name? "mango"))
    (xah-fly-keys-set-layout 'colemak-mod-dh))
   ((system-name? "localhost")
    (xah-fly-keys-set-layout 'qwerty))
   (t
    (xah-fly-keys-set-layout 'qwerty)))
  (xah-fly-keys 1))
(use-package xah-find)

(use-package which-key :config (which-key-mode))


;; ivy, counsel, swiper (completion, UIs, isearch replacement respectively)
(use-package ivy
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))
(use-package counsel
  :after ivy
  :config (counsel-mode))
(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))
(use-package ivy-prescient ; brings back the smartness of smex to ivy, makes search more predictable
  :after counsel
  :config (ivy-prescient-mode t))

;; an amazing front-end to git
(use-package magit)


(use-package org
  :bind (("C-c a" . org-agenda)
	 ("C-c l" . org-store-link))
  :hook (org-mode-hook . visual-line-mode)
  :config
  (setq org-log-done t)
  (setq org-startup-indented t)
  (setq org-agenda-files (list "~/stuff/notes/zk/life.org"))
  (setq org-todo-keywords
	'((sequence "TODO(t)"
		    "ASAP(a)"
		    "ONGOING(o)"
		    "IFFREE(f)"
		    "IFSUPERFREE(s)"
		    "IFREALLYNOTHINGTODO(r)"
		    "|"
		    "USELESSED(u)"
		    "TOOLATE(l)"
		    "CANCELLED(c)"
		    "DONE(d)"))))
;;org-agenda-custom-commands is under custom-set-variables for convenience; the "Easy Customisation" updates to there.

;;org roam
(use-package dash)
(use-package f)
(use-package s)
(use-package emacsql)
(if (system-name? "localhost")
    (use-package emacsql-sqlite3)
  (use-package emacsql-sqlite))
(use-package magit-section)

(use-package org-roam
  :after (dash f s org emacsql magit-section)
  :init
  (setq org-roam-v2-ack t)
  :custom  
  (org-roam-directory (file-truename (if (system-name? "localhost")
					 "/data/data/com.termux/files/home/storage/shared/stuff/notes/zk"
				       "~/stuff/notes/zk")))
  (org-roam-dailies-directory "daily/")
  (define-key org-roam-mode-map [mouse-1] #'org-roam-visit-thing)
  (org-roam-file-exclude-regexp ".*~.*")
  (org-roam-capture-templates
   '(("d" "default without ${slug}" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>.org"
			 "#+title: ${title}\n")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %?"
      :if-new (file+head "%<%Y-%m-%d>.org"
			 "#+title: %<%Y-%m-%d>\n\n")
      :unarrowed t)))

  :bind (:map org-mode-map
	      (("C-c n l" . org-roam-buffer)
	       ("C-c n f" . org-roam-node-find)
	       ("C-c n d" . org-roam-dailies-goto-date)
	       ("C-c n p" . org-roam-dailies-goto-previous-note)
	       ("C-c n n" . org-roam-dailies-goto-next-note)
	       ("C-c n g" . org-roam-graph)
	       ("C-c n r" . org-roam-buffer-toggle)
	       ("C-c n b" . org-roam-switch-to-buffer) ;not in v2 yet
	       ("C-c n c" . org-id-get-create)
	       ("C-c n i" . org-roam-node-insert)
	       ("C-c n I" . org-roam-node-insert-immediate) ;wait for the "immediate" version in v2
	       ("C-c n w" . org-roam-alias-add)
	       ("C-c n y" . org-roam-alias-remove)
	       ))
  :config
  (when (system-name? "localhost")
    (defun org-roam-db ()
      "Entrypoint to the Org-roam sqlite database.
Initializes and stores the database, and the database connection.
Performs a database upgrade when required."
      (unless (and (org-roam-db--get-connection)
                   (emacsql-live-p (org-roam-db--get-connection)))
	(let ((init-db (not (file-exists-p org-roam-db-location))))
          (make-directory (file-name-directory org-roam-db-location) t)
          ;; (let ((conn (emacsql-sqlite org-roam-db-location)))
          (let ((conn (emacsql-sqlite3 org-roam-db-location)))
            (emacsql conn [:pragma (= foreign_keys ON)])
            (set-process-query-on-exit-flag (emacsql-process conn) nil)
            (puthash (expand-file-name org-roam-directory)
                     conn
                     org-roam-db--connection)
            (when init-db
              (org-roam-db--init conn))
            (let* ((version (caar (emacsql conn "PRAGMA user_version")))
                   (version (org-roam-db--upgrade-maybe conn version)))
              (cond
               ((> version org-roam-db-version)
		(emacsql-close conn)
		(user-error
		 "The Org-roam database was created with a newer Org-roam version.  "
		 "You need to update the Org-roam package"))
               ((< version org-roam-db-version)
		(emacsql-close conn)
		(error "BUG: The Org-roam database scheme changed %s"
                       "and there is no upgrade path")))))))
      (org-roam-db--get-connection))
    (defun org-roam-db--init (db)
      "Initialize database DB with the correct schema and user version."
      (emacsql-with-transaction db
	;; (emacsql db "PRAGMA foreign_keys = ON")
	(emacsql db [:pragma (= foreign_keys ON)])
	(pcase-dolist (`(,table ,schema) org-roam-db--table-schemata)
          (emacsql db [:create-table $i1 $S2] table schema))
	(pcase-dolist (`(,index-name ,table ,columns) org-roam-db--table-indices)
          (emacsql db [:create-index $i1 :on $i2 $S3] index-name table columns))
	(emacsql db (format "PRAGMA user_version = %s" org-roam-db-version)))))
  (org-roam-db-autosync-mode) ;; need org-roam-sqlite-available-p to be true
  )

(when (system-name? "mango wait till when this package is more grown and available.")
  (use-package websocket :after org-roam)
  (use-package httpd)
  (use-package org-roam-ui
    :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
    ;; :hook 
    ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
    ;;         a hookable mode anymore, you're advised to pick something yourself
    ;;         if you don't care about startup time, use
    ;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t)))


;; for programming in Scheme
(use-package geiser)
(use-package geiser-guile)
(use-package geiser-racket); for racket if you download minimal racket you need to "raco pkg install compatibility-lib"
(use-package paredit
  :hook ((emacs-lisp-mode
	  lisp-interaction-mode
	  ielm-mode
	  lisp-mode
	  eval-expression-minibuffer-setup
	  scheme-mode) . paredit-mode))

;; for programming in Haskell
(use-package attrap)
(use-package dante
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  :config
  (flycheck-add-next-checker 'haskell-dante '(info . haskell-hlint))
  )

;;Language Server Protocol(LSP)-related
(use-package lsp-mode :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
	 (c-mode . lsp-deferred)
	 (c++-mode . lsp-deferred)
	 (python-mode . lsp-deferred)
	 (haskell-mode . lsp-deferred)
	 ;; if you want which-key integration
	 (lsp-mode . lsp-enable-which-key-integration))
  :config (setq lsp-idle-delay 0.1)
  :commands lsp-deferred)
(use-package lsp-ui
  :commands lsp-ui-mode
  :after lsp-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs
  :config (setq treemacs-space-between-root-nodes nil)
  :commands lsp-treemacs-errors-list)
(when (not (system-name? "ASSES-UX310UQK"))
  (use-package lsp-pyright
    :hook (python-mode . (lambda ()
			   (require 'lsp-pyright)
			   (lsp-deferred))))
  (use-package lsp-haskell)
  (if (system-name? "DURIAN")
     (setq ghc-location "~/.ghcup/bin/ghc")))

;; Debug Adaptor Protocol(DAP)-related
(use-package dap-mode
  :after lsp-mode
  :config
  (require 'dap-cpptools) ; afterwards run dap-cpptools-setup
  (require 'dap-python) ; requires pip install "ptvsd>=4.2"
  (dap-auto-configure-mode)
)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language
;; I don't understand the above line
(use-package company ;;company = complete anything
  :bind (:map company-active-map
	      ("<return>" . nil)
	      ("RET" . nil)
	      ("M-<return>" . company-complete-selection)
	      ("M-RET" . company-complete-selection))
  :config
  (setq company-minimum-prefix-length 1
	company-idle-delay 0.0)) ;; default is 0.2

;; Built-in Python utilities
(use-package python
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil)
  ;; Use IPython when available or fall back to regular Python 
  (cond
   ((executable-find "ipython")
    (progn
      (setq python-shell-buffer-name "IPython")
      (setq python-shell-interpreter "ipython")
      (setq python-shell-interpreter-args "-i --simple-prompt")))
   ((executable-find "python3")
    (setq python-shell-interpreter "python3"))
   ((executable-find "python2")
    (setq python-shell-interpreter "python2"))
   (t
    (setq python-shell-interpreter "python"))))

(if (or (system-name? "mango") (system-name? "DURIAN"))
    (use-package guix))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-custom-commands
   '(("c" "To-dos of Noted Life"
      ((tags-todo "+health"
		  ((org-agenda-overriding-header "Health first~!")))
       (tags-todo "+job"
		  ((org-agenda-overriding-header "Job")))
       (tags-todo "+indep"
		  ((org-agenda-overriding-header "Independence(neat-to-have skills)")))
       (tags-todo "+physics"
		  ((org-agenda-overriding-header "Lifelong Dreams: Physics")))
       (tags-todo "+math-physics"
		  ((org-agenda-overriding-header "Lifelong Dreams: Mathematics(Calculus is so magical!)")))
       (tags-todo "+piano"
		  ((org-agenda-overriding-header "Lifelong Dreams: Piano/(?Music)"))))
      nil)
     ("z" "testing easy \"customization\""
      ((agenda "" nil)
       (todo "TODO"
	     ((org-agenda-overriding-header "Physics")
	      (org-agenda-tag-filter-preset
	       '("+physics"))))
       (tags-todo "+math-physics"
		  ((org-agenda-overriding-header "Mathematics")))
       (stuck ""
	      ((org-agenda-overriding-header "what's stuck projects?"))))
      nil)))
 )

(when (system-name? "ASSES-UX310UQK")
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:family "mononoki NF" :foundry "outline" :slant normal :weight normal :height 120 :width normal))))))
(when (system-name? "DURIAN")
  (custom-set-faces
   '(default ((t (:family "mononoki" :foundry "UKWN" :slant normal :weight normal :height 151 :width normal))))))
(when (system-name? "mango")
  (custom-set-faces
   '(default ((t (:family "mononoki" :foundry "UKWN" :slant normal :weight normal :height 113 :width normal))))))

(when (or (system-name? "mango") (system-name? "nix-on-droid-placeholder-name"))
  (use-package nix-mode
    :mode "\\.nix\\'"))
;; (when (system-name? "mango")
;;   (use-package pdf-tools
;;     :after tablist
;;     :custom (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

;;     )

(defun delete-file-visited-by-buffer (buffername)
  "Delete the file visited by the buffer named BUFFERNAME."
  (interactive "b")
  (let* ((buffer (get-buffer buffername))
	 (filename (buffer-file-name buffer)))
    (when filename
      (delete-file filename)
      (kill-buffer-ask buffer))))


