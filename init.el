
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


(defun system-name? (name-string)
  (string-equal system-name name-string))

(when (not (system-name? "mango"))
    (toggle-frame-fullscreen)) 

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


(require 'recentf);recent files browsing feature
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode 1)


(use-package xah-fly-keys
  :config
  ;; set-layout required before enabling
  (xah-fly-keys-set-layout (cond 
			    ((or (system-name? "ASSES-UX310UQK") (system-name? "DURIAN") (system-name? "mango")) 'colemak-mod-dh)
			    ((system-name? "localhost") 'qwerty)
			    (t 'qwerty)))
  (xah-fly-keys 1)
  (autoload 'View-scroll-half-page-up "view")
  (autoload 'View-scroll-half-page-down "view")
  (global-set-key (kbd "C-v") 'View-scroll-half-page-forward)
  (global-set-key (kbd "M-v") 'View-scroll-half-page-backward))
(use-package xah-find)

(use-package which-key :config (which-key-mode))

;; completion-helping stuff

;; save minibuffer command history
(use-package savehist
  :init
  (savehist-mode 1)
  (setq history-length 10000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t))

;; vertico is the vertical autocomplete selection menu
(use-package vertico
  :init (vertico-mode)
  (setq vertico-scroll-margin 0) ;; no idea what this does, I don't see a difference
  (setq vertico-count (if (system-name? "localhost")
			  10
			20))
  (setq vertico-resize t)
  (setq vertico-cycle t))

;; orderless is the completion style
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; marginalia annotates the minibuffer like the margins in a book (look on the right side)
(use-package marginalia
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle))
  :init
  (setq marginalia-max-relative-age 0)
  (marginalia-mode 1)  )

;; provides _good shit_ versions of common commands and more
(use-package consult
  :bind (("C-x M-:" . consult-complex-command)
	 ("C-c h" . consult-history)
	 ("C-c m" . consult-mode-command)
	 ("C-x b" . consult-buffer)
	 ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r x" . consult-register)
         ("C-x r b" . consult-bookmark)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)       ;; "M-s o" is a good alternative.
         ("M-g l" . consult-line)          ;; "M-s l" is a good alternative.
         ("M-g m" . consult-mark)          ;; I recommend to bind Consult navigation
         ("M-g k" . consult-global-mark)   ;; commands under the "M-g" prefix.
         ("M-g r" . consult-ripgrep)      ;; or consult-grep, consult-ripgrep
         ("M-g f" . consult-find)          ;; or consult-locate, my-fdfind
         ("M-g i" . consult-project-imenu) ;; or consult-imenu
         ("M-g e" . consult-error)
         ("M-s m" . consult-multi-occur)
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos)
	 )
  :init
  (global-set-key [remap switch-to-buffer] 'consult-buffer)
  (global-set-key [remap find-file] 'consult-find)
  ;; Replace `multi-occur' with `consult-multi-occur', which is a drop-in replacement.
  (fset 'multi-occur #'consult-multi-occur)
  )

(use-package consult-dir
       :ensure t
       :bind (("C-x C-d" . consult-dir)
              :map minibuffer-local-completion-map
              ("C-x C-d" . consult-dir)
              ("C-x C-j" . consult-dir-jump-file)
	      )
       )
(use-package consult-company
  :after consult
  :config
  (define-key company-mode-map [remap completion-at-point] #'consult-company)
  )
(use-package embark
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :bind (("C-." . embark-act)   ; like a right-click
	 ("M-." . embark-dwim)
	 ("C-h B" . embark-bindings)) ; like a  left-click
  
  :config
  ;; show Embark via whichkey
  (setq embark-action-indicator
	(lambda (map)
	  (which-key--show-keymap "Embark" map nil nil 'no-paging)
	  #'which-key--hide-popup-ignore-command)
	embark-become-indicator embark-action-indicator)
  
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
;; end completion-helping stuff

;; an amazing front-end to git
(use-package magit)


(use-package org
  :bind (("C-c a" . org-agenda)
	 ("C-c l" . org-store-link))
  :hook (org-mode-hook . visual-line-mode)
  :init
  (setq org-return-follows-link t)
  (setq org-startup-folded 'content)
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
  (when (system-name? "localhost")
    (setq org-roam-database-connector 'sqlite3))
  :custom  
  (org-roam-directory (file-truename (if (system-name? "localhost")
					      "/data/data/com.termux/files/home/storage/shared/stuff/notes/zk/"
					    "~/stuff/notes/zk/")))
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

  :bind (("C-c n f" . org-roam-node-find)
	 ("C-c n d" . org-roam-dailies-goto-date)
	 (:map org-mode-map
	       (("C-c n p" . org-roam-dailies-goto-previous-note)
		("C-c n n" . org-roam-dailies-goto-next-note)
		;; ("C-c n g" . org-roam-graph) ;; use org-roam-ui to generate the graph, it's probably vastly superior
		("C-c n l" . org-roam-buffer-toggle)
		("C-c n b" . org-roam-switch-to-buffer) ;not in v2 yet
		("C-c n c" . org-id-get-create)
		("C-c n i" . org-roam-node-insert)
		("C-c n I" . org-roam-node-insert-immediate) ;wait for the "immediate" version in v2
		("C-c n a" . org-roam-alias-add)
		("C-c n r" . org-roam-alias-remove)
		)))
  :config
  (org-roam-db-autosync-mode) ;; need org-roam-sqlite-available-p to be true
  ;; from https://babbagefiles.xyz/org-roam-on-android/
  ;; org-roam-rg-search - this is a much faster way to search Org-roam notes:
  ;; requires the Selectrum+Consult setup immediately preceding.
  ;; Use C-c r r to search notes via consult's ripgrep interface
  (defun bms/org-roam-rg-search ()
    "Search org-roam directory using consult-ripgrep. With live-preview."
    (interactive)
    (let ((consult-ripgrep "rg --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
      (consult-ripgrep org-roam-directory)))
  (global-set-key (kbd "C-c rr") 'bms/org-roam-rg-search)
  )

(when (system-name? "mango")
  (use-package websocket :after org-roam)
  (use-package simple-httpd)
  (use-package org-roam-ui
    :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after (org-roam websocket simple-httpd f)
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
 '(custom-safe-themes
   '("e9d47d6d41e42a8313c81995a60b2af6588e9f01a1cf19ca42669a7ffd5c2fde" default))
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
      nil))))

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
(when (system-name? "mango")
  (use-package pdf-tools
    :defer t
    :after tablist
    :config
    (pdf-tools-install)
    ;; (setq-default pdf-view-display-size 'fit-page)
    :custom (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
    ))

(when (system-name? "localhost")
  (use-package cyberpunk-theme
    :init (load-theme 'cyberpunk)))



(defun delete-file-visited-by-buffer (buffername)
  "Delete the file visited by the buffer named BUFFERNAME."
  (interactive "b")
  (let* ((buffer (get-buffer buffername))
	 (filename (buffer-file-name buffer)))
    (when filename
      (delete-file filename)
      (kill-buffer-ask buffer))))


(defun er-sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))




