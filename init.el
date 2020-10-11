
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

(show-paren-mode)
;;(setq show-paren

;;increase garbage collection limit to 50MB
(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000);; ~100MB?


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
 '("melpa" . "http://melpa.milkbox.net/packages/")
 t)

(require 'recentf);recent files browsing feature
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode 1)

(use-package xah-fly-keys
  :config ; set-layout required
  (cond 
   ((string-equal system-name "ASSES-UX310UQK")
    (xah-fly-keys-set-layout 'colemak-mod-dh))
   ((string-equal system-name "localhost")
    (xah-fly-keys-set-layout 'qwerty))
   ((string-equal system-name "DURIAN")
    (xah-fly-keys-set-layout 'qwerty))) 
  (xah-fly-keys 1))


(use-package which-key
  :config
  (which-key-mode))

(use-package org
  :bind (("C-c a" . org-agenda)
	 ("C-c l" . org-store-link))
  :config
  (setq org-log-done t)
  (setq org-startup-indented t)
  (setq org-agenda-files (list "~/stuff/Notes/.org"))
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

(use-package geiser)
(use-package paredit
  :hook ((emacs-lisp-mode
	  lisp-interaction-mode
	  ielm-mode
	  lisp-mode
	  eval-expression-minibuffer-setup
	  scheme-mode) . paredit-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bf66464043eeb31ee08a92de73d786787ecadf9cd3a1a08a886fe0052a35841a" default)))
 '(org-agenda-custom-commands
   (quote
    (("c" "To-dos of Noted Life"
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
	       (quote
		("+physics")))))
       (tags-todo "+math-physics"
		  ((org-agenda-overriding-header "Mathematics")))
       (stuck ""
	      ((org-agenda-overriding-header "what's stuck projects?"))))
      nil)))))

(if (string-equal system-name "ASSES-UX310UQK")
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:family "mononoki NF" :foundry "outline" :slant normal :weight normal :height 120 :width normal))))))

