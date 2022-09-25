

;; when building packages:
;; nix's cmake =/= homebrew's cmake, so (setenv "PATH" (concat "/opt/homebrew/bin/:" (getenv "PATH"))) from https://www.emacswiki.org/emacs/ExecPath

;; use-package-expand-minimally
;; keybindings to remember --------------------------------------------
;; save-buffer = SPC ;
;; xah-close-current-buffer(C-w) = SPC u
;; find-file = SPC i e
;; eval = SPC , r
;; delete-horizontal-space = <command mode> w
;; undo = C-/
;; xah-open-last-closed(C-S-t) = SPC i r
;; text-scale-increase/adjustment mode (C-=)/(C--) = SPC l a +/-
;; SPC C-h = list all leader keys
;; SPC p = move the screen so cursor is at centre
;; C-2 = pop-global-mark, basically jump to last previously marked/visited with the cursor
;; SPC l l  narrow-to-region & SPC l j to widen, basically edit only in region
;; SPC RET = execute-extended-command

;; system-type 'darwin, system-name Apexless/Apexless.local/??? = macbook pro 14 m1 pro
;; system-name DURIAN = poly laptop running Manjaro
;; system-name mango = my own desktop running NixOS
;; system-name localhost = phone running Termux

;; Cool Packages to maybe have a look at ------------------------------
;; - org-noter, annotating pdf,epub with complete org files in the margin
;; - apheleia, asynchronous code formatting
;; - org-contrib, additional org packages
;; - undo fu, undo between sessions
;; - ipretty, pretty-print sexps https://framagit.org/steckerhalter/ipretty
;; - org-latex-impatient, preview as you type latex in org-mode
;; - company-box, sick company UI with icons and different colors for different backends
;; - forge, for working with git forges
;; - transmission
;; - sgml-mode, for working with html files
;; - emms
;; - proof-general, for working with proof assistants, targetd at intermediate to experts

;; Cool packages that i want to install later on----------------------------
;; - persp-mode, workspace manager


;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 0.8MB.  Measured in bytes.
;; (setq gc-cons-threshold (* 128 1024 1024)) ; increase garbage collection limit to 100MiB, default is 0.8MB, measured in bytes


(setq large-file-warning-threshold (* 128 1024 1024))
;; Portion of heap used for allocation.  Defaults to 0.1.
(setq gc-cons-percentage 0.6)

;; Function and variable Definitions ----------------------------------

(defun system-name? (name-string)
  (string-equal system-name name-string))

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

(defvar +apexless (and (eq system-type 'darwin)) "Whether Emacs is running on my macbook")

;; Default configs ------------------------------------------------------

;; System locale to use for formatting time values.
(setq system-time-locale "C")         ; Make sure that the weekdays in the
																				; time stamps of your Org mode files and
																				; in the agenda appear in English.

(setq backup-file-directory (file-truename "~/.emacs.d/backups/"))
;;Put all backups in one directory so emacs doesn't strew them 
(setq backup-directory-alist `(("." . ,backup-file-directory)))
;;Put all autosave files like #filename.whatever# in the same directory
(setq auto-save-file-name-transforms `((".*" ,backup-file-directory t)))
;;turn all "yes" and "no" prompts into "y" and "n" prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Tab width of 2 is compact and readable
(setq-default tab-width 2)
(setq-default c-basic-offset 4);; but not in C
(blink-cursor-mode -1)

(setq show-paren-delay 0.1)
(show-paren-mode 1)

;;enable these commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; scroll bar not useful as its behaviour is weird(too lazy to learn), and there's a percentage to show vertical position so...
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


(unless +apexless (display-time-mode 1))	;apexless has time permanently displayed so you don't need this
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(menu-bar-mode -1)


(cond (+apexless (toggle-frame-maximized))
      ((system-name? "mango") nil)
      (t (toggle-frame-fullscreen)))


;; set buffer to auto-update when the associated file is written to externally, and set it to update in 1s
(global-auto-revert-mode 1)
(setq auto-revert-interval 1)

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

																				;recent files browsing feature
(setq recentf-max-saved-items 10000
      recentf-max-menu-items 10000)
(recentf-mode 1)

;; speed up unicode loading, but uses more memory
(setq inhibit-compacting-font-caches t)

(set-language-environment 'utf-8) ; fixes the "haskell process has died" error somehow

(setq enable-recursive-minibuffers t)

(setq force-load-messages t)

;; from https://bytemeta.vip/index.php/repo/alexluigit/emacs-grandview
(setq-default bidi-display-reordering 'left-to-right)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t) ;; we don't need the bidirectional parenthesis algorithm if we don't even read from right to left
(setq-default truncate-lines t)
(setq echo-keystrokes 0.25)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq scroll-step 1)
(setq scroll-margin 1)
(setq hscroll-step 1)
(setq hscroll-margin 1)
(setq scroll-preserve-screen-position 1)



;; Potentially speed up cursor operations
;; https://emacs.stackexchange.com/questions/28736
(setq auto-window-vscroll nil)
(put 'narrow-to-region 'disabled nil)


;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))



;; All non-in-built package-related stuff goes under here---------------------------------


;; speed up straight initialisation, and neatened up the bootstrap code.
;; Previously it looks like this, kept so you can see when it suddenly breaks 'cuz it updates and the recommended bootstrap code changes.
;; Straight.el bootstrap code
;; (defvar bootstrap-version)
;; (let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 6))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer (url-retrieve-synchronously "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el" 'silent 'inhibit-cookies)
;;       (goto-char (point-max)) (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))
;; (defvar bootstrap-version)
(let ((bootstrap (locate-user-emacs-file "straight/repos/straight.el/bootstrap.el"))
      (script "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el")
      (file-name-handler-alist nil))
  ;; Init package manager `straight.el'
  (setq straight-use-package-by-default t ; makes each use-package form also invoke straight.el to install the package, unless otherwise specified
        straight-vc-git-default-clone-depth 1
        straight-check-for-modifications '(check-on-save find-when-checking)
        straight-repository-branch "develop")
	
	;; modified straight bootstrap code
  (unless (file-exists-p bootstrap)
    (with-current-buffer (url-retrieve-synchronously script 'silent 'inhibit-cookies)
      (goto-char (point-max)) (eval-print-last-sexp)))
  (load bootstrap nil 'nomessage)

	)

(straight-use-package 'bind-key) ; for `bind-keys' macro

;; wanna speed up your init? here!
(straight-use-package '(once :type git :host github :repo "emacs-magus/once"))


;; install use-package
(straight-use-package 'use-package)

;; ;; This use-package.el code is kept to enable browsing of MELPA packages. It says package-archives is a void variable...
;; (add-to-list
;;  'package-archives
;;  '("melpa" . "https://melpa.org/packages/")
;;  t)

;; must be put asap after use-package for most complete benchmark. Look at its functions named benchmark-init/...
(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package esup
  :ensure t
	;; Work around a bug where esup tries to step into the byte-compiled
  ;; version of `cl-lib', and fails horribly.
	:config
  (setq esup-depth 0))



(use-package xah-fly-keys
  :demand t
  :config
  ;; set-layout required before enabling
  (xah-fly-keys-set-layout (cond 
														((or (system-name? "ASSES-UX310UQK")  (system-name? "mango")) 'colemak-mod-dh)
														(t 'qwerty)))
  (xah-fly-keys 1)
  (autoload 'View-scroll-half-page-up "view")
  (autoload 'View-scroll-half-page-down "view")
  (global-set-key (kbd "C-v") 'View-scroll-half-page-forward)
  (global-set-key (kbd "M-v") 'View-scroll-half-page-backward))
(use-package xah-find)

(use-package which-key :config (which-key-mode))

;; save minibuffer command history
(use-package savehist
	:init
	(savehist-mode 1)
  :config
  (setq history-length 10000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t))

;; vertico is the vertical autocomplete selection menu
(use-package vertico
	:straight (vertico :files (:defaults "extensions/*")
										 :includes (vertico-indexed vertico-flat vertico-grid vertico-mouse vertico-quick vertico-buffer vertico-repeat vertico-reverse vertico-directory vertico-multiform vertico-unobtrusive )) 	:bind (:map vertico-map ("M-DEL" . vertico-directory-delete-word))
  :init
  (vertico-mode)
  (setq vertico-count (if (system-name? "localhost") 10 20))
	(setq vertico-resize t)
  (setq vertico-cycle t)
	)


;; orderless is the completion style
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion)))))
  ;; fix the dollar sign regex "$"
  (defun fix-dollar (args)
    (if (string-suffix-p "$" (car args))
				(list (format "%s[%c-%c]*$"
                      (substring (car args) 0 -1)
                      consult--tofu-char
                      (+ consult--tofu-char consult--tofu-range -1)))
      args))
  (advice-add #'orderless-regexp :filter-args #'fix-dollar)
  (advice-add #'prescient-regexp-regexp :filter-args #'fix-dollar))

;; marginalia annotates the minibuffer like the margins in a book (look on the right side)
(use-package marginalia
  :bind (:map minibuffer-local-map
							("M-A" . marginalia-cycle))
  :init
  (setq marginalia-max-relative-age 0)
  (marginalia-mode 1))


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
	:custom
	(xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :init
  (global-set-key [remap switch-to-buffer] 'consult-buffer)
  ;; (global-set-key [remap find-file] 'consult-find)
  ;; Replace `multi-occur' with `consult-multi-occur', which is a drop-in replacement.
  (fset 'multi-occur #'consult-multi-occur)
  )
(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package consult-eglot
  :after (consult eglot))

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
  :hook	(embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
	:defer)


(use-package helpful
	:bind (([remap describe-command] . helpful-command)
				 ([remap describe-function] . helpful-callable)
				 ([remap describe-key] . helpful-key)
				 ([remap describe-symbol] . helpful-symbol)
				 ([remap describe-variable] . helpful-variable)
				 ;; Lookup the current symbol at point. C-c C-d is a common keybinding
				 ;; for this in lisp modes.
				 ("C-c C-d" . helpful-at-point)
				 ;; Look up *F*unctions (excludes macros).
				 ;;
				 ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
				 ;; already links to the manual, if a function is referenced there.
				 ("C-h F" . helpful-function)
				 (:map helpful-mode-map
							 ;; Make `describe-*' screens more helpful
							 ([remap revert-buffer] . helpful-update))
				 )
	)


(use-package elisp-demos
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;; Annotate value of lines containing ; => .
(use-package lispxmp
	:init
	(setq byte-compile-warnings '(cl-functions)) ;make it not complain about using the depreciated cl.el instead of cl-lib
	)

;; macroexpand conveniently
(use-package macrostep
	:bind (:map emacs-lisp-mode-map ("C-c e" . macrostep-mode)
							:map lisp-mode-map ("C-c e" . macrostep-mode))	)

;; extra emacs lisp syntax highlighting
(use-package highlight-defined
  :config
  (add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode))

;; (use-package highlight-quoted
;;   :config
;;   (add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode)
;;   (set-face-attribute 'highlight-quoted-symbol nil
;;                       :inherit 'font-lock-string-face)
;; 	)



(when +apexless
  (setq dired-use-ls-dired t
				insert-directory-program "/opt/homebrew/bin/gls")
	(setq mac-system-move-file-to-trash-use-finder t))
(setq delete-by-moving-to-trash t)
(setq find-file-visit-truename t) ; follow symlinks when visiting files or directories


(use-package dirvish
  :if +apexless
	:defer t
  :init
	(once '(:hooks pre-command-hook)
		(dirvish-override-dired-mode))
	:bind ((:map dirvish-mode-map
							 ([mouse-1] . dirvish-subtree-toggle-or-open)))
  :config
  (dirvish-peek-mode) ;; shows preview minibuffer when scrolling through find-file minibuffer
  (setq dirvish-hide-details t) ;; hide how dired shows the details on left of file/folder names
  (setq dirvish-reuse-session nil)
  (setq dirvish-attributes
        '(all-the-icons file-size collapse subtree-state vc-state git-msg))
  (setq dired-listing-switches
        "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
  ;; (define-key dirvish-mode-map (kbd "<mouse-1>") 'dirvish-subtree-toggle-or-open)
  (define-key dirvish-mode-map (kbd "<mouse-2>") 'dired-mouse-find-file-other-window)
  (define-key dirvish-mode-map (kbd "<mouse-3>") 'dired-mouse-find-file)
  (setq dirvish-preview-dispatchers
				(cl-substitute 'pdf-preface 'pdf dirvish-preview-dispatchers)) ;requires pdftoppm executable

  ;; Height
  ;; '(25 . 35) means
  ;;   - height in single window sessions is 25
  ;;   - height in full-frame sessions is 35
  (setq dirvish-header-line-height '(25 . 35))
  (setq dirvish-mode-line-height 15) ; 25 is shorthand for '(25 . 25), why isn't this option working?
  (setq dirvish-mode-line-format
				'(:left (sort file-time " " file-size symlink) :right (omit yank index)))

  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ;; ("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump))
  )

(use-package all-the-icons ; for dashboard & dirvish & citar
	:defer t
	:config (setq all-the-icons-scale-factor 1.0)
	)


(use-package tramp
	:straight (:type built-in)
	:after dirvish
  :config
	;; Some tips to speed up Dired/Dirvish over TRAMP
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/ssh:YOUR_HOSTNAME:")
                     "direct-async-process" t))
  (setq tramp-verbose 0)
  (setq tramp-auto-save-directory (locate-user-emacs-file "tramp/"))
  (setq tramp-chunksize 2000)
  (setq tramp-use-ssh-controlmaster-options nil))


;; Addtional syntax highlighting for dired
(use-package diredfl
  :hook (dired-mode . diredfl-mode)
  :config (set-face-attribute 'diredfl-dir-name nil :bold t))


(use-package reveal-in-folder 					; Open Finder at location
	:if +apexless 												; only works on macOS
	:defer)
(use-package terminal-here
	:defer
	:config
	(setq terminal-here-mac-terminal-command 'iterm2)
	)


;; colors hex colors
(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (custom-set-faces 
   '(rainbow-delimiters-depth-0-face ((t (:foreground "dark orange"))))
   '(rainbow-delimiters-depth-1-face ((t (:foreground "light grey"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1"))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "grey"))))))



;; highlights diffs in the margins
(use-package diff-hl
	:hook ((after-init . global-diff-hl-mode)
				 (magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
				 (magit-post-refresh-hook . diff-hl-magit-post-refresh)))


;; an amazing front-end to git
(use-package magit
  :bind ("C-x g" . magit-status))

(use-package org-contrib)
(use-package org
  :bind (("C-c a" . org-agenda)
				 ("C-c l" . org-store-link))
  :hook ((org-mode . org-toggle-pretty-entities)
				 (org-mode . visual-line-mode)
				 ;; (org-mode . +org-font-setup)
				 )
	
  :init
  (setq org-return-follows-link t)
  (setq org-startup-folded 'content)
  :config
	;; (setq org-hide-emphasis-markers t)
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
										"DONE(d)")))
  (when +apexless
		(setq org-latex-create-formula-image-program 'dvisvgm)
		(setq org-display-remote-inline-images 'cache);; https://www.fromkk.com/posts/preview-latex-in-org-mode-with-emacs-in-macos/
		) 
	

	
  )
;;org-agenda-custom-commands is under custom-set-variables for convenience; the "Easy Customisation" updates to there.


(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-directory (file-truename (if (system-name? "localhost")
																							"/data/data/com.termux/files/home/storage/shared/stuff/notes/zk/"
																						"~/stuff/notes/zk/")))
  (setq org-roam-dailies-directory "daily/")
  (when (system-name? "localhost")
    (setq org-roam-database-connector 'sqlite3))
  
  ;; from https://babbagefiles.xyz/org-roam-on-android/
  ;; org-roam-rg-search - this is a much faster way to search Org-roam notes:
  ;; requires the Selectrum+Consult setup immediately preceding.
  ;; Use C-c r r to search notes via consult's ripgrep interface
  (defun bms/org-roam-rg-search ()
    "Search org-roam directory using consult-ripgrep. With live-preview."
    (interactive)
    (let ((consult-ripgrep "rg --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
      (consult-ripgrep org-roam-directory)))
  :custom  
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
				 ("C-c r r" . bms/org-roam-rg-search)
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
	(use-package dash)
	(use-package f) 
	(use-package s)
	(use-package emacsql)
	(if (system-name? "localhost") (use-package emacsql-sqlite3) (use-package emacsql-sqlite))
	(use-package magit-section)
  (org-roam-db-autosync-mode) ;; need org-roam-sqlite-available-p to be true
  (define-key org-roam-mode-map [mouse-1] #'org-roam-visit-thing) ;;TODO org-roam-mode-map is deprecated, fix this
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


(use-package paredit
  :hook ((emacs-lisp-mode
					lisp-interaction-mode
					ielm-mode
					lisp-mode
					eval-expression-minibuffer-setup
					scheme-mode) . paredit-mode))

(use-package company ; company = complete anything
	;; :init (global-company-mode t)
  :bind (:map company-active-map
							("<return>" . nil)
							("RET" . nil)
							("M-<return>" . company-complete-selection)
							("M-RET" . company-complete-selection)
							;; ("<tab>" . company-abort)
							;; ("TAB" . company-abort)
							)
	:hook ((prog-mode) . company-mode)
  :config
	;; (define-key company-active-map "<tab>" 'company-abort) ; TODO fix cdlatex and company working together, this may be tough
	;; (define-key company-active-map "TAB" 'company-abort)
	
  (setq company-minimum-prefix-length 1
				company-idle-delay 0.0 ; default is 0.2
				)

	)
(use-package company-posframe
	:when (posframe-workable-p)
	:after company
	:hook (company-mode . company-posframe-mode)
	;; if you use desktop.el

	;; :config	
	;; (push '(company-posframe-mode . nil)
	;; 			desktop-minor-mode-table)
	)

(use-package consult-company
  :after (consult company)
  :config
  (define-key company-mode-map [remap completion-at-point] #'consult-company)
  )


;; ;; not using corfu at the moment as I can't figure out how to unbind down and up in xah-fly-keys to not move the goddamn completion in company. Maybe there is support for company in xah-fly-keys. Also corfu seems more buggy while company justWorks
;; (use-package corfu
;;   :if +apexless
;;   :bind (:map corfu-map
;; 							("<return>" . nil)
;; 							("RET" . nil)
;; 							("M-<return>" . corfu-insert)
;; 							("M-RET" . corfu-insert)
;; 							("M-SPC" . corfu-insert-separator)
;; 							("C-n" . corfu-next)
;; 							("C-p" . corfu-previous)
;; 							("next-line" . nil)
;; 							("previous-line" . nil)
;; 							("<up>" . nil)
;; 							("<down>" . nil)
;; 							)
;;   :custom
;;   (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
;;   (corfu-auto t)                 ;; Enable auto completion
;;   (corfu-separator ?\s)          ;; Orderless field separator
;;   ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
;;   ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
;;   ;; (corfu-preview-current nil)    ;; Disable current candidate preview
;;   ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
;;   ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
;;   ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
;;   ;; (corfu-scroll-margin 5)        ;; Use scroll margin

;;   ;; Recommended: Enable Corfu globally.
;;   ;; This is recommended since Dabbrev can be used globally (M-/).
;;   ;; See also `corfu-excluded-modes'.
;;   :init
;;   (global-corfu-mode)
;;   :config
;;   (defun orderless-fast-dispatch (word index total)
;;     (and (= index 0) (= total 1) (length< word 4)
;; 				 `(orderless-regexp . ,(concat "^" (regexp-quote word)))))

;;   (orderless-define-completion-style orderless-fast
;;     (orderless-style-dispatchers '(orderless-fast-dispatch))
;;     (orderless-matching-styles '(orderless-literal orderless-regexp)))

;;   (setq-local corfu-auto t
;;               corfu-auto-delay 0
;;               corfu-auto-prefix 0
;;               completion-styles '(orderless-fast))
;;   )
;; ;; Icons in corfu!
;; (use-package kind-icon
;;   :after corfu
;;   :custom
;;   (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Use Dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;; ;;Language Server Protocol(LSP)-related
;; these are for eglot
(use-package xref)
(use-package project)
(use-package eldoc)
(use-package eglot
  :hook ((python-mode c-mode-hook c++-mode-hook rust-mode
											;; LaTeX-mode
											) . eglot-ensure)
  :config
  ;; (setq completion-category-overrides '((eglot (styles orderless))))
	)


;; (use-package lsp-mode
;;   :commands (lsp lsp-deferred)
;;   :init
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   (setq lsp-keymap-prefix "C-c l")

;;   ;;for lsp to be faster
;;   (setq read-process-output-max (* 1024 1024)) ;; 1mb

;;   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;; 	 ((c-mode c++-mode python-mode haskell-mode) . lsp-deferred)

;; 	 ;; if you want which-key integration
;; 	 (lsp-mode . lsp-enable-which-key-integration))
;;   :config
;;   (setq lsp-idle-delay 0.1)
;;   (setq lsp-ui-doc-enable nil)
;;   (setq lsp-ui-doc-header t)
;;   (setq lsp-ui-doc-include-signature t)
;;   (setq lsp-ui-doc-border (face-foreground 'default))
;;   (setq lsp-ui-sideline-show-code-actions t)
;;   (setq lsp-ui-sideline-delay 0.05)
;;   )
;; (use-package lsp-ui
;;   :after lsp-mode
;;   :hook (lsp-mode . lsp-ui-mode)
;;     :commands lsp-ui-mode)
;; ;; (use-package lsp-treemacs
;; ;;   :after lsp-mode
;; ;;   :config (setq treemacs-space-between-root-nodes nil)
;; ;;   :commands lsp-treemacs-errors-list)

;; (use-package lsp-pyright
;;   :if +apexless
;;   :hook (python-mode . (lambda ()
;; 			 (require 'lsp-pyright)
;; 			 (lsp-deferred)))
;;   :init
;;   (when (executable-find "python3")
;;     (setq lsp-pyright-python-executable-cmd "python3"))
;;   (setq lsp-pyright-use-library-code-for-types t) ;; set this to nil if getting too many false positive type errors
;;   (setq lsp-pyright-stub-path (concat (getenv "HOME") "/stuff/compro/microsoft/python-type-stubs")) ;; example)
;;   )

;; (use-package lsp-haskell
;;   :init
;;   (if (system-name? "DURIAN")
;;       (setq ghc-location "~/.ghcup/bin/ghc")))

;; ;; Debug Adaptor Protocol(DAP)-related
;; (use-package dap-mode
;;   :after lsp-mode
;;   :config
;;   (require 'dap-cpptools) ; afterwards run dap-cpptools-setup
;;   (require 'dap-python) ; requires pip install "ptvsd>=4.2"
;;   (dap-auto-configure-mode)
;; )
;; ;; (use-package dap-LANGUAGE) to load the dap adapter for your language
;; ;; I don't understand the above line

;; for programming in Scheme, use geiser
(use-package geiser-guile
  :defer t
  :commands geiser-guile); geiser-guile to connect to guile repo!
(use-package geiser-racket
  :defer t
  :commands geiser-racket); for racket if you download minimal racket you need to "raco pkg install compatibility-lib"


(use-package python
  :straight (:type built-in)
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq python-indent-offset 4)
	)

(use-package pyenv ; what does this do?
	:defer t)
(use-package anaconda-mode
  :bind (("C-c C-x" . next-error))
	:hook (python-mode . anaconda-mode)
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
	)
(use-package company-anaconda
	:after company
  :config
	(add-to-list 'company-backends '(company-anaconda :with company-capf)))
;; manage python imports from emacs! pyimport-insert-missing requires another buffer open with an example of importing the missing library
(use-package pyimport
	:after python-mode)



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


(cond
 ((system-name? "ASSES-UX310UQK")
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:family "mononoki NF" :foundry "outline"  :height 120 :width normal))))))
 ((system-name? "DURIAN")
  (custom-set-faces
   '(default ((t (:family "mononoki" :foundry "UKWN"   :height 151 :width normal))))))
 ((system-name? "mango")
  (custom-set-faces
   '(default ((t (:family "mononoki" :foundry "UKWN"   :height 113 :width normal))))))
 (+apexless
  (custom-set-faces
   '(default ((t ( :height 140 :foundry "nil" :family "mononoki Nerd Font"))))))
 )
;; when you wanna fix the font on apexless not being displayed probably 'cuz of dashboard, this might help: http://xahlee.info/emacs/emacs/emacs_list_and_set_font.html


;; for reading pdf, look out for image-roll.el when the bugs are fixed for continuous scrolling, and wait for a gif to see whether it allows preview-like scrolling
(use-package pdf-tools
  :if (or (system-name? "mango") +apexless)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  ;; (setq-default pdf-view-display-size 'fit-page)
  (setq-default pdf-view-use-scaling t)
  (setq pdf-view-resize-factor 1.1)
  :custom (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  )
(use-package pdf-view-restore
  :after pdf-tools
	:hook (pdf-view-mode . pdf-view-restore-mode)
  :init	(setq pdf-view-restore-filename "~/.emacs.d/.pdf-view-restore"))

;; for reading epub, needs more config for epub to look nice
(use-package nov
  :defer t
	:mode ("\\.epub\\'" . nov-mode)
  :init
  (setq nov-shr-rendering-functions '((img . nov-render-img)
                                      (title . nov-render-title)
                                      (b . shr-tag-b)))
  :config
  (message "nov loaded")
  )


;; (use-package dashboard ;;this package extends startup time from 145ms to 900ms as it loads org-mode, but it also loads org-roam so that's convenient.
;; 	:if +apexless
;;   :init
;;   (setq dashboard-set-heading-icons t)
;;   (setq dashboard-set-file-icons t)
;;   (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
;;   ;; (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
;;   (setq dashboard-startup-banner "~/.emacs.d/emacs-dash.png")  ;; use custom image as banner
;;   (setq dashboard-center-content t) ;; set to 't' for centered content
;;   (setq dashboard-set-footer nil) ;; don't put random message below 
;;   (setq dashboard-items '((recents . 15)
;; 													(registers . 3)))
;;   :config
;;   (dashboard-setup-startup-hook)
;;   (dashboard-modify-heading-icons '((recents . "file-text")
;; 																		(bookmarks . "book"))))


(use-package vterm
	:defer t ; package already has basic commands autoloaded
	)



(use-package aggressive-indent
	:hook ((emacs-lisp-mode
					lisp-mode) .  aggressive-indent-mode))


(use-package sly
	:defer t
	:hook (lisp-mode . sly-editing-mode)
	:init
	(setq inferior-lisp-program "sbcl")
	)
(use-package sly-asdf
	:after sly)
(use-package sly-quicklisp
	:after sly)
(use-package sly-repl-ansi-color
	:after sly)
(use-package sly-macrostep
	:after (sly macrostep)
	;; Once it's done, M-x sly should now bring up a macrostep-enabled SLY.
	;; In .lisp files you can now use C-c M-e or M-x macrostep-expand to expand a macro.
	)

;; https://old.reddit.com/r/emacs/comments/x6rg1u/rust_with_emacs/inb9qka/
(use-package rust-mode
  :ensure t
  :mode "\\.rs$"
	:hook (rust-mode . cargo-minor-mode)
	:config
  (setq rust-format-on-save t)
  :custom-face
  (rust-question-mark-face ((t (:inherit font-lock-builtin-face :foreground "#ff0000" :weight bold)))))
(use-package cargo
  :ensure t
  :defer t
	:diminish cargo-minor-mode)


;; for tex info, digestif, the LaTeX lsp, creator can't live without this
(add-to-list 'Info-directory-list "/usr/local/texlive/2022/texmf-dist/doc/info/")


(use-package tex
	:straight auctex
	:mode ("\\.tex\\'" . latex-mode)
	:config
	(setq bibtex-dialect 'biblatex)
	(setq TeX-auto-save t)
	(setq TeX-parse-self t)
	;; ;; Enable LaTeX math support, maybe don't need this 'cuz of cdlatex
	;; (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
	;; ;; ;; Enable reference mangment
	;; (add-hook 'LaTeX-mode-hook #'reftex-mode)
	(add-hook 'LaTeX-mode-hook 'prettify-symbols-mode) ;; prettify symbols like \alpha \beta

	)

;; it seems installation of auctex with straight is a bit fucked, might need this if i run into problems

;; ;; As of Emacs 485622bbd1a you/I need AUCTeX > 13.0.5, or else you
;; ;; get errors when reftex tries to create labels.
;; ;;
;; ;; The el-get recipe won't work for AUCTeX because (I assume) el-get
;; ;; runs Elisp right out of the repo it clones to, which contains
;; ;; support files (ex. styles/*.el) that won't be there if you use that
;; ;; same recipe with straight.el.  Hence we make our own recipe.
;; (straight-use-package '(auctex :source el-get
;; 															 :files ("*.el" "*.info" "dir"
;; 																			 "doc" "etc" "images" "latex" "style")))
;; ;; See the :load bits of
;; ;; https://github.com/dimitri/el-get/blob/master/recipes/auctex.rcp,
;; ;; which are not supported by straight.el as of this writing.  Without
;; ;; these you will get built-in Emacs LaTeX modes, not AUCTeX.
;; (require 'tex-site)
;; (require 'preview-latex)

;; fast math input
(use-package cdlatex
	:hook (((latex-mode LaTeX-mode) . turn-on-cdlatex)
				 ;; (org-mode . turn-on-org-cdlatex)
				 ) 
	)

;; automatic live math preview that gets out of your way
(use-package xenops
	:hook ((latex-mode LaTeX-mode org-mode). xenops-mode)
	:config
	(setq xenops-reveal-on-entry t)
	(setq xenops-math-image-scale-factor 1.25)
	)


(use-package org-modern
	:defer t
	:hook
	(org-agenda-finalize . org-modern-agenda)
	;; (org-mode . org-modern-mode)
	)

;; un-emphasize when cursor is on element
;; will fail to detect elements that are nested inside "certain other elements", like comments or document titles
(use-package org-appear
	:defer t
	:after org
	;; :hook (org-mode . org-appear-mode)
	;; hook it with org-modern if possible, 'cuz I want to see everything with default prefs in life.org
	:config
	(setq org-appear-autoemphasis t				;the only one that's on by default, like for /italic/, _underline_, +strikethrough+, etc.
				org-appear-autoentities t
				org-appear-autolinks t
				org-appear-autosubmarkers t))


(use-package valign
	:hook (org-mode . valign-mode)
	:init (setq valign-fancy-bar t))



;; from https://bytemeta.vip/index.php/repo/alexluigit/emacs-grandview
(defvar +font-size 140)
(defvar +default-font "mononoki Nerd Font")
(defvar +fixed-font "mononoki Nerd") ; for info
(defvar +variable-font "Sarasa Mono SC"); variable-pitch font
(defvar +CJK-font "LXGW WenKai Mono") ; Chinese, Japanese, Korean characters

;;;###autoload
(defun +font-setup (&optional frame)
  "Setup default/fixed-pitch/variable-pitch/zh-font."
  (custom-theme-set-faces
   'user
   '(font-lock-keyword-face ((t (:slant italic :foreground "Cyan1")))); remember there's the color set here
   ;; '(font-lock-variable-name-face ((t (:weight demibold))))
   ;; '(font-lock-function-name-face ((t (:weight demibold))))
   `(default ((t (:font ,(font-spec :family +default-font) :height ,+font-size))))
   `(fixed-pitch ((t (:font ,(font-spec :family +fixed-font) :height ,+font-size))))
   `(variable-pitch ((t (:font ,(font-spec :family +variable-font)
                               :height ,+font-size  )))))
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font t charset (font-spec :family +CJK-font))))

;;;###autoload
(defun +font-cn-set-title (beg end)
  (interactive "r")
  (remove-overlays beg end)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'display '(height 1.5))))

;;;###autoload
(defun +font-cn-set-quote (beg end)
  (interactive "r")
  (remove-overlays beg end)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face 'font-lock-comment-face)))

(setq +font-size 141)
(setq +default-font "mononoki Nerd Font")
(setq +fixed-font "Sarasa Mono SC") 
(setq +variable-font "ETBembo")
(setq +CJK-font "LXGW WenKai Mono")


;;;###autoload
(defun +org-font-setup ()
  "Setup variable-pitch fonts for org-mode."
  (variable-pitch-mode)
  (let ((variable-pitch `(:font ,+variable-font))
        (default `(:font ,+default-font)))
    (custom-theme-set-faces
     'user
     `(org-level-1 ((t (,@variable-pitch :height 1.5))))
     `(org-level-2 ((t (,@variable-pitch :height 1.4))))
     `(org-level-3 ((t (,@variable-pitch :height 1.3))))
     `(org-level-4 ((t (,@variable-pitch :height 1.2))))
     `(org-table ((t (,@default))))
     `(org-verbatim ((t (,@default))))
     `(org-formula ((t (,@default))))
     `(org-code ((t (,@default))))
     `(org-block ((t (,@default))))
     `(org-block-begin-line ((t (:foreground "#606060" :extend t))))
     '(org-tag ((t (:inherit (shadow) :weight bold :height 0.8)))))))

(+font-setup)

(use-package matlab-mode
	:defer t)
(use-package fish-mode ; fish shell scripting syntax highlighting
	:defer t) 

;; if you run the (use-package, the packages will be git cloned, even if they are not loaded
(when (or (system-name? "mango") (system-name? "DURIAN"))
  (use-package guix))

(when (or (system-name? "mango") (system-name? "nix-on-droid-placeholder-name") +apexless)
  (use-package nix-mode
    :mode "\\.nix\\'"))

;; Citations ---------------------------------------------------------------
;; HOW TO USE: 
;; 1. let org-cite know the bib file, by "#+bibliography: path-to-your-file" or org-cite-global-bibliography which you already set
;; 2. put "#+cite_export: csl ieee.csl", where ieee can be whatever csl file in "~/Zotero/styles/" or a full path to a csl file
;; 3. put "#+PRINT_BIBLIOGRAPHY:" at wherever you want bibliography to be printed.
;; Further info: https://blog.tecosaur.com/tmio/2021-07-31-citations.html
(use-package oc ; org-cite is part of default org-mode
	:straight (:type built-in)
	:after org
	:custom-face
	;; Have citation link faces look closer to as they were for `org-ref', otherwise they're cyan, same as links in org-mode
	(org-cite ((t (:foreground "DarkSeaGreen4"))))
	(org-cite-key ((t (:foreground "forest green" :slant italic))))
	:config
	(setq org-cite-global-bibliography `(,(expand-file-name "~/stuff/notes/bib/references.bib")))
	(setq org-cite-csl-styles-dir (expand-file-name "~/Zotero/styles/"))
	(setq org-cite-export-processors
				'((org . (csl "ieee.csl"))
					(md . (csl "chicago-fullnote-bibliography.csl"))   ; Footnote reliant
					(latex . biblatex)                                 ; For humanities
					(odt . (csl "chicago-fullnote-bibliography.csl"))  ; Footnote reliant
					(t . (csl "modern-language-association.csl"))      ; Fallback
					))
	)

(use-package citar
	:if +apexless
	:after (org all-the-icons)
	:straight (citar :type git :host github :repo "emacs-citar/citar" :includes (citar-org))
  :bind (("C-c b" . citar-insert-citation)
         :map minibuffer-local-map
         ("M-b" . citar-insert-preset))
	:custom
	;; from https://kristofferbalintona.me/posts/202206141852/#aesthetics
	(citar-templates
	 '((main . "${author editor:30}   ${date year issued:4}    ${title:110}")
		 (suffix . "     ${=type=:20}    ${tags keywords keywords:*}")
		 (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
		 (note . "#+title: Notes on ${author editor}, ${title}") ; For new notes
		 ))
	;; Configuring all-the-icons. From
	;; https://github.com/bdarcus/citar#rich-ui
	(citar-symbols
	 `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) .
					 ,(all-the-icons-faicon "file-o" :face 'kb/citar-icon-dim :v-adjust -0.1) )
		 (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) .
					 ,(all-the-icons-material "speaker_notes" :face 'kb/citar-icon-dim :v-adjust -0.3))
		 (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) .
					 ,(all-the-icons-octicon "link" :face 'kb/citar-icon-dim :v-adjust 0.01))))
	(citar-symbol-separator "  ")
	:init
	;; Here we define a face to dim non 'active' icons, but preserve alignment.
	;; Change to your own theme's background(s)
	(defface kb/citar-icon-dim
		;; Change these colors to match your theme. Using something like
		;; `face-attribute' to get the value of a particular attribute of a face might
		;; be more convenient.
		'((((background dark)) :foreground "#212428")
			(((background light)) :foreground "#f0f0f0"))
		"Face for having icons' color be identical to the theme
  background when \"not shown\".")

	:config
  (setq citar-bibliography `(,(expand-file-name "~/stuff/notes/bib/references.bib")))
	(setq citar-notes-paths (list org-roam-directory))
	;; (setq citar-open-note-function 'orb-citar-edit-note) ; if you use org-roam-bibtex
	)
;; makes org-cite use citar's nicer cite function, as org-cite's is very basic
(use-package citar-org
	:after oc
	:no-require
	:config
	(setq org-cite-insert-processor 'citar)
  (setq org-cite-follow-processor 'citar)
  (setq org-cite-activate-processor 'citar))
;; allows you to find all citations of a reference from the note of the reference. It's in infant stage, so might switch to org-roam-bibtex then back. Problem is org-roam-bibtex needs helm-bibtex...
(use-package citar-org-roam
  :after (citar org-roam)
  :no-require
  :config
	(citar-org-roam-mode))

(use-package citar-embark
  :after (citar embark)
  :no-require
  :config
	(citar-embark-mode)
	(setq citar-at-point-function 'embark-act); changes citar-dwim to embark-act
	)

;; END Citation ------------------------------------------------------------

(use-package restart-emacs ; to restart emacs, durr
	:defer t) 




;; Set gc threshold back to normal
;; if pauses are too long, decrease the threshold
;; if pauses are too frequent, increase the threshold
(setq gc-cons-threshold (* 128 1024 1024)) ; increase garbage collection limit to 100MiB, default is 0.8MB, measured in bytes




(use-package tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert)
				 (:map tempel-map
							 ("M-]" . tempel-next)
							 ("M-[" . tempel-previous)))
  :init
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
	
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
	)
(use-package racket-mode
	:defer)

(defun my-clear ()
	"clear current comint buffer"
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

;; #emacs@Libera.Chat <thuna`> for youtube specifically, elfeed + youtube-dl + mpv is pretty much all you need
;; use this to get rss feed of a youtube channel:
;; https://www.youtube.com/feeds/videos.xml?channel_id=<CHANNEL-ID>
;; (use-package elfeed)

(use-package circe ; currently i prefer ERC more! but circe is more modern and takes the lessons learnt from ERC
	:defer)

;; (use-package telega
;; 	:defer
;; 	:config
;; 	;; (setq telega-server-libs-prefix "/opt/homebrew/Cellar/tdlib/HEAD-faa738d/")
;; 	)

(use-package nix-haskell-mode
	:disabled ; enable for cabal projects and have a look
  :hook (haskell-mode . nix-haskell-mode))

(use-package eshell-vterm
  :after eshell
  :config
  (eshell-vterm-mode))

(use-package consult-org-roam ; keeping this around for live-preview when searching org-roam.
	:defer
	:custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  ;; :config
  ;; ;; Eventually suppress previewing for certain functions
  ;; (consult-customize consult-org-roam-forward-links :preview-key (kbd "M-."))
  ;; :bind
  ;; ("C-c n e" . consult-org-roam-file-find)
  ;; ("C-c n b" . consult-org-roam-backlinks)
  ;; ("C-c n r" . consult-org-roam-search)
	)
(use-package all-the-icons-completion
	:after (marginalia all-the-icons)
	:init (all-the-icons-completion-mode)
	:hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
	)

(use-package devdocs
	:defer
	:init
	(add-hook 'python-mode-hook
						(lambda () (setq-local devdocs-current-docs '("python~3.10"))))
	:bind ("C-h D" . devdocs-lookup))

(use-package eshell
	:defer
	:init
	(setq eshell-history-size 10000))
