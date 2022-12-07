

;; when building packages:
;; nix's cmake =/= homebrew's cmake, so (setenv "PATH" (concat "/opt/homebrew/bin/:" (getenv "PATH"))) from https://www.emacswiki.org/emacs/ExecPath

;; use-package-expand-minimally

;; keybindings to remember -----------------------------------------------------
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
;; s = (qwerty)xah-fly-keys RET at point without moving cursor

;; Packages to maybe have a look at
;; - gamegrid
;; - narrow-indirect
;; - mixed-pitch
;; - ace-link
;; - ledger-mode & flycheck-ledger
;; - consult-flycheck & flycheck
;; - dwim-shell-command
;; - undo-fu & undo-fu-session
;; - org-bookmark-heading
;; - org-visibility
;; - cmake-mode
;; - ws-butler
;; - topsy
;; - with-editor
;; - scratch
;; - so-long
;; - easy-kill
;; - ox-jira
;; - gcmh
;; - symbol-overlay
;; - git-auto-commit-mode
;; - transient
;; - just-mode
;; - json-snatcher
;; - iedit
;; - archive-rpm

;; Cool Packages to maybe have a look at ---------------------------------------
;; - org-noter, annotating pdf,epub with complete org files in the margin
;; - org-transclusion, live preview of parts of another org file via links
;; - bind-key
;; - apheleia, asynchronous code formatting
;; - org-contrib, additional org packages
;; - undo fu, undo between sessions
;; - ipretty, pretty-print sexps https://framagit.org/steckerhalter/ipretty
;; - org-latex-impatient, preview as you type latex in org-mode
;; - forge, for working with git forges
;; - transmission
;; - sgml-mode, for working with html files
;; - emms
;; - proof-general, for working with proof assistants, targetd at intermediate to experts
;; - compile.el
;; - lingva, interface with google translate
;; - mw-thesaurus, merrian webster thesaurus usage, donwloaded in org format
;; - spell-fu, spell checking without external dependencies (???)
;; - org-ql, query language for org files. I don't know why you would want this when there's ripgrep, maybe a nicer ripgrep for org files?
;; - youdao-dictionary, an interface for the chinese-english/english-chinese online dictionary
;; - migemo, allowing you to isearch the romanisation of japanese instead of typing the japanese character itself
;; - DDSKK (DareDevil Simple Kana to Kanji conversion), if you wanna type japanese?
;; - ox-twbs, a modification to ox-html for a "modern style"

;; Cool packages that i want to install later on--------------------------------
;; - persp-mode, workspace manager
;; - highlight-symbol-mode, highlights all occurances of the symbol under point
;; - dumb-jump, for when u don't have lsp and want to jump to definitions
;; - visual-regexps & visual-regexps-steroids, the first for live highlighting of regexps, replacing replace-regexp w/ visual-regexp Before installing test whether I need it or not!

;; Intro to Config -------------------------------------------------------------
;; This config is sorted in least to most likely to break... as I test stuff with my init.el and restart often.
;; I use hs-minor-mode and progn to sort and view this config, so if you don't use them, good luck!

(setq large-file-warning-threshold (* 128 1024 1024))
;; Portion of heap used for allocation.  Defaults to 0.1.
(setq gc-cons-percentage 0.6)

(progn ; Function & Variable Definitions ---------------------------------------
	(defun +system-name? (name-string)
		(string-equal system-name name-string))

	;; This file supports a few computers
	(defvar +apexless (and (eq system-type 'darwin)) "Whether Emacs is running on my macbook pro 14-inch m1 pro") ;; system-name Apexless/Apexless.local/???
	(defvar +termux (and (+system-name? "localhost")) "Whether Emacs is running on termux (probably on my phone)")
	(defvar +mango (and (+system-name? "mango")) "Whether Emacs is running on my linux desktop running NixOS")
	(defvar +asses (and (+system-name? "ASSES-UX310UQK")) "Whether Emacs is running on ASSES-UX310UQK (my poly laptop)")
	(defvar +durian (and (+system-name? "DURIAN")) "Whether Emacs is running on kor's poly laptop running Manjaro")
	(defvar +nix-on-droid (+system-name? "nix-on-droid-placeholder-name") "Whether emacs is running on nix-on-droid")

	(defun +delete-file-visited-by-buffer (buffername)
		"Delete the file visited by the buffer named BUFFERNAME."
		(interactive "b")
		(let* ((buffer (get-buffer buffername))
					 (filename (buffer-file-name buffer)))
			(when filename
				(delete-file filename)
				(kill-buffer-ask buffer))))

	(defun +er-sudo-edit (&optional arg)
		"Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
		(interactive "P")
		(if (or arg (not buffer-file-name))
				(find-file (concat "/sudo:root@localhost:"
													 (ido-read-file-name "Find file(as root): ")))
			(find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

	(defun +clear ()
		"clear current comint buffer"
		(interactive)
		(let ((comint-buffer-maximum-size 0))
			(comint-truncate-buffer)))


	;; from https://www.emacswiki.org/emacs/HalfScrolling#h5o-4
	(defun +scroll-half-page (direction)
		"Scrolls half page up if `direction' is non-nil, otherwise will scroll half page down."
		(let ((opos (cdr (nth 6 (posn-at-point)))))
			;; opos = original position line relative to window
			(move-to-window-line nil)  ;; Move cursor to middle line
			(if direction
					(recenter-top-bottom -1)  ;; Current line becomes last
				(recenter-top-bottom 0))  ;; Current line becomes first
			(move-to-window-line opos)))  ;; Restore cursor/point position
	;;;###autoload
	(defun +scroll-half-page-down ()
		"Scrolls exactly half page down keeping cursor/point position."
		(interactive)
		(+scroll-half-page nil))
	;;;###autoload
	(defun +scroll-half-page-up ()
		"Scrolls exactly half page up keeping cursor/point position."
		(interactive)
		(+scroll-half-page t))

	(defun irc () ;; overwrites rcirc command, but I don't use rcirc anyways
		"Connect to IRC"
		(interactive)
		(circe "Libera Chat"))
	) 

(progn ; Default configs -------------------------------------------------------
	;; System locale to use for formatting time values.
	(setq system-time-locale "C")           ; Make sure that the weekdays in the
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
	(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
	(menu-bar-mode -1)

	(unless +apexless (display-time-mode 1))	;apexless has time permanently displayed so you don't need this


	(cond (+apexless (toggle-frame-maximized))
				(+mango nil)
				(t (toggle-frame-fullscreen)))


	;; set buffer to auto-update when the associated file is written to externally, and set it to update in 1s
	(global-auto-revert-mode 1)
	(setq auto-revert-interval 1)

	;; Remember and restore the last cursor location of opened files
	(save-place-mode 1)

	;; recent files browsing feature
	(setq recentf-max-saved-items 10000
				recentf-max-menu-items 10000)
	(recentf-mode 1)

	;; speed up unicode loading, but uses more memory
	(setq inhibit-compacting-font-caches t)

	(set-language-environment 'utf-8) ; fixes the "haskell process has died" error somehow

	(setq enable-recursive-minibuffers t) ; enables more than 1 minibuffer to be available at once
	(minibuffer-depth-indicate-mode 1) ; shows [minibuffer-depth] at left of the echo area when depth >1


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

	(setq user-mail-address "healtermon@gmail.com")

	;; Use a hook so the message doesn't get clobbered by other messages.
	(add-hook 'emacs-startup-hook
						(lambda ()
							(message "Emacs ready in %s with %d garbage collections."
											 (format "%.2f seconds"
															 (float-time
																(time-subtract after-init-time before-init-time)))
											 gcs-done)))

	;; in isearch, highlight the line u are currently on 'cuz I tend to spend quite some time searching for it; it jumps all over the screen
	;; this will disable hl-line-mode if already on, so beware.
	(defun +turn-off-hl-line-mode () (hl-line-mode -1))
	(add-hook 'isearch-mode-hook #'hl-line-mode)
	(add-hook 'isearch-mode-end-hook '+turn-off-hl-line-mode)

	)

;; All non-in-built package-related stuff goes under here-----------------------

(progn ; straight, use-package and benchmarking --------------------------------
	
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
					straight-repository-branch "develop"
					straight-hosts '((github "github.com" ".git")
													 (gitlab "gitlab.com" ".git")
													 (sourcehut "git.sr.ht" ".git"); I still don't know how to get it to work
													 (bitbucket "bitbucket.com" ".git")
													 (codeberg "codeberg.org" ".git")))
		
		;; modified straight bootstrap code
		(unless (file-exists-p bootstrap)
			(with-current-buffer (url-retrieve-synchronously script 'silent 'inhibit-cookies)
				(goto-char (point-max)) (eval-print-last-sexp)))
		(load bootstrap nil 'nomessage)

		)

	(straight-use-package 'bind-key) ; for `bind-keys' macro
	(straight-use-package '(once :type git :host github :repo "emacs-magus/once")) ; wanna speed up your init? here!
	(straight-use-package 'use-package) ; install use-package

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

	)

(progn ; essential packages for everyone ---------------------------------------
	
	(use-package xah-fly-keys
		:demand t
		:config
		;; set-layout required before enabling
		(xah-fly-keys-set-layout (cond 
															((or +asses +mango) 'colemak-mod-dh)
															(t 'qwerty)))
		(xah-fly-keys 1)
		(global-set-key (kbd "C-v") '+scroll-half-page-down)
		(global-set-key (kbd "M-v") '+scroll-half-page-up)
		(defun +dirvish-xfk-command-mode-n ()
			(interactive)
			(cond ((string-equal major-mode "dirvish-mode") (dirvish-narrow))
						(t (isearch-forward))))
		(define-key xah-fly-command-map (kbd "n") '+dirvish-xfk-command-mode-n)
		)
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
	
	(use-package vertico ;; a vertical autocomplete selection menu
		:straight (vertico :files (:defaults "extensions/*")
											 :includes (vertico-indexed vertico-flat vertico-grid vertico-mouse vertico-quick vertico-buffer vertico-repeat vertico-reverse vertico-directory vertico-multiform vertico-unobtrusive ))
		:bind (:map vertico-map ("M-DEL" . vertico-directory-delete-word))
		:init
		(vertico-mode)
		(setq vertico-count (if +termux 10 20))
		(setq vertico-resize t)
		(setq vertico-cycle t)
		)

	(use-package orderless ;; a completion style
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
		)

	(use-package marginalia ;; annotates the minibuffer like the margins in a book (look on the right side)
		:bind (:map minibuffer-local-map
								("M-A" . marginalia-cycle))
		:init
		(setq marginalia-max-relative-age 0)
		(marginalia-mode 1))

	(use-package consult ;; provides _good shit_ versions of common commands and more
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

	(use-package vundo
		:defer
		:config
		(setq vundo-glyph-alist vundo-unicode-symbols)
		(set-face-attribute 'vundo-default nil :family "Unifont")
		)
	)

(progn ; elisp programming -----------------------------------------------------
	;; These are put in their own heading as I consider elisp programming
	;; to be more important than other sorts of programming, especially in
	;; emacs. It's equivalent to app settings working or not.

	(use-package restart-emacs ; to restart emacs, durr. Obsolete in emacs 29.
		:defer t)
	
	(use-package paredit
		:hook ((emacs-lisp-mode
						lisp-interaction-mode
						ielm-mode
						lisp-mode
						eval-expression-minibuffer-setup
						scheme-mode) . paredit-mode))

	(use-package aggressive-indent
		:hook ((emacs-lisp-mode
						lisp-mode) .  aggressive-indent-mode))

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
	
	(use-package hideshow
		:straight nil
		:hook (prog-mode . hs-minor-mode)
		:bind (:map prog-mode-map
								("A-<tab>" . hs-toggle-hiding)
								("A-S-<tab>" . +toggle-hideshow-all))
		:init

		;; taken from hideshow.el top commentary
		(defvar +hs-hide nil "Current state of hideshow for toggling all.")
	  ;;;###autoload
		(defun +toggle-hideshow-all () "Toggle hideshow all."
					 (interactive)
					 (setq +hs-hide (not +hs-hide))
					 (if +hs-hide
							 (hs-hide-all)
						 (hs-show-all)))
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

	;; Annotate value of lines containing ; => .
	(use-package lispxmp
		:init
		(setq byte-compile-warnings '(cl-functions)) ;make it not complain about using the depreciated cl.el instead of cl-lib
		)

	(use-package highlight-symbol ; highlight all occurances of symbol at point in buffer
		:disabled ; "<f7> e e" binded in  xah-fly-keys also does this
		:hook (prog-mode . highlight-symbol-mode))

	)

(use-package crux
	:defer)
(use-package reveal-in-folder ;; Open Finder at location
	:if +apexless 												; only works on macOS
	:defer)
(use-package terminal-here ;; Open location in external terminal
	:defer
	:config
	(setq terminal-here-mac-terminal-command 'iterm2)
	)



(progn ; file manager ----------------------------------------------------
	;; dired-related settings

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
	)

(progn ; editing on remote machines --------------------------------------
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
	)

(progn ; Version-control-related -----------------------------------------
	(use-package magit ; an amazing front-end to git
		:bind ("C-x g" . magit-status))
	(use-package magit-delta ;; syntax hightlighting with delta(command-line program) in magit diffs
		:hook (magit-mode . magit-delta-mode))

	(use-package diff-hl ; highlights diffs in the margins. If you want nicer ones like in vscode, https://ianyepan.github.io/posts/emacs-git-gutter/
		:hook ((after-init . global-diff-hl-mode)
					 (magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
					 (magit-post-refresh-hook . diff-hl-magit-post-refresh)))
	)

(progn ; Templates/snippets ---------------------
	(use-package tempel
		;; Require trigger prefix before template name when completing.
		;; :custom
		;; (tempel-trigger-prefix "<")

		:bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
					 ("M-*" . tempel-insert)
					 (:map tempel-map
								 ("M-]" . tempel-next)
								 ("M-[" . tempel-previous)))
		:hook ((prog-mode text-mode) . tempel-setup-capf)

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
		
		;; Optionally make the Tempel templates available to Abbrev,
		;; either locally or globally. `expand-abbrev' is bound to C-x '.
		;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
		;; (global-tempel-abbrev-mode)

		)
	)

(progn ; Notes/the org ecosystem -----------------------------------------------
	(use-package org
		:bind (("C-c a" . org-agenda)
					 ("C-c l" . org-store-link))
		:hook (;; (org-mode . org-toggle-pretty-entities)
					 (org-mode . visual-line-mode)
					 ;; (org-mode . +org-font-setup)
					 )
		
		:init
		(setq org-return-follows-link t)
		(setq org-startup-folded 'content)
		:config
		;; (setq org-hide-emphasis-markers t)
		(setq org-hide-leading-stars t)
		(setq org-log-done t)
		(setq org-startup-indented t) ; with prot's themes, org-indent-mode adds additional line spacing that makes me unhappy as less information can be displayed on-screen. I believe his themes bring more convenience than org-indent-mode.
		(setq org-agenda-files (list "~/stuff/notes/zk/life.org"
																 "~/stuff/notes/calendars/healtermon-gmail.org"))
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
		(setq org-image-actual-width nil)

		
		)
	;;org-agenda-custom-commands is under custom-set-variables for convenience; the "Easy Customisation" updates to there.

	(use-package org-contrib)
	(use-package org-download
		:after org
		:init
		(setq org-download-method 'directory)
		(setq-default org-download-image-dir "~/stuff/notes/zk/p/")
		)

	(use-package org-roam
		:init
		(setq org-roam-v2-ack t)
		(setq org-roam-directory (file-truename (if +termux
																								"/data/data/com.termux/files/home/storage/shared/stuff/notes/zk/"
																							"~/stuff/notes/zk/")))
		(setq org-roam-dailies-directory "daily/")
		(when (+system-name? "localhost")
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

		;; Excludes all nodes
		;; 1. under the .stversions directory, or
		;; 2. with the "NO_ORG_ROAM" tag from the org-roam database.
		(setq org-roam-db-node-include-function
					(lambda ()
						(and
						 (not (string-match-p (regexp-quote ".stversions") (buffer-file-name)))
						 (not (member "NO_ORG_ROAM" (org-get-tags))))))
		(setq org-agenda-hide-tags-regexp "NO_ORG_ROAM")

		;; if you ever wanna rename your file titles, look at https://org-roam.discourse.group/t/does-renaming-title-no-longer-renames-the-filename/2018
		:custom
		;; (org-roam-completion-everywhere t)
		(org-roam-node-display-template
		 (concat "${type:20} ${title:*} "
						 (propertize "${tags:20}" 'face 'org-tag)))
		(org-roam-file-exclude-regexp ".*~.*")
		(org-roam-capture-templates
		 '(("d" "default without ${slug}" plain
				"%?"
				:if-new (file+head "%<%Y%m%d%H%M%S>.org"
													 "#+title: ${title}\n")
				:unnarrowed t)
			 ("c" "contacts" plain ; why I put contacts here instead of org-contacts is so that I won't read about another person when searching someone, as org-contacts makes you keep it all in 1 file. I can replicate search with ripgrep at certain file.
				"%?"
				:if-new (file+head "contacts/%<%Y%m%d%H%M%S>.org"
													 ":PROPERTIES:
:DATEFIRSTMET: %^t
:PLACEFIRSTMET:
:BIRTHDAY:
:END:
#+title: ${title}
Likes:
Dislikes:
Hobbies:
Jobs:
Addresses:
Emails:
Dreams:
Views on Life:
Contact Mediums:
Notes:
")
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
		(cl-defmethod org-roam-node-type ((node org-roam-node))
			"Return the TYPE of NODE."
			(condition-case nil
					(directory-file-name
					 (file-name-directory
						(file-relative-name (org-roam-node-file node) org-roam-directory)))
				(error "")))
		
		(use-package dash)
		(use-package f) 
		(use-package s)
		(use-package emacsql)
		(if +termux (use-package emacsql-sqlite3) (use-package emacsql-sqlite))
		(use-package magit-section)
		(org-roam-db-autosync-mode) ;; need org-roam-sqlite-available-p to be true
		(define-key org-roam-mode-map [mouse-1] #'org-roam-visit-thing) ;;TODO org-roam-mode-map is deprecated, fix this
		(use-package consult-org-roam
			:config
			(consult-org-roam-mode 1)) 
		)

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

	(when +mango ;; if you run the (use-package, the packages will be git cloned, even if they are not loaded
		(use-package websocket :after org-roam)
		(use-package simple-httpd)
		(use-package org-roam-ui
			:straight
			(:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
			:after (org-roam websocket simple-httpd f)
			;;  if you don't care about startup time, use
			;;  :hook (after-init . org-roam-ui-mode)
			:config
			(setq org-roam-ui-sync-theme t
						org-roam-ui-follow t
						org-roam-ui-update-on-save t
						org-roam-ui-open-on-start t)))

	)

(progn ; LaTeX related ----------------------
	(use-package xenops ;; automatic live math preview that gets out of your way
		:hook ((latex-mode LaTeX-mode org-mode). xenops-mode)
		:config
		(setq xenops-reveal-on-entry t)
		(setq xenops-math-image-scale-factor 1.25)
		)
	
	;; for tex info. The LaTeX lsp digestif's creator can't live without this
	(add-to-list 'Info-directory-list "/usr/local/texlive/2022/texmf-dist/doc/info/")

	(use-package tex
		:straight auctex
		:mode ("\\.tex\\'" . latex-mode)
		:config
		(require 'texmathp) ; Needed for checking whether in math environments. TODO test this lol
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

	
	(use-package cdlatex ;; fast LaTeX math input
		:hook (((latex-mode LaTeX-mode) . turn-on-cdlatex)
					 ;; (org-mode . turn-on-org-cdlatex)
					 )
		)
	)

(progn ; Citations -------------------------------------------------------------
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

	)

(progn ; Viewing & Editing pdf, epub, idk...  ----------------------------------
	;; for reading pdf, look out for image-roll.el when the bugs are fixed for continuous scrolling, and wait for a gif to see whether it allows preview-like scrolling
	(use-package pdf-tools
		:if (or +mango +apexless)
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
	)

(progn ; Shells & Terminals ----------------------------------------------------
	(use-package vterm
		:defer t ; package already has basic commands autoloaded
		:custom (vterm-install t)
		)
	(use-package multi-vterm ;; default vterm allows only 1 buffer, this is to allow more
		:defer t
		:after vterm
		)

	(use-package eshell
		:defer
		:init
		(setq eshell-history-size 10000))

	(use-package eshell-vterm
		:after eshell
		:config
		(eshell-vterm-mode))
	)

(progn ; Completion-related... Idk what to name this ---------------------------
	(use-package dabbrev
		;; Tip: use Dabbrev with autocompletion globally! 
		
		;; Swap M-/ and C-M-/
		:bind (("M-/" . dabbrev-completion)
					 ("C-M-/" . dabbrev-expand))
		;; Other useful Dabbrev configurations.
		:custom
		(dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

	(use-package company ; CompAny = Complete Anything
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

	(use-package company-posframe ; use posframes for the popup, and also comes with icon support, plus backend-showing out-of-the-box
		;;  DISCLAIMER: frame saving with burly saves the posframes, and of course, gives error when it tries to restore a #<buffer item>, "Invalid Syntax "#" "

		:when (posframe-workable-p)
		:after company
		:hook (company-mode . company-posframe-mode)
		;; if you use desktop.el

		;; :config	
		;; (push '(company-posframe-mode . nil)
		;; 			desktop-minor-mode-table)
		)

	(use-package consult-company ; what is this?

		:after (consult company)
		:config
		(define-key company-mode-map [remap completion-at-point] #'consult-company)
		)

	)

(progn ; General Programming -------------------------------------------------------------------
	;; HOW TO USE: C-u extended-command devdocs to set new default docset to search, otherwise just search normally with command devdocs-lookup
	(use-package devdocs
		:defer
		:init
		(add-hook 'python-mode-hook
							(lambda () (setq-local devdocs-current-docs '("python~3.10"))))
		:bind ("C-h D" . devdocs-lookup))

	(use-package rainbow-mode ;; colors hex colors
		:hook (prog-mode . rainbow-mode))
	)
(progn ; Language Server Protocol(LSP)-related ----
	;; these are for eglot
	(use-package xref)
	(use-package project)
	(use-package eldoc)
	(use-package eglot
		:hook ((python-mode c-mode c++-mode rust-mode nix-mode clojure-mode
												;; LaTeX-mode
												) . eglot-ensure)
		:config
		(setq eglot-events-buffer-size 0)	;; In the name of speed, this stops eglot from logging the json events of lsp server
		;; (setq completion-category-overrides '((eglot (styles orderless))))
		;; if you wanna have yasnippet completions show up while using eglot either corfu/company: https://stackoverflow.com/questions/72601990/how-to-show-suggestions-for-yasnippets-when-using-eglot
		)
	(use-package consult-eglot
		:after (consult eglot))
	)
(progn ; Scheme Programming -----------------------
	(use-package geiser-guile
		:defer t
		:commands geiser-guile); geiser-guile to connect to guile repl!

	(use-package geiser-racket
		:defer t
		:commands geiser-racket); for racket if you download minimal racket you need to "raco pkg install compatibility-lib"
	)
(progn ; Python Programming -----------------------
	(use-package python
		;; DON'T confuse this with python-mode.el, they are 2 different packages:
		;; python.el is built-in and has better integration with emacs, while
		;; python-mode.el is a mess in terms of fucntions to call.
		;; Having both installed makes it very confusing.
		:straight
		:mode ("\\.py\\'" . python-mode)
		:interpreter ("python" . python-mode)
		:bind (:map python-mode-map
								("C-c e" . python-shell-send-statement))
		:config
		;; Remove guess indent python message
		(setq python-indent-guess-indent-offset-verbose nil)
		(setq python-indent-offset 4)
		)
	)
(progn ; Common Lisp Programming ------------------
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
	)
(progn ; Other-languages Programming ---------------------
	(use-package matlab-mode
		:defer t)
	(use-package fish-mode ; fish shell scripting syntax highlighting
		:defer t) 
	(when (or +mango +nix-on-droid +apexless)
		(use-package nix-mode ; for writing nix expressions
			:mode "\\.nix\\'"))
	)

(when (or (+system-name? "mango") +durian)
  (use-package guix ; interface for the guix package manager
		))

(progn ; Password-Manager ------------------------------------------------------
	(use-package bitwarden
		:straight (:type git
										 :host github
										 :repo "seanfarley/emacs-bitwarden")
		:init
		(setq bitwarden-user user-mail-address)
		;; (bitwarden-auth-source-enable) ;; don't need it (yet)
		)
	)

(progn ; Communication Protocols -----------------------------------------------
	(use-package elpher ; a gopher and gemini client, super simple to use
		:defer)
	
	(use-package mastodon ; mastodon client
		;; not practical LMAO I'd rather use Mastonaut
		:defer 
		:init
		;; change these whenever you wanna connect to another server
		(setq mastodon-instance-url "https://emacs.ch"
					mastodon-active-user "healtermon"))

	(use-package ement ; matrix client and hence also IRC
		:straight (:type git
										 :host github
										 :repo "alphapapa/ement.el")
		:defer)

	(use-package circe ; IRC Client; takes the lessons learnt from ERC and is more easily extensible, and has nicer documentation IMO. Also since it's simpler it's easier to undertand, though also very noob-unfriendly from experience (see below)
		;; Q: honestly I still don't know how to login without using circe-network-options, unlike in ERC where they prompt you, circe doesn't seem to let you msg ppl?
		;; A: well it's actually 'cuz "/msg NickServ IDENTIFY user pass" opens in another buffer, which if you didn't notice and typed the wrong password, makes it seem like nothing happened... So it is a beautiful client after all, separating all the chats :)
		:defer
		:config
		(setq circe-network-options
					`(("Libera Chat"
						 :tls t
						 :nick "healtermon"
						 :sasl-username "healtermon"
						 :sasl-password ,(bitwarden-getpass "libera.chat") ;; gotta login via bw-cli and  "(bitwarden-unlock)" before "(circe)" to get this to work
						 :channels ( "#emacs-circe" "#emacs"  "#emacs-nixos" "#nixos" "#nix-darwin"
												 "#guix" "#systemcrafters" "#hammerspoon" "#asahi"
												 "#clschool" "#scheme")
						 )))
		(setq circe-reduce-lurker-spam t)
		(setq circe-format-server-topic "*** Topic change by {userhost}: {topic-diff}") ;; topic change diff
		;; enable logging of chats
		(load "lui-logging" nil t)
		(setq lui-logging-directory "~/.emacs.d/.luilogs")
		(enable-lui-logging-globally)
		;; enable coloring of nicknames
		(require 'circe-color-nicks)
		(enable-circe-color-nicks)

		(setq lui-time-stamp-format "%H:%M"
					lui-time-stamp-position 'right-margin
					lui-fill-column 89 ; unused as fill-type is nil, means no filling, or restricting the output to within a certain number of columns
					lui-fill-type nil
					)

		(add-hook 'lui-mode-hook 'my-lui-setup)
		(defun my-lui-setup ()
			(setq fringes-outside-margins t
						right-margin-width 5
						word-wrap t
						wrap-prefix "    "
						truncate-lines nil ;; disable truncate-lines so you can comfortably read
						)
			(setf (cdr (assoc 'continuation fringe-indicator-alist)) nil)
			)

		(setq lui-track-bar-behavior 'before-switch-to-buffer)
		(enable-lui-track-bar)

		)
	
	(use-package erc
		:straight (:type built-in)
		:defer
		:config
		(setq erc-nick "healtermon")
		(setq erc-fill-column 90
					erc-fill-function 'erc-fill-static
					erc-fill-static-center 20)
		(setq erc-track-enable-keybindings t) ; enable C-c C-SPC to go to new messages

		)
	(use-package erc-hl-nicks
		:after erc
		:config
		(add-to-list 'erc-modules 'hl-nicks))
	(use-package erc-image
		:after erc
		:config
		(setq erc-image-inline-rescale 300)
		(add-to-list 'erc-modules 'image))

	
	
	(use-package telega ; GOATed Telegram Client
		:defer
		:init
		;; (setq telega-server-libs-prefix "/var/empty/local/")	
		(setq telega-server-libs-prefix "/opt/homebrew/Cellar/tdlib/HEAD-d581e04/")
		;; :config
		)


	)

(use-package google-this
	:commands (google-this-translate-query-or-region) ;; there's no autoload for just this 1 command, but there is for the 15 other commands. Why?
	:defer)
(use-package langtool
	;; from https://sqrtminusone.xyz/configs/emacs/#languagetool
	;; LanguageTool is a great offline spell checker. For some reason, the download link is nowhere to be found on the home page, so it is listed below
 	;; https://dev.languagetool.org/http-server
	:defer
	:init
	(setq langtool-language-tool-server-jar  "/Users/s/stuff/compro/LanguageTool/LanguageTool-5.9/languagetool-server.jar")
	(setq langtool-default-language "en-US")
	(setq langtool-mother-tongue "zh-CN"))

(progn ; Prettifying Everything ------------------------------------------------
	(use-package all-the-icons ; for dashboard & dirvish & citar
		:defer t
		:config (setq all-the-icons-scale-factor 1.0)
		)

	(use-package all-the-icons-completion ; adds icons to minibuffer completion
		:after (marginalia all-the-icons)
		:init (all-the-icons-completion-mode)
		:hook (marginalia-mode . all-the-icons-completion-marginalia-setup) ; makes the mode follow marginalia-mode when on and off
		)
	
	;; Addtional syntax highlighting for dired
	(use-package diredfl
		:hook (dired-mode . diredfl-mode)
		:config (set-face-attribute 'diredfl-dir-name nil :bold t))

	
	;; from https://bytemeta.vip/index.php/repo/alexluigit/emacs-grandview
	(defvar +font-size 140)
	(defvar +default-font "mononoki Nerd Font")
	(defvar +fixed-font "mononoki Nerd Font") ; for info
	(defvar +variable-font "Sarasa Mono SC"); variable-pitch font
	(defvar +CJK-font "LXGW WenKai Mono") ; Chinese, Japanese, Korean characters

;;;###autoload
	(defun +font-setup (&optional frame)
		"Setup default/fixed-pitch/variable-pitch/zh-font."
		(custom-theme-set-faces
		 'user
		 '(font-lock-keyword-face ((t (:slant italic)))); remember there's the color set here
		 '(font-lock-variable-name-face ((t (:weight demibold))))
		 '(font-lock-function-name-face ((t (:weight demibold))))
		 `(default ((t (:font ,(font-spec :family +default-font) :height ,+font-size))))
		 `(fixed-pitch ((t (:font ,(font-spec :family +fixed-font) :height ,+font-size))))
		 `(variable-pitch ((t (:font ,(font-spec :family +variable-font)
																 :height ,+font-size  )))))
		(dolist (charset '(kana han cjk-misc bopomofo))
			(set-fontset-font t charset (font-spec :family +CJK-font))))
	;; (set-face-attribute 'fixed-pitch nil :family "Hack" :height 1.0)
	;; TODO figure out how to fix this. The problem comes when fixed-width font is different from default font. I think it's 'cuz of the stars being a  different font or something?

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
	;; (setq +fixed-font "Sarasa Mono SC") ; I can't use this as org-indent-mode will fuck up 'cuz the size of this font is different from mononoki, creating fucked up line spacing and alignment in life.org. This fix didn't work either: https://github.com/jdtsmith/org-modern-indent/issues/1#issuecomment-1153065940
	(setq +fixed-font "mononoki Nerd Font") 
	(setq +variable-font "ETBembo")
	(setq +CJK-font "LXGW WenKai Mono")

	(+font-setup)

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


	(once '(:hooks after-init-hook)
		;; Make customisations that affect Emacs faces BEFORE loading a theme
		;; (any change needs a theme re-load to take effect).
		(use-package standard-themes
			:straight (:type git
											 :host github
											 :repo "protesilaos/standard-themes"))
		
		;; Read the doc string of each of those user options.  These are some
		;; sample values.
		(setq standard-themes-bold-constructs t
					standard-themes-italic-constructs t
					standard-themes-mixed-fonts nil
					standard-themes-variable-pitch-ui nil
					standard-themes-mode-line-accented t

					standard-themes-fringes nil

					;; The following accept lists of properties
					standard-themes-links '(neutral-underline faint)
					;; standard-themes-region '(no-extend neutral intense)
					standard-themes-prompts '(bold italic)
					
					;; ;; more complex alist to set weight, height, and optional
					;; ;; `variable-pitch' per heading level (t is for any level not
					;; ;; specified):
					standard-themes-headings
					'((t . (default 1)))
					)

		;; Disable all other themes to avoid awkward blending:
		(mapc #'disable-theme custom-enabled-themes)


		(set-face-attribute 'fixed-pitch nil :family "Hack" :height 1.0)
		(defun my-standard-themes-custom-faces ()
			"My customizations on top of the Standard themes.
This function is added to the `standard-themes-post-load-hook'."
			(set-background-color "#212121")
			)

		;; Using the hook lets our changes persist when we use the commands
		;; `standard-themes-toggle', `standard-themes-load-dark',
		;; `standard-themes-load-light'.
		(add-hook 'standard-themes-post-load-hook #'my-standard-themes-custom-faces)

		(standard-themes-load-dark)
		)

	(use-package minions
		:hook (after-init . minions-mode))

	(use-package doom-modeline
		:hook (after-init . doom-modeline-mode)
		:config
		(column-number-mode 1)
		(custom-set-faces
		 '(mode-line ((t ( :height 0.8))))
		 '(mode-line-active ((t ( :height 0.8)))) ; For 29+
		 '(mode-line-inactive ((t ( :height 0.8)))))
		;; If non-nil, cause imenu to see `doom-modeline' declarations.
		;; This is done by adjusting `lisp-imenu-generic-expression' to
		;; include support for finding `doom-modeline-def-*' forms.
		;; Must be set before loading doom-modeline.
		(setq doom-modeline-support-imenu t)
		(setq doom-modeline-height 1)
		;; (setq doom-modeline-project-detection 'auto)
		;; (setq doom-modeline-buffer-file-name-style 'auto)
		(setq doom-modeline-enable-word-count t)
		(setq doom-modeline-time-icon t)
		(setq doom-modeline-minor-modes t)

		;; ;; If non-nil, only display one number for checker information if applicable.
		;; (setq doom-modeline-checker-simple-format t)

		;; ;; Whether display the workspace name. Non-nil to display in the mode-line.
		;; (setq doom-modeline-workspace-name t)

		;; ;; Whether display the perspective name. Non-nil to display in the mode-line.
		;; (setq doom-modeline-persp-name t)

		;; ;; If non nil the default perspective name is displayed in the mode-line.
		;; (setq doom-modeline-display-default-persp-name nil)

		;; ;; If non nil the perspective name is displayed alongside a folder icon.
		;; (setq doom-modeline-persp-icon t)

		;; ;; Whether display the GitHub notifications. It requires `ghub' package.
		;; (setq doom-modeline-github nil)

		;; ;; The interval of checking GitHub.
		;; (setq doom-modeline-github-interval (* 30 60))

		;; ;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
		;; (setq doom-modeline-mu4e nil)
		;; ;; also enable the start of mu4e-alert
		;; (mu4e-alert-enable-mode-line-display)

		;; (setq doom-modeline-gnus t)
		(setq doom-modeline-gnus-timer -1)

		;; ;; Wheter groups should be excludede when gnus automatically being updated.
		;; (setq doom-modeline-gnus-excluded-groups '("dummy.group"))
		;; (setq doom-modeline-irc t) ; irc unread messages number 
		;; (setq doom-modeline-irc-stylize 'identity) ; convert some IRC buffers to their font-awesome icon

		;; ;; Change the executables to use for the language version string
		;; (setq doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
		;; (setq doom-modeline-env-ruby-executable "ruby")
		;; (setq doom-modeline-env-perl-executable "perl")
		;; (setq doom-modeline-env-go-executable "go")
		;; (setq doom-modeline-env-elixir-executable "iex")
		;; (setq doom-modeline-env-rust-executable "rustc")

		;; ;; What to display as the version while a new one is being loaded
		;; (setq doom-modeline-env-load-string "...")

		;; ;; By default, almost all segments are displayed only in the active window. To
		;; ;; display such segments in all windows, specify e.g.
		;; (setq doom-modeline-always-visible-segments '(mu4e irc))

		;; ;; Hooks that run before/after the modeline version string is updated
		;; (setq doom-modeline-before-update-env-hook nil)
		;; (setq doom-modeline-after-update-env-hook nil)
		)

	(use-package valign
		:hook (org-mode . valign-mode)
		:init (setq valign-fancy-bar t))
	
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
		:hook (org-mode . org-appear-mode)
		;; hook it with org-modern if possible, 'cuz I want to see everything with default prefs in life.org
		:config
		(setq org-appear-autoemphasis nil				;the only one that's on by default, like for /italic/, _underline_, +strikethrough+, etc.
					org-appear-autoentities t
					org-appear-autolinks nil
					org-appear-autosubmarkers t))
	

	)

(progn ; For Fun / Useless -----------------------------------------------------
	(use-package elcord ;; enables the "in emacs editing xxx" discord status, use "(elcord-mode)"
		:defer
		)
	
	)

;; Set gc threshold back to normal
;; if pauses are too long, decrease the threshold
;; if pauses are too frequent, increase the threshold
(setq gc-cons-threshold (* 128 1024 1024)) ; increase garbage collection limit to 100MiB, default is 0.8MB, measured in bytes


(progn ; Graveyard -------------------------------------------------------------
	;; this package.el stuff is just here 'cuz I'll definitely forget the structure of this if ever need be
	;; ;; This use-package.el code is kept to enable browsing of MELPA packages. It says package-archives is a void variable...
	;; (add-to-list
	;;  'package-archives
	;;  '("melpa" . "https://melpa.org/packages/")
	;;  t)

	;; when you wanna fix the font on apexless not being displayed probably 'cuz of dashboard, this might help: http://xahlee.info/emacs/emacs/emacs_list_and_set_font.html
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

	;; commented out 'cuz I like company-posframe more, and would just not save frames with burly.el but windows instead. I'm also horrified by the hardcoding of icons and the terrible border around the help doc
	;; (use-package company-box; sick company UI with icons and different colors for different backends;; - company-box, 
	;; 	:hook (company-mode . company-box-mode)
	;; 	:after company
	;; 	:config
	;; 	)

	
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
	;;   (if (+system-name? "DURIAN")
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

	)

(progn ; Doesn't work yet / To-test --------------------------------------------

	(use-package emms
		:defer
		:config
		(emms-minimalistic))
	
	(use-package ox-twbs ;; ox-html with more modern styling
		:after org-roam
		)

	(setq calendar-date-style 'iso) ;; YYYY/mm/dd
	(setq calendar-week-start-day 1)
	(setq calendar-time-display-form '(24-hours ":" minutes))
	(setq calendar-latitude 1.290270)
	(setq calendar-longitude 103.851959)
	(use-package org-gcal
		:defer
		:init
		(setq org-gcal-client-id "550288085404-br7cr31089v7ss1e45ocnonmv4d2ki2v.apps.googleusercontent.com"
					org-gcal-client-secret "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"
					org-gcal-file-alist '(("healtermon@gmail.com" .  "~/stuff/notes/calendars/healtermon-gmail.org")
																;; ("another-mail@gmail.com" .  "~/task.org")
																)))
	;; client id:550288085404-br7cr31089v7ss1e45ocnonmv4d2ki2v.apps.googleusercontent.com
	;; client secret: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
	;; calendar id:healtermon@gmail.com
	;; public URL to calendar:https://calendar.google.com/calendar/embed?src=healtermon%40gmail.com&ctz=Asia%2FSingapore
	;; public address in iCal format:https://calendar.google.com/calendar/ical/healtermon%40gmail.com/public/basic.ics


	;; #emacs@Libera.Chat <thuna`> for youtube specifically, elfeed + youtube-dl + mpv is pretty much all you need
	;; use this to get rss feed of a youtube channel:
	;; https://www.youtube.com/feeds/videos.xml?channel_id=<CHANNEL-ID>
	(use-package elfeed
		:defer)
	(use-package elfeed-tube
		:after elfeed)

	
	(use-package eat
		:disabled ; till Emacs 29 comes, it gives error "eat-exec: Invalid function: (window (get-buffer-window nil t))"
		:straight (:type git
										 :host codeberg
										 :repo "akib/emacs-eat"))

	;; for haskell setup, refer to https://github.com/patrickt/emacs#haskell
	(use-package haskell-mode
		:disabled)
	(use-package nix-haskell-mode
		:disabled ; enable for cabal projects and have a look
		:hook (haskell-mode . nix-haskell-mode)
		:after (nix haskell-mode))
	
	;; ;; (use-package conda)  ;; I don't use anaconda environments
	;; (use-package virtualenvwrapper)
	;; (use-package jupyter)
	;; (org-babel-do-load-languages
	;;  'org-babel-load-languages
	;;  '((emacs-lisp . t) ;; Other languages
	;;    (shell . t)
	;;    ;; Python & Jupyter
	;;    (python . t)
	;;    (jupyter . t)))
	;; (org-babel-jupyter-override-src-block "python")
	;; ;;(setq ob-async-no-async-languages-alist '("python" "jupyter-python")) ; if you use ob-async

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

	(use-package racket-mode
		:defer)
	
	(use-package julia-mode ;; for julia programming, julia-vterm, ob-julia-vterm and julia-mode. Alternatively, also check out julia-repl
		:mode "\\.jl\\'")

	)

;; custom-set stuff ------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 '("c3af4ee7a19412fb5a032ac287041171784abf23eb5e3107948388bc04ebc70b" "22c213e81a533c259127302ef1e0f2d1f332df83969a1f9cf6d5696cbe789543" "931ee45708e894d5233fc4a94ae0065c765c1a0aeb1bd8d9feee22f5622f44b4" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "e9d47d6d41e42a8313c81995a60b2af6588e9f01a1cf19ca42669a7ffd5c2fde" default))
 '(ignored-local-variable-values
	 '((cider-print-fn . "sicmutils.expression/expression->stream")))
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
 (+asses (custom-set-faces
					;; custom-set-faces was added by Custom.
					;; If you edit it by hand, you could mess it up, so be careful.
					;; Your init file should contain only one such instance.
					;; If there is more than one, they won't work right.
					'(default ((t (:family "mononoki NF" :foundry "outline"  :height 120 :width normal))))))
 (+durian (custom-set-faces
					 '(default ((t (:family "mononoki" :foundry "UKWN"   :height 151 :width normal))))))
 (+mango (custom-set-faces
					'(default ((t (:family "mononoki" :foundry "UKWN"   :height 113 :width normal))))))
 (+apexless (custom-set-faces ;;it's just here so Emacs doesn't randomly strew custom-set-faces over this file
						 '(default ((t ( :height 140 :foundry "nil" :family "mononoki Nerd Font"))))))
 )

;; TESTING GROUNDS -------------------------------------------------------------

(use-package burly
	:defer)

(setq tab-bar-show t)
(use-package tabspaces
  :straight (:type git :host github :repo "mclear-tools/tabspaces")
  :hook (after-init . +tabspace-setup)
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-default-tab "what")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*" "*messages*"))
  ;; sessions
  (tabspaces-session t)
  (tabspaces-session-auto-restore t)
	:init
;;;###autoload
	(defun +tabspace-setup ()
		"Set up tabspace at startup."
		;; Add *Messages* and *splash* to Tab `Home'
		(tabspaces-mode 1)
		(progn
			(tab-bar-rename-tab "Home")
			(when (get-buffer "*Messages*")
				(set-frame-parameter nil
														 'buffer-list
														 (cons (get-buffer "*Messages*")
																	 (frame-parameter nil 'buffer-list))))
			(when (get-buffer "*splash*")
				(set-frame-parameter nil
														 'buffer-list
														 (cons (get-buffer "*splash*")
																	 (frame-parameter nil 'buffer-list))))))

	:config
	;; Filter Buffers for Consult-Buffer
	(with-eval-after-load 'consult
		;; hide full buffer list (still available with "b" prefix)
		(consult-customize consult--source-buffer :hidden t :default nil)
		;; set consult-workspace buffer list
		(defvar consult--source-workspace
			(list :name     "Workspace Buffers"
						:narrow   ?w
						:history  'buffer-name-history
						:category 'buffer
						:state    #'consult--buffer-state
						:default  t
						:items    (lambda () (consult--buffer-query
																	:predicate #'tabspaces--local-buffer-p
																	:sort 'visibility
																	:as #'buffer-name)))

			"Set workspace buffer list for consult-buffer.")
		(add-to-list 'consult-buffer-sources 'consult--source-workspace))

	;; ya know this can turn tabspace integration off but not back on lmao
	(defun +consult-tabspaces ()
		"Deactivate isolated buffers when not using tabspaces."
		(require 'consult)
		(cond (tabspaces-mode
					 ;; hide full buffer list (still available with "b")
					 (consult-customize consult--source-buffer :hidden t :default nil)
					 (add-to-list 'consult-buffer-sources 'consult--source-workspace))
					(t
					 ;; reset consult-buffer to show all buffers 
					 (consult-customize consult--source-buffer :hidden nil :default t)
					 (setq consult-buffer-sources (remove #'consult--source-workspace consult-buffer-sources)))))

	(add-hook 'tabspaces-mode-hook #'+consult-tabspaces)

	)

(use-package hl-todo ;; highlight "TODO"s, jump between them and also a todo-occur
	;; copied from https://git.sjtu.edu.cn/sjtug/doom-emacs/-/blob/master/modules/ui/hl-todo/config.el
	:hook (prog-mode . hl-todo-mode)
  :config
	(setq hl-todo-highlight-punctuation ":"
				hl-todo-keyword-faces
				'(;; For reminders to change or add something at a later date.
					("TODO" warning bold)
					;; For code (or code paths) that are broken, unimplemented, or slow,
					;; and may become bigger problems later.
					("FIXME" error bold)
					;; For code that needs to be revisited later, either to upstream it,
					;; improve it, or address non-critical issues.
					("REVIEW" font-lock-keyword-face bold)
					;; For code smells where questionable practices are used
					;; intentionally, and/or is likely to break in a future update.
					("HACK" font-lock-constant-face bold)
					;; For sections of code that just gotta go, and will be gone soon.
					;; Specifically, this means the code is deprecated, not necessarily
					;; the feature it enables.
					("DEPRECATED" font-lock-doc-face bold)
					;; Extra keywords commonly found in the wild, whose meaning may vary
					;; from project to project.
					("NOTE" success bold)
					("BUG" error bold)
					("XXX" font-lock-constant-face bold))))


(use-package cider
	:defer)
;; flymake-kondor/flycheck-clj-kondo



