
;;; keybindings to remember
;; http://xahlee.info/emacs/emacs/ergoemacs_and_paredit.html
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
;; s = (qwerty)xah-fly-keys RET at point without moving cursor
;; SPC i j = open recent file

;;; Packages to look at or consider
;;;; Packages to maybe have a look at
;; - gamegrid
;; - narrow-indirect
;; - mixed-pitch
;; - ace-link
;; - ledger-mode & flycheck-ledger
;; - consult-flycheck & flycheck
;; - dwim-shell-command
;; - org-bookmark-heading
;; - org-visibility
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

;;;; Cool Packages to maybe have a look at
;; - org-noter, annotating pdf,epub with complete org files in the margin
;; - org-transclusion, live preview of parts of another org file via links
;; - org-contrib, additional org packages
;; - undo-fu & undo-fu-session, undo between sessions
;; - org-latex-impatient, preview as you type latex in org-mode
;; - forge, for working with git forges
;; - transmission
;; - sgml-mode, for working with html files
;; - emms
;; - proof-general, for working with proof assistants, targetd at intermediate to experts
;; - compile.el
;; - lingva, interface with google translate
;; - mw-thesaurus, merrian webster thesaurus usage, downloaded in org format
;; - spell-fu, spell checking without external dependencies (???)
;; - hunspell, more spellchecking
;; - org-ql, query language for org files. I don't know why you would want this when there's ripgrep, maybe a nicer ripgrep for org files? A: for org headlines and objects, a nice DSL.
;; - youdao-dictionary, an interface for the chinese-english/english-chinese online dictionary
;; - migemo, allowing you to isearch the romanisation of japanese instead of typing the japanese character itself
;; - DDSKK (DareDevil Simple Kana to Kanji conversion), if you wanna type japanese?
;; - ox-twbs, a modification to ox-html for a "modern style"
;; - log-interaction-mode for presenting
;; - centaur tabs, nice-looking tabs
;; - zotra, zotero translators without using zotera client
;; - sotlisp, speed elisp function typing and editing style
;; - blackout, an easier delight/diminish/dim, for changing both major and minor mode appearance in modeline
;; - GCMH, the Garbage Collector Magic Hack, changes GC threshold based on user activity

;;;; Cool packages that I want to install later on
;; - persp-mode, workspace manager
;; - dumb-jump, for when u don't have lsp and want to jump to definitions

;;; Intro to Config
;; This config is sorted in least to most likely to break... as I test stuff with my init.el and restart often.
;; I use hs-minor-mode(code-folding) and parentheses and outli-mode to sort and view this config, so if you don't use them, good luck!

;;; Function & Variable Definitions
(defun +system-name? (name-string)
  (string-equal system-name name-string))

;; This file supports a few computers
(defvar +apexless (and (eq system-type 'darwin)) "Whether Emacs is running on my macbook pro 14-inch m1 pro") ;; system-name Apexless/Apexless.local/???
(defvar +termux (and (+system-name? "localhost")) "Whether Emacs is running on termux (probably on my phone)")
(defvar +mango (and (+system-name? "mango")) "Whether Emacs is running on my linux desktop running NixOS")
(defvar +asses (and (+system-name? "ASSES-UX310UQK")) "Whether Emacs is running on ASSES-UX310UQK (my poly laptop)")
(defvar +durian (and (+system-name? "DURIAN")) "Whether Emacs is running on kor's poly laptop running Manjaro")
(defvar +nix-on-droid (+system-name? "nix-on-droid-placeholder-name") "Whether emacs is running on nix-on-droid")
(defvar +healtermon-gcal-file "~/stuff/notes/calendars/gcal.org" "healtermon@gmail.com main calendar") ;; i'll elogate the names if variety of files expands
(defvar +healtermon-gtasks-file "~/stuff/notes/tasks/gtasks.org" "healtermon@gmail.com \"My Tasks\" tasklist")



(when +apexless (load "~/.emacs.d/lisp/random-secrets"))

(setq init-file-debug nil)
(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

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
    (move-to-window-line nil) ;; Move cursor to middle line
    (if direction
        (recenter-top-bottom -1) ;; Current line becomes last
      (recenter-top-bottom 0))   ;; Current line becomes first
    (move-to-window-line opos))) ;; Restore cursor/point position
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

(defun +irc () ;; overwrites rcirc command, but I don't use rcirc anyways
  "Connect to IRC"
  (interactive)
  (circe "Libera Chat"))

(defun +execute-in-vterm (command)
  "Insert text in vterm and execute."
  (interactive)
  (require 'vterm)
  (let ((buf (current-buffer)))
    (unless (get-buffer vterm-buffer-name)
      (vterm))
    (display-buffer vterm-buffer-name t)
    (switch-to-buffer-other-window vterm-buffer-name)
    (vterm--goto-line -1)
    (message (concat "sending to vterm:\"" command "\""))
    (vterm-send-string command)
    (vterm-send-return)
    (switch-to-buffer-other-window buf))) 

;;; Default configs
(setq user-mail-address "healtermon@gmail.com")
(setq user-full-name "L.R.J, Samuel")

;; System locale to use for formatting time values.
(setq system-time-locale "C") ; Make sure that the weekdays in the time stamps of your Org mode files and agenda appear in English.


(setq use-dialog-box nil)

(setq-default
 fill-column 80                        ; Set width for automatic line breaks
 uniquify-buffer-name-style 'forward   ; Uniquify buffer names
 window-combination-resize t           ; Resize windows proportionally
 x-stretch-cursor t                    ; Stretch cursor to the glyph width
 indent-tabs-mode nil
 tab-width 2                          ; Tab width of 2 is compact and readable
 c-basic-offset 4                     ; but not in C
 )

;; scroll bar not useful as its behaviour is weird(too lazy to learn), and there's a percentage to show vertical position so...
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(menu-bar-mode -1)


(setq large-file-warning-threshold (* 128 1024 1024)) ;; 128 MebiBytes
(setq force-load-messages t)
(set-language-environment "UTF-8"); fixes the "haskell process has died" error somehow
(setq selection-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)   ; Default to utf-8 encoding

;; from https://bytemeta.vip/index.php/repo/alexluigit/emacs-grandview
(setq-default bidi-display-reordering 'left-to-right) ;; we don't use right-to-left languages/fonts, YET
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t) ;; we don't need the bidirectional parenthesis algorithm if we don't even read from right to left
(setq-default truncate-lines t)
(setq echo-keystrokes 0.25)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil) ;; Potentially speed up cursor operations: https://emacs.stackexchange.com/questions/28736
(setq scroll-step 1)
(setq scroll-margin 1)
(setq hscroll-step 1)
(setq hscroll-margin 1)
(setq scroll-preserve-screen-position 1)

;; enable some disabled commands
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)      ; actually moves left the on-screen words, scroll-right brings u back to column 0.


(setq backup-file-directory (file-truename "~/.emacs.d/backups/"))

;;Put all backups in one directory so emacs doesn't strew them
(setq backup-directory-alist `(("." . ,backup-file-directory)))

;;Put all autosave files like #filename.whatever# in the same directory
(setq auto-save-file-name-transforms `((".*" ,backup-file-directory t)))

(blink-cursor-mode -1)

(setq show-paren-delay 0.125)
(show-paren-mode 1)

;; set buffer to auto-update when the associated file is written to externally, and set it to update in 1s
;; (customize-set-variable 'auto-revert-interval 1)
(setq auto-revert-interval 10)
(global-auto-revert-mode 1)

(setq enable-recursive-minibuffers t) ; enables more than 1 minibuffer to be available at once
(minibuffer-depth-indicate-mode 1) ; shows [minibuffer-depth] at left of the echo area when depth >1

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

(cond (+apexless ;; no (toggle-frame-maximized) as you can't move or resize the window without undoing it, and no fullscreen 'cuz stupid notch
       (setq default-frame-alist
             (append ;; these parameters perfectly fit my screen, like (toggle-frame-maximized), gotten by (frame-height)+1 and (frame-width)
              '((top . 0)
                (left . 0)
                (width . 187)
                (height . 63))
              default-frame-alist)))
      (+mango nil)
      (t (toggle-frame-fullscreen)))


;;; Package Manager
;; Bootstrap `straight.el' package manager
;; I copied alexlugit's neatened-up bootstrap code version 6 at https://github.com/alexluigit/emacs-grandview/blob/master/init.el
(setq straight-use-package-by-default nil ; makes each use-package form also invoke straight.el to install the package, unless otherwise specified
      straight-vc-git-default-clone-depth 1
      straight-check-for-modifications '(check-on-save find-when-checking) ;; speeds up straight initialisation
      straight-repository-branch "develop"
      straight-hosts '((github "github.com" ".git")
                       (gitlab "gitlab.com" ".git")
                       (sourcehut "git.sr.ht" ".git") ; I still don't know how to get it to work
                       (bitbucket "bitbucket.com" ".git")
                       (codeberg "codeberg.org" ".git")))
(let ((bootstrap (locate-user-emacs-file "straight/repos/straight.el/bootstrap.el"))
      (script "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el")
      (file-name-handler-alist nil))
  ;; modified straight bootstrap code, initialise straight
  (unless (file-exists-p bootstrap)
    (with-current-buffer (url-retrieve-synchronously script 'silent 'inhibit-cookies)
      (goto-char (point-max)) (eval-print-last-sexp)))
  (load bootstrap nil 'nomessage)

  ;; notes on straight:
  ;; it's used like (straight-use-package '(name :keyword keywordval ...)) where name is the feature ; found by going into the code and seeing the name provided in the file by (provide 'name)
  )

;;; Configuration Macros
(straight-use-package 'leaf)
(leaf leaf
  :doc
  "Leaner, Better-Documented & Easier-To-Extend `use-package'. Philosophy is to be clear about everything, base package is minimalistic."
  "list of configs to refer to: https://github.com/conao3/leaf.el/issues/306"
  "it seems it is not clearer..."
  "using custom seems to load package at init time, not after deferring... thank god it's easy to change the block to setq"
  "you either use :defer-config or :config. :defer-config when there's no other thing activating deferring, and :config otherwise"
  "REMEMBER TO ADD :straight t. REMEMBER TO ADD :straight t. REMEMBER TO ADD :straight t. REMEMBER TO ADD :straight t"
  "comparison to use-package (that are not on github)
+ the :if  doesn't come after loading the package with :straight or :ensure like in use-package, it comes BEFORE so you don't have to wrap the whole use-package macro in a when when you want to conditionally add packages to load-path
- no keyword to customize variables after eval of package that are meant to be customized take less time to set if , but it'll do it at init time which takes a lot of time so nah...")
(leaf leaf-keywords
  :doc "provides more keywords for base leaf package for easier configuration"
  :preface
  ;; only after installing this can we make this neater
  (straight-use-package 'leaf-keywords)
  :config
  (leaf-keywords-init))

(leaf once
  :doc "more configuration macros, yay!"
  :straight (once :type git :host github :repo "emacs-magus/once"))

(leaf use-package
  :doc "macros to neaten configuration. I keep it around to slowly convert my init file and try others' code blocks"
  :straight t)
(leaf bind-key
  :doc "macros for binding keys, comes with use-package too"
  :straight t)
;;; Benchmarking
(leaf *benchmarking
  :config
  ;; must be put asap after use-package for most complete benchmark. Look at its functions named benchmark-init/...
  (leaf benchmark-init
    :straight t
    ;; To disable collection of benchmark data after init is done.
    :hook (after-init-hook . benchmark-init/deactivate))

  (leaf esup
    :straight t
    :config
    ;; Work around a bug where esup tries to step into the byte-compiled
    ;; version of `cl-lib', and fails horribly.
    (setq esup-depth 0)))

;;; Default config?
(leaf time
  :doc "display of time, date, load numbers, name of mail inbox with new mail, etc..."
  :unless +apexless ; apexless has time permanently displayed so you don't need this
  :straight (time :type built-in)
  :setq
  (display-time-default-load-average . nil) ; Don't display load average
  (display-time-24hr-format . t)          ; use hh:mm format instead
  :config
  (display-time-mode 1))

(leaf recentf
  :doc "recent files browsing feature"
  :straight (recentf :type built-in)
  
  :init
  ;; (once '(:before after-find-file) ;; 0.05s saved
  ;;   (setq recentf-max-saved-items 10000
  ;;         recentf-max-menu-items 10000)
  ;;   (recentf-mode 1))
  :config
  (setq recentf-max-saved-items 10000
        recentf-max-menu-items 10000)
  (recentf-mode 1) ;; 0.05s lag is worth it
  )

(leaf saveplace
  :doc "Remember and restore the last cursor location of opened files. 10/10 package."
  :straight (saveplace :type built-in)
  :init
  (setq save-place-forget-unreadable-files t)
  (save-place-mode 1)                   ;; the one-time 0.05s lag is worth it
  )

(leaf *history-setting
  :setq
  (history-length . 10000)
  (history-delete-duplicates . t))
(leaf savehist
  :doc "save minibuffer command history"
  :straight (savehist :type built-in)
  :require t
  :setq
  (savehist-save-minibuffer-history . t)
  :config
  (savehist-mode 1))





;;; essential packages for everyone
(leaf xah-fly-keys
  :doc "modal editing, efficient. Prob would have tried meow if I had known it first"
  :straight t
  :require t
  :bind ((global-map
          ("C-n" . next-line)
          ("C-v" . +scroll-half-page-down)
          ("M-v" . +scroll-half-page-up)
          ("C-a" . +move-beginning-of-line))
         (xah-fly-command-map
          ("n" . +xfk-command-mode-n)
          ("j" . +xfk-command-mode-j)
          ("l" . +xfk-command-mode-l)
          ("8" . er/expand-region)
          ("<SPC> 1 i" . crux-find-user-init-file)
          ("<SPC> 1 I" . (lambda () (interactive) (find-file (expand-file-name (concat user-emacs-directory "early-init.el")))))
          ("<SPC> 1 c" . (lambda () (interactive) (require 'calfw) (cfw:open-calendar)))
          ("<SPC> 1 h" . (lambda () (interactive) (dired "~/stuff/compro/healtermon/")))))
  :config
  ;; set-layout required before enabling
  (xah-fly-keys-set-layout (cond
                            ((or +asses +mango) 'colemak-mod-dh)
                            (t 'qwerty)))
  (xah-fly-keys 1)
  :preface
  (defun +move-beginning-of-line () "moves all the way to the start" (interactive) (move-beginning-of-line 1))
  (defun +xfk-command-mode-n ()
    "in dirvish-mode, does dirvish-narrow, otherwise isearch."
    (interactive)
    (cond ((string-equal major-mode "dirvish-mode") (dirvish-narrow))
          (t (isearch-forward))))
  (defun +xfk-command-mode-j ()
    "in dirvish-mode, does dired-up-directory, otherwise backwards-char"
    (interactive)
    (cond ((string-equal major-mode "dirvish-mode") (dired-up-directory))
          (t (backward-char))))
  (defun +xfk-command-mode-l ()
    "in dirvish-mode, does dired-up-directory, otherwise forwards-char"
    (interactive)
    (cond ((string-equal major-mode "dirvish-mode") (dired-find-file))
          (t (forward-char)))))
(leaf crux
  :doc "lots of random useful functions from the emacs Prelude 'distro'"
  :straight t)
(leaf restart-emacs ;;  to restart emacs, durr. Obsolete in emacs 29.
  :straight t)
(leaf puni
  :doc "leverages built-in features for structural editing, warning: not all-encompassing"
  :straight t
  ;; :require t                            ;; something is broken in either leaf or puni 'cuz every other package is fine with this... the autoloads don't seem to work
  :hook (prog-mode-hook
         eval-expression-minibuffer-setup-hook . puni-mode)
  :bind (puni-mode-map
         :package leaf ;; IDK why this works.. The difference between leaf-keys and bind-keys is, leaf-keys accepts a :package pkg1 pkg2 pk3... which chains `eval-after-load's for those packages before loading keybindings to the maps, while bind-keys will skip the `eval-after-load's. Henceforth if you want it to load immediately while deferring loading of the current package you give it some other already-loaded package, which is definitely leaf... Otherwise it can be nice to create complex criteria to load your keymaps, like here you can ":package (prog-mode whatever-loads-minibuffer)"
         ("C-<right>" . puni-slurp-forward)
         ("C-<left>" . puni-barf-forward)))
(leaf expand-region ; TODO: test against puni-expand-region and see which I like more, then rebind it in xah-fly-command-map
  :doc "a better expand-region than xah-fly-keys'"
  :straight t)
(leaf xah-find :straight t)

(leaf which-key :straight t :init (which-key-mode))



(leaf vertico
  :doc "a vertical autocomplete selection menu"
  :straight (vertico :files (:defaults "extensions/*"))
  :require t
  :bind ((:vertico-map
          ("M-DEL" . vertico-directory-delete-word)))
  :init
  (vertico-mode)
  (vertico-mouse-mode)
  :setq
  (vertico-resize . t)
  (vertico-cycle . t)
  ;; (vertico-count . ( (if +termux 10 20))) ;; ERROR! TODO: figure out how to conditionally set customize
  :defer-config
  (setq vertico-count (if +termux 10 20))
  )

(leaf orderless ;; a completion style
  :straight t
  :require t
  :setq
  (completion-styles . '(orderless basic))
  (completion-category-overrides . '((file (styles . (partial-completion)))))
  :setq (completion-category-defaults . nil)
  :defer-config
  ;; fix the dollar sign regex "$"
  (defun fix-dollar (args)
    (if (string-suffix-p "$" (car args))
        (list (format "%s[%c-%c]*$"
                      (substring (car args) 0 -1)
                      consult--tofu-char
                      (+ consult--tofu-char consult--tofu-range -1)))
      args))
  (advice-add #'orderless-regexp :filter-args #'fix-dollar)

  ;; for corfu fast style
  (with-eval-after-load 'corfu
    (defun orderless-fast-dispatch (word index total)
      (and (= index 0) (= total 1) (length< word 4)
           `(orderless-regexp . ,(concat "^" (regexp-quote word)))))
    (orderless-define-completion-style orderless-fast
      (orderless-style-dispatchers '(orderless-fast-dispatch))
      (orderless-matching-styles '(orderless-literal orderless-regexp)))
    (setq completion-styles '(orderless-fast))
    )
  )

(leaf marginalia ;; annotates the minibuffer like the margins in a book (look on the right side)
  :straight t
  :after vertico
  :bind (minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :require t
  :setq
  (marginalia-max-relative-age . 0)
  :config
  (marginalia-mode 1))

(leaf consult ;; provides _good shit_ versions of common commands and more
  :straight t
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)            ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)           ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop) ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)     ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)   ;; orig. goto-line
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ("M-g o" . consult-outline)     ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-g x" . consult-xref)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s m" . consult-multi-occur)
         ;; Misc
         ("<help> a" . consult-apropos)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap recentf-open-files] . consult-recent-file)
         ;; ([remap find-file] . consult-find) ;; I'm damn sure this is not a replacement
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         (isearch-mode-map
          :package leaf
          ("M-e" . consult-isearch-history)   ;; orig. isearch-edit-string
          ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
          ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
          ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
          )
         ;; Minibuffer history
         (minibuffer-local-map
          :package leaf
          ("M-s" . consult-history) ;; orig. next-matching-history-element
          ("M-r" . consult-history) ;; orig. previous-matching-history-element
          ))
  
  :init
  ;; CUSTOMISABLE
  (setq register-preview-delay 0.5)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)

  ;; NON-CUSTOMISABLE
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-function #'consult-register-format)
  
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  (fset 'multi-occur #'consult-multi-occur) ;; drop-in replacement.
  )
(leaf consult-dir
  :straight t
  :bind (("C-x C-d" . consult-dir)
         (vertico-map
          :package vertico
          ("C-x C-d" . consult-dir)
          ("C-x C-j" . consult-dir-jump-file))))

(use-package embark
  :straight t
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :bind (("C-." . embark-act)           ; like a right-click
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings))   ; like a left-click
  
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
                 (window-parameters (mode-line-format . none))))
  )
(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t        ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :straight t
  :defer)

(use-package helpful
  :straight t
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
  :straight t
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package vundo
  :straight t
  :defer
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)
  (set-face-attribute 'vundo-default nil :family "Unifont")
  )

(use-package visual-regexp ;; live highlighting of regexps, replacing replace-regexp w/ visual-regexp
  :straight t
  :defer
  :bind (([remap replace-regexp] . vr/replace)
         ([remap query-replace-regexp] . vr/query-replace)))
(use-package visual-regexp-steroids ;; enables changing regex backend
  :straight t
  :defer
  :config
  (setq vr/engine 'emacs))

;;; Elisp Programming
;; in Emacs, elisp programming is more important than other sorts of programming,
;; equivalent to whether the app settings work or not
;; suggestions: https://old.reddit.com/r/emacs/comments/zfwsc0/please_recommend_packages_for_editing_elisp/

;; (use-package paredit
;;   :hook ((emacs-lisp-mode
;;           lisp-interaction-mode
;;           ielm-mode
;;           lisp-mode
;;           eval-expression-minibuffer-setup
;;           scheme-mode
;;           clojure-mode
;;           cider-repl-mode) . paredit-mode)
;;   :bind (:map paredit-mode-map
;;               ("M-s" . nil)))

;; modern libraries, depended on by plenty of programs
(leaf dash :straight t)
(leaf f :straight t) 
(leaf s :straight t)

(leaf aggressive-indent
  :straight t
  :hook ((emacs-lisp-mode-hook
          lisp-mode-hook
          clojure-mode-hook) .  aggressive-indent-mode))

(leaf rainbow-delimiters
  :straight t
  :hook (prog-mode-hook . rainbow-delimiters-mode)
  :custom-face
  (rainbow-delimiters-depth-0-face . '((t (:foreground "dark orange"))))
  (rainbow-delimiters-depth-1-face . '((t (:foreground "light grey"))))
  (rainbow-delimiters-depth-2-face . '((t (:foreground "deep pink"))))
  (rainbow-delimiters-depth-3-face . '((t (:foreground "chartreuse"))))
  (rainbow-delimiters-depth-4-face . '((t (:foreground "deep sky blue"))))
  (rainbow-delimiters-depth-5-face . '((t (:foreground "yellow"))))
  (rainbow-delimiters-depth-6-face . '((t (:foreground "orchid"))))
  (rainbow-delimiters-depth-7-face . '((t (:foreground "spring green"))))
  (rainbow-delimiters-depth-8-face . '((t (:foreground "sienna1"))))
  (rainbow-delimiters-unmatched-face . '((t (:foreground "grey")))))

(leaf hideshow ; TODO: PROBABLY HARD: make hs-fold not fuck up outli's headers in the case like 2 headers stacked together
  :doc "code folding! yay!"
  :straight (hideshow :type built-in)
  :hook (prog-mode-hook . hs-minor-mode)
  :bind (prog-mode-map
         ("A-<tab>" . +fold-toggle)
         ("A-S-<tab>" . +fold-toggle-all))
  :init
  ;; make a command to toggle all hideshow, taken from hideshow.el top commentary
  (defvar +hs-hide nil "Current state of hideshow for toggling all.")
  (defun +toggle-hideshow-all ()
    (interactive)
    (setq +hs-hide (not +hs-hide))
    (if +hs-hide
        (hs-hide-all)
      (hs-show-all)))
  ;; copy of above, but for ts-fold
  (defvar +ts-fold-hide nil "Current state of hideshow for toggling all.")
  (defun +toggle-ts-fold-all ()
    (interactive)
    (setq +ts-fold-hide (not +ts-fold-hide))
    (if +ts-fold-hide
        (ts-fold-open-all)
      (ts-fold-close-all)))
  ;; taken from Doom Emacs
  (defun +fold--ts-fold-p ()
    (and (bound-and-true-p tree-sitter-mode)
         (featurep 'ts-fold)))
  ;; the 2 below commands rely on ts-fold, and falls back to hideshow when not available
  (defun +fold-toggle-all ()
    (interactive)
    (cond ((+fold--ts-fold-p) (+toggle-ts-fold-all))
          (t (+toggle-hideshow-all))))
  (defun +fold-toggle ()
    (interactive)
    (cond ((+fold--ts-fold-p) (ts-fold-toggle))
          (t (hs-toggle-hiding))))
  )

(use-package macrostep ;; macroexpand conveniently
  :straight t
  ;; if you wanna expand use-package macros, if there are no errors in the config, you can set use-package-expand-minimally to t to get a much more readable expansion
  :bind ( :map emacs-lisp-mode-map ("C-c e" . macrostep-mode)
          :map lisp-mode-map ("C-c e" . macrostep-mode))  )

(use-package ipretty ;; eval and pretty-print a sexp
  :straight t
  :init
  ;; global mode that advices `eval-print-last-sexp' to use ipretty-last-sexp instead
  (ipretty-mode))

(use-package eros ;; Show emacs-lisp eval results in an overlay, CIDER style.
  :straight t
  :init
  (eros-mode 1))



(leaf string-edit-at-point
  :doc "avoid escape nightmares by editing strings in a separate buffer"
  :straight t)
(leaf elisp-docstring-mode
  :doc "syntax highlighting for elisp docstrings, can use after calling string-edit on an elisp docstring"
  :straight (elisp-docstring-mode :type git :host github :repo "Fuco1/elisp-docstring-mode"))

;; highlighting! --------------------------------------------

(use-package highlight-defined          ; extra emacs lisp syntax highlighting
  :straight t
  :config
  (add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode))

;; (use-package highlight-quoted
;;   :config
;;   (add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode)
;;   (set-face-attribute 'highlight-quoted-symbol nil
;;                       :inherit 'font-lock-string-face)
;;  )

(leaf lisp-extra-font-lock ;; TODO: figure why user-defined variables don't get highlight. I'm using highlight-defined instead till then...
  :when nil ;; I value highlight-defined functionality more than quoted color
  :straight t
  :hook ((emacs-lisp-mode-hook) . lisp-extra-font-lock-mode))
(leaf morlock
  :straight t
  :defer-config
  (font-lock-add-keywords 'emacs-lisp-mode morlock-el-font-lock-keywords))

(leaf lispxmp
  :doc "Annotate value of lines containing ; => ."
  :straight t
  :init
  (setq byte-compile-warnings '(cl-functions)) ;make it not complain about using the depreciated cl.el instead of cl-lib
  )

(use-package highlight-symbol ; highlight all occurances of symbol at point in buffer
  :disabled                  ; "<f7> e e" binded in  xah-fly-keys also does this
  :straight t
  :hook (prog-mode . highlight-symbol-mode))

;;;; ELisp Debugging
;; see https://github.com/progfolio/.emacs.d/blob/master/init.org#debugging

"stop headliine from getting folded"
;;; Generally Useful
(use-package reveal-in-folder ;; Open Finder at location
  :straight t
  :if +apexless                         ; only works on macOS
  :defer)
(use-package terminal-here ;; Open location in external terminal
  :straight t
  :defer
  :config
  (setq terminal-here-mac-terminal-command 'iterm2)
  )
(use-package hl-todo ;; highlight "TODO"s, jump between them and also a todo-occur
  :straight t
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

(leaf free-keys ;; shows free keys in a buffer
  :straight (free-keys :type git
                       :host github
                       :repo "Fuco1/free-keys"))

;;; file manager

(leaf *dired-related
  :init
  (when +apexless
    (setq dired-use-ls-dired t)
    (setq insert-directory-program "/opt/homebrew/bin/gls")
    (setq mac-system-move-file-to-trash-use-finder t))
  (setq delete-by-moving-to-trash t) 
  (setq find-file-visit-truename t) ; follow symlinks when visiting files or directories
  (setq 
   ;; dired-do-revert-buffer t ;; update dir listing(s) after dired-do-something
   ;; Sensible mark behavior
   dired-mark-region t)
  )

(leaf dired
  :straight
  (dired     :type built-in)
  (dired-x   :type built-in)
  (dired-aux :type built-in)
  :config
  (setq dired-mouse-drag-files t)                   ; added in Emacs 29
  (setq mouse-drag-and-drop-region-cross-program t) ; added in Emacs 29
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-dwim-target t)
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  (setq dired-do-revert-buffer t)
  (setq dired-auto-revert-buffer #'dired-directory-changed-p)
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t)
  )
(leaf wdired
  :straight (wdired :type built-in)
  )
(with-eval-after-load 'dired-x
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..*$")))



(leaf dirvish
  :if +apexless
  :straight t
  :bind                ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   (dirvish-mode-map                    ; Dirvish inherits `dired-mode-map'
    ("a"   . dirvish-quick-access)
    ("f"   . dirvish-file-info-menu)
    ("y"   . dirvish-yank-menu)
    ("N"   . dirvish-narrow)
    ("^"   . dirvish-history-last)
    ("h"   . dirvish-history-jump)      ; remapped `describe-mode'
    ("s"   . dirvish-quicksort)         ; remapped `dired-sort-toggle-or-edit'
    ("v"   . dirvish-vc-menu)           ; remapped `dired-view-file'
    ("TAB" . dirvish-subtree-toggle)
    ("M-f" . dirvish-history-go-forward)
    ("M-b" . dirvish-history-go-backward)
    ("M-l" . dirvish-ls-switches-menu)
    ("M-m" . dirvish-mark-menu)
    ("M-t" . dirvish-layout-toggle)
    ("M-s" . dirvish-setup-menu)
    ("M-e" . dirvish-emerge-menu)
    ("M-j" . dirvish-fd-jump)
    ([mouse-1] . dirvish-subtree-toggle-or-open)
    ([mouse-2] . dired-mouse-find-file-other-window)
    ([mouse-3] . dired-mouse-find-file)))
  :init
  (once '(:hooks pre-command-hook)
    (dirvish-override-dired-mode))
  :setq 
  (dirvish-hide-details . t) ;; hide how dired shows the details on left of file/folder names
  (dirvish-reuse-session . nil)
  (dirvish-attributes . '(all-the-icons file-size collapse subtree-state vc-state git-msg))
  (dired-listing-switches . "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
  (dirvish-preview-dispatchers . (cl-substitute 'pdf-preface 'pdf dirvish-preview-dispatchers)) ;requires pdftoppm executable
  ;; Height
  ;; '(25 . 35) means
  ;;   - height in single window sessions is 25
  ;;   - height in full-frame sessions is 35
  (dirvish-header-line-height . '(25 . 35))
  (dirvish-mode-line-height . 15) ; 25 is shorthand for '(25 . 25), why isn't this option working?
  (dirvish-mode-line-format . '( :left (sort file-time " " file-size symlink)
                                 :right (omit yank index)))
  (dirvish-time-format-string . "%Y/%m/%d-%R")
  :defer-config  
  (dirvish-peek-mode) ;; shows preview minibuffer when scrolling through find-file minibuffer
  )

;;; editing on remote machines 
(leaf *remote-editing
  :doc "editing on remote machines"
  :config
  (leaf tramp
    :straight (tramp :type built-in)
    :require t
    :after dirvish
    :config
    ;; Some tips to speed up Dired/Dirvish over TRAMP
    (add-to-list 'tramp-connection-properties
                 (list (regexp-quote "/ssh:YOUR_HOSTNAME:")
                       "direct-async-process" t))
    (setq tramp-verbose 0)
    (setq tramp-auto-save-directory (locate-user-emacs-file "tramp/"))
    (setq tramp-chunksize 2000)
    (setq tramp-use-ssh-controlmaster-options nil)))
;;; Version-control-related 
(leaf magit ; an amazing front-end to git
  :straight t
  :bind ("C-x g" . magit-status))
(leaf magit-delta ;; syntax hightlighting with delta(command-line program) in magit diffs
  :straight t
  :hook (magit-mode-hook . magit-delta-mode))

(leaf diff-hl
  :doc "highlights diffs in the margins. If you want nicer ones like in vscode, https://ianyepan.github.io/posts/emacs-git-gutter/"
  :straight t
  :hook ((after-init-hook . global-diff-hl-mode)
         (magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
         (magit-post-refresh-hook . diff-hl-magit-post-refresh)))
;;; Templates/snippets 
(use-package tempel
  :straight t
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


(use-package yasnippet
  :straight t
  :defer)
;;; Notes/The Org Ecosystem 

(use-package org
  :straight t
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         )
  :hook (;; (org-mode . org-toggle-pretty-entities)
         (org-mode . visual-line-mode)
         ;; (org-mode . +org-font-setup)
         )
  
  :init
  (setq org-list-allow-alphabetical t)
  (setq org-return-follows-link t)
  (setq org-startup-folded 'content)
  
  :config
  ;; (setq org-hide-emphasis-markers t)
  (setq org-hide-leading-stars t)
  (setq org-log-done t)
  (setq org-startup-indented t)
  (when +apexless
    (setq org-latex-create-formula-image-program 'dvisvgm)
    (setq org-display-remote-inline-images 'cache) ;; https://www.fromkk.com/posts/preview-latex-in-org-mode-with-emacs-in-macos/
    ) 
  (setq org-image-actual-width nil)

  (setq org-agenda-files (list "~/stuff/notes/zk/life.org"
                               +healtermon-gcal-file
                               +healtermon-gtasks-file))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "ASAP(a)" "ONGOING(o)" "IFFREE(f)" "IFSUPERFREE(s)" "IFREALLYNOTHINGTODO(r)"
                    "|" "USELESSED(u)" "TOOLATE(l)" "CANCELLED(c)" "DONE(d)")))
  (setq org-capture-templates
        `(("t" "task" entry (file ,+healtermon-gtasks-file)
           "* TODO %?\n  SCHEDULED:\n  DEADLINE:\n")))
  (setq org-agenda-custom-commands
        '(("c" "To-dos of Noted Life"
           ((tags-todo "+health"         ((org-agenda-overriding-header "Health first~!")))
            (tags-todo "+job"            ((org-agenda-overriding-header "Job")))
            (tags-todo "+indep"          ((org-agenda-overriding-header "Independence(neat-to-have skills)")))
            (tags-todo "+physics"        ((org-agenda-overriding-header "Lifelong Dreams: Physics")))
            (tags-todo "+math-physics"   ((org-agenda-overriding-header "Lifelong Dreams: Mathematics(Calculus is so magical!)")))
            (tags-todo "+piano"          ((org-agenda-overriding-header "Lifelong Dreams: Piano/(?Music)"))))
           nil)
          ("z" "testing easy \"customization\""
           ((agenda "" nil)
            (todo      "TODO"
                       ((org-agenda-overriding-header "Physics")
                        (org-agenda-tag-filter-preset '("+physics"))))
            (tags-todo "+math-physics"
                       ((org-agenda-overriding-header "Mathematics")))
            (stuck     ""
                       ((org-agenda-overriding-header "what's stuck projects?"))))
           nil)
          ("A" "agenda -3d to +30d"
           ((agenda ""))
           (
            (org-agenda-overriding-header "-3d to +30d")
            (org-agenda-start-on-weekday nil)
            (org-agenda-span 33)
            (org-agenda-start-day "-3d")    
            ))
          ))
  )

(use-package org-contrib
  :straight t
  :after org)
(use-package org-download
  :straight t
  :after org
  :init
  (setq org-download-method 'directory)
  (setq-default org-download-image-dir "~/stuff/notes/zk/p/")
  )

(use-package org-roam
  :straight t
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-directory (file-truename (if +termux
                                              "/data/data/com.termux/files/home/storage/shared/stuff/notes/zk/"
                                            "~/stuff/notes/zk/")))
  (setq org-roam-dailies-directory "daily/")
  (when +termux (setq org-roam-database-connector 'sqlite3))
  
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
      :unnarrowed t)
     ("b" "book" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>.org"
                         "#+filetags: :book:\n#+title: ${title}\n"))))
  ;; (org-roam-dailies-capture-templates
  ;;  '(("d" "default" entry
  ;;     "%?"
  ;;     :if-new (file+head "%<%Y-%m-%d>.org"
  ;;                        "#+title: %<%Y-%m-%d>\n\n")
  ;;     :unarrowed t)))

  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n d" . org-roam-dailies-goto-date)
         ("C-c r r" . bms/org-roam-rg-search)
         (:map org-mode-map
               (("C-c n p" . org-roam-dailies-goto-previous-note)
                ("C-c n n" . org-roam-dailies-goto-next-note)
                ;; ("C-c n g" . org-roam-graph) ;; use org-roam-ui to generate the graph, it's vastly superior
                ("C-c n l" . org-roam-buffer-toggle)
                ("C-c n b" . org-roam-switch-to-buffer) ;not in v2 yet
                ("C-c n c" . org-id-get-create)
                ("C-c n i" . org-roam-node-insert)
                ("C-c n a" . org-roam-alias-add)
                ("C-c n r" . org-roam-alias-remove)
                ("C-c n t a" . org-roam-tag-add)
                ("C-c n t r" . org-roam-tag-remove)
                )))
  :config
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (directory-file-name
         (file-name-directory
          (file-relative-name (org-roam-node-file node) org-roam-directory)))
      (error "")))
  

  (use-package emacsql :straight t)
  (if +termux (use-package emacsql-sqlite3 :straight t) (use-package emacsql-sqlite :straight t))
  (use-package magit-section :straight t)
  (org-roam-db-autosync-mode) ;; need org-roam-sqlite-available-p to be true
  ;; (use-package consult-org-roam
  ;;   :config
  ;;   (consult-org-roam-mode 1))
  (leaf consult-org-roam
    :straight t
    :init
    (consult-org-roam-mode 1))
  
  (defun +org-roam-node-insert-immediate (arg &rest args) ;; was removed in v2, from https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/
    (interactive "P")
    (let ((args (cons arg args))
          (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                    '(:immediate-finish t)))))
      (apply #'org-roam-node-insert args)))
  (bind-keys :map org-mode-map ("C-c n I"  . +org-roam-node-insert-immediate))

  (use-package consult-org-roam ; keeping this around for live-preview when searching org-roam.
    :straight t
    :defer
    :custom
    (consult-org-roam-grep-func #'consult-ripgrep)
    ;; :config
    ;; ;; Eventually suppress previewing for certain functions
    ;; (consult-customize consult-org-roam-forward-links :preview-key (kbd "M-."))
    :bind
    (:map org-mode-map
          (("C-c n e" . consult-org-roam-file-find)
           ("C-c n b" . consult-org-roam-backlinks)))
    ))

;; org-roam-ui
(leaf websocket :straight t)
(leaf simple-httpd :straight t)
(use-package org-roam-ui
  :straight (org-roam-ui :host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after (org-roam websocket simple-httpd f)
  :defer
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))
;;; LaTeX related 
(use-package xenops ;; automatic live math preview that gets out of your way
  :hook ((latex-mode LaTeX-mode org-mode). xenops-mode)
  :config
  (setq xenops-reveal-on-entry t)
  (setq xenops-math-image-scale-factor 1.25)
  )

;; for tex info. The LaTeX lsp digestif's creator can't live without this
(add-to-list 'Info-directory-list "/usr/local/texlive/2022/texmf-dist/doc/info/")

(use-package tex
  :straight auctex ;; TODO fix trippy af auctex-tex declaration
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
;;                               :files ("*.el" "*.info" "dir"
;;                                       "doc" "etc" "images" "latex" "style")))
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
;;; Citations 
;; HOW TO USE: 
;; 1. let org-cite know the bib file, by "#+bibliography: path-to-your-file" or org-cite-global-bibliography which you already set
;; 2. put "#+cite_export: csl ieee.csl", where ieee can be whatever csl file in "~/Zotero/styles/" or a full path to a csl file
;; 3. put "#+PRINT_BIBLIOGRAPHY:" at wherever you want bibliography to be printed.
;; Further info: https://blog.tecosaur.com/tmio/2021-07-31-citations.html
(use-package oc                       ; org-cite is part of default org-mode
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
          (md . (csl "chicago-fullnote-bibliography.csl"))  ; Footnote reliant
          (latex . biblatex)                                ; For humanities
          (odt . (csl "chicago-fullnote-bibliography.csl")) ; Footnote reliant
          (t . (csl "modern-language-association.csl"))     ; Fallback
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
  (setq citar-at-point-function 'embark-act) ; changes citar-dwim to embark-act
  )

;;; Viewing & Editing pdf, epub, idk...  
;; for reading pdf, look out for image-roll.el when the bugs are fixed for continuous scrolling, and wait for a gif to see whether it allows preview-like scrolling
(use-package pdf-tools
  :if (or +mango +apexless)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  ;; (setq-default pdf-view-display-size 'fit-page)
  (setq-default pdf-view-use-scaling t)
  (setq pdf-view-resize-factor 1.1)
  :custom (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer) ;; for some reason it must be in customize, so eh
  ) 
(use-package pdf-view-restore ;; must be put below pdf-tools as it might load pdf-tools before any custom recipes are loaded
  :after pdf-tools
  :hook (pdf-view-mode . pdf-view-restore-mode)
  :init (setq pdf-view-restore-filename "~/.emacs.d/.pdf-view-restore"))

;; for reading epub, needs more config for epub to look nice
(use-package nov
  ;; check out https://chainsawriot.com/postmannheim/2022/12/22/aoe22.html for nov customisations
  :defer t
  :mode ("\\.epub\\'" . nov-mode)
  :init
  (setq nov-shr-rendering-functions '((img . nov-render-img)
                                      (title . nov-render-title)
                                      (b . shr-tag-b)))
  :config
  (message "nov loaded")
  )
;;; Shells & Terminals 
(use-package vterm
  :defer t                     ; package already has basic commands autoloaded
  :custom (vterm-install t)
  )
(use-package multi-vterm ;; default vterm allows only 1 buffer, this is to allow more
  :defer t
  :after vterm
  )

;; set shell-mode derivatives' indentation to 2
(setq sh-basic-offset 2
      sh-basic-indentation 2)

(use-package eshell
  ;; https://emacsconf.org/2022/talks/eshell/
  :straight (eshell :type built-in)
  :defer
  :init
  (setq eshell-history-size 10000)
  (setq eshell-hist-ignoredups 'erase
        eshell-error-if-no-glob t)
  :bind
  (:map eshell-mode-map
        ("C-l" . +eshell-delete-clear))
  :config
  (defun +eshell-delete-clear ()
    "deletes everything except the current prompt line, 
from https://www.n16f.net/blog/clearing-the-eshell-buffer/"
    (interactive)
    (let ((input (eshell-get-old-input)))
      (eshell/clear t)
      (eshell-emit-prompt)
      (insert input))))

(use-package eshell-syntax-highlighting
  :straight t
  :after esh-mode ; eshell-mode here doesn't work, the file where eshell-mode is defined is called esh.el. Coincidence? I think not.
  :config
  ;; Enable in all Eshell buffers.
  (eshell-syntax-highlighting-global-mode +1))

(use-package eshell-vterm
  :straight t
  :after eshell
  :config
  (eshell-vterm-mode))

;; PComplete stuff ------------------------------------------------------------
;; pcomplete is the completion-at-point when you press TAB in shell/eshell

(leaf pcmpl-args ; pcomplete extension pack; redefines a bunch of pcomplete terms
  :straight t
  :after pcomplete
  :require t)
(leaf fish-completion ;; adds to pcomplete suggestions via FISh
  :straight t
  :after pcomplete
  :init
  (when (and (executable-find "fish")
             (require 'fish-completion nil t))
    (global-fish-completion-mode))
  :config
  (setq fish-completion-fallback-on-bash-p t))
(use-package bash-completion ;; adds to pcomplete suggestions via BASh
  :defer)
;;; Completion-related... Idk what to name this 
(use-package dabbrev        ; Dynamic Abbrev
  ;; Tip: use Dabbrev with autocompletion globally! 

  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(leaf hippie-exp
  :straight (hippie-exp :type built-in)
  :setq
  ;; mess with ordering of this list as simple configuration
  (hippie-expand-try-functions-list
   .
   '(try-complete-file-name-partially
     try-complete-file-name
     try-expand-all-abbrevs
     try-expand-list try-expand-line
     try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-dabbrev-from-kill
     try-complete-lisp-symbol-partially
     try-complete-lisp-symbol))
  )

(leaf company
  :doc " CompAny = Complete Anything"
  :straight t
  :bind (company-active-map
         ("<return>" . nil)
         ("RET" . nil)
         ("M-<return>" . company-complete-selection)
         ("M-RET" . company-complete-selection)
         ("<tab>" . company-abort) ; TODO fix cdlatex and company working together, this may be tough
         ("TAB" . company-abort)
         )
  ;; :hook ((prog-mode) . company-mode)
  :defer-config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.0)            ; default is 0.2
  )

(use-package company-posframe ; use posframes for the popup, and also comes with icon support, plus backend-showing out-of-the-box
  ;;  DISCLAIMER: frame saving with burly saves the posframes, and of course, gives error when it tries to restore a #<buffer item>, "Invalid Syntax "#" "
  :straight t
  :when (posframe-workable-p)
  :after company
  :hook (company-mode . company-posframe-mode)
  ;; :config
  ;; ;; if you use desktop.el
  ;; (push '(company-posframe-mode . nil)
  ;;       desktop-minor-mode-table)
  )

(use-package corfu ;; adds a child frame for completion-at-point
  :if +apexless
  :straight (corfu :files (:defaults "extensions/*")
                   :includes (corfu-info corfu-history))
  :hook ((prog-mode . corfu-mode)
         (eshell-mode . (lambda () (setq-local corfu-quit-at-boundary t
                                          corfu-quit-no-match t
                                          corfu-auto nil)
                          (corfu-mode 1))))
  :bind ((:map corfu-map)
         ;; unfuck the mappings check corfu-mode-map & (defvar corfu-map ...) in corfu.el
         ([remap beginning-of-buffer] . nil)
         ([remap end-of-buffer] . nil)
         ([remap scroll-down-command] . nil)
         ([remap scroll-up-command] . nil)
         ([remap next-line] . nil)
         ([remap previous-line] . nil)
         ("RET" . nil)
         ("<return>" . nil)
         ;; my preferred mappings
         ("M-<return>" . corfu-insert)
         ("C-<return>" . corfu-insert)
         ("C-n" . corfu-next)    ; also can use up/down arrow keys
         ("C-p" . corfu-previous)
         ("M-n" . corfu-scroll-up)
         ("M-p" . corfu-scroll-down)) 
  :custom 
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t) ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (setq corfu-preselect 'valid) ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  (corfu-scroll-margin 5)        ;; Use scroll margin
  :init
  ;; (global-corfu-mode) ; enable corfu globally
  
  (defun +corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input)
                (eq (current-local-map) read-passwd-map))
      (setq-local corfu-auto t)        ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo
                  ;; corfu-popupinfo-delay nil ;; Disable automatic popup
                  )
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'+corfu-enable-always-in-minibuffer 1)
  :config
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  
  (setq corfu-auto-delay 0
        corfu-auto-prefix 1
        corfu-auto t
        )
  )

(use-package corfu-terminal ;; use popup/popon instead of childframes for GUI-less setup
  :straight t
  :after corfu
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))
(use-package kind-icon ;; Icons in corfu!
  :straight t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config 
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  )
(use-package cape ; adds capf backends and functions to deal with and convert from company backends
  :straight t
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init ;; Add `completion-at-point-functions', used by `completion-at-point'
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; (add-to-list 'completion-at-point-functions #'cape-history)
  ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-tex)
  ;; (add-to-list 'completion-at-point-functions #'cape-sgml)
  ;; (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-ispell)
  ;; (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;; (add-to-list 'completion-at-point-functions #'cape-line) ; this is too hard to use as the first selection
  ;; Example 1: Sanitize the `pcomplete-completions-at-point' Capf.
  ;; The Capf has undesired side effects on Emacs 28 and earlier. UNCOMMENT WHEN EMACS 29 AND SEE WHAT IT DOES
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
  )

;;; General Programming 
;; HOW TO USE: C-u extended-command devdocs to set new default docset to search, otherwise just search normally with command devdocs-lookup
(use-package devdocs
  :straight t
  :init
  (add-hook 'python-mode-hook
            (lambda () (setq-local devdocs-current-docs '("python~3.10"))))
  :bind ("C-h D" . devdocs-lookup))

(leaf rainbow-mode ;; colors hex colors
  :straight t
  :hook (prog-mode-hook . rainbow-mode))

(use-package tree-sitter              ; semantic structural knowledge of code
  :straight t
  :hook ( (agda-mode
           sh-mode
           c-mode
           caml-mode
           csharp-mode
           c++-mode
           d-mode
           css-mode
           elm-mode
           elixir-mode
           erlang-mode
           ess-r-mode
           go-mode
           haskell-mode
           hcl-mode
           terraform-mode
           html-mode
           mhtml-mode
           nix-mode
           java-mode
           javascript-mode
           js-mode
           js2-mode
           js3-mode
           json-mode
           jsonc-mode
           julia-mode
           lua-mode
           ocaml-mode
           perl-mode
           php-mode
           prisma-mode
           python-mode
           pygn-mode
           rjsx-mode
           ruby-mode
           rust-mode
           rustic-mode
           scala-mode
           swift-mode
           tuareg-mode
           typescript-mode
           verilog-mode
           yaml-mode
           zig-mode). tree-sitter-mode))
(use-package tree-sitter-langs        ; language pack for tree-sitter
  :straight t
  :after tree-sitter
  :hook (tree-sitter-after-on . tree-sitter-hl-mode))
(use-package ts-fold                  ; cold-folding with tree-sitter
  :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
  :after tree-sitter)

(leaf apheleia
  :straight t
  :doc "asynchronous code formatting"
  ;; :hook ((c-mode csharp-mode c++-mode) . apheleia-mode)
  ;; :init
  ;; (apheleia-global-mode +1) ;; commented out 'cuz I don't like python's black formatter; I like it to be more compact. So I got pycodestyle and flake8 to shut up
  )

(leaf topsy ;; show at top of window, the first line of top-level form
  :straight (topsy :type git :host github :repo "alphapapa/topsy.el")
  ;; :hook (prog-mode . topsy-mode)
  )

;;; Language Server Protocol(LSP)-related
;;;; Eglot-related
(leaf xref
  :straight t)
(leaf project
  :straight t
  :defer-config
  ;; from https://christiantietze.de/posts/2022/03/mark-local-project.el-directories/
  ;; make project.el recognise any directory with a .project file to be the project,
  ;; for rapid prototyping. Stolen from karthink's project-x package
  (defgroup project-local nil
    "Local, non-VC-backed project.el root directories."
    :group 'project)

  (defcustom project-local-identifier ".project"

    "You can specify a single filename or a list of names."
    :type '(choice (string :tag "Single file")
                   (repeat (string :tag "Filename")))
    :group 'project-local)
  (cl-defmethod project-root ((project (head local)))
    "Return root directory of current PROJECT."
    (cdr project))
  (defun project-local-try-local (dir)
    "Determine if DIR is a non-VC project.
DIR must include a file with the name determined by the
variable `project-local-identifier' to be considered a project."
    (if-let ((root (if (listp project-local-identifier)
                       (seq-some (lambda (n)
                                   (locate-dominating-file dir n))
                                 project-local-identifier)
                     (locate-dominating-file dir project-local-identifier))))
        (cons 'local root)))
  (customize-set-variable 'project-find-functions
                          (list #'project-try-vc
                                #'project-local-try-local)))
(leaf eldoc
  :defer-config
  (setq eldoc-echo-area-use-multiline-p nil) ; fucking stop using multiline echo area for your documentation, it's a screen-wide annoyance
  )
(leaf flymake
  :straight t
  :bind ((flymake-mode-map
          ("C-#" . flymake-goto-next-error)
          ("C-$" . flymake-goto-prev-error))))
(use-package eglot
  :hook ((python-mode c-mode c++-mode rust-mode nix-mode clojure-mode julia-mode
                      ;; LaTeX-mode
                      ) . eglot-ensure)
  :config
  (setq eglot-events-buffer-size 0) ;; In the name of speed, this stops eglot from logging the json events of lsp server
  (setq completion-category-overrides '((eglot (styles orderless-fast))))

  ;; ;; if you wanna have yasnippet completions show up while using eglot either corfu/company: https://stackoverflow.com/questions/72601990/how-to-show-suggestions-for-yasnippets-when-using-eglot
  ;; ;; for company
  ;; (add-hook 'eglot-managed-mode-hook (lambda ()
  ;;                                      (add-to-list 'company-backends
  ;;                                                   '(company-capf :with company-yasnippet))))
  ;; for corfu
  (straight-use-package 'cape)
  (defun +eglot-capf ()
    (setq-local completion-at-point-functions
                (add-to-list 'completion-at-point-functions
                             (cape-super-capf
                              #'eglot-completion-at-point
                              (cape-company-to-capf #'company-yasnippet)))))
  (add-hook 'eglot-managed-mode-hook #'+eglot-capf)

  ;; get pycodestyle to shut up
  (defun +python-eglot-config ()
    (setq-default eglot-workspace-configuration
                  '((:pylsp . ( :configurationSources ["flake8"]
                                :plugins ( :pycodestyle (:enabled nil)
                                           :mccabe (:enabled nil)
                                           :flake8 (:enabled nil)))))))
  (add-hook 'python-mode-hook #'+python-eglot-config)

  )
(use-package consult-eglot
  :after (consult eglot))
;;; Python Programming 
(use-package python
  ;; DON'T confuse this with python-mode.el, they are 2 different packages:
  ;; python.el is built-in and has better integration with emacs, while
  ;; python-mode.el is a mess in terms of fucntions to call.
  ;; Having both installed makes it very confusing.
  :straight
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq python-indent-offset 4)
  )
;;; Scheme Programming 
(use-package geiser-guile
  :defer t
  :commands geiser-guile); geiser-guile to connect to guile repl!

(use-package geiser-racket
  :defer t
  :commands geiser-racket); for racket if you download minimal racket you need to "raco pkg install compatibility-lib"

(use-package macrostep-geiser ; macrostep in geiser!
  :after geiser-mode
  :bind (:map geiser-mode-map ("C-c e" . macrostep-mode))
  :init
  (add-hook 'geiser-mode-hook #'macrostep-geiser-setup)
  (add-hook 'geiser-repl-mode-hook #'macrostep-geiser-setup)
  )

;;; Common Lisp Programming 
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
;;; Ruby Programming 
;; see professional setup: https://old.reddit.com/r/emacs/comments/xqojo7/emacs_and_rails/iqbh0id/
;;; Other-languages Programming 

(use-package matlab-mode
  :defer t)
(use-package fish-mode              ; fish shell scripting syntax highlighting
  :defer t) 

(leaf nix-mode
  :doc "for writing nix expressions"
  :when (or +mango +nix-on-droid +apexless)
  :straight t
  :defer-config
  (defun +rebuild-nix-config ()
    (interactive)
    (+execute-in-vterm
     "cd ~/stuff/compro/healtermon/nixconfig/ && ./result/sw/bin/darwin-rebuild switch --flake . --show-trace"))
  :bind (nix-mode-map
         :package nix-mode
         ("C-c C-c" . +rebuild-nix-config))
  )

(leaf guix
  :doc "interface for the guix package manager"
  :when (or +mango +durian)
  :straight t)

;;; Password-Manager 
(leaf bitwarden
  :straight (bitwarden :type git
                       :host github
                       :repo "seanfarley/emacs-bitwarden")
  :init
  (setq bitwarden-user user-mail-address)
  ;; (bitwarden-auth-source-enable) ;; don't need it (yet)
  )
;;; Communication Protocols 
(leaf elpher ; a gopher and gemini client, super simple to use
  :straight t)

(use-package mastodon ; mastodon client
  ;; not practical LMAO I'd rather use Mastonaut
  :straight t
  :defer 
  :init
  ;; change these whenever you wanna connect to another server
  (setq mastodon-instance-url "https://emacs.ch"
        mastodon-active-user "healtermon"))

(use-package ement ; matrix client and hence also IRC
  :straight (ement :type git
                   :host github
                   :repo "alphapapa/ement.el")
  :defer)

(use-package circe ; IRC Client; takes the lessons learnt from ERC and is more easily extensible, and has nicer documentation IMO. Also since it's simpler it's easier to undertand, though also very noob-unfriendly from experience (see below)
  ;; Q: honestly I still don't know how to login without using circe-network-options, unlike in ERC where they prompt you, circe doesn't seem to let you msg ppl?
  ;; A: well it's actually 'cuz "/msg NickServ IDENTIFY user pass" opens in another buffer, which if you didn't notice and typed the wrong password, makes it seem like nothing happened... So it is a beautiful client after all, separating all the chats :)
  :straight t
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
  :straight (erc :type built-in)
  :defer
  :config
  (setq erc-nick "healtermon")
  (setq erc-fill-column 90
        erc-fill-function 'erc-fill-static
        erc-fill-static-center 20)
  (setq erc-track-enable-keybindings t) ; enable C-c C-SPC to go to new messages

  )
(use-package erc-hl-nicks
  :straight t
  :after erc
  :config
  (add-to-list 'erc-modules 'hl-nicks))
(use-package erc-image
  :straight t
  :after erc
  :config
  (setq erc-image-inline-rescale 300)
  (add-to-list 'erc-modules 'image))



(leaf telega
  :doc "GOATed Telegram Client"
  :straight t
  :setq
  ;; has to be set before calling telega command, can be after loading telega.el
  (telega-server-libs-prefix . "/opt/homebrew/Cellar/tdlib/HEAD-d581e04/"))

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

;;; Prettifying Everything 
(add-hook 'prog-mode-hook #'global-prettify-symbols-mode) ; prettify some symbols in prog-mode derivatives

(leaf all-the-icons
  :doc "for dashboard & dirvish & citar, on first install, run all-the-icons-install-fonts"
  :straight t
  :defer-config (setq all-the-icons-scale-factor 1.0)
  )

(leaf all-the-icons-completion
  :doc "adds icons to minibuffer completion"
  :straight t
  :after (marginalia all-the-icons)
  :init (all-the-icons-completion-mode)
  :hook (marginalia-mode-hook . all-the-icons-completion-marginalia-setup) ; makes the mode follow marginalia-mode when on and off
  )

;; Addtional syntax highlighting for dired
(use-package diredfl
  :straight t
  :hook ((dired-mode-hook dirvish-directory-view-mode-hook) . diredfl-mode)
  :config (set-face-attribute 'diredfl-dir-name nil :bold t))


;; from https://bytemeta.vip/index.php/repo/alexluigit/emacs-grandview

;;;###autoload
(defun +font-setup (&optional frame)
  "Setup default/fixed-pitch/variable-pitch/zh-font."
  (custom-theme-set-faces
   'user
   '(font-lock-keyword-face ((t (:slant italic)))) ; remember there's the color set here
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


;; Make customisations that affect Emacs faces BEFORE loading a theme
;; (any change needs a theme re-load to take effect).
(once '(:hooks after-init-hook)
  (use-package standard-themes ;; MY FAVOURITE THEME, default-dark with "#212121" background, which is emacs-mac's default
    :straight (standard-themes :type git
                               :host github
                               :buffer read-only
                               :repo "protesilaos/standard-themes")
    :config
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
      (set-background-color "#212121"))

    ;; Using the hook lets our changes persist when we use the commands
    ;; `standard-themes-toggle', `standard-themes-load-dark',
    ;; `standard-themes-load-light'.
    (add-hook 'standard-themes-post-load-hook #'my-standard-themes-custom-faces)

    (standard-themes-load-dark)
    )
  )

(leaf minions
  :straight t
  :hook (after-init-hook . minions-mode))

(use-package doom-modeline
  :straight t
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
  ;; (setq doom-modeline-checker-simple-format t)
  ;; (setq doom-modeline-workspace-name t)
  ;; (setq doom-modeline-persp-name t)
  ;; (setq doom-modeline-display-default-persp-name nil)
  ;; (setq doom-modeline-persp-icon t)
  ;; (setq doom-modeline-github nil)
  ;; (setq doom-modeline-github-interval (* 30 60))
  ;; ;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
  ;; (setq doom-modeline-mu4e nil)
  ;; ;; also enable the start of mu4e-alert
  ;; (mu4e-alert-enable-mode-line-display)

  ;; (setq doom-modeline-gnus t)
  (setq doom-modeline-gnus-timer -1)

  ;; ;; Wheter groups should be excludede when gnus automatically being updated.
  ;; (setq doom-modeline-gnus-excluded-groups '("dummy.group"))
  (setq doom-modeline-irc nil)          ; irc unread messages number 
  ;; (setq doom-modeline-irc-stylize 'identity) ; convert some IRC buffers to their font-awesome icon

  ;; ;; Change the executables to use for the language version string
  ;; (setq doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'

  ;; (setq doom-modeline-env-load-string "...")

  ;; ;; By default, almost all segments are displayed only in the active window. To
  ;; ;; display such segments in all windows, specify e.g.
  ;; (setq doom-modeline-always-visible-segments '(mu4e irc))

  ;; ;; Hooks that run before/after the modeline version string is updated
  ;; (setq doom-modeline-before-update-env-hook nil)
  ;; (setq doom-modeline-after-update-env-hook nil)
  )

(use-package valign
  :straight t
  :hook (org-mode . valign-mode)
  :init (setq valign-fancy-bar t))

(use-package org-modern
  :straight t
  :defer t
  :hook
  (org-agenda-finalize . org-modern-agenda)
  ;; (org-mode . org-modern-mode)
  )

;; un-emphasize when cursor is on element
;; will fail to detect elements that are nested inside "certain other elements", like comments or document titles
(use-package org-appear
  :straight t
  :defer
  :after org
  :hook (org-mode . org-appear-mode)
  ;; hook it with org-modern if possible, 'cuz I want to see everything with default prefs in life.org
  :config
  (setq org-appear-autoemphasis nil ;the only one that's on by default, like for /italic/, _underline_, +strikethrough+, etc.
        org-appear-autoentities t
        org-appear-autolinks nil
        org-appear-autosubmarkers t))

(use-package org-sticky-header
  :straight t
  :defer
  ;; :hook (org-mode . org-sticky-header-mode)
  )



;;; For Fun / Useless / Miscellaneous 
(leaf elcord ;; enables the "in emacs editing xxx" discord status, use "(elcord-mode)"
  :straight t)

(leaf fsbot-data-browser ; fsbot is the IRC bot at #emacs@Libera.Chat
  :straight t)
;;; Graveyard 
;; this package.el stuff is just here 'cuz I'll definitely forget the structure of this if ever need be
;; ;; This use-package.el code is kept to enable browsing of MELPA packages. It says package-archives is a void variable...
;; (add-to-list
;;  'package-archives
;;  '("melpa" . "https://melpa.org/packages/")
;;  t)

;; (use-package dashboard ;;this package extends startup time from 145ms to 900ms as it loads org-mode, but it also loads org-roam so that's convenient.
;;  :if +apexless
;;   :init
;;   (setq dashboard-set-heading-icons t)
;;   (setq dashboard-set-file-icons t)
;;   (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
;;   ;; (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
;;   (setq dashboard-startup-banner "~/.emacs.d/emacs-dash.png")  ;; use custom image as banner
;;   (setq dashboard-center-content t) ;; set to 't' for centered content
;;   (setq dashboard-set-footer nil) ;; don't put random message below 
;;   (setq dashboard-items '((recents . 15)
;;                          (registers . 3)))
;;   :config
;;   (dashboard-setup-startup-hook)
;;   (dashboard-modify-heading-icons '((recents . "file-text")
;;                                    (bookmarks . "book"))))


;; commented out 'cuz I like company-posframe more, and would just not save frames with burly.el but windows instead. I'm also horrified by the hardcoding of icons and the terrible border around the help doc
;; (use-package company-box; sick company UI with icons and different colors for different backends;; - company-box, 
;;  :hook (company-mode . company-box-mode)
;;  :after company
;;  :config
;;  )

;; (use-package lsp-mode
;;  :commands (lsp lsp-deferred)
;;  :init
;;  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;  (setq lsp-keymap-prefix "C-c l")

;;  ;;for lsp to be faster
;;  (setq read-process-output-max (* 1024 1024)) ;; 1mb

;;  :hook (((c-mode
;;           c++-mode
;;           python-mode
;;           haskell-mode) . lsp-deferred)
;;         (lsp-mode . lsp-enable-which-key-integration))
;;  :config
;;  (setq lsp-idle-delay 0.1)
;;  (setq lsp-ui-doc-enable nil)
;;  (setq lsp-ui-doc-header t)
;;  (setq lsp-ui-doc-include-signature t)
;;  (setq lsp-ui-doc-border (face-foreground 'default))
;;  (setq lsp-ui-sideline-show-code-actions t)
;;  (setq lsp-ui-sideline-delay 0.05)
;;  )

;; (setq lsp-ui-doc-show-with-cursor t)
;; (setq lsp-ui-doc-show-with-mouse t)
;; (setq lsp-lens-enable t)
;; (setq lsp-completion-show-detail t)
;; (setq lsp-completion-show-kind t)
;; ;; (setq lsp-ui-sideline-show-diagnostics nil)

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
;;       (require 'lsp-pyright)
;;       (lsp-deferred)))
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


;; couldn't get it to work; I couldn't get straight to git pull pdf-scroll.el in pdf-roll branch of https://github.com/dalanicolai/pdf-tools
;; (use-package pdf-tools
;;  :straight (:type git :host github :repo "dalanicolai/pdf-tools" :branch "pdf-roll"
;;                   :files ("lisp/*.el"
;;                           "README"
;;                           ("build" "Makefile")
;;                           ("build" "server")
;;                           (:exclude "lisp/tablist.el" "lisp/tablist-filter.el")))
;;  :if (or +mango +apexless)
;;  :magic ("%PDF" . pdf-view-mode)
;;  :hook (pdf-view-mode . pdf-view-roll-minor-mode)
;;  :config
;;  (pdf-tools-install :noquery)
;;  ;; (setq-default pdf-view-display-size 'fit-page)
;;  (setq-default pdf-view-use-scaling t)
;;  (setq pdf-view-resize-factor 1.1)
;;  :custom (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer) ;; for some reason it must be in customize, so eh
;;  ) 
;; (use-package image-roll
;;  :straight (:type git
;;                   :host github
;;                   :repo "dalanicolai/image-roll.el")
;;  ;; :after pdf-tools
;;  ;; :init
;;  ;; (add-hook 'window-configuration-change-hook  #'image-roll-redisplay 0 t)
;;  ;; (add-to-list 'image-mode-new-window-functions 'image-roll-new-window-function)
;;  )


;; ;; NO! DON'T DO LITERATE PROGRAMMING WITHOUT LEARNING CLOJURE FIRST ARE YOU TRYING TO KILL YOURSELF?
;; (use-package  async)
;; (use-package ob-async)
;; (use-package ob-clojurescript)
;; (use-package org-babel-eval-in-repl)
;; (use-package eval-in-repl)
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((emacs-lisp . t) 
;;    (shell . t)
;;   (clojure . t)
;;   (clojurescript . t)
;;   ))
;; (setq org-babel-clojure-backend 'cider
;;      orb-babel-clojure-sync-nrepl-timeout nil)

;; ;; commented out 'cuz behaviour is too inconsistent between sessions, and maybe bugs 'cuz I restart emacs often.
;; (setq tab-bar-show t)
;; (use-package tabspaces
;;   :straight (:type git :host github :repo "mclear-tools/tabspaces")
;;   :hook (after-init . +tabspace-setup)
;;   :commands (tabspaces-switch-or-create-workspace
;;              tabspaces-open-or-create-project-and-workspace)
;;   :custom
;;   (tabspaces-default-tab "what")
;;   (tabspaces-remove-to-default t)
;;   (tabspaces-include-buffers '("*scratch*" "*messages*"))
;;   ;; sessions
;;   (tabspaces-session t)
;;   (tabspaces-session-auto-restore t)
;;  :init
;; ;;;###autoload
;;  (defun +tabspace-setup ()
;;    "Set up tabspace at startup."
;;    ;; Add *Messages* and *splash* to Tab `Home'
;;    (tabspaces-mode 1)
;;    (progn
;;      (tab-bar-rename-tab "Home")
;;      (when (get-buffer "*Messages*")
;;        (set-frame-parameter nil
;;                             'buffer-list
;;                             (cons (get-buffer "*Messages*")
;;                                   (frame-parameter nil 'buffer-list))))
;;      (when (get-buffer "*splash*")
;;        (set-frame-parameter nil
;;                             'buffer-list
;;                             (cons (get-buffer "*splash*")
;;                                   (frame-parameter nil 'buffer-list))))))

;;  :config
;;  ;; Filter Buffers for Consult-Buffer
;;  (with-eval-after-load 'consult
;;    ;; hide full buffer list (still available with "b" prefix)
;;    (consult-customize consult--source-buffer :hidden t :default nil)
;;    ;; set consult-workspace buffer list
;;    (defvar consult--source-workspace
;;      (list :name     "Workspace Buffers"
;;            :narrow   ?w
;;            :history  'buffer-name-history
;;            :category 'buffer
;;            :state    #'consult--buffer-state
;;            :default  t
;;            :items    (lambda () (consult--buffer-query
;;                                  :predicate #'tabspaces--local-buffer-p
;;                                  :sort 'visibility
;;                                  :as #'buffer-name)))

;;      "Set workspace buffer list for consult-buffer.")
;;    (add-to-list 'consult-buffer-sources 'consult--source-workspace))

;;  ;; ya know this can turn tabspace integration off but not back on lmao
;;  (defun +consult-tabspaces ()
;;    "Deactivate isolated buffers when not using tabspaces."
;;    (require 'consult)
;;    (cond (tabspaces-mode
;;           ;; hide full buffer list (still available with "b")
;;           (consult-customize consult--source-buffer :hidden t :default nil)
;;           (add-to-list 'consult-buffer-sources 'consult--source-workspace))
;;          (t
;;           ;; reset consult-buffer to show all buffers 
;;           (consult-customize consult--source-buffer :hidden nil :default t)
;;           (setq consult-buffer-sources (remove #'consult--source-workspace consult-buffer-sources)))))

;;  (add-hook 'tabspaces-mode-hook #'+consult-tabspaces)

;;  )


;; ;; well, not using it ATM 'cuz eyebrowse-restore-mode keeps telling me while using eyebrowse-restore-save-all the emacs.d/eyebrowse-directory isn't a file
;; (set-frame-parameter nil 'name "Main")
;; (use-package eyebrowse
;;   :hook (after-init . eyebrowse-mode))
;; (use-package eyebrowse-restore
;;   :straight (:type git :host github :repo "FrostyX/eyebrowse-restore")
;;   :after eyebrowse
;;   :config (eyebrowse-restore-mode))

(use-package org-gtasks ; sync google tasks, probably won't use it as google tasks don't support scheduling of tasks, only deadline
  :defer
  :straight (:type git :host github :repo "JulienMasson/org-gtasks")
  :config
  (org-gtasks-register-account :name "S L"
                               :directory "~/stuff/notes/tasks/"
                               :client-id +gclient-id
                               :client-secret +gclient-secret))

;; takes too long to load, idk what to do with it either
;; (use-package emojify :config (global-emojify-mode))

;; ;; once I figured out what this is, I didnt' want it anymore
;; (use-package consult-company ; uses consult minibuffer thingy instead of company popup
;;   :after (consult company)
;;   :config
;;   (define-key company-mode-map [remap completion-at-point] #'consult-company)
;;   )
"stop hs-mode from folding the header"
;;; Doesn't work yet / To-test 
(use-package emms
  :straight t
  :defer
  :config
  (emms-minimalistic))

(use-package ox-twbs ;; ox-html with more modern styling
  :straight t
  :after org-roam
  )

;; #emacs@Libera.Chat <thuna`> for youtube specifically, elfeed + youtube-dl + mpv is pretty much all you need
;; use this to get rss feed of a youtube channel:
;; https://www.youtube.com/feeds/videos.xml?channel_id=<CHANNEL-ID>
(leaf elfeed
  :straight t)
(leaf elfeed-tube
  :straight t
  :after elfeed)

;; for haskell setup, refer to https://github.com/patrickt/emacs#haskell
(use-package haskell-mode
  :straight t
  :defer
  ;; from https://github.com/patrickt/emacs/blob/master/readme.org
  ;; :bind (:map haskell-mode-map
  ;;             ("C-c a c" . haskell-cabal-visit-file)
  ;;             ("C-c a i" . haskell-navigate-imports)
  ;;             ("C-c m"   . haskell-compile)
  ;;             ("C-c a I" . haskell-navigate-imports-return)
  ;;             :map haskell-cabal-mode-map
  ;;             ("C-c m"   . haskell-compile)))
  )
(use-package nix-haskell-mode
  :straight t
  :disabled                          ; enable for cabal projects and have a look
  :after (nix-mode haskell-mode)
  :hook (haskell-mode . nix-haskell-mode))

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

(use-package pyenv                      ; what does this do?
  :straight t
  :defer t)
(use-package anaconda-mode
  :straight t
  :bind (("C-c C-x" . next-error))
  :hook (python-mode . anaconda-mode))


(use-package company-anaconda           ; anaconda backend for company-mode
  :straight t
  :after company
  :config
  (add-to-list 'company-backends '(company-anaconda :with company-capf)))

(use-package pyimport ;; manage python imports from emacs! pyimport-insert-missing requires another buffer open with an example of importing the missing library
  :straight t
  :after python-mode)

;; https://old.reddit.com/r/emacs/comments/x6rg1u/rust_with_emacs/inb9qka/
(use-package rust-mode
  :straight t
  :defer
  :hook (rust-mode . cargo-minor-mode)
  :config
  (setq rust-format-on-save t)
  :custom-face
  (rust-question-mark-face ((t (:inherit font-lock-builtin-face :foreground "#ff0000" :weight bold)))))
(leaf rustic
  :when nil ;; in the name of speed
  :straight t
  ;; from https://github.com/patrickt/emacs/blob/master/readme.org
  ;; :bind (:map rustic-mode-map
  ;;             ("C-c a t" . rustic-cargo-current-test)
  ;;             ("C-c m" . rustic-compile))
  )
(use-package cargo
  :straight t
  :defer)

(use-package racket-mode
  :straight t
  :defer)

(use-package geiser-mit   ; idk I can't get MIT-Scheme repl to connect to geiser
  :straight t
  :defer
  :commands geiser-mit)

;; I feel like clojure LSP doesn't work the way I want it to yet, it doesn't show me the errors linted even right after lsp-bridge-goto-next-error or whatever it was
(defun +load-lsp-bridge () ;; call only after loading org-mode because otherwise org-list-allow-alphabetical bugs out... with-eval-after-load init-hook or org-mode doesn't even work :(. I found it has to do with some buffer-local variable!
  (interactive)
  (use-package posframe)
  (use-package markdown-mode)
  (use-package yasnippet
    :config
    (yas-global-mode 1))
  (add-to-list 'load-path "~/stuff/compro/manateelazycat/lsp-bridge")
  (require 'lsp-bridge)
  (global-lsp-bridge-mode))

(leaf kotlin-mode
  :straight t
  )
;; currently broken due to sly's bug when describe-mode
(leaf mode-minder ; look at major-mode hierarchy
  :straight (mode-minder :type git :host github :repo "jdtsmith/mode-minder"))

(leaf lua-mode
  :straight t)

(leaf go-mode
  :straight t)
(leaf go-snippets
  :straight t
  :after (go-mode yasnippet))
(use-package gotest
  :straight t
  :defer
  ;; from https://github.com/patrickt/emacs/blob/master/readme.org
  ;; :bind (:map go-mode-map
  ;;             ("C-c a t" . #'go-test-current-test)
  ;;             ("C-c a T" . #'go-test-current-file)
  ;;             ("C-c a i" . #'go-import-add))
  )

(use-package markdown-mode
  :straight t
  :mode (("README\\.md\\'" . gfm-mode))
  :hook (gfm-mode . visual-line-mode)
  :init
  (setq markdown-command "multimarkdown")
  )
(leaf toml-mode
  :straight t)
(leaf yaml-mode
  :straight t)

(leaf dyalog-mode
  :doc "APL hahahahaha"
  :straight t)

(leaf dockerfile-mode
  :straight t)
(leaf dhall-mode
  :straight t)

;; ;; commented out 'cuz it takes so long to load wtf
;; (leaf elm-mode
;;   :straight t
;;   :hook ((elm-mode-hook . elm-format-on-save-mode) ; requires elm-format to be installed(outside of emacs)
;;          (elm-mode-hook . elm-indent-mode)))

(leaf elixir-mode
  :straight t)



;;; custom-set stuff
(cond
 (+asses    (custom-set-faces
             ;; custom-set-faces was added by Custom.
             ;; If you edit it by hand, you could mess it up, so be careful.
             ;; Your init file should contain only one such instance.
             ;; If there is more than one, they won't work right.
             '(default ((t (:family "mononoki NF" :foundry "outline"  :height 120 :width normal))))))
 (+durian   (custom-set-faces
             '(default ((t (:family "mononoki"    :foundry "UKWN"   :height 151 :width normal))))))
 (+mango    (custom-set-faces
             '(default ((t (:family "mononoki"    :foundry "UKWN"   :height 113 :width normal))))))
 (+apexless (custom-set-faces ;;it's just here so Emacs doesn't randomly strew custom-set-faces over this file
             '(default ((t (:family "mononoki Nerd Font" :foundry "nil"  :height 140))))))
 ) 

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c3af4ee7a19412fb5a032ac287041171784abf23eb5e3107948388bc04ebc70b" "22c213e81a533c259127302ef1e0f2d1f332df83969a1f9cf6d5696cbe789543" "931ee45708e894d5233fc4a94ae0065c765c1a0aeb1bd8d9feee22f5622f44b4" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "e9d47d6d41e42a8313c81995a60b2af6588e9f01a1cf19ca42669a7ffd5c2fde" default))
 '(ignored-local-variable-values
   '((cider-print-fn . "sicmutils.expression/expression->stream")))
 '(org-roam-dailies-capture-templates
   '(("d" "default" entry "* %?" :target
      (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>
")
      :prepend t))))

;;; TESTING GROUNDS

(leaf burly ;; bookmark window or frame configurations
  :straight t)

(leaf org-pdftools ;; for links to specific pages in a PDF
  :straight t  )

(leaf calendar
  :straight (calendar :type built-in)
  :setq
  (calendar-date-style . 'iso) ;; YYYY/mm/dd
  (calendar-week-start-day . 1))
(setq calendar-time-display-form '(24-hours ":" minutes))
(setq calendar-latitude 1.290270)
(setq calendar-longitude 103.851959)

(use-package org-gcal ;; sync google calendar events
  :straight t
  :defer
  :init
  (setq org-gcal-down-days 60
        org-gcal-up-days 300
        org-gcal-client-id +gclient-id
        org-gcal-client-secret +gclient-secret
        org-gcal-file-alist `(("healtermon@gmail.com" .  ,+healtermon-gcal-file)
                              ;; ("another-mail@gmail.com" .  "~/more-mail.org")
                              )))
;; calendar id:healtermon@gmail.com
;; public URL to calendar:https://calendar.google.com/calendar/embed?src=healtermon%40gmail.com&ctz=Asia%2FSingapore
;; public address in iCal format:https://calendar.google.com/calendar/ical/healtermon%40gmail.com/public/basic.ics

(use-package calfw ;; calendar framework
  :straight t
  :commands (cfw:open-calendar-buffer)
  :config
  ;; better frame for calendar, copied from doom config
  (setq cfw:face-item-separator-color nil
        cfw:render-line-breaker 'cfw:render-line-breaker-none
        cfw:fchar-junction ?╋
        cfw:fchar-vertical-line ?┃
        cfw:fchar-horizontal-line ?━
        cfw:fchar-left-junction ?┣
        cfw:fchar-right-junction ?┫
        cfw:fchar-top-junction ?┯
        cfw:fchar-top-left-corner ?┏
        cfw:fchar-top-right-corner ?┓)

  ;; Please evaluate cfw:org-capture-template before requiring calfw-org.
  (setq cfw:org-capture-template
        '( "c"  "calfw2org" entry
           (file +healtermon-gcal-file )
           "* %?\n %(cfw:org-capture-day)"))

  (use-package calfw-org)

  (defun cfw:open-calendar ()
    (interactive)
    (let ((cp
           (cfw:create-calendar-component-buffer
            :view 'month
            :contents-sources
            (list
             (cfw:org-create-file-source
              "main"  +healtermon-gcal-file  "#268bd2")
             (cfw:org-create-file-source
              "tasks"  +healtermon-gtasks-file  "#859900")
             ))))
      (switch-to-buffer (cfw:cp-get-buffer cp))))
  )

(use-package org-hyperscheduler
  :straight (org-hyperscheduler :type git :host github :repo "dmitrym0/org-hyperscheduler" :files ("*"))
  :defer
  )
(defun org-agenda-add-time-grid-maybe (list ndays todayp)
  "Add a time-grid for agenda items which need it.

LIST is the list of agenda items formatted by `org-agenda-list'.
NDAYS is the span of the current agenda view.
TODAYP is t when the current agenda view is on today."

  (catch 'exit
    (cond ((not org-agenda-use-time-grid) (throw 'exit list))
          ((and todayp (member 'today (car org-agenda-time-grid))))
          ((and (= ndays 1) (member 'daily (car org-agenda-time-grid))))
          ((member 'weekly (car org-agenda-time-grid)))
          (t (throw 'exit list)))
    (let* ((blocks (mapcar (lambda (x)
                             (let ((start (get-text-property 1 'time-of-day x))
                                   (dur (get-text-property 1 'duration x)))
                               (cond
                                ((and start dur) (cons start
                                                       (org-time-from-minutes
                                                        (truncate
                                                         (+ dur (org-time-to-minutes start))))))
                                (start start)
                                (t nil))))
                           list))
           (have (delq nil (mapcar
                            (lambda (x) (get-text-property 1 'time-of-day x))
                            list)))
           (string (nth 3 org-agenda-time-grid))
           (gridtimes (nth 1 org-agenda-time-grid))
           (req (car org-agenda-time-grid))
           (remove (member 'remove-match req))
           new time)
      (if (and (member 'require-timed req) (not have))
          ;; don't show empty grid
          (throw 'exit list))

      (while (setq time (pop gridtimes))
        (unless (and remove (member time have))
          (let* ((windows (delq nil blocks))
                 (hit nil))
            (dolist (busy windows)
              (unless hit
                (when (and (>= time (car busy))
                           (< time (cdr busy)))
                  (setq hit t))))
            (setq time (replace-regexp-in-string " " "0" (format "%04s" time)))
            (if hit
                (progn
                  (push (org-agenda-format-item
                         (concat string " dito") string nil "" nil
                         (concat (substring time 0 -2) ":" (substring time -2)))
                        new)
                  (put-text-property 2 (length (car new)) 'face 'org-archived (car new)))
              (progn
                (push (org-agenda-format-item
                       nil string nil "" nil
                       (concat (substring time 0 -2) ":" (substring time -2)))
                      new)
                (put-text-property 2 (length (car new)) 'face 'org-time-grid (car new))))
            (setq hit nil))))

      (when (and todayp org-agenda-show-current-time-in-grid)
        (push (org-agenda-format-item
               nil org-agenda-current-time-string nil "" nil
               (format-time-string "%H:%M "))
              new)
        (put-text-property
         2 (length (car new)) 'face 'org-agenda-current-time (car new)))

      (if (member 'time-up org-agenda-sorting-strategy-selected)
          (append new list)
        (append list new)))))
(defun org-time-to-minutes (time)
  "Convert an HHMM TIME to minutes."
  (+ (* (/ time 100) 60) (% time 100)))
(defun org-time-from-minutes (minutes)
  "Convert a number of MINUTES to an HHMM time."
  (+ (* (/ minutes 60) 100) (% minutes 60)))


(use-package forge ;; for working with git forges
  :straight t
  :defer)
(use-package org-contacts
  :straight t
  :defer)


;;; Julia Programming 
(use-package julia-mode ; for julia programming, julia-vterm, ob-julia-vterm and julia-mode. Alternatively, also check out julia-repl
  :straight t
  :mode "\\.jl\\'"
  :interpreter ("julia" . julia-mode)
  :init
  (setenv "JULIA_NUM_THREADS" "auto") ;; default is 1
  
  :config
  ;; Borrow matlab.el's fontification of math operators. From
  ;; <https://web.archive.org/web/20170326183805/https://ogbe.net/emacsconfig.html>
  (dolist (mode '(julia-mode ess-julia-mode))
    (font-lock-add-keywords
     mode
     `((,(let ((OR "\\|"))
           (concat "\\("                ; stolen `matlab.el' operators first
                   ;; `:` defines a symbol in Julia and must not be highlighted
                   ;; as an operator. The only operators that start with `:` are
                   ;; `:<` and `::`. This must be defined before `<`.
                   "[:<]:" OR
                   "[<>]=?" OR
                   "\\.[/*^']" OR
                   "===" OR
                   "==" OR
                   "=>" OR
                   "\\<xor\\>" OR
                   "[-+*\\/^&|$]=?" OR ; this has to come before next (updating operators)
                   "[-^&|*+\\/~]" OR
                   ;; Julia variables and names can have `!`. Thus, `!` must be
                   ;; highlighted as a single operator only in some
                   ;; circumstances. However, full support can only be
                   ;; implemented by a full parser. Thus, here, we will handle
                   ;; only the simple cases.
                   "[[:space:]]!=?=?" OR "^!=?=?" OR
                   ;; The other math operators that starts with `!`.
                   ;; more extra julia operators follow
                   "[%$]" OR
                   ;; bitwise operators
                   ">>>" OR ">>" OR "<<" OR
                   ">>>=" OR ">>" OR "<<" OR
                   "\\)"))
        1 font-lock-type-face))))
  )

(leaf julia-snail
  :straight t
  :hook (julia-mode . julia-snail-mode))

(leaf eglot-jl
  :straight t
  :after (eglot julia-mode)
  :hook (julia-mode . eglot-jl-init)
  :init
  ;; Prevent timeout while installing LanguageServer.jl
  (add-hook 'julia-mode-hook (lambda () (setq eglot-connect-timeout (max eglot-connect-timeout 120))))
  ;; :config
  ;; (setq eglot-jl-language-server-project eglot-jl-base)
  )

;;; Clojure Programming 
(use-package cider
  :straight t
  :defer
  :config
  (setq cider-repl-display-help-banner t))

(use-package macrostep-geiser           ; macrostep in CIDER!
  :straight t
  :after cider-mode
  :bind (:map cider-mode-map ("C-c e" . macrostep-mode))
  :init
  (add-hook 'cider-mode-hook #'macrostep-geiser-setup))

(use-package kibit-helper ; uses Clojure's core.logic to find functions in standard library that are abbreviations of your code
  :straight t
  :defer)

(use-package clj-refactor
  ;; try cljr-add-missing-libspec!
  :straight t  
  :defer
  :config
  (defun +my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1)                ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    ;; that's ok 'cuz we have macroexpand-mode anyways
    (cljr-add-keybindings-with-prefix "C-c C-m"))

  (add-hook 'clojure-mode-hook #'+my-clojure-mode-hook))
;;; C++/C#/C Programming
;; for c-mode, to get that workflow flowing
(defun +compile-and-execute-in-vterm ()
  (interactive)
  (+execute-in-vterm
   "cd ~/stuff/compro/healtermon/sudoku-ncurses/ && make && ./sudoku-ncurses"))


(add-hook 'c-mode-common-hook
          (lambda () ;; But c-mode-base-map is only defined after cc-mode is loaded(only after visiting the c file), so putting it in the common-c-mode-hook works
            (define-key 'c-mode-base-map "C-c C-c" #'+compile-and-execute-in-vterm)))

(add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1))) ; use // instead of /* */ so ts-fold can fold it better)

(leaf cmake-mode
  :doc "for cmake files"
  :straight t)

(leaf csharp-mode
  :doc "C# syntax highlighting"
  :straight t)

(leaf modern-cpp-font-lock ;; C++ syntax highlighting
  :straight t
  :hook (c++-mode-hook . modern-c++-font-lock-mode))
(leaf disaster ;; Disassemble C, C++ or Fortran code under cursor
  :straight t)
;;; The Rest
(leaf eat
  :doc "Emulate-A-Terminal"
  :straight (eat :type git
                 :host codeberg
                 :repo "akib/emacs-eat"
                 :files ("*.el" ("term" "term/*.el") "*.texi"
                         "*.ti" ("terminfo/e" "terminfo/e/*")
                         ("terminfo/65" "terminfo/65/*")
                         ("integration" "integration/*")
                         (:exclude ".dir-locals.el" "*-tests.el")))
  :defer
  ;; :hook ((eshell-load . eat-eshell-mode)
  ;;        ;; (eshell-load . eat-eshell-visual-command-mode)
  ;;        )
  :defer-config
  (setq eat-kill-buffer-on-exit t))

(leaf outli
  :doc "outline fontification and nesting, org-style tabbing on them, plus more commands, see outli speed-command-help "
  :straight (outli :type git :host github :repo "jdtsmith/outli") 
  :bind (outli-mode-map     ; convenience key to get back to containing heading
         :package outli
	       ("C-c C-p" . (lambda () (interactive) (outline-back-to-heading))))
  :hook ((prog-mode-hook text-mode-hook) . outli-mode))

(leaf lispy
  ;; keybindings to remember: "number (", wrap in () and go from |() to (| (), pretty good!
  ;; learning this is like a WTF HOW DO I DO THIS BASIC THING till eureka and you see how it all comes together. Watch the demo vids to see how it's done, it helps a LOT.
  :doc "holy shit a genius parens editing mode; Smart, short keybind lisp editing"
  :straight t
  :hook ((emacs-lisp-mode-hook
          eval-expression-minibuffer-setup-hook
          lisp-interaction-mode-hook
          ielm-mode-hook
          lisp-mode-hook
          scheme-mode-hook
          clojure-mode-hook
          cider-repl-mode-hook) . lispy-mode)
  :bind ((lispy-mode-map
          :package lispy
          ("C-<return>")
          ("M-<return>")
          ("M-RET")
          ("M-.")
          ;; ("<SPC>" . xah-insert-space-before) ; ugh what a headache
          ("M-,"))
         ;; (lispy-mode-map-special
         ;;  :package lispy
         ;;  ("<SPC>" . xah-insert-space-before))
         )
  :config
  (setq lispy-compat '(edebug ;; adds overhead so careful! don't need to setq-local 'cuz I use these 3 all the time anyways (except edebug)
                       ;; magit-blame-mode
                       cider
                       macrostep))
  ) 

(use-package sotlisp ;; abbrev way of typing elisp TODO: figure out M-RET keybinding clashes
  :straight t
  :hook (emacs-lisp-mode . speed-of-thought-mode))

(use-package erefactor                  ;; elisp refactoring, how to use?
  :straight t
  :defer
  ;; :config
  ;; ;; highlight local variables
  ;; (add-hook 'emacs-lisp-mode-hook 'erefactor-lazy-highlight-turn-on)
  ;; (add-hook 'lisp-interaction-mode-hook 'erefactor-lazy-highlight-turn-on)
  )

(defun slime-eval-last-expression-eros ()
  (interactive)
  (cl-destructuring-bind (output value)
      (slime-eval `(swank:eval-and-grab-output ,(slime-last-expression)))
    (eros--make-result-overlay (concat output value)
      :where (point)
      :duration eros-eval-result-duration)))

(defun sly-eval-last-expression-eros ()
  (interactive)
  (cl-destructuring-bind (output value)
      (sly-eval `(swank:eval-and-grab-output ,(slime-last-expression)))
    (eros--make-result-overlay (concat output value)
      :where (point)
      :duration eros-eval-result-duration)))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet)
(use-package common-lisp-snippets
  :straight t
  :defer
  :after (yasnippet sly))
(leaf haskell-snippets
  :straight t
  :after haskell-mode)


(leaf *leaf-buds ;; next it's flower, then fruits?
  :doc "fun leaf packages"
  :config)
(leaf leaf-convert
  :doc "converts to leaf any sexp passed to it, doesn't work perfectly when converting bind-keys->leaf-keys"
  :straight t)
(leaf leaf-tree
  :straight t
  :defer-config
  (setq imenu-list-size 30
        imenu-list-position 'left))
