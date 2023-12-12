
;;; Notes
;;;; keybindings to remember
;; http://xahlee.info/emacs/emacs/ergoemacs_and_paredit.html
;; p = insert space before cursor
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
;; f3, start typing, then f4 to define keyboard macro and f4 to apply it again or M-100 f4 to apply it 100 times in one go.
;; try subword-mode and superword-mode when coding in java
;; try pressing "!" in magit-status buffer to run arbitrary git commands
;;;; Packages to look at or consider
;;;;; Packages to maybe have a look at
;; - gamegrid
;; - narrow-indirect
;; - mixed-pitch
;; - ace-link
;; - ledger-mode & flycheck-ledger
;; - dwim-shell-command
;; - org-bookmark-heading
;; - org-visibility
;; - ws-butler, WhiteSpace butler, unobtrusive whitespace-trimming
;; - with-editor
;; - scratch
;; - so-long
;; - easy-kill
;; - ox-jira
;; - symbol-overlay
;; - git-auto-commit-mode
;; - transient
;; - just-mode
;; - json-snatcher
;; - iedit
;; - archive-rpm
;; - org-block-capf, completion for org blocks with "<" prepended https://github.com/xenodium/org-block-capf
;; - compile-multi, https://github.com/mohkale/compile-multi#deviations-from-emacs-multi-compile
;;;;; Packages I'm excited about 
;; - Delve, org-roam notes browser, integration with org-roam-ui, just wait and see

;;;;; Cool Packages to maybe have a look at
;; - org-noter, annotating pdf,epub with complete org files in the margin
;; - org-transclusion, live preview of parts of another org file via links
;; - suggest.el
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
;; - hammy, programmable timers!
;; - vulpea, org and org-roam functions to help with programming org-roam-related stuff https://github.com/d12frosted/vulpea
;; - winner-mode, allows you to undo and redo changes to window configurations
;; - org-modern-indent, makes fringe area indent show properly indented instead of disabled when org-indent is used with org-modern
;; - lilypond-auto-insert-mode, more lilypond auto-insertions in emacs
;; - using English dictionaries in Emacs: https://irreal.org/blog/?p=7303
;; - auto-sudoedit

;;;;; Cool packages that I want to install later on
;; - persp-mode, workspace manager
;; - dumb-jump, for when u don't have lsp and want to jump to definitions
;; - undo-propose, stage undos in a separate buffer
;; - volatile-highlights, highlights editing part when yanking
;; - pulse.el, to pulse regions when editing. idk for now what is "download through CVS": https://www.emacswiki.org/emacs/PulseRegion, for this https://blog.meain.io/2020/emacs-highlight-yanked or equivalent advice given to functions from Nasy Emacs.
;; - dogears/bookmark+

nil
;;; Config Debugging
;; idk where to put this, here will do.
(setq +init-file-debug t)
(setq debug-on-error nil)
(if +init-file-debug
    (setq use-package-verbose t
          use-package-compute-statistics t
          use-package-expand-minimally nil
          leaf-expand-minimally nil
          )
  (setq use-package-verbose nil
        use-package-compute-statistics nil
        use-package-expand-minimally t
        leaf-expand-minimally t
        debug-on-error nil)
  )
;; (setq force-load-messages t)

;;; Package Managers
;; I have 2 package managers 'cuz moving slowly to Elpaca, from Straight

;; Commented out 'cuz I wanna try only using Elpaca
;; Bootstrap `straight.el' package manager
;; I copied alexlugit's neatened-up bootstrap code version 6 at https://github.com/alexluigit/emacs-grandview/blob/master/init.el
(setq straight-use-package-by-default nil ; makes each use-package form also invoke straight.el to install the package, unless otherwise specified
      straight-vc-git-default-clone-depth 1
      straight-check-for-modifications '(check-on-save find-when-checking) ;; speeds up straight initialisation
      straight-repository-branch "develop"
      straight-hosts '((github "github.com" ".git")
                       (gitlab "gitlab.com" ".git")
                       (sourcehut "git.sr.ht" "") ; Apparently only works without the ".git". less confusing for git newbies, more confusing for people used to the other sites!
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


;; Bootstrap `elpaca.el' package manager
(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;;; Configuration Macros
(elpaca leaf)
(elpaca-wait) ;; makes sure leaf is loaded

;; for using elpaca with leaf, from https://github.com/progfolio/elpaca/issues/7#issuecomment-1406432109
;; basically  (elpaca-leaf name <leaf-stuff>) becomes (elpaca name (leaf name <leaf-stuff>)),
;; and recipes are supported! (elpaca-leaf (name :rest-of-recipe) <leaf-stuff>) becomes (elpaca (name :rest-of-recipe) (leaf name <leaf-stuff>))
(defmacro elpaca-leaf (order &rest body)
  "Execute BODY in `leaf' declaration after ORDER is finished.
If the :disabled keyword is present in body, the package is completely ignored.
This happens regardless of the value associated with :disabled.
The expansion is a string indicating the package has been disabled."
  (declare (indent 1))
  (if (memq :disabled body)
      (format "%S :disabled by elpaca-leaf" order)
    (let ((o order))
      (when-let ((ensure (seq-position body :ensure)))
				(setq o (if (null (nth (1+ ensure) body)) nil order)
							body (append (seq-subseq body 0 ensure)
													 (seq-subseq body (+ ensure 2)))))
      `(elpaca ,o (leaf
										,(if-let (((memq (car-safe order) '(quote \`)))
															(feature (flatten-tree order)))
												 (cadr feature)
											 (elpaca--first order))
										,@body)))))

(leaf leaf
	:doc
	"Leaner, Better-Documented & Easier-To-Extend `use-package'. Philosophy is to be clear about everything, base package is minimalistic.
- list of configs to refer to: https://github.com/conao3/leaf.el/issues/306
- using custom seems to load package at init time, not after deferring... thank god it's easy to change the block to setq
- you either use :defer-config or :config. :defer-config when there's no other thing activating deferring, and :config otherwise
- REMEMBER TO ADD :straight t. REMEMBER TO ADD :straight t. REMEMBER TO ADD :straight t. REMEMBER TO ADD :straight t
- Understanding :pkg-map :package pkg in leaf-keys, think of the distribution of keys: defined at one place, pointed to everywhere else. All file-related config goes in one leaf node.
  read with the idea of wanting to lazy-require packages:
  in your init file, the packages are sequentially evaluated. For keymaps to evaluate to valid values, the interpreter should allow you to assign or set functions to void things.
  However, this is emacs lisp; obviously not the case. How then? by creating dummy variables to bind to like function prototypes in c, combined with the autoloading by the package
  manager, you can group relevant parts of your config together.
- :leaf-defer t/nil (default t) controls whether u want the keywords in leaf-defer-keywords to defer :config and the rest, till the package loads. :defer-config defers either way. Play with this example:
(leaf org
	:leaf-defer t
	:mode ("\\.org$'" . org-mode)
	:init
	(setq a 'b)
	:config
	(setq c 'd)
	:defer-config
	(setq e 'f))

IMMEDIATE   |:DEFERRED
:preface     |:preface
             |DECLARATION
:conditional |:conditional
:bind        |:bind
:init        |:init              
:require     |
:config      |:config
            |LIBRARY-LOADED  
            |:defer-config       
- I realised there's multiple ways of hooking... eval-after-load, mode-hook, add-advice, ...


Comparison to use-package (that are not on github)
= it seems it is not really clearer all-around, just different
+ you feel like you are making a tree
+ the :if  doesn't come after loading the package with :straight or :ensure like in use-package, it comes BEFORE so you don't have to wrap the whole use-package macro in a when when you want to conditionally add packages to load-path
- no keyword to customize variables after eval of package that are meant to be customized take less time to set if , but it'll do it at init time which takes a lot of time so nah...
+ :custom and :setq family support backquote-commans syntax
= less parens for :hook, but need to type -hook which is OK, that part fucked me over so many times in use-package
"
	:config
	(elpaca-leaf leaf-keywords
		:doc "provides more keywords for base leaf package for easier configuration"
		:config
		(leaf-keywords-init))
	(elpaca-leaf blackout
		:doc "an easier delight/diminish/dim, for changing both major and minor mode appearance in modeline. Works with leaf to make a :blackout keyword")
	(elpaca-leaf leaf-convert
		:doc "converts to leaf any sexp passed to it, doesn't work perfectly when converting bind-keys->leaf-keys")
	(elpaca-leaf leaf-tree
		:defer-config
		(setq imenu-list-size 30
					imenu-list-position 'left)))

(elpaca-leaf (once :type git :host github :repo "emacs-magus/once") ;; "TODO: ':type' in recipe is not supported by Elpaca, but it still works. Why?"
	:doc "macros for adding code to run on the first time a hook is run")

(leaf use-package
	:doc "macros to neaten configuration. I keep it around to slowly convert my init file and try others' code blocks.
If you wanna expand use-package macros, if there are no errors in the config, you can set use-package-expand-minimally to t to get a much more readable expansion"
	:tag "built-in"
	:config
	(setq use-package-hook-name-suffix nil))
(elpaca-leaf elpaca-use-package
	:doc "for use-package integration with elpaca"
	:config
	(elpaca-use-package-mode))
(leaf bind-key :doc "macros for binding keys, comes with use-package too")

(elpaca-leaf setup
	:doc "more configuration macros, yay!")

(elpaca-wait)
;;; Benchmarking
;; must be put asap after use-package for most complete benchmark. Look at its functions named benchmark-init/...
(elpaca-leaf benchmark-init
	:require t
	;; To disable collection of benchmark data after init is done.
	;; :hook (after-init-hook . benchmark-init/deactivate) ;; elpaca only process after after-init-hook
	:hook (elpaca-after-init-hook . benchmark-init/deactivate))

(elpaca-leaf esup
	:doc "Emacs Start UP"
	;; note that a package that loads a library first will take up more time, any package that relies on what is
	;; already loaded will then take up less time. By moving the loading sequence of packages around, you may "pass"
	;; time from one package to another when they both require a common package, and startup time seems to be lifted off one
	;; package but in reality stays the same as it is passed to another package.
	
	;; Work around a bug where esup tries to step into the byte-compiled
	;; version of `cl-lib', and fails horribly.
	:init
	(setq esup-depth 0))

(defun +message-time-from-after-init ()
	(message "Elpaca finished in %s with %d garbage collections total."
           (format "%.2f seconds"
                   (float-time
										(time-subtract elpaca-after-init-time after-init-time)))
					 gcs-done))
(add-hook 'elpaca-after-init-hook #'+message-time-from-after-init)
;;; Default configs that are absolutely shared across all systems
(when +apexless
	(setq mac-command-modifier      'meta
				ns-command-modifier       'meta
				mac-option-modifier       'alt
				ns-option-modifier        'alt
				mac-right-option-modifier 'none
				ns-right-option-modifier  'none))

(setq user-mail-address "healtermon@gmail.com")
(setq user-full-name "L.R.J, Samuel")

;; System locale to use for formatting time values.
(setq system-time-locale "C") ; Make sure that the weekdays in the time stamps of your Org mode files and agenda appear in English.
(setq use-dialog-box nil)
(setq-default
 fill-column 80                      ; Set width for automatic line breaks
 uniquify-buffer-name-style 'forward ; Uniquify buffer names
 window-combination-resize t         ; Resize windows proportionally
 x-stretch-cursor t                  ; Stretch cursor to the glyph width
 )

(setq large-file-warning-threshold (* 128 1024 1024)) ;; 128 MebiBytes

(set-language-environment "UTF-8") ; fixes the "haskell process has died" error somehow
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)   ; Default to utf-8 encoding
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq selection-coding-system 'utf-8)

;; enable some disabled commands
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)      ; actually moves left the on-screen words, scroll-right brings u back to column 0.

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

(setq-default
 tab-width 2                            ; Tab width of 2 is compact and readable
 c-basic-offset 4                       ; but not in C
 )
(setq-default indent-tabs-mode nil) ; Toggle whether indentation can insert TAB characters

(blink-cursor-mode -1)
(leaf paren
  :tag "built-in"
  :global-minor-mode show-paren-mode)

;; set buffer to auto-update every 1.0s when the associated file is written to externally
(setq auto-revert-interval 1.0)
(global-auto-revert-mode 1)

(setq enable-recursive-minibuffers t) ; enables more than 1 minibuffer to be available at once
(minibuffer-depth-indicate-mode 1) ; shows [minibuffer-depth] at left of the echo area when depth >1


;; in isearch, highlight the line u are currently on 'cuz I tend to spend quite some time searching for it; it jumps all over the screen
;; this will disable hl-line-mode if already on, so beware.
(defun +turn-off-hl-line-mode () (hl-line-mode -1))
(add-hook 'isearch-mode-hook #'hl-line-mode)
(add-hook 'isearch-mode-end-hook '+turn-off-hl-line-mode)

;; display match count when using isearch
(setq isearch-lazy-count t)

;; (pixel-scroll-precision-mode 1) ;; TODO doesn't work on mitsuharu emacs-mac version, undoing the smooth scroll instead??? Check if the scroll steps affect this!

;;;; Elisp Programming
;; in Emacs, elisp programming is more important than other sorts of programming,
;; equivalent to whether the app settings work or not
;; suggestions: https://old.reddit.com/r/emacs/comments/zfwsc0/please_recommend_packages_for_editing_elisp/
(elpaca-leaf crux
	:doc "lots of random useful functions from the emacs Prelude 'distro'. It's up here 'cuz of crux-find-user-init-file")

(elpaca-leaf zygospore)

(elpaca-leaf xah-fly-keys
  :require t
  :doc "modal editing, efficient. Prob would have tried meow if I had known it first"
  :preface
  (defun +move-beginning-of-line () "moves all the way to the start" (interactive) (move-beginning-of-line 1))
  (defun +xfk-command-mode-n () (interactive)
         (cond ((and (string= major-mode "dired-mode")
                     (bound-and-true-p dirvish-override-dired-mode))
                (dirvish-narrow))
               (t (isearch-forward))))
  (defun +xfk-command-mode-j () (interactive)
         (cond ((string= major-mode "dired-mode") (dired-up-directory))
               (t (backward-char))))
  (defun +xfk-command-mode-l () (interactive)
         (cond ((string= major-mode "dired-mode") (dired-find-file))
               (t (forward-char))))
  (defun +xfk-command-mode-g () (interactive)
				 (cond ((string= major-mode "dired-mode") (wdired-change-to-wdired-mode))
               (t (xah-delete-current-text-block))))
  :bind
  (global-map
   ("C-y" . yank)
   ("C-n" . next-line)
   ("C-v" . +scroll-half-page-down)
   ("M-v" . +scroll-half-page-up)
   ("C-a" . +move-beginning-of-line))
  (xah-fly-command-map
	 ("w" . xah-shrink-whitespaces)
   ("e" . puni-backward-kill-word)
   ("r" . puni-forward-kill-word)
   ("n" . +xfk-command-mode-n)
   ("j" . +xfk-command-mode-j)
   ("l" . +xfk-command-mode-l)
   ("g" . +xfk-command-mode-g)
   ("8" . er/expand-region)
	 ("3" . zygospore-toggle-delete-other-windows)
   ("<SPC> 1 i" . crux-find-user-init-file)
   ("<SPC> 1 I" . (lambda () (interactive) (find-file-other-window +user-early-init-file)))
   ("<SPC> 1 t" . (lambda () (interactive) (find-file-other-window (concat +org-roam-dir "20230105103905.org"))))
	 ("<SPC> 1 y" . (lambda () (interactive) (find-file-other-window (concat +org-roam-dir "tasks.org"))))
   ("<SPC> 1 f" . (lambda () (interactive) (find-file-other-window (concat +emacs-dir "lisp/funcs.el"))))
	 ("<SPC> 1 s" . (lambda () (interactive) (dired-other-window (concat +stuff-dir))))
	 ("<SPC> 1 h" . (lambda () (interactive) (dired-other-window "~/")))
   ("<SPC> 1 c" . (lambda () (interactive) (dired-other-window (concat +stuff-dir "compro/healtermon/"))))
	 ("<SPC> 1 b" . (lambda () (interactive) (find-file-other-window (concat +org-roam-dir "20230521212331.org"))))
	 ("<SPC> 1 u" . (lambda () (interactive) (dired-other-window (concat +stuff-dir "university/"))))
	 ("<SPC> 1 d" . (lambda () (interactive) (dired-other-window (concat "~/Downloads/"))))
   ("<SPC> 1 l" . (lambda () (interactive) (find-file-other-window (concat +org-roam-dir "life.org"))))
	 ("<SPC> 1 e" . (lambda () (interactive) (find-file-other-window (concat +stuff-dir "compro/mitsuharu/emacs-mac/"))))
	 ("<SPC> 2 c" . (lambda () (interactive) (require 'calfw) (cfw:open-calendar)))
	 ("<SPC> 2 e" . elpaca-manager)
	 ("<SPC> 2 a" . ascii-table))
  :config
  ;; set-layout required before enabling
  (xah-fly-keys-set-layout (cond ((or +asses +mango) 'colemak-mod-dh)
                                 (t 'qwerty)))
  (xah-fly-keys 1)
	)

(elpaca-leaf puni
	:doc "leverages built-in features for structural editing, warning: not all-encompassing"
	:hook
	prog-mode-hook
	eval-expression-minibuffer-setup-hook
	:bind
	(puni-mode-map
	 :package puni ;; The difference between leaf-keys and bind-keys is, leaf-keys accepts a :package pkg1 pkg2 pk3... which chains `eval-after-load's for those packages before loading keybindings to the maps, while bind-keys will skip the `eval-after-load's. Henceforth if you want it to load immediately while deferring loading of the current package you give it some other already-loaded package, which is definitely leaf... Otherwise it can be nice to create complex criteria to load your keymaps, like here you can ":package (prog-mode whatever-loads-minibuffer)"
	 ("C-<right>" . puni-slurp-forward)
	 ("C-<left>" . puni-barf-forward)))
(elpaca-leaf expand-region ; TODO: test against puni-expand-region and see which I like more, then rebind it in xah-fly-command-map
	:doc "a better expand-region than xah-fly-keys'")
(elpaca-leaf (xah-find :type git :host github :repo "emacsattic/xah-find"))

(elpaca-leaf lispy
	;; keybindings to remember: "number (", wrap in () and go from |() to (| (), pretty good!
	;; learning this is like a WTF HOW DO I DO THIS BASIC THING till eureka and you see how it all comes together. Watch the demo vids to see how it's done, it helps a LOT.
	:doc "holy shit a genius parens editing mode; Smart, short keybind lisp editing"
	:hook
	emacs-lisp-mode-hook
	eval-expression-minibuffer-setup-hook
	lisp-interaction-mode-hook
	ielm-mode-hook
	lisp-mode-hook
	scheme-mode-hook
	clojure-mode-hook
	cider-repl-mode-hook
	:bind
	(lispy-mode-map
	 :package lispy
	 ("C-<return>")
	 ("M-<return>")
	 ("M-RET")
	 ("M-.")
	 ("M-,")
	 ("<SPC>")
	 ("<backspace>"))
	(lispy-mode-map-special
	 :package lispy
	 ("<SPC>")
	 ("<backspace>"))
	:init
	;; Enable compatibility with other modes,
	;; has to be set BEFORE (require 'lispy). Adds overhead, no need to setq-local 'cuz I use these 3 all the time anyways (except edebug)
	(setq lispy-compat  '(edebug 
												;; magit-blame-mode
												cider
												macrostep))
	(setq lispy-close-quotes-at-end-p t))

;; (leaf sotlisp ;; TODO: figure out M-RET keybinding clashes, and clashes with lispy and xah-fly-keys
;;   :straight t
;;   :doc "Speed-Of-Thought, abbrev way of typing elisp"
;;   :hook (emacs-lisp-mode . speed-of-thought-mode))

;; ;; Commented out as trying puni + lispy
;; (leaf paredit
;;   :straight t
;;   :doc "functions for parentheses editing"
;;   :hook
;;   emacs-lisp-mode-hook
;;   lisp-interaction-mode-hook
;;   ielm-mode-hook
;;   lisp-mode-hook
;;   eval-expression-minibuffer-setup-hook
;;   scheme-mode-hook
;;   clojure-mode-hook
;;   cider-repl-mode-hook
;;   :bind (paredit-mode-map
;;          ("M-s")))

;; modern libraries, depended on by plenty of programs
(elpaca-leaf dash
	:doc "Dash is a modern list library for emacs-lisp which can replace the outdated cl-lib libraries."
	;; :global-minor-mode global-dash-fontify-mode
	)
(elpaca-leaf f
	:doc "F is a modern emacs-lisp library for working with files and filesystem paths. I use it in some of my functions and configurations.") 
(elpaca-leaf s
	:doc "S is a modern emacs-lisp library for string manipulation.")

(elpaca-leaf aggressive-indent
	:hook
	emacs-lisp-mode-hook
	lisp-mode-hook
	clojure-mode-hook)

(elpaca-leaf rainbow-delimiters
	:hook prog-mode-hook
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
(elpaca nil
	(leaf hideshow ; TODO: PROBABLY HARD: make hs-fold not fuck up outli's headers in the case like 2 headers stacked together
		:doc "code folding! yay!"
		:tag "built-in"
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
	)

(elpaca-leaf macrostep 
	:require t
	:doc "macroexpand conveniently"
	:bind
	(emacs-lisp-mode-map
	 ("C-c e" . macrostep-mode)))

(elpaca-leaf ipretty 
	:doc "eval and pretty-print a sexp"
	;; global mode that advices `eval-print-last-sexp' to use ipretty-last-sexp instead
	:global-minor-mode t)

(elpaca-leaf eros 
	:doc "Show emacs-lisp eval results in an overlay, CIDER style."
	;; global mode that remaps eval-last-sexp to eros-eval-last-sexp TODO: I don't think this works with ipretty
	:global-minor-mode t)

(elpaca-leaf string-edit-at-point
	:doc "avoid escape nightmares by editing strings in a separate buffer")
(elpaca-leaf elisp-docstring-mode
	:doc "syntax highlighting for elisp docstrings, can use after calling string-edit on an elisp docstring")

(elpaca-leaf lispxmp
	:doc "Annotate value of lines containing ; => ."
	:init
	(setq byte-compile-warnings '(cl-functions)) ;make it not complain about using the depreciated cl.el instead of cl-lib
	)

;; highlighting! --------------------------------------------
(elpaca-leaf highlight-defined
	:doc "extra emacs lisp syntax highlighting"
	:hook
	emacs-lisp-mode-hook)

;; (use-package highlight-quoted
;;   :config
;;   (add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode)
;;   (set-face-attribute 'highlight-quoted-symbol nil
;;                       :inherit 'font-lock-string-face)
;;  )

;; ;; Commented out 'cuz I value highlight-defined functionality more than quoted color
;; (leaf lisp-extra-font-lock ;; TODO: figure why user-defined variables don't get highlight. I'm using highlight-defined instead till then...
;;   :straight t
;;   :when nil ;; 
;;   :hook
;; 	emacs-lisp-mode-hook)
(elpaca-leaf morlock
	:defer-config
	(font-lock-add-keywords 'emacs-lisp-mode morlock-el-font-lock-keywords))

;; Commented out 'cuz "<f7> e e" binded in  xah-fly-keys also does this
;; (leaf highlight-symbol
;;   :straight t
;; 	:doc "highlight all occurances of symbol at point in buffer"
;;   :hook
;;   prog-mode-hook)

(elpaca-leaf xr
	:doc "inverse of rx, converts emacs lisp regexp to rx form")

(leaf lisp-mode
	:mode ("\\.eld\\'" . lisp-data-mode)
	)

;;;; ELisp Debugging
;; see https://github.com/progfolio/.emacs.d/blob/master/init.org#debugging

"stop headline from getting folded"
;;; Function & Variable Definitions
(defvar +home-dir nil) ;; will change if it gets more complicated than this, currently unused
(defvar +emacs-dir (file-truename user-emacs-directory) "user-emacs-directory")
(defvar +stuff-dir (file-truename "~/stuff/") "my main storage directory")
(defvar +lisp-dir                     (concat +emacs-dir "lisp/"))
(defvar +user-early-init-file         (concat +emacs-dir "early-init.el") "early-init.el in user-emacs-directory")

;; SET THIS BEFORE ANY OTHER DIRECTORY VARIABLES
(elpaca-leaf no-littering
	:require t
	:doc "sets the data and temp dirs of many packages to /etc and /var in `user-emacs-directory'."
	:pre-setq
	;; ;; if you want to set your own values
	;; `(no-littering-etc-directory . ,(expand-file-name "config/" user-emacs-directory))
	;; `(no-littering-var-directory . ,(expand-file-name "data/" user-emacs-directory))
	)
(elpaca-wait)
(cl-letf (((symbol-function 'etc)
           (symbol-function #'no-littering-expand-etc-file-name))
          ((symbol-function 'var)
           (symbol-function #'no-littering-expand-var-file-name)))
  (setq xenops-cache-directory		(var "xenops/cache/")) ;; stores SVGs
	;; (setq xenops-image-directory (var "xenops/image/")) ;; the images stored are providing the latex document, so idk whether to set this as default
	(setq pdf-view-restore-filename (var "pdf-view-restore.eld")) ;; eld, original default-name is .pdf-view-restore
	(setq lui-logging-directory			(var "lui-log/"))
	(setq eyebrowse-restore-dir			(var "eyebrowse-restore/"))
	(setq tabspaces-session-file		(var "tabspaces-session.el"))
	(setq telega-temp-dir (var "telega/temp/"))
	)
;; ;; commented out due to no-littering

;; (defvar +backup-file-dir              (concat +emacs-dir "backups/"))

;; ;; Put all autosave files like #filename.whatever# in the same directory
;; (setq auto-save-file-name-transforms `((,(rx (zero-or-more not-newline)) ,+backup-file-dir t)))

;; ;; Put all backups like filename.whatever~ in one directory so emacs doesn't strew them
;;(setq backup-directory-alist `((,(rx (zero-or-more not-newline)) . ,+backup-file-dir)))

;; (defvar +pdf-view-restore-filename    (concat +emacs-dir ".pdf-view-restore"))

(defvar +bibliography-directory       (concat +stuff-dir "notes/bib/references.bib"))
(defvar +zotero-styles-directory      (concat +stuff-dir "Zotero/styles/"))
(defvar +org-cite-csl-styles-dir +zotero-styles-directory)
(defvar +org-cite-global-bibliography `(,+bibliography-directory))
(defvar +healtermon-gcal-file         (concat +stuff-dir "notes/calendars/gcal.org") "healtermon@gmail.com main calendar") ;; i'll elogate the names if variety of files expands
(defvar +healtermon-gtasks-file       (concat +stuff-dir "notes/zk/tasks.org")   "healtermon@gmail.com \"My Tasks\" tasklist, used to be for gtasks, now I given up on it, org alone is simple enough, because of orgzly on android.")
(defvar +org-roam-dir                 (concat +stuff-dir "notes/zk/"))
(defvar +org-download-image-directory (concat +stuff-dir "notes/zk/p/"))
(defvar +org-roam-dailies-directory "daily/" "diary directory relative to org-roam-directory")

(defvar +fuel-factor-root-dir "/Applications/factor/")
(defvar +forth-executable "/opt/homebrew/bin/gforth")

(when +apexless
  (load (concat +lisp-dir "funcs.el"))
  (load (concat +lisp-dir "random-secrets.el"))
	)

(defvar +java-home-folder "/opt/homebrew/Cellar/openjdk/21.0.1/libexec/openjdk.jdk/Contents/Home/"  "java home folder, find with this line at command line: mvn --version")
(setenv "JAVA_HOME" +java-home-folder)
(defvar +lsp-java-java-path (concat +java-home-folder "bin/java"))

(defvar +tempel-path (expand-file-name "templates.eld" user-emacs-directory))

(defvar +elixir-ls-build-output-path (concat +stuff-dir "compro/elixir-lsp/elixir-ls/outputsam") "output directory of mix build tool. It has language_server.sh, and launch.sh to decide how to initialise the language server, hence you can't just have the elixir-ls executable on path, you need to call this script to start it.")


(defun +delete-file-visited-by-buffer (buffername)
  "Delete the file visited by the buffer named BUFFERNAME."
  (interactive "b")
  (let* ((buffer (get-buffer buffername))
         (filename (buffer-file-name buffer)))
    (when filename
      (delete-file filename)
      (kill-buffer-ask buffer))))

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

(defalias '+elisp-mode (symbol-function 'emacs-lisp-mode))

;; idk where I got this from, the original was named "rex/consult-grep-in-a-directory"
(defun +consult-grep-in-a-directory ()
  "Query the user for a directory to grep in."
  (interactive)
  (let ((current-prefix-arg 1))
    (call-interactively 'consult-ripgrep)))

;; these 2 below are from https://github.com/d12frosted/homebrew-emacs-plus/issues/588
(defun +toggle-undecorated-frame-maximised ()
  "Toggle the 'undecorated' parameter and frame maximized state for the current frame."
  (interactive)
  (let* ((undecorated-value (frame-parameter nil 'undecorated))
         (new-value (not undecorated-value)))
    (set-frame-parameter nil 'undecorated new-value)
    (toggle-frame-maximized))
  (message "Undecorated frame setting and maximized state toggled."))

(defun +toggle-undecorated-frame-unmaximised ()
  "Toggle the 'undecorated' parameter for the current frame."
  (interactive)
  (let* ((undecorated-value (frame-parameter nil 'undecorated))
         (new-value (not undecorated-value)))
    (set-frame-parameter nil 'undecorated new-value))
  (message "Undecorated frame setting toggled."))


;;; The rest of Default Configuration
(leaf recentf
  :doc "recent files browsing feature"
  :tag "built-in"
	:init
  ;; (once '(:before after-find-file) ;; 0.05s saved
  ;;   (setq recentf-max-saved-items 10000
  ;;         recentf-max-menu-items 10000)
  ;;   (recentf-mode 1))
  :config
  (setq recentf-max-saved-items 10000
        recentf-max-menu-items 10000)
  :global-minor-mode t ;; 0.05s lag is worth it
  )

(leaf saveplace
  :doc "Remember and restore the last cursor location of opened files. 10/10 package."
	:tag "built-in"
  :init
  (setq save-place-forget-unreadable-files t)
  :global-minor-mode save-place-mode ;; the one-time 0.05s lag is worth it
  )

(leaf *history-setting
  :setq
  (history-delete-duplicates . t))
(leaf savehist
  :doc "save minibuffer command history"
	:tag "built-in"
	:require t
  :setq
  (savehist-save-minibuffer-history . t)
  :global-minor-mode t)

(leaf time
  :doc "display of time, date, load numbers, name of mail inbox with new mail, etc..."
	:tag "built-in"
  :unless +apexless ; apexless has time permanently displayed so you don't need this
  :setq
  (display-time-default-load-average . nil) ; Don't display load average
  (display-time-24hr-format . t)            ; use hh:mm format instead
  :config
  (display-time-mode t))

(when +apexless
	(column-number-mode 1))

;; ;; COMMENTED OUT as I want emacs to be resizable and movable on my mac...
;; ;; to get fullscreen immediately when emacs opens, after the frame loads after early-init.el
;; (when +apexless (+toggle-undecorated-frame-maximised))

;;; essential packages for everyone

(elpaca-leaf which-key
  :doc "shows a popup with key bindings and functions associated with them"
  :global-minor-mode t)

(elpaca-leaf (vertico :files (:defaults "extensions/*"))
	:doc "a vertical autocomplete selection menu"
	:bind ((vertico-map
					("M-DEL" . vertico-directory-delete-word)))
	:init
	(once '(:hooks pre-command-hook)
		(vertico-mode 1)
		(vertico-mouse-mode))
	:defer-config
	(setq vertico-resize t)
	(setq vertico-cycle t)
	(setq vertico-count (if +termux 10 20))

	;; Sort directories before files. Copied from Vertico README.
	(defun +sort-directories-first (files)
		(setq files (vertico-sort-history-length-alpha files))
		(nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
					 (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))
	)

(elpaca-leaf orderless ;; a completion style
  :require t
  :setq
  (completion-styles . '(orderless basic))
  (completion-category-overrides . '((file (styles . (partial-completion)))))
  (completion-category-defaults . nil)
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

(elpaca-leaf marginalia ;; annotates the minibuffer like the margins in a book (look on the right side)
  :require t
  :bind
	(minibuffer-local-map
   ("M-A" . marginalia-cycle))
  :after vertico 
  :setq
  (marginalia-max-relative-age . 0)
  :global-minor-mode t)

(elpaca-leaf consult ;; provides _good shit_ versions of common commands and more
  :bind
	(;; C-c bindings (mode-specific-map)
   ("C-c h" . consult-history)
   ("C-c m" . consult-mode-command)
   ("C-c k" . consult-kmacro)
   ;; C-x bindings (ctl-x-map)
   ("C-x M-:" . consult-complex-command)		 ;; orig. repeat-complex-command
   ("C-x b" . consult-buffer)								 ;; orig. switch-to-buffer
   ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
   ("C-x r b" . consult-bookmark)           ;; orig. bookmark-jump
   ("C-x p b" . consult-project-buffer)			;; orig. project-switch-to-buffer
   ;; Custom M-# bindings for fast register access
   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
   ("C-M-#" . consult-register)
   ;; Other custom bindings
   ("M-y" . consult-yank-pop) ;; orig. yank-pop
   ;; M-g bindings (goto-map)
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake)		 ;; Alternative: consult-flycheck
   ("M-g g" . consult-goto-line)	 ;; orig. goto-line
   ("M-g M-g" . consult-goto-line) ;; orig. goto-line
   ("M-g o" . consult-outline)		 ;; Alternative: consult-org-heading
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
	 ("M-s i" . consult-info)
   ;; Misc
   ("<help> a" . consult-apropos)
   ([remap switch-to-buffer] . consult-buffer)
   ([remap recentf-open-files] . consult-recent-file)
   ;; ([remap find-file] . consult-find) ;; I'm damn sure this is not a replacement
   ;; Isearch integration
   ("M-s e" . consult-isearch-history)
   (isearch-mode-map
    :package isearch
    ("M-e" . consult-isearch-history)		;; orig. isearch-edit-string
    ("M-s e" . consult-isearch-history)	;; orig. isearch-edit-string
    ("M-s l" . consult-line)			 ;; needed by consult-line to detect isearch
    ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
    )
   ;; Minibuffer history
   (minibuffer-local-map
    :package leaf
    ("M-s" . consult-history)	;; orig. next-matching-history-element
    ("M-r" . consult-history)	;; orig. previous-matching-history-element
    ))
  
  :init
  ;; CUSTOMISABLE WITH CUSTOM INTERFACE
  (setq register-preview-delay 0.5)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)

  ;; NON-CUSTOMISABLE WITH CUSTOM INTERFACE
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-function #'consult-register-format)
  
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  (fset 'multi-occur #'consult-multi-occur) ;; drop-in replacement.
	;; :defer-config
	;; ;; Exclude Tramp buffers from preview, see consult wiki: https://github.com/minad/consult/wiki#do-not-preview-exwm-windows-or-tramp-buffers
	;; (defun consult-buffer-state-no-tramp ()
	;; 	"Buffer state function that doesn't preview Tramp buffers."
	;; 	(let ((orig-state (consult--buffer-state))
	;; 				(filter (lambda (action cand)
	;; 									(if (or (eq action 'return)
	;; 													(let ((buffer (get-buffer cand)))
	;; 														(and buffer
	;; 																 (not (file-remote-p (buffer-local-value 'default-directory buffer))))))
	;; 											cand
	;; 										nil))))
	;; 		(lambda (action cand)
	;; 			(funcall orig-state action (funcall filter action cand)))))
	;; (setq consult--source-buffer
	;; 			(plist-put consult--source-buffer :state #'consult-buffer-state-no-tramp))
  )
(elpaca-leaf consult-dir
  :bind
	("C-x C-d" . consult-dir)
  (vertico-map
   :package vertico
   ("C-x C-d" . consult-dir)
   ("C-x C-j" . consult-dir-jump-file)))

(elpaca-leaf embark
  :bind
  ("C-." . embark-act)                  ; like a right-click
  ("M-." . embark-dwim)
  ("C-h B" . embark-bindings)           ; like a left-click
  
  :defer-config
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

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
(elpaca-leaf embark-consult
  :after (embark consult)
  ;; :demand t        ; only necessary if you have the hook below
  ;; ;; if you want to have consult previews as you move around an
  ;; ;; auto-updating embark collect buffer
  :hook (embark-collect-mode-hook . consult-preview-at-point-mode)) ;; TODO doesn't work, fix it.

(elpaca-leaf wgrep
  :doc "edit your grepped lines, all from the same buffer!")

(elpaca-leaf helpful
  :bind
  ([remap describe-command]  . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key]      . helpful-key)
  ([remap describe-symbol]   . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ;; Lookup the current symbol at point. C-c C-d is a common keybinding
  ;; for this in lisp modes.
  ("C-c C-d" . helpful-at-point)
  ;; Look up *F*unctions (excludes macros).
  ;;
  ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
  ;; already links to the manual, if a function is referenced there.
  ("C-h F" . helpful-function)
  (helpful-mode-map
   ;; Make `describe-*' screens more helpful
   ([remap revert-buffer] . helpful-update)))

(elpaca-leaf elisp-demos
  :advice
  (:after describe-function-1 elisp-demos-advice-describe-function-1)
  (:after helpful-update      elisp-demos-advice-helpful-update) ;; if you use helpful
  )


(elpaca-leaf '(xht :host sourcehut :repo "flandrew/xht")
	:doc "eXtensive Hash Table library")
(elpaca-leaf '(democratize :host sourcehut :repo "flandrew/democratize")
	:doc "most confusing package ever. It adds more examples to help and helpful buffers.
This is feasible by having package authors have in their repository,
some file that is structurally easy to parse, that corresponds function names to their examples.
The parsing and fitting into help buffers is then programmed by this package's author, and
made available to use in this package.

Since emacs 28, shortdoc is available. Try command `shortdoc-display-group' interactively!
Shortdocs come in a very easy to parse format, and the description is taken directly from the function's docstring. Try:
    M-x find-library RET shortdoc RET"
	:config
	(democratize-enable-examples-in-helpful)
	(democratize-enable-examples-in-help)
	;; ;; COMMENTED OUT 'cuz symlinks already made and docs generated; will run with errors otherwise.
	;; (make-symbolic-link (expand-file-name "f/README.org" elpaca-repos-directory)
	;; 										(expand-file-name "f/README.org" democratize--downloads-dir))
	;; (make-symbolic-link (expand-file-name "s/dev/examples.el" elpaca-repos-directory)
	;; 										(expand-file-name "s/examples.el" democratize--downloads-dir))
	;; (make-symbolic-link (expand-file-name "dash/dev/examples.el" elpaca-repos-directory)
	;; 										(expand-file-name "dash/examples.el" democratize--downloads-dir))
	;; (make-symbolic-link (expand-file-name "xht/README.org" elpaca-repos-directory)
	;; 										(expand-file-name "xht/README.org" democratize--downloads-dir))
	;; ;; call this when any of the above 4 libraries are available
	;; (democratize-all-libraries)
	)


(elpaca-leaf vundo
  :defer-config
  (setq vundo-glyph-alist vundo-unicode-symbols)
  (set-face-attribute 'vundo-default nil :family "Unifont"))

(elpaca-leaf visual-regexp
  :doc "live highlighting of regexps, replacing replace-regexp w/ visual-regexp"
  :bind
  ([remap replace-regexp] . vr/replace)
  ([remap query-replace-regexp] . vr/query-replace))
(elpaca-leaf visual-regexp-steroids
  :doc "enables changing regex backend"
  :config
  (setq vr/engine 'emacs))

(elpaca-leaf  (outli :type git :host github :repo "jdtsmith/outli") 
	:doc "outline fontification and nesting, org-style tabbing on them, plus more commands, see outli speed-command-help "
	:bind (outli-mode-map			 ; convenience key to get back to containing heading
				 :package outli
				 ("C-c C-p" . (lambda () (interactive) (outline-back-to-heading))))
	:hook ((prog-mode-hook text-mode-hook) . outli-mode)
	:config
	(push '(fennel-mode ";;" ?\; t) outli-heading-config)
	)

(elpaca-leaf keycast
	:doc "absolutely fabulous package, by default can use even the tab-bar w/ `keycast-tab-bar-mode' and header-line w/ `keycast-header-line-mode'
You get to see the latest command u typed and what it's binded to. Great for learning what weird things like trackpad gestures do without \"C-h k <action>\"."
	:require t ;; 'cuz only the global autoload works. Let's just load it anyways, shouldn't take much time. TODO if u turn off the global minor mode,  benchmark, and if needed make it faster
	:after doom-modeline
	:hook (doom-modeline-mode-hook . +keycast-mode)
	;; :global-minor-mode keycast-tab-bar-mode
	;; :defer-config
	:config

	;; from: https://www.const.no/init/
	;; though I don't know how to make it work with multiple screens...
	(define-minor-mode +keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if +keycast-mode
        (progn
          (add-hook 'pre-command-hook 'keycast--update t)
          (add-to-list 'global-mode-string '("" keycast-mode-line " ")))
			(remove-hook 'pre-command-hook 'keycast--update)
      (setq global-mode-string (remove '("" keycast-mode-line " ") global-mode-string))))

	;; ;; COMMENTED OUT 'cuz the provided keycast-mode-line-mode auto-config doesn't work with doom-modeline
	;; 	(defun +toggle-keycast()
	;; 		"add to doom-modeline, because by default, it's hard to add lol.
	;; From https://github.com/seagle0128/doom-modeline/issues/122#issuecomment-1133838869"
	;; 		(interactive)
	;; 		(if (member '("" keycast-mode-line " ") global-mode-string)
	;; 				(progn (setq global-mode-string (delete '("" keycast-mode-line " ") global-mode-string))
	;; 							 (remove-hook 'pre-command-hook 'keycast--update)
	;; 							 (message "Keycast OFF"))
	;; 			(add-to-list 'global-mode-string '("" keycast-mode-line " "))
	;; 			(add-hook 'pre-command-hook 'keycast--update t)
	;; 			(message "Keycast ON")))
	)


;;; Generally Useful
(elpaca-leaf reveal-in-folder
	:doc "Open Finder at location, only works on macOS"
	:if +apexless
	:init
	(defalias '+reveal-in-finder #'reveal-in-folder)
	(defalias '+open-in-finder #'reveal-in-folder)
	)
(elpaca-leaf terminal-here
	:doc "Open location in external terminal"
	:defer-config
	(setq terminal-here-mac-terminal-command 'iterm2)
	)
(elpaca-leaf hl-todo
	:doc "highlight words optionally with punctuation after them, jump between them and also an occur for them"
	;; copied from https://git.sjtu.edu.cn/sjtug/doom-emacs/-/blob/master/modules/ui/hl-todo/config.el
	:hook prog-mode-hook
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

(elpaca-leaf free-keys
	:doc "shows free keys in a buffer")

;;; file manager

(leaf *dired-related
  :init
  (when +apexless
    (setq dired-use-ls-dired t)
    (setq insert-directory-program "/opt/homebrew/bin/gls")
    (setq mac-system-move-file-to-trash-use-finder t)		;; if this is true and Finder is not open, it'll give an error "move-file-to-trash: Removing old name: Mac error -600"
		)
  (setq delete-by-moving-to-trash t) 
  (setq find-file-visit-truename t) ; follow symlinks when visiting files or directories
  )

(leaf dired  
	:tag "built-in"
	:config
  (setq dired-listing-switches "-aBhl  --group-directories-first") ; group directories first
  (setq dired-mouse-drag-files t)                   ; added in Emacs 29
  (setq mouse-drag-and-drop-region-cross-program t) ; added in Emacs 29
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-dwim-target t)
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  (setq dired-do-revert-buffer t) ;; update dir listing(s) after dired-do-something
  (setq dired-mark-region t)      ;; Sensible mark behavior
  (setq dired-auto-revert-buffer #'dired-directory-changed-p)
  )
(leaf dired-x
  :tag "built-in"
  :after dired
  :hook
  (dired-mode-hook . dired-omit-mode)
  :config
  (setq dired-omit-verbose nil)
  ;; matches an empty line, or one or more dots.
  (setq dired-omit-files (concat dired-omit-files (rx (or "" (seq line-start "." (zero-or-more ".") line-end)))))
  ;; ;; alternative to above; Make dired-omit-mode hide all "dotfiles"
  ;; (setq dired-omit-files
  ;;       (concat dired-omit-files (rx (or "" (seq line-start "." (zero-or-more not-newline) line-end)))))
  )

(elpaca-leaf dirvish ;; I don't know why this isn't bugged out now.
	:if +apexless
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
		;; ("M-s" . dirvish-setup-menu) ;; would rather have consult commands available
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
	(setq dirvish-preview-dispatchers (cl-substitute 'pdf-preface 'pdf dirvish-preview-dispatchers)) ;requires pdftoppm executable ;; Who knows why it can't be in setq instead of defer-config,
	(dirvish-peek-mode) ;; shows preview minibuffer when scrolling through find-file minibuffer
	(dirvish-side-follow-mode)
	)

(leaf dired-aux
  :tag "built-in"
  :after dired)
(leaf wdired
  :tag "built-in"
  :init
  ;; not customised for speed
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))



;;; editing on remote machines 

(leaf tramp
  :tag "built-in"
  :defer-config
  ;; Some tips to speed up Dired/Dirvish over TRAMP
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/ssh:YOUR_HOSTNAME:")
                     "direct-async-process" t))
  (setq tramp-verbose 0)
  (setq tramp-auto-save-directory (locate-user-emacs-file "tramp/"))
  (setq tramp-chunksize 2000)
  (setq tramp-use-ssh-controlmaster-options nil))
;;; Version-control-related 
(elpaca-leaf magit ; an amazing front-end to git
	:bind ("C-x g" . magit-status))
(elpaca-leaf magit-delta ;; syntax hightlighting with delta(command-line program) in magit diffs
	:hook (magit-mode-hook . magit-delta-mode))

(elpaca-leaf diff-hl
	:doc "highlights diffs in the margins. If you want nicer ones like in vscode, https://ianyepan.github.io/posts/emacs-git-gutter/"
	:hook ((after-init-hook . global-diff-hl-mode)
				 (magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
				 (magit-post-refresh-hook . diff-hl-magit-post-refresh)))
;;; Templates/snippets 
(elpaca-leaf tempel
	:bind
	("M-+" . tempel-complete) ;; Alternative tempel-expand
	("M-*" . tempel-insert)
	(tempel-map
	 ("M-]" . tempel-next)
	 ("M-[" . tempel-previous))
	:hook
	((prog-mode-hook
		text-mode-hook) . tempel-setup-capf)
	:init
	;; Require trigger prefix before template name when completing.
	;; :custom
	(setq tempel-trigger-prefix "<")
	(setq tempel-path +tempel-path)
	
	(defun tempel-setup-capf ()
		;; Add the Tempel Capf to `completion-at-point-functions'.
		;; `tempel-expand' only triggers on exact matches. Alternatively use
		;; `tempel-complete' if you want to see all matches, but then you
		;; should also configure `tempel-trigger-prefix', such that Tempel
		;; does not trigger too often when you don't expect it. NOTE: We add
		;; `tempel-expand' *before* the main programming mode Capf, such
		;; that it will be tried first.
		(setq-local completion-at-point-functions
								(cons #'tempel-complete
											completion-at-point-functions)))

	;; Optionally make the Tempel templates available to Abbrev,
	;; either locally or globally. `expand-abbrev' is bound to C-x '.
	;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
	;; (global-tempel-abbrev-mode)
	)

(elpaca-leaf tempel-collection
	:doc "more templates. Don't need to require, as during autoloading it runs an (eval-when-compile 'tempel (add-to-list <new templates> tempel-template-sources)), which is genius.")


(elpaca-leaf yasnippet
	:hook
	(java-mode-hook . yas-minor-mode))
;;; Notes/The Org Ecosystem 

(elpaca-leaf org
	:bind
	("C-c a" . org-agenda)
	("C-c l" . org-store-link)
	("C-c c" . org-capture)
	:hook
	(org-mode-hook . visual-line-mode)
	;; (org-mode . org-toggle-pretty-entities)
	;; (org-mode . +org-font-setup)
	:defer-config
	(setq org-hide-emphasis-markers t)
	(setq org-list-allow-alphabetical t)
	(setq org-return-follows-link t)
	(setq org-startup-folded 'content)
	(setq org-startup-indented t)
	(setq org-hide-leading-stars t)
	(setq org-log-done t)
	(setq org-image-actual-width nil)
	(when +apexless
		(setq org-latex-create-formula-image-program 'dvisvgm)
		(setq org-display-remote-inline-images 'cache)
		) ;; https://www.fromkk.com/posts/preview-latex-in-org-mode-with-emacs-in-macos/
	(setq org-agenda-files (list (concat +org-roam-dir "life.org")
															 +healtermon-gcal-file
															 +healtermon-gtasks-file))
	(setq org-todo-keywords
				'((sequence "TODO(t)" "ASAP(a)" "ONGOING(o)" "IFFREE(f)" "IFSUPERFREE(s)" "IFREALLYNOTHINGTODO(r)"
										"|" "USELESSED(u)" "TOOLATE(l)" "CANCELLED(c)" "DONE(d)")))
	(setq org-capture-templates
				`(("t" "task" entry (file ,+healtermon-gtasks-file)
					 "* TODO %?\n  SCHEDULED:\n  DEADLINE:\n")
					())
				)
	(setq org-agenda-custom-commands
				'(("c" "To-dos of Noted Life"
					 ((tags-todo "+health" ((org-agenda-overriding-header "Health first~!")))
						(tags-todo "+job" ((org-agenda-overriding-header "Job")))
						(tags-todo "+indep" ((org-agenda-overriding-header "Independence(neat-to-have skills)")))
						(tags-todo "+physics" ((org-agenda-overriding-header "Lifelong Dreams: Physics")))
						(tags-todo "+math-physics" ((org-agenda-overriding-header "Lifelong Dreams: Mathematics(Calculus is so magical!)")))
						(tags-todo "+piano" ((org-agenda-overriding-header "Lifelong Dreams: Piano/(?Music)"))))
					 nil)
					("z" "testing easy \"customization\""
					 ((agenda "" nil)
						(todo "TODO"
									((org-agenda-overriding-header "Physics")
									 (org-agenda-tag-filter-preset '("+physics"))))
						(tags-todo "+math-physics"
											 ((org-agenda-overriding-header "Mathematics")))
						(stuck ""
									 ((org-agenda-overriding-header "what's stuck projects?"))))
					 nil)
					("A" "agenda -3d to +30d"
					 ((agenda ""))
					 ((org-agenda-overriding-header "-3d to +30d")
						(org-agenda-start-on-weekday nil)
						(org-agenda-span 33)
						(org-agenda-start-day "-3d"))))))

(elpaca-leaf org-contrib                       ; currently does nothing but whatever
	:require t
	:after org)
(elpaca-leaf org-download
	:doc "org-download-clipboard doesn't seem to work, just drag the image in! It'll download automatically since org-download method is set to `directory'."
	:after org
	:require t
	:setq
	(org-download-method . 'directory)
	:setq-default
	(org-download-image-dir . +org-download-image-directory)
	)


(elpaca emacsql)
(elpaca magit-section)
(elpaca emacsql-sqlite)
(elpaca '(emacsql-sqlite3 :type git :host github :repo "emacsattic/emacsql-sqlite3"));; sqlite3 for +termux, just load them both, I can't care to find a predicate version of :straight keyword
(elpaca-leaf org-roam
	;; if you ever wanna rename your file titles, look at https://org-roam.discourse.group/t/does-renaming-title-no-longer-renames-the-filename/2018
	:init
	(setq org-roam-v2-ack t)
	(setq org-roam-directory +org-roam-dir)
	(setq org-roam-dailies-directory +org-roam-dailies-directory)
	(when +termux (setq org-roam-database-connector 'sqlite3))

	:pre-setq
	;; (org-roam-completion-everywhere t)
	(org-roam-file-exclude-regexp . ".*~.*")
	(org-roam-capture-templates
	 .
	 '(("d" "default without ${slug}" plain
			"%?"
			:if-new (file+head "%<%Y%m%d%H%M%S>.org"
												 "#+title: ${title}\n")
			:unnarrowed t)
		 ("t" "and with tags header ${slug}" plain
			"%?"
			:if-new (file+head "%<%Y%m%d%H%M%S>.org"
												 "#+filetags: \n#+title: ${title}\n")
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
	(org-roam-dailies-capture-templates
	 .
	 '(("d" "default" entry
			"%?"
			:if-new (file+head "%<%Y-%m-%d>.org"
												 "#+title: %<%Y-%m-%d>\n\n")
			:unarrowed t)))

	:bind
	("C-c n f" . org-roam-node-find)
	("C-c n d" . org-roam-dailies-goto-date)
	("C-c r r" . bms/org-roam-rg-search)
	(org-mode-map
	 :package org
	 ("C-c n p" . org-roam-dailies-goto-previous-note)
	 ("C-c n n" . org-roam-dailies-goto-next-note)
	 ("C-c n g" . org-roam-ui-mode) ;; use org-roam-ui to generate the graph, it's vastly superior
	 ("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n b" . org-roam-switch-to-buffer) ;not in v2 yet
	 ("C-c n c" . org-id-get-create)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n a" . org-roam-alias-add)
	 ("C-c n r" . org-roam-alias-remove)
	 ("C-c n t a" . org-roam-tag-add)
	 ("C-c n t r" . org-roam-tag-remove))
	:preface
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

	:defer-config
	;; show tags in the org-roam search
	;; specializer org-roam-node needs to be defined so put this after (require 'org-roam)
	(cl-defmethod org-roam-node-type ((node org-roam-node))
		"Return the TYPE of NODE."
		(condition-case nil
				(directory-file-name
				 (file-name-directory
					(file-relative-name (org-roam-node-file node) org-roam-directory)))
			(error "")))
	(setq org-roam-node-display-template (concat "${type:20} ${title:*} "
																							 (propertize "${tags:20}" 'face 'org-tag)))

	(org-roam-db-autosync-mode) ;; need org-roam-sqlite-available-p to be true

	

	(defun +org-roam-node-insert-immediate (arg &rest args) ;; was removed in v2, from https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/
		(interactive "P")
		(let ((args (cons arg args))
					(org-roam-capture-templates (list (append (car org-roam-capture-templates)
																										'(:immediate-finish t)))))
			(apply #'org-roam-node-insert args)))
	(define-key org-mode-map (kbd "C-c n I") #'+org-roam-node-insert-immediate))
(elpaca-leaf consult-org-roam
	:doc "live-preview when searching org-roam."
	:custom (consult-org-roam-grep-func . #'consult-ripgrep)
	:bind
	(org-mode-map
	 :package org
	 ("C-c n e" . consult-org-roam-file-find)
	 ("C-c n b" . consult-org-roam-backlinks))
	:init
	(consult-org-roam-mode 1))

;; org-roam-ui
(elpaca websocket)
(elpaca simple-httpd)
(elpaca-leaf org-roam-ui; dependencies are `require'ed in org-roam-ui.el so no need to require
	:after org-roam
	;;  :hook (after-init . org-roam-ui-mode) ; don't hook it anywhere, maybe bind it to a key
	:bind (:org-mode-map
				 :package org-roam
				 ("C-c n g" . org-roam-ui-mode))
	:pre-setq
	;; currently same as default values
	(org-roam-ui-sync-theme . t)
	(org-roam-ui-follow . t)
	(org-roam-ui-update-on-save . t)
	(org-roam-ui-open-on-start . t))

(elpaca-wait)
;;; LaTeX related 
;; TODO fix this, it doesn't work because "preview" for latex previewing isn't found, which can't be loaded 'cuz the tex stuff isn't found. So fix the tex stuff, maybe reinstall texlive distribution...
(elpaca-leaf xenops
	:doc "automatic live math preview that gets out of your way"
	:hook
	latex-mode-hook
	LaTeX-mode-hook
	org-mode-hook
	:config
	(setq xenops-reveal-on-entry t)
	(setq xenops-math-image-scale-factor 1.25))

;; TODO fix this: this shit doesn't work because the directory doesn't even exist anymore. Where is it? Homebrew? Nix? Oh my. No time for this, I don't even use this info manual.
;; for tex info. The LaTeX lsp digestif's creator can't live without this
;; 						(add-to-list 'Info-directory-list "/usr/local/texlive/2022/texmf-dist/doc/info/"))

;; ;; COMMENTED OUT 'cuz the below auctex config from a reddit post kinda works, we'll see...: https://old.reddit.com/r/emacs/comments/17f5yd4/failing_to_load_auctex_when_installed_via_elpaca/
;; (use-package tex
;;   :elpaca auctex ;; TODO fix trippy af auctex-tex declaration
;;   :mode ("\\.tex\\'" . latex-mode)
;;   :config
;;   (require 'texmathp) ; Needed for checking whether in math environments. TODO test this lol
;;   (setq bibtex-dialect 'biblatex)
;;   (setq TeX-auto-save t)
;;   (setq TeX-parse-self t)
;;   ;; ;; Enable LaTeX math support, maybe don't need this 'cuz of cdlatex
;;   ;; (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
;;   ;; ;; ;; Enable reference mangment
;;   ;; (add-hook 'LaTeX-mode-hook #'reftex-mode)
;;   (add-hook 'LaTeX-mode-hook 'prettify-symbols-mode) ;; prettify symbols like \alpha \beta

;;   )

(defun ded:elpaca-build-dir (p)
  "Return the elpaca build directory for package symbol p"
  (-first-item (f-directories elpaca-builds-directory (lambda (dir)
                                                        (string-match-p (concat "" (symbol-name p) "$") (f-filename dir))))))

(use-package auctex
  :elpaca (auctex :pre-build (("./autogen.sh")
                              ("./configure" "--without-texmf-dir" "--with-lispdir=.")
                              ("make")
                              ("install-info" "doc/auctex.info" "doc/dir")
                              ("install-info" "doc/preview-latex.info" "doc/dir")))
  :mode
  (("\.tex\'" . TeX-latex-mode)
   ("\.tex\.erb\'" . TeX-latex-mode)
   ("\.etx\'" . TeX-latex-mode))
  :init
  ;; (add-to-list 'Info-additional-directory-list (f-join (ded:elpaca-build-dir 'auctex) "doc"))
  (add-hook 'tex-mode-hook (lambda ()
                             (load "auctex.el")
                             (setq TeX-command-extra-options "-shell-escape")))
  :config
  (setq-default TeX-global-PDF-mode 1)
  (setq-default preview-scale-function 1.5)
  (setq TeX-auto-save t TeX-parse-self t default-truncate-lines t TeX-save-query nil TeX-source-correlate-method 'synctex))

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


(elpaca-leaf cdlatex ;; fast LaTeX math input
	:hook
	((latex-mode-hook
		LaTeX-mode-hook) . turn-on-cdlatex)
	;; (org-mode . turn-on-org-cdlatex)
	)
;;; Citations 
;; HOW TO USE: 
;; 1. let org-cite know the bib file, by "#+bibliography: path-to-your-file" or org-cite-global-bibliography which you already set
;; 2. put "#+cite_export: csl ieee.csl", where ieee can be whatever csl file in "~/Zotero/styles/" or a full path to a csl file
;; 3. put "#+PRINT_BIBLIOGRAPHY:" at wherever you want bibliography to be printed.
;; Further info: https://blog.tecosaur.com/tmio/2021-07-31-citations.html
(use-package oc ;; "org-cite" ;; TODO THIS SHIT IS TOO HARD TO CONVERT TO LEAF 'cuz of custom faces ; leaf doesn't have anything like use-package's custom-set-themes
  ;; :straight (:type built-in) ;; technically NOT built-in, i use the latest org, how to declare??? Edit: commented out 'cuz i want to get rid of dependency on straight
  :after org
  ;; :custom-face
  ;; Have citation link faces look closer to as they were for `org-ref', otherwise they're cyan, same as links in org-mode
  ;; (org-cite . '((t (:foreground "DarkSeaGreen4")))) ;; commented out, these 2 lines were for usage with leaf macro, it didn't work...
  ;; (org-cite-key . '((t (:foreground "forest green" :slant italic))))
  :custom-face
  (org-cite ((t (:foreground "DarkSeaGreen4"))))
  (org-cite-key ((t (:foreground "forest green" :slant italic))))
  :config
	(setq org-cite-global-bibliography +org-cite-global-bibliography)
  (setq org-cite-csl-styles-dir +org-cite-csl-styles-dir)
  (setq org-cite-export-processors
        '((org . (csl "ieee.csl"))
          (md . (csl "chicago-fullnote-bibliography.csl"))	; Footnote reliant
          (latex . biblatex)																; For humanities
          (odt . (csl "chicago-fullnote-bibliography.csl")) ; Footnote reliant
          (t . (csl "modern-language-association.csl"))     ; Fallback
          )))

(use-package citar
  :elpaca (citar :type git :host github :repo "emacs-citar/citar" :includes (citar-org))
	:if +apexless
  :after (org all-the-icons)
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

  (leaf citar-org ;; it's in citar's repo, so no need to download
    :after org citar
    :require t
    :init
    ;; makes org-cite use citar's nicer cite function, as org-cite's is very basic
    (setq org-cite-insert-processor 'citar)
    (setq org-cite-follow-processor 'citar)
    (setq org-cite-activate-processor 'citar))
  :config
  (setq citar-bibliography `(,(expand-file-name "~/stuff/notes/bib/references.bib")))
  (setq citar-notes-paths (list org-roam-directory))
  ;; (setq citar-open-note-function 'orb-citar-edit-note) ; if you use org-roam-bibtex
  )

;; allows you to find all citations of a reference from the note of the reference. It's in infant stage, so might switch to org-roam-bibtex then back. Problem is org-roam-bibtex needs helm-bibtex...
(elpaca-leaf citar-org-roam
	:after (citar org-roam)
	:global-minor-mode t)

(elpaca-leaf citar-embark
	:after (citar embark)
	:global-minor-mode t
	:defer-config
	(setq citar-at-point-function 'embark-act) ; changes citar-dwim to embark-act
	)

;;; Viewing & Editing pdf, epub, idk...  
;; for reading pdf, look out for image-roll.el when the bugs are fixed for continuous scrolling, and wait for a gif to see whether it allows preview-like scrolling
(elpaca-leaf pdf-tools
	:if (or +mango +apexless)
	:magic ("%PDF" . pdf-view-mode)
	:config
	(pdf-tools-install)
	;; (setq-default pdf-view-display-size 'fit-page)
	(setq-default pdf-view-use-scaling t)
	(setq pdf-view-resize-factor 1.1)
	:custom (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer) ;; for some reason it must be in customize, so eh
	) 
(elpaca-leaf pdf-view-restore ;; must be put below pdf-tools as it might load pdf-tools before any custom recipes are loaded
	:after pdf-tools
	:hook pdf-view-mode-hook
	;; ;; commented out because my extention to no-littering in my config above takes care of this already
	;; :pre-setq
	;; (pdf-view-restore-filename . +pdf-view-restore-file)
	)

;; for reading epub, needs more config for epub to look nice
(elpaca-leaf nov
	;; check out https://chainsawriot.com/postmannheim/2022/12/22/aoe22.html for nov customisations
	:mode "\\.epub\\'"
	:init
	(setq nov-shr-rendering-functions '((img . nov-render-img)
																			(title . nov-render-title)
																			(b . shr-tag-b)))
	:config
	(message "nov loaded")
	)
;;; Shells & Terminals 
(elpaca-leaf vterm)
(elpaca-leaf multi-vterm
	:doc " default vterm allows only 1 buffer, this is to allow more")

(leaf eshell
  ;; resources:
  ;; https://emacsconf.org/2022/talks/eshell/
  ;; https://www.masteringemacs.org/article/complete-guide-mastering-eshell
  ;; https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org
  :tag "built-in"
  :bind
  (eshell-mode-map
   ("C-l" . +eshell-delete-clear))
	:defer-config
  ;; must add-to-list AFTER (load 'eshell) as the modules list otherwise it'll be an invalid list(as it has your added item(/s) only) that customize will override
  (add-to-list 'eshell-modules-list 'eshell-smart)
  (setq eshell-history-size 10000)
  (setq eshell-hist-ignoredups 'erase
        eshell-error-if-no-glob t)

  (defun +eshell-delete-clear ()
    "deletes everything except the current prompt line, 
from https://www.n16f.net/blog/clearing-the-eshell-buffer/"
    (interactive)
    (let ((input (eshell-get-old-input)))
      (eshell/clear t)
      (eshell-emit-prompt)
      (insert input)))
	
  (leaf em-smart
    :doc "an eshell module, comes with eshell, eshell-smart-initialize is called from eshell-mode by default"
		:after esh-mode
    :pre-setq
    (eshell-where-to-jump . 'begin)
    ;; (eshell-review-quick-commands . nil)
    (eshell-smart-space-goes-to-end . t)
    )
  )
(elpaca-leaf eshell-prompt-extras
	:require t
	:doc "hella awesome eshell customisations"
	:after esh-mode
	:defer-config
	(setq eshell-prompt-function 'epe-theme-multiline-with-status))
;; (leaf eshell-git-prompt
;;   :straight t
;;   :doc "more prompts, but with git info as well.
;; I don't like any 'cuz no fish-style directory abbreviation")
(elpaca-leaf eshell-up
	:after esh-mode
	:require t
	:setq
	;; (eshell-up-ignore-case . nil) ;; make eshell-up searches case sensitive:
	(eshell-up-print-parent-dir . t))
(elpaca-leaf esh-autosuggest
	:doc "FISh-like history autosuggestions"
	:hook eshell-mode-hook)

(elpaca-leaf eshell-syntax-highlighting
	:after esh-mode
	:global-minor-mode eshell-syntax-highlighting-global-mode ;; Enable in all Eshell buffers.
	)
;; ;; TODO fix this, elpaca giving error "Warning (leaf): Error in `fish-completion' block.  Error msg: Autoloading file /Users/s/.emacs.d/elpaca/builds/eshell-vterm/eshell-vterm.elc failed to define function eshell-vterm-mode"
;; (elpaca-leaf eshell-vterm
;; 	:after esh-mode
;; 	:config
;; 	(eshell-vterm-mode))

;; set shell-mode derivatives' indentation to 2
(setq sh-basic-offset 2
      sh-basic-indentation 2)
(elpaca-leaf fish-mode
	:doc "fish shell scripting syntax highlighting") 


;; PComplete stuff ------------------------------------------------------------
;; pcomplete is the completion-at-point when you press TAB in shell/eshell

(elpaca-leaf pcmpl-args
	:doc "pcomplete extension pack; redefines a bunch of pcomplete terms"
	:after pcomplete
	:require t)
(elpaca-leaf fish-completion
	:doc "adds to pcomplete suggestions via FISh"
	:require t
	:when (executable-find "fish")
	:after pcomplete
	:global-minor-mode global-fish-completion-mode
	:config
	(setq fish-completion-fallback-on-bash-p t))
(elpaca-leaf bash-completion
	:doc "adds to pcomplete suggestions via BASh")
;;; Completion-related... Idk what to name this 
(leaf dabbrev
	:doc "Dynamic Abbrev"
	:tag "built-in"
	;; Tip: use Dabbrev with autocompletion globally! 

	;; Swap M-/ and C-M-/
	:bind
	("M-/" . dabbrev-completion)
	("C-M-/" . dabbrev-expand)
	;; Other useful Dabbrev configurations.
	:pre-setq
	(dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(leaf hippie-exp
	:tag "built-in"
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

(elpaca-leaf company  ; I don't really know how to get the backends to work properly...
	:doc " CompAny = Complete Anything"
	:bind (company-active-map
				 ("<return>")
				 ("RET")
				 ("C-<return>" . company-complete-selection)
				 ("M-<return>" . compan)
				 ("M-RET" . company-complete-selection)
				 ("<tab>" . company-complete-selection) ; TODO fix cdlatex and company working together, this may be tough
				 ("TAB" . company-complete-selection))
	:hook
	matlab-mode-hook
	:defer-config
	(setq company-minimum-prefix-length 1)
	(setq company-idle-delay 0.0) ;; default is 0.2
	;; :global-minor-mode global-company-mode
	)

(elpaca-leaf company-posframe
	;;  DISCLAIMER: frame saving with burly saves the posframes, and of course, gives error when it tries to restore a #<buffer item>, "Invalid Syntax "#" "
	:doc "use posframes for the popup, and also comes with icon support, plus backend-showing out-of-the-box"
	:when (posframe-workable-p)
	:after company
	:hook company-mode-hook
	;; :init
	;; ;; if you use desktop.el
	;; (push '(company-posframe-mode . nil)
	;;       desktop-minor-mode-table)
	)

(elpaca-leaf (corfu :files (:defaults "extensions/*")
										:includes (corfu-info corfu-history))
	:if +apexless
	:doc "adds a child frame for completion-at-point"
	:hook
	(prog-mode-hook . (lambda ()
											(when (not (or (string= major-mode "matlab-mode")
																		 (string= major-mode "java-mode"))) ;put the modes u wanna exclude here haha
												(corfu-mode))))
	inferior-python-mode-hook
	(eshell-mode-hook . (lambda ()
												(setq-local corfu-quit-at-boundary t ;; make it more like a normal shell
																		corfu-quit-no-match t
																		;; corfu-popupinfo-delay nil ;; Disable automatic info popup
																		corfu-auto nil ;; disable automatic activation of popup
																		)
												(corfu-mode 1)))
	(org-mode-hook . (lambda ()
										 (setq-local corfu-auto-prefix 2)))
	:bind (corfu-map
				 ;; unfuck the mappings check corfu-mode-map & (defvar corfu-map ...) in corfu.el
				 ([remap beginning-of-buffer])
				 ([remap end-of-buffer])
				 ([remap scroll-down-command])
				 ([remap scroll-up-command])
				 ([remap next-line])
				 ([remap previous-line])
				 ("RET")
				 ("<return>")
				 ;; my preferred mappings
				 ("C-<return>" . corfu-insert)
				 ("M-<return>" . corfu-insert)
				 ("C-n" . corfu-next)
				 ("C-p" . corfu-previous)
				 ("M-n" . corfu-scroll-up)      ; also can use up/down arrow keys
				 ("M-p" . corfu-scroll-down))
	:setq
	(corfu-scroll-margin . 5) ;; Use scroll margin
	(corfu-auto . t)          ;; Enable/disable auto completion
	(corfu-auto-delay . 0)	 ; seconds after u type a character that popup appears
	(corfu-auto-prefix . 1)	 ;; number of characters before popup appears
	;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
	;; (corfu-separator ?\s)          ;; Orderless field separator
	;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
	;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
	;; (corfu-preview-current nil)    ;; Disable current candidate preview
	;; (setq corfu-preselect 'valid) ;; Preselect the prompt
	;; (corfu-on-exact-match . nil)     ;; Configure handling of exact matches
	;; (corfu-echo-documentation . nil) ;; Disable documentation in the echo area

	:defer-config
	(defun +corfu-enable-always-in-minibuffer ()
		"Enable Corfu in the minibuffer if Vertico/Mct are not active."
		(unless (or (bound-and-true-p mct--active)
								(bound-and-true-p vertico--input)
								(eq (current-local-map) read-passwd-map))
			(setq-local corfu-auto t)
			(setq-local corfu-echo-delay nil)
			(corfu-mode 1)))
	(add-hook 'minibuffer-setup-hook #'+corfu-enable-always-in-minibuffer 1)
	
	(corfu-history-mode)
	(corfu-popupinfo-mode))

(elpaca-leaf corfu-terminal ;; use popup/popon instead of childframes for GUI-less setup
	:unless (display-graphic-p)
	:after corfu
	:global-minor-mode t)
(elpaca-leaf kind-icon ;; Icons in corfu!
	:after corfu
	:setq
	(kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
	:config 
	(add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
	)
(elpaca-leaf cape ; adds capf backends and functions to deal with and convert from company backends
	:bind
	("C-c p p" . completion-at-point) ;; capf
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
	("C-c p r" . cape-rfc1345)
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
	:advice
	;; The Capf has undesired side effects on Emacs 28 and earlier. UNCOMMENT WHEN EMACS 29 AND SEE WHAT IT DOES
	(:around pcomplete-completions-at-point cape-wrap-silent)
	(:around pcomplete-completions-at-point cape-wrap-purify)
	)

;;; General Programming 
;; HOW TO USE: C-u extended-command devdocs to set new default docset to search, otherwise just search normally with command devdocs-lookup

(elpaca-leaf devdocs
	:bind ("C-h D" . devdocs-lookup)
	:init
	(add-hook 'python-mode-hook
						(lambda () (setq-local devdocs-current-docs '("python~3.11")))))

(elpaca-leaf rainbow-mode
	:blackout t
	:doc "colors hex colors"
	:hook prog-mode-hook)

(elpaca-leaf tree-sitter                       
	:doc "semantic structural knowledge of code"
	:hook
	agda-mode-hook
	sh-mode-hook
	c-mode-hook
	caml-mode-hook
	csharp-mode-hook
	c++-mode-hook
	d-mode-hook
	css-mode-hook
	elm-mode-hook
	elixir-mode-hook
	erlang-mode-hook
	ess-r-mode-hook
	go-mode-hook
	haskell-mode-hook
	hcl-mode-hook
	terraform-mode-hook
	html-mode-hook
	mhtml-mode-hook
	nix-mode-hook
	java-mode-hook
	javascript-mode-hook
	js-mode-hook
	js2-mode-hook
	js3-mode-hook
	json-mode-hook
	jsonc-mode-hook
	julia-mode-hook
	lua-mode-hook
	ocaml-mode-hook
	perl-mode-hook
	php-mode-hook
	prisma-mode-hook
	python-mode-hook
	pygn-mode-hook
	rjsx-mode-hook
	ruby-mode-hook
	rust-mode-hook
	rustic-mode-hook
	scala-mode-hook
	swift-mode-hook
	tuareg-mode-hook
	typescript-mode-hook
	verilog-mode-hook
	yaml-mode-hook
	zig-mode-hook)
(elpaca-leaf tree-sitter-langs
	:doc "language pack for tree-sitter"
	:after tree-sitter
	:hook (tree-sitter-after-on-hook . tree-sitter-hl-mode))
(elpaca-leaf (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
	:doc "code-folding with tree-sitter"
	:after tree-sitter)

(elpaca-leaf apheleia
	:doc "asynchronous code formatting"
	;; :global-minor-mode apheleia-global-mode ;; commented out 'cuz I don't like python's black formatter; I like it to be more compact. So I got pycodestyle and flake8 to shut up
	:hook
	;; ;; why are all the formatters not to my liking?
	;; c-mode-hook
	;; csharp-mode-hook
	;; c++-mode-hook
	;; python-mode-hook
	)

(elpaca-leaf (topsy :type git :host github :repo "alphapapa/topsy.el")
	:doc "show at top of window, the first line of top-level form"
	:hook
	;; prog-mode-hook
	)

;;;; Language Server Protocol(LSP)-related
;;;;; Eglot-related
(leaf xref)                             ; Not using elpaca ATM
(leaf project                           ; Not using elpaca ATM
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
(leaf eldoc                             ; Not using elpaca ATM
  :defer-config
  (setq eldoc-echo-area-use-multiline-p nil) ; fucking stop using multiline echo area for your documentation, it's a screen-wide annoyance
  )
(elpaca-leaf flymake
	:doc "Linter. I don't use flycheck 'cuz I haven't found a need to, and some say it's slower than current flymake on large files."
	:bind
	(flymake-mode-map
	 ("C-#" . flymake-goto-next-error)
	 ("C-$" . flymake-goto-prev-error)))
(leaf eglot 														; use built-in eglot
	:doc "Emacs Glot(EGlot)"
	:tag "built-in"
	:hook
	((python-mode-hook
		c-mode-hook
		c++-mode-hook
		rust-mode-hook
		nix-mode-hook
		clojure-mode-hook
		julia-mode-hook
		scala-mode-hook
		;; java-mode-hook
		) . eglot-ensure)
	:preface
	;; get pycodestyle to shut up, should set before loading eglot, otherwise too noisy...
	(defun +python-eglot-config ()
		(setq-default eglot-workspace-configuration
									'(:pylsp (;; :configurationSources ["flake8"]
														:plugins ( :pycodestyle (:enabled nil)
																			 ;; :mccabe (:enabled nil)
																			 ;; :flake8 (:enabled nil)
																			 )))))
	(add-hook 'python-mode-hook #'+python-eglot-config)
	:defer-config
	(add-to-list 'eglot-server-programs '(elixir-ts-mode "language_server.sh"))
	(setq eglot-events-buffer-size 0) ;; In the name of speed, this stops eglot from logging the json events of lsp server
	(setq completion-category-overrides '((eglot (styles orderless-fast))))

	;; ;; if you wanna have yasnippet completions show up while using eglot either corfu/company: https://stackoverflow.com/questions/72601990/how-to-show-suggestions-for-yasnippets-when-using-eglot
	;; ;; for company
	;; (add-hook 'eglot-managed-mode-hook (lambda ()
	;;                                      (add-to-list 'company-backends
	;;                                                   '(company-capf :with company-yasnippet))))
	;; for corfu
	;; (straight-use-package 'cape)
	(require 'cape)
	(defun +eglot-capf ()
		(setq-local completion-at-point-functions
								(add-to-list 'completion-at-point-functions
														 (cape-super-capf
															#'eglot-completion-at-point
															(cape-company-to-capf #'company-yasnippet)))))
	(add-hook 'eglot-managed-mode-hook #'+eglot-capf)

	)

(elpaca-leaf consult-eglot
	:after (consult eglot))
;;;; Python Programming 
(elpaca-leaf python
	;; DON'T confuse this with python-mode.el, they are 2 different packages:
	;; python.el is built-in and has better integration with emacs, while
	;; python-mode.el is a mess in terms of fucntions to call.
	;; Having both installed makes it very confusing.
	:defer-config
	;; Remove guess indent python message
	(setq python-indent-guess-indent-offset-verbose nil)
	(setq python-indent-offset 4)
	(setq python-shell-interpreter "python3.10") ; for cocos2d and other cs1010s stuff to work :(
	)
;;;; Scheme Programming 
(elpaca-leaf geiser-guile
	:commands geiser-guile)             ; geiser-guile to connect to guile repl!

(elpaca-leaf geiser-racket
	:commands geiser-racket) ; for racket if you download minimal racket you need to "raco pkg install compatibility-lib"

(elpaca-leaf macrostep-geiser           ; macrostep in geiser!
	:after geiser-mode
	:bind (geiser-mode-map
				 :package geiser-mode
				 ("C-c e" . macrostep-mode))
	:hook ((geiser-mode-hook
					geiser-repl-mode-hook) . macrostep-geiser-setup))

;;;; Common Lisp Programming 
(elpaca-leaf sly
	:init
	(setq inferior-lisp-program "sbcl"))
(elpaca-leaf sly-asdf
	:after sly)
(elpaca-leaf sly-quicklisp
	:after sly)
(elpaca-leaf sly-repl-ansi-color
	:after sly)
(elpaca-leaf sly-macrostep
	:after (sly macrostep)
	;; Once it's done, M-x sly should now bring up a macrostep-enabled SLY.
	;; In .lisp files you can now use C-c M-e or M-x macrostep-expand to expand a macro.
	)

(leaf slime ;; if I ever use slime
  :defer-config
  (defun slime-eval-last-expression-eros ()
    (interactive)
    (cl-destructuring-bind (output value)
        (slime-eval `(swank:eval-and-grab-output ,(slime-last-expression)))
      (eros--make-result-overlay (concat output value)
        :where (point)
        :duration eros-eval-result-duration))))
;;;; Ruby Programming 
;; see professional setup: https://old.reddit.com/r/emacs/comments/xqojo7/emacs_and_rails/iqbh0id/
;;; Other-languages Programming


(elpaca-leaf nix-mode
	:doc "for writing nix expressions"
	:when (or +mango +nix-on-droid +apexless)
	:defer-config (defun +rebuild-nix-config ()
									(interactive)
									(+execute-in-vterm
									 "cd ~/stuff/compro/healtermon/nixconfig/ && ./result/sw/bin/darwin-rebuild switch --flake . --show-trace"))
	:bind (nix-mode-map
				 :package nix-mode
				 ("C-c C-c" . +rebuild-nix-config)))

(elpaca-leaf guix
	:doc "interface for the guix package manager"
	:when (or +mango +durian))

;;; Password-Manager 
(elpaca-leaf (bitwarden :type git
												:host github
												:repo "seanfarley/emacs-bitwarden")
	:init
	(setq bitwarden-user user-mail-address)
	;; (bitwarden-auth-source-enable) ;; don't need it (yet)
	)
;;; Communication Protocols 
(elpaca-leaf elpher
	:doc "A gopher and gemini client, super simple to use")

(elpaca-leaf mastodon
	:doc "mastodon client
  not practical LMAO I'd rather use Mastonaut"
	:pre-setq
	;; change these whenever you wanna connect to another server
	(mastodon-instance-url . "https://emacs.ch")
	(mastodon-active-user . "healtermon"))

(elpaca-leaf ement
	:doc "matrix client and hence also supports IRC")

(elpaca-leaf circe ; IRC Client; takes the lessons learnt from ERC and is more easily extensible, and has nicer documentation IMO. Also since it's simpler it's easier to undertand, though also very noob-unfriendly from experience (see below)
	;; Q: honestly I still don't know how to login without using circe-network-options, unlike in ERC where they prompt you, circe doesn't seem to let you msg ppl?
	;; A: well it's actually 'cuz "/msg NickServ IDENTIFY user pass" opens in another buffer, which if you didn't notice and typed the wrong password, makes it seem like nothing happened... So it is a beautiful client after all, separating all the chats :)
	:defer-config
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
	;; (setq lui-logging-directory "~/.emacs.d/.luilogs")
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

(leaf erc
  :tag "built-in"
  :defer-config
  (setq erc-nick "healtermon")
  (setq erc-fill-column 90
        erc-fill-function 'erc-fill-static
        erc-fill-static-center 20)
  (setq erc-track-enable-keybindings t) ; enable C-c C-SPC to go to new messages
  )
(elpaca-leaf erc-hl-nicks
	:after erc
	:defer-config
	(add-to-list 'erc-modules 'hl-nicks))
(elpaca-leaf erc-image
	:after erc
	:defer-config
	(setq erc-image-inline-rescale 300)
	(add-to-list 'erc-modules 'image))

;; see https://codeberg.org/jao/elibs/src/branch/main/init.el
(elpaca-leaf telega
	:doc "GOATed Telegram Client"
	;; 59464c3 is the latest commit that supports TDLib 1.8.8 (what I have ATM from homebrew tdlib HEAD-d581e04)
	;; just go magit-checkout that commit and (straight-rebuild-package "telega")
	:setq
	;; has to be set before calling telega command, can be after loading telega.el
	(telega-server-libs-prefix . "/opt/homebrew/Cellar/tdlib/HEAD-d581e04/")
	(telega-rainbow-color-custom-for . nil)
	(telega-msg-rainbow-title . nil)
	(telega-sticker-set-download . t)
	:setq
	(telega-chat-show-avatars . nil)
	(telega-chat-prompt-format . ">> ")
	(telega-root-show-avatars . nil)
	(telega-emoji-use-images . nil)
	(telega-symbol-checkmark . "·")
	(telega-symbol-heavy-checkmark . "×")
	(telega-symbol-verified . "*")
	;; (telega-symbol-horizontal-bar . (propertize "-" 'face 'jao-themes-f00))
	;; (telega-symbol-vertical-bar . (propertize "| " 'face 'jao-themes-dimm))
	(telega-mode-line-string-format . '(:eval (telega-mode-line-unread-unmuted)))
	`(telega-use-images . ,(display-graphic-p))
	;; `(telega-open-file-function . ,(if (display-graphic-p) #'find-file #'jao--see))
	`(telega-open-message-as-file . ,(unless (display-graphic-p) '(photo video animation)))
	:defer-config
	(telega-mode-line-mode 1))

(elpaca-leaf google-this
	:commands (google-this-translate-query-or-region) ;; there's no autoload for just this 1 command, but there is for the 15 other commands. Why?
	)
(elpaca-leaf langtool
	;; from https://sqrtminusone.xyz/configs/emacs/#languagetool
	;; LanguageTool is a great offline spell checker. For some reason, the download link is nowhere to be found on the home page, so it is listed below
	;; https://dev.languagetool.org/http-server
	:init
	(setq langtool-language-tool-server-jar  "/Users/s/stuff/compro/LanguageTool/LanguageTool-5.9/languagetool-server.jar")
	(setq langtool-default-language "en-US")
	(setq langtool-mother-tongue "zh-CN"))

;;; Prettifying Everything 
(add-hook 'prog-mode-hook #'global-prettify-symbols-mode) ; prettify some symbols in prog-mode derivatives

(elpaca-leaf all-the-icons
	:doc "for dashboard & dirvish & citar, on first install, run all-the-icons-install-fonts"
	:defer-config (setq all-the-icons-scale-factor 1.0))

(elpaca-leaf all-the-icons-completion
	:doc "adds icons to minibuffer completion"
	:after marginalia
	:require all-the-icons
	:init (all-the-icons-completion-mode)
	:hook (marginalia-mode-hook . all-the-icons-completion-marginalia-setup) ; makes the mode follow marginalia-mode when on and off
	)


(elpaca-leaf nerd-icons
	:doc "on first install, run nerd-icons-install-fonts")

;; Addtional syntax highlighting for dired
(elpaca-leaf diredfl
	:hook
	dired-mode-hook
	dirvish-directory-view-mode-hook
	:require t
	:defer-config
	(set-face-attribute 'diredfl-dir-name nil :bold t))


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
(cond
 (+asses    (custom-set-faces '(default ((t (:family "mononoki NF"  :foundry "outline" :height 120 :width normal))))))
 (+durian   (custom-set-faces '(default ((t (:family "mononoki"     :foundry "UKWN"    :height 151 :width normal))))))
 (+mango    (custom-set-faces '(default ((t (:family "mononoki"     :foundry "UKWN"    :height 113 :width normal))))))
 (+apexless (+font-setup)))

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
(elpaca-leaf standard-themes ;; MY FAVOURITE THEME, default-dark with "#212121" background, which is emacs-mac's default
	:require t
	:config
	(once '(:hooks elpaca-after-init-hook)
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
		;; ;; COMMENTED OUT 'cuz NO NEED; i figured out how to customize the color at a deeper level
		;; (defun my-standard-themes-custom-faces ()
		;; 	"My customizations on top of the Standard themes.
		;; This function is added to the `standard-themes-post-load-hook'."
		;; 	(set-background-color "#212121"))

		;; ;; Using the hook lets our changes persist when we use the commands
		;; ;; `standard-themes-toggle', `standard-themes-load-dark',
		;; ;; `standard-themes-load-light'.
		;; (add-hook 'standard-themes-post-load-hook #'my-standard-themes-custom-faces)

		;; if you customise the colors like below, and use consult-theme to switch themes (`standard-themes-post-load-hook' is not run), standard-dark will remain with the customisations!
		(setq standard-dark-palette-overrides '((bg-main "#212121")
																						(bg-dim "#414141")))

		;; loads theme and runs `standard-themes-post-load-hook'
		(standard-themes-load-dark)
		)
	)

(elpaca-leaf minions
	;; :disabled t 													;; messing around with turning off minions-mode
	:hook
	;; after-init-hook ;; changed due to using elpaca
	elpaca-after-init-hook)

(leaf *modeline
  :init
  ;; WARNING: THIS CAUSES MODELINE TO FAIL IF PUT AFTER INIT, IDK WHY, MAYBE 'CUZ OF THEME LOADING AFTER after-init-hook
  (custom-set-faces
   ;; '(mode-line ((t (:height 0.8))))
   '(mode-line-active ((t ( :height 0.8)))) ; For 29+
   '(mode-line-inactive ((t ( :height 0.8))))
   ))


(elpaca-leaf doom-modeline
	:hook
	;; after-init-hook ;; changed due to using elpaca
	elpaca-after-init-hook
	:defer-config
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
	(setq doom-modeline-bar-width 1)
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
	;; 
	;; (setq doom-modeline-gnus t)
	(setq doom-modeline-gnus-timer -1)
	;; 
	;; ;; Wheter groups should be excludede when gnus automatically being updated.
	;; (setq doom-modeline-gnus-excluded-groups '("dummy.group"))
	(setq doom-modeline-irc nil)          ; irc unread messages number 
	;; (setq doom-modeline-irc-stylize 'identity) ; convert some IRC buffers to their font-awesome icon
	;; 
	;; ;; Change the executables to use for the language version string
	;; (setq doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
	;; 
	;; (setq doom-modeline-env-load-string "...")
	;; 
	;; ;; By default, almost all segments are displayed only in the active window. To
	;; ;; display such segments in all windows, specify e.g.
	;; (setq doom-modeline-always-visible-segments '(mu4e irc))
	;; 
	;; ;; Hooks that run before/after the modeline version string is updated
	;; (setq doom-modeline-before-update-env-hook nil)
	;; (setq doom-modeline-after-update-env-hook nil)
	)

(elpaca-leaf valign
	:disabled t 													;this shit too laggy
	:hook org-mode-hook
	:init (setq valign-fancy-bar t))

(elpaca-leaf org-modern
	:hook
	(org-agenda-finalize-hook . org-modern-agenda)
	;; org-mode-hook
	)


(elpaca-leaf org-sticky-header
	:hook
	;; org-mode-hook
	)

;; un-emphasize when cursor is on element
;; will fail to detect elements that are nested inside "certain other elements", like comments or document titles
(elpaca-leaf org-appear
	:after org
	:hook org-mode-hook
	;; hook it with org-modern if possible, 'cuz I want to see everything with default prefs in life.org
	:defer-config	;; over-excessive defer here, but whatever
	(setq org-appear-autoemphasis nil ;the only one that's on by default, like for /italic/, _underline_, +strikethrough+, etc.
				org-appear-autoentities t
				org-appear-autolinks nil
				org-appear-autosubmarkers t))


;;; For Fun / Useless / Miscellaneous 
(elpaca-leaf elcord ;; enables the "in emacs editing xxx" discord status, use "(elcord-mode)"
	)

(elpaca-leaf fsbot-data-browser ; fsbot is the IRC bot at #emacs@Libera.Chat
	)

(elpaca-leaf (2048-game :type git :host github :repo "emacsmirror/2048-game"))

(elpaca-leaf wordel
	:doc "wordle, a word-guessing game")

;; Deldo, vibration control: https://www.youtube.com/watch?v=D1sXuHnf_lo
;; 

(elpaca-leaf selectric-mode
	:doc "Make your Emacs sound like a proper typewriter (IBM Selectric II).")
(elpaca zone-nyan)
(elpaca flames-of-freedom)							; useful to find out how to optimise graphics speed, I suppose. (Read the code.)
(elpaca-leaf autotetris-mode
	:doc "since we have no time to play tetris, let emacs play it for you")
(elpaca-leaf pacmacs
	:doc "PacMan!")

;; TODO fix it? idk... not important
(elpaca c-c-combo)

;; TODO make a package out of this?
(defun animated-self-insert ()
  (let* ((undo-entry (car buffer-undo-list))
         (beginning (and (consp undo-entry) (car undo-entry)))
         (end (and (consp undo-entry) (cdr undo-entry)))
         (str (when (and (numberp beginning)
												 (numberp end))
                (buffer-substring-no-properties beginning end)))
         (animate-n-steps 3))
    (when str
      (delete-region beginning end)
      (animate-string str (1- (line-number-at-pos)) (current-column)))))
;; (add-hook 'post-self-insert-hook 'animated-self-insert)

;; TODO fix it? idk... not important
(elpaca-leaf '(quake-mode :host github :repo "jordonbiondo/quake-mode")
	:doc "sounds from Unreal Tournament 2004 when you kill lines, so you get triple-kill, ultra-kill sounds. Get it?")


(elpaca (highlight-tail :type git :host github :repo "emacsmirror/highlight-tail"))
(leaf highlight-tail
	:doc "This minor-mode draws a tail in real time, when you write."
	:url "https://www.emacswiki.org/emacs/highlight-tail.el"
	:tag "color" "effect" "highlight" "visual")

;; ref: https://www.emacswiki.org/emacs/CategoryGames
(elpaca (u-mandelbrot :type git :host github :repo "emacsattic/u-mandelbrot"))
;; Ref: https://www.masteringemacs.org/article/fun-games-in-emacs
;; M-x pong, snake, tetris, life, gomoku, dunnet, solitaire (& solitaire-solve if you get stuck), zone, mpuz: multiplication puzzle  (& mpuz-show-solution if you get stuck), blackbox (& to read manual, M-x f blackbox), bubbles, animate-birthday-present, 5x5
;; Ctrl-u 42 M-x hanoi, 

;; TODO what is this game? most extensible RPG? https://www.nongnu.org/nethack-el/
(elpaca '(nethack :type git :host github :repo "emacsattic/nethack")
	:doc "'REQUIRES a copy of nethack with lisp window port', from the code comments...")


(elpaca-leaf '(nes :type git :host github :repo "gongo/emacs-nes")
	:doc "an NES emulator. u need to download nes games to try. It's super slow according to the git README."
	:url "https://github.com/gongo/emacs-nes")

(elpaca-leaf (tron :repo "https://github.com/killdash9/tron.el")
	:doc "multiplayer support...")
(elpaca mines
	:doc "minesweeper!")

;;; Graveyard 
;; this package.el stuff is just here 'cuz I'll definitely forget the structure of this if ever need be
;; ;; This use-package.el code is kept to enable browsing of MELPA packages. It says package-archives is a void variable...
;; (add-to-list
;;  'package-archives
;;  '("melpa" . "https://melpa.org/packages/")
;;  t)

(elpaca-leaf dashboard ;;this package extends startup time from 145ms to 900ms as it loads org-mode
	:when +apexless
	:init
	(setq dashboard-set-heading-icons t)
	(setq dashboard-set-file-icons t)
	(setq dashboard-banner-logo-title "Lispy fun. Great glue.")
	;; (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
	(setq dashboard-startup-banner "~/.emacs.d/emacs-dash.png") ;; use custom image as banner
	(setq dashboard-center-content t) ;; set to 't' for centered content
	(setq dashboard-set-footer nil)   ;; don't put random message below 
	(setq dashboard-items '((recents . 15)
													(registers . 3)))
	:defer-config
	(dashboard-setup-startup-hook)
	(dashboard-modify-heading-icons '((recents . "file-text")
																		(bookmarks . "book"))))


;; commented out 'cuz I like company-posframe more, and would just not save frames with burly.el but windows instead. I'm also horrified by the hardcoding of icons and the terrible border around the help doc
;; (use-package company-box; sick company UI with icons and different colors for different backends;; - company-box, 
;;  :hook (company-mode . company-box-mode)
;;  :after company
;;  :config
;;  )


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
;; (elpaca-leaf async) ;; idk why elpaca says "duplicate items queued", but apparently it's loaded already so whatever
(elpaca-leaf ob-async)
(elpaca-leaf ob-clojurescript)
(elpaca-leaf org-babel-eval-in-repl
  :after org eval-in-repl)
(elpaca-leaf eval-in-repl)
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
;; 	:init

;; 	(defun +tabspace-setup ()
;; 		"Set up tabspace at startup."
;; 		;; Add *Messages* and *splash* to Tab `Home'
;; 		(tabspaces-mode 1)
;; 		(progn
;; 			(tab-bar-rename-tab "Home")
;; 			(when (get-buffer "*Messages*")
;; 				(set-frame-parameter nil
;;                              'buffer-list
;;                              (cons (get-buffer "*Messages*")
;;                                    (frame-parameter nil 'buffer-list))))
;; 			(when (get-buffer "*splash*")
;; 				(set-frame-parameter nil
;;                              'buffer-list
;;                              (cons (get-buffer "*splash*")
;;                                    (frame-parameter nil 'buffer-list))))))

;; 	:config
;; 	;; Filter Buffers for Consult-Buffer
;; 	(with-eval-after-load 'consult
;; 		;; hide full buffer list (still available with "b" prefix)
;; 		(consult-customize consult--source-buffer :hidden t :default nil)
;; 		;; set consult-workspace buffer list
;; 		(defvar consult--source-workspace
;; 			(list :name     "Workspace Buffers"
;; 						:narrow   ?w
;; 						:history  'buffer-name-history
;; 						:category 'buffer
;; 						:state    #'consult--buffer-state
;; 						:default  t
;; 						:items    (lambda () (consult--buffer-query
;;                              :predicate #'tabspaces--local-buffer-p
;;                              :sort 'visibility
;;                              :as #'buffer-name)))

;; 			"Set workspace buffer list for consult-buffer.")
;; 		(add-to-list 'consult-buffer-sources 'consult--source-workspace))

;; 	;; ya know this can turn tabspace integration off but not back on lmao
;; 	(defun +consult-tabspaces ()
;; 		"Deactivate isolated buffers when not using tabspaces."
;; 		(require 'consult)
;; 		(cond (tabspaces-mode
;;            ;; hide full buffer list (still available with "b")
;;            (consult-customize consult--source-buffer :hidden t :default nil)
;;            (add-to-list 'consult-buffer-sources 'consult--source-workspace))
;; 					(t
;;            ;; reset consult-buffer to show all buffers 
;;            (consult-customize consult--source-buffer :hidden nil :default t)
;;            (setq consult-buffer-sources (remove #'consult--source-workspace consult-buffer-sources)))))

;; 	(add-hook 'tabspaces-mode-hook #'+consult-tabspaces)

;; 	)


;; ;; well, not using it ATM 'cuz eyebrowse-restore-mode keeps telling me while using eyebrowse-restore-save-all the emacs.d/eyebrowse-directory isn't a file
;; ;; A: because the frame name was nil, it appended the name, which is nil, to the directory, and tried to access it as a file. So just name all your frames before saving.
;; ;; nope, still failing...
;; (set-frame-parameter nil 'name "Main")
;; (leaf eyebrowse
;; 	:straight t	
;; 	:hook after-init-hook)
;; (leaf eyebrowse-restore
;; 	:straight (eyebrowse-restore :type git :host github :repo "FrostyX/eyebrowse-restore")
;; 	:after eyebrowse
;; 	:global-minor-mode t)

;; ;; removed 'cuz I decided that orgzly was good enough to show TODOs. Org tasks can have schedule and deadlines, so it works. I gave up on having everything show up on a GUI calendar, letting emacs agenda do it instead. Orgzly gives notifications, so all good!
;; (leaf org-gtasks ; sync google tasks, probably won't use it as google tasks don't support scheduling of tasks, only deadline
;; 	:straight (org-gtasks :type git :host github :repo "JulienMasson/org-gtasks")
;; 	:defer-config
;; 	(org-gtasks-register-account :name "S L"
;; 															 :directory +healtermon-gtasks-file
;; 															 :client-id +gclient-id
;; 															 :client-secret +gclient-secret))

;; takes too long to load, idk what to do with it either
;; (use-package emojify :config (global-emojify-mode))

;; ;; once I figured out what this is, I didnt' want it anymore
;; (use-package consult-company ; uses consult minibuffer thingy instead of company popup
;;   :after (consult company)
;;   :config
;;   (define-key company-mode-map [remap completion-at-point] #'consult-company)
;;   )


;; ;; Commented out 'cuz I couldn't get it to work nicely, the prompt detection was messed up. I think it's outdated. Lovely ideas tho.
;; (leaf aweshell
;;   :straight (aweshell :type git :host github :repo "manateelazycat/aweshell"
;;                       :files (:defaults (:exclude ("eshell-did-you-mean.el"
;;                                                    "eshell-prompt-extras.el"
;;                                                    "eshell-up.el"
;;                                                    "exec-path-from-shell.el"))))
;;   eshell-did-you-mean eshell-prompt-extras eshell-up exec-path-from-shell
;;   :require t
;;   :doc "eshell extensions, powered by eshell-prompt-extras"
;;   :after esh-mode
;;   )

"stop hs-mode from folding the header"
;;; Doesn't work yet / To-test 
(elpaca-leaf emms
	:defer-config
	(emms-minimalistic))

(elpaca-leaf ox-twbs ;; ox-html with more modern styling
	:disabled t ;; uhhh not now
	:after org-roam
	)

;; #emacs@Libera.Chat <thuna`> for youtube specifically, elfeed + youtube-dl + mpv is pretty much all you need
;; use this to get rss feed of a youtube channel:
;; https://www.youtube.com/feeds/videos.xml?channel_id=<CHANNEL-ID>
(elpaca-leaf elfeed
	:config
	(setq elfeed-feeds
				'("https://nullprogram.com/feed/"
					"https://ambrevar.xyz/atom.xml"
					"https://guix.gnu.org/feeds/blog.atom"
					"https://valdyas.org/fading/feed/"
					;; "https://www.reddit.com/r/emacs/.rss"
					"https://sachachua.com/blog/category/emacs-news/feed/"
					"https://explained-from-first-principles.com/feed.xml"))
	)
(elpaca-leaf elfeed-tube
	:after elfeed)

;; for haskell setup, refer to https://github.com/patrickt/emacs#haskell
(elpaca-leaf haskell-mode
	;; from https://github.com/patrickt/emacs/blob/master/readme.org
	;; :bind (:map haskell-mode-map
	;;             ("C-c a c" . haskell-cabal-visit-file)
	;;             ("C-c a i" . haskell-navigate-imports)
	;;             ("C-c m"   . haskell-compile)
	;;             ("C-c a I" . haskell-navigate-imports-return)
	;;             :map haskell-cabal-mode-map
	;;             ("C-c m"   . haskell-compile)))
	)
(elpaca-leaf nix-haskell-mode
	:disabled t  ; enable for cabal projects and have a look
	:after (nix-mode haskell-mode)
	:hook haskell-mode)

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

(elpaca-leaf pyenv-mode                      ; what does this do?
	)
(elpaca-leaf anaconda-mode
	:hook python-mode-hook)

(elpaca-leaf company-anaconda           ; anaconda backend for company-mode
	:after company anaconda-mode
	:defer-config
	(add-to-list 'company-backends '(company-anaconda :with company-capf)))

(elpaca-leaf pyimport ;; manage python imports from emacs! pyimport-insert-missing requires another buffer open with an example of importing the missing library
	:after python-mode)

;; https://old.reddit.com/r/emacs/comments/x6rg1u/rust_with_emacs/inb9qka/
(elpaca-leaf rust-mode
	:hook (rust-mode-hook . cargo-minor-mode)
	:defer-config
	(setq rust-format-on-save t)
	;; :custom-faces ;; left-over from use-package, idk how to translate this to leaf
	;; (rust-question-mark-face . ((t (:inherit font-lock-builtin-face :foreground "#ff0000" :weight bold))))
	)
(elpaca-leaf rustic
	:disabled t ;; in the name of speed
	;; from https://github.com/patrickt/emacs/blob/master/readme.org
	;; :bind (:map rustic-mode-map
	;;             ("C-c a t" . rustic-cargo-current-test)
	;;             ("C-c m" . rustic-compile))
	)
(elpaca-leaf cargo
	)

(elpaca-leaf racket-mode
	;; check out racket-xp-mode config in doom if you wanna use racket!
	)

(elpaca-leaf geiser-mit					; idk I can't get MIT-Scheme repl to connect to geiser
	;; TODO: try following https://github.com/karthink/.emacs.d/blob/master/lisp/setup-scheme.el
	:doc "use command geiser-mit to begin")

;; I feel like clojure LSP doesn't work the way I want it to yet, it doesn't show me the errors linted even right after lsp-bridge-goto-next-error or whatever it was
(defun +load-lsp-bridge () ;; call only after loading org-mode because otherwise org-list-allow-alphabetical bugs out... with-eval-after-load init-hook or org-mode doesn't even work :(. I found it has to do with some buffer-local variable!
  (interactive)
  (leaf lsp-bridge
    :load-path "~/stuff/compro/manateelazycat/lsp-bridge/"
    :straight posframe markdown-mode yasnippet
    :require lsp-bridge
    :global-minor-mode yas-global-mode
    :config (global-lsp-bridge-mode)))

(elpaca-leaf kotlin-mode)

;; currently broken due to sly's bug when describe-mode
(elpaca-leaf (mode-minder :type git :host github :repo "jdtsmith/mode-minder")
	:doc "used to look at major-mode hierarchy")

(elpaca-leaf lua-mode
	;; if want to use hammerspoon lua with annotations, see this: https://www.hammerspoon.org/Spoons/EmmyLua.html
	;; I 've already downloaded lua-language-server with homebrew
	)

(elpaca-leaf go-mode)
(elpaca-leaf go-snippets
	:after (go-mode yasnippet))
(elpaca-leaf gotest
	;; from https://github.com/patrickt/emacs/blob/master/readme.org
	;; commented out 'cuz the binds conflict with org-agenda
	;; :bind
	;; (go-mode-map
	;;  ("C-c a t" . go-test-current-test)
	;;  ("C-c a T" . go-test-current-file)
	;;  ("C-c a i" . go-import-add))
	)

(elpaca-leaf markdown-mode
	:mode ("README\\.md\\'" . gfm-mode)
	:hook (gfm-mode . visual-line-mode)
	:defer-config
	(setq markdown-command "multimarkdown")
	(setq markdown-fontify-code-blocks-natively t)
	(setq markdown-hide-urls nil)
	(custom-set-variables
	 '(markdown-code-lang-modes
		 (append
			'(("diff" . diff-mode)
				("hs" . haskell-mode)
				("html" . web-mode)
				("ini" . conf-mode)
				("js" . web-mode)
				("jsx" . web-mode)
				("md" . markdown-mode)
				("pl6" . raku-mode)
				("py" . python-mode)
				("rb" . ruby-mode)
				("rs" . rustic-mode)
				("sqlite3" . sql-mode)
				("ts" . web-mode)
				("tsx" . web-mode)
				("yaml". yaml-mode)
				("zsh" . sh-mode))
			markdown-code-lang-modes))))
(elpaca-leaf toml-mode)
(elpaca-leaf yaml-mode)

(elpaca-leaf dyalog-mode
	:doc "APL hahahahaha")

(elpaca-leaf dockerfile-mode)
(leaf dhall-mode)

;; commented out 'cuz it takes so long to load wtf
(elpaca-leaf elm-mode
	:hook
	(elm-mode-hook . elm-format-on-save-mode) ; requires elm-format to be installed(outside of emacs)
	(elm-mode-hook . elm-indent-mode))

;; lots of languages configured with leaf macro in this link, scala, OCaml, swift, VB, Web-mode, yarn,js,ts-comint,json,yaml,css,nxml,: https://github.com/ncaq/.emacs.d/blob/master/init.el#L797
(elpaca-leaf raku-mode
	:defer-config
	(setq raku-indent-offset 2))


(elpaca-leaf swift-mode)
(elpaca-leaf swift-helpful
	:after swift-mode
	:when +apexless
	:bind
	(swift-mode-map
	 ([remap lsp-describe-thing-at-point] . swift-helpful)))

(elpaca-leaf plantuml-mode
	:custom
	(plantuml-default-exec-mode . 'jar)
	(plantuml-jar-path . "~/bin/plantuml.jar")) ; no time to execute?

(elpaca-leaf ponylang-mode)
(elpaca-leaf pony-snippets)

;;; TESTING GROUNDS

(elpaca-leaf burly
	:doc "bookmark window or frame configurations")

(elpaca-leaf org-pdftools
	:doc "for links to specific pages in a PDF")

(leaf calendar
	:tag "built-in"
	:setq
	(calendar-date-style . 'iso) ;; YYYY/mm/dd
	(calendar-week-start-day . 1))

;; are these even useful?
(setq calendar-time-display-form '(24-hours ":" minutes))
(setq calendar-latitude 1.290270)
(setq calendar-longitude 103.851959)

;; COMMENTED OUT 'cuz shit is stalling elpaca and I don't know what (request-deferred) means for now
;; (elpaca-leaf org-gcal
;; 	:doc "sync google calendar events"
;; 	:init
;; 	(setq org-gcal-down-days 60
;; 				org-gcal-up-days 300
;; 				org-gcal-client-id +gclient-id
;; 				org-gcal-client-secret +gclient-secret
;; 				org-gcal-file-alist `(("healtermon@gmail.com" .  ,+healtermon-gcal-file)
;; 															;; ("another-mail@gmail.com" .  "~/more-mail.org")
;; 															)))

(elpaca-leaf calfw ;; calendar framework
	:commands cfw:open-calendar-buffer
	:defer-config
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


	)
(elpaca-leaf calfw-org
	:after calfw
	:require t
	:config
	(defun cfw:open-calendar ()
		(interactive)
		(let ((cp
					 (cfw:create-calendar-component-buffer
						:view 'month
						:contents-sources
						(list
						 (cfw:org-create-file-source ;this function is defined only in calfw-org, so it has to be here, loaded after both calfw and calfw-org.
							"main"  +healtermon-gcal-file  "#268bd2")
						 (cfw:org-create-file-source
							"tasks"  +healtermon-gtasks-file  "#859900")
						 ))))
			(switch-to-buffer (cfw:cp-get-buffer cp)))))

(elpaca-leaf (org-hyperscheduler :type git :host github :repo "dmitrym0/org-hyperscheduler" :files ("*")))

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


(elpaca-leaf forge
	:doc "for working with git forges")
(elpaca-leaf org-contacts)


;;; Julia Programming 
(elpaca-leaf julia-mode ; for julia programming, julia-vterm, ob-julia-vterm and julia-mode. Alternatively, also check out julia-repl
	:interpreter "julia"
	:defer-config
	(setenv "JULIA_NUM_THREADS" "auto") ;; default is 1
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

(elpaca-leaf julia-snail
	:hook julia-mode-hook)

(elpaca-leaf eglot-jl
	:after (eglot julia-mode)
	:hook julia-mode-hook
	;; Prevent timeout while installing LanguageServer.jl
	(julia-mode-hook . (lambda () (setq eglot-connect-timeout (max eglot-connect-timeout 120))))
	;; :defer-config
	;; (setq eglot-jl-language-server-project eglot-jl-base)
	)

;;; Clojure Programming 
(elpaca-leaf cider
	:defer-config
	(setq cider-repl-display-help-banner t))

(elpaca nil
	(leaf macrostep-geiser
		:doc "macrostep in CIDER!"
		:after cider
		:bind (cider-mode-map
					 :package cider
					 ("C-c e" . macrostep-mode))
		:hook (cider-mode-hook . macrostep-geiser-setup)))

(elpaca-leaf kibit-helper
	:doc "uses Clojure's core.logic to find functions in standard library that are abbreviations of your code")

(elpaca-leaf clj-refactor
	;; try cljr-add-missing-libspec!
	:defer-config
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
   "cd ~/stuff/compro/healtermon/sudoku-c-ncurses/ && make && ./sudoku-ncurses"))


(leaf cc-mode
  :hook
  (c-mode-hook . (lambda () (c-toggle-comment-style -1))) ; use // instead of /* */ so ts-fold can fold comments better)
  ;; (c-mode-common-hook
  ;;  .
  ;;  (lambda () ;; c-mode-base-map is only defined after cc-mode is loaded(only after visiting the c file), so putting it in the common-c-mode-hook works
  ;;    (define-key c-mode-base-map (kbd "C-c C-c") #'+compile-and-execute-in-vterm)))
	(c-mode-common-hook
	 .
	 (lambda () ;; to get "case" blocks in "switch-case" to be indented in Java, and all c-mode languages
		 (c-set-offset 'case-label '+))))

(elpaca-leaf cmake-mode
	:doc "for cmake files")

(elpaca-leaf csharp-mode
	:doc "C# syntax highlighting")

(elpaca-leaf modern-cpp-font-lock
	:doc "C++ syntax highlighting"
	:hook c++-mode-hook)

(elpaca-leaf disaster                          ; TODO TEST
	:doc "Disassemble C, C++ or Fortran code under cursor")

;;; chinese input!!! (C-\ to switch)

(elpaca-leaf pyim
	;; C-\ to switch between chinese and english input methods
	:require t
	:config
	(setq pyim-dcache-backend 'pyim-dregcache) ; 对低配置电脑使用更节省内存启动更快的引擎， *只对*拼音用户有效 
	(setq default-input-method "pyim")
	;; if the default dictionary isn't big enough, this may be of help (idk): https://github.com/redguardtoo/emacs.d#use-pinyin
	)
(elpaca-leaf pyim-basedict		; without using this, the suggestions aren't good hahaha Edit: well apparently even this ain't good enough, time to use TRIME
	:require t
	:config
	(pyim-basedict-enable)							; 拼音词库，五笔用户 *不需要* 此行设置
	)


;; ;; don't need it, the defaults handle it so well I don't need to auto-switch
;; (leaf sis
;;  doc: "comes with a list that you can put functions in, that when trigerred, will help change input source automatically"
;; 	:straight t
;; 	)


;;; The Rest
(elpaca-leaf (eat :type git
									:host codeberg
									:repo "akib/emacs-eat"
									:files ("*.el" ("term" "term/*.el") "*.texi"
													"*.ti" ("terminfo/e" "terminfo/e/*")
													("terminfo/65" "terminfo/65/*")
													("integration" "integration/*")
													(:exclude ".dir-locals.el" "*-tests.el")))
	:doc "Emulate-A-Terminal"
	:hook
	(eshell-load-hook . eat-eshell-mode)
	;; (eshell-load-hook . eat-eshell-visual-command-mode)
	:defer-config
	(setq eat-kill-buffer-on-exit t))

(elpaca-leaf erefactor ;; how to use?
	:doc "elisp refactoring"
	:hook
	;; highlight local variables
	(emacs-lisp-mode-hook . erefactor-lazy-highlight-turn-on)
	(lisp-interaction-mode-hook . erefactor-lazy-highlight-turn-on))

(elpaca-leaf yasnippet-snippets
	:after yasnippet)
(elpaca-leaf common-lisp-snippets
	:after (yasnippet sly))
(elpaca-leaf haskell-snippets
	:after (yasnippet haskell-mode))

(elpaca-leaf pcre2el
	:doc "Perl-Compatible RegEx to Emacs Lisp rx"
	:init
	;; from https://howardism.org/Technical/Emacs/eshell-why.html
	(defmacro prx (&rest expressions)
		"Convert the rx-compatible regular EXPRESSIONS to PCRE.
  Most shell applications accept Perl Compatible Regular Expressions."
		`(rx-let ((integer (1+ digit))
							(float   (seq integer "." integer))
							(b256    (seq (optional (or "1" "2"))
														(regexp "[0-9]\\{1,2\\}")))
							(ipaddr  (seq b256 "." b256 "." b256 "." b256))
							(time    (seq digit (optional digit) ":" (= 2 digit) (optional ":" (= 2 digit))))
							(email   (seq (1+ (regexp "[^,< ]")) "@" (1+ (seq (1+ (any alnum "-"))) ".") (1+ alnum)))
							(date    (seq (= 2 digit) (or "/" "-") (= 2 digit) (or "/" "-") (= 4 digit)))
							(ymd     (seq (= 4 digit) (or "/" "-") (= 2 digit) (or "/" "-") (= 2 digit)))
							(uuid    (seq (= 8 hex) "-" (= 3 (seq (= 4 hex) "-")) (= 12 hex)))
							(guid    (seq uuid)))
			 (rxt-elisp-to-pcre (rx ,@expressions)))))

(elpaca-leaf fennel-mode
	:config
	(leaf antifennel
		:doc "compile lua code to equivalent fennel code and view in buffer! Requires installed antifennel"
		:hook
		lua-mode-hook))

;; ;; commented out 'cuz it breaks em-smart for now
;; (leaf eshell-did-you-mean               
;;   :straight t
;;   :doc "supposed to be a do-u-mean-this? kinda thing, but I think the eshell extensions
;; I loaded messed up its setup function (eshell-did-you-mean-setup)." ;TODO fix this, read the string on left
;;   :after esh-mode
;;   :init
;;   (eshell-did-you-mean-setup))


;; (leaf exec-path-from-shell
;;   :straight t
;;   :require t
;;   :doc "TODO: well read up (below link) boi, set things right."
;;   :url "https://github.com/purcell/exec-path-from-shell"
;;   :after esh-mode
;;   :config
;;   (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
;;     (add-to-list 'exec-path-from-shell-variables var))
;;   (when (memq window-system '(mac ns x))
;;     (exec-path-from-shell-initialize))
;;   )

(elpaca-leaf pulsar
	:global-minor-mode pulsar-global-mode
	:hook
	(next-error-hook . pulsar-pulse-line)
	;; integration with the `consult' package:
	(consult-after-jump-hook . pulsar-recenter-top)
	(consult-after-jump-hook . pulsar-reveal-entry)

	;; integration with the built-in `imenu':
	(imenu-after-jump-hook . pulsar-recenter-top)
	(imenu-after-jump-hook . pulsar-reveal-entry)
	:config
	(add-to-list 'pulsar-pulse-functions #'isearch-repeat) ; idk how to do this?
	)

;; ;; Commented out 'cuz it's broken, TODO: figure out what's broken, its a nice package and worth it
;; (leaf volatile-highlights
;;   :straight t
;; 	:require t
;; 	)
;; (straight-use-package 'volatile-highlights)
;; (volatile-highlights-mode)

(elpaca-leaf gcmh
	:blackout gcmh
	:doc "GCMH, the Garbage Collector Magic Hack, changes GC threshold based on user activity"
	;; :custom
	;; (gcmh-verbose . t) 										;idk why this takes no time to execute
	:global-minor-mode t
	)


(elpaca-leaf (pp+ :type git :host github :repo "emacsmirror/pp-plus")
	;; :require t
	)

;; (leaf activity-watch-mode 							;; TODO: figure out what's broken, it's a nice package yet again...
;; 	:straight t
;; 	)

(elpaca-leaf wolfram
	:doc "query wolfram alpha and receive its reply in a (non-interactive) buffer."
	:bind
	;; (modi-mode-map
	;;  ("C-x / a" . wolfram-alpha)
	;;  )
	:config
	;; 1. First get a Wolfram ID (or developer account) at
	;;    https://developer.wolframalpha.com/portal/signin.html
	;;    Your "Wolfram ID" will be the email you used to sign up for the
	;;    developer account.
	;; 2. Once you sign in with the Wolfram ID at
	;;    https://developer.wolframalpha.com/portal/myapps/, click on "Get an
	;;    AppID" to get your Wolfram API or AppID.
	;; 3. Follow the steps where you put in your app name and description, and
	;;    you will end up with an AppID that looks like "ABCDEF-GHIJKLMNOP",
	;;    where few of those characters could be numbers too.
	(setq wolfram-alpha-app-id +wolfram-alpha-app-id)
	)

(elpaca-leaf wolfram-mode
	:doc "major mode for editing Mathematica text files")




(elpaca-leaf lsp-mode
	;; :disabled t 
	:hook
	((;; c-mode-hook
		;; c++-mode-hook
		;; python-mode-hook ;; Disabled as it crashes my emacs after running for a while? lmao what's going on? I was doing python coding, was it python interpreter and infinite running programs that crashed emacs? Though I keyboard-interrupted it...
		;; haskell-mode-hook
		java-mode-hook
		) . lsp-deferred)
	(lsp-mode-hook . lsp-enable-which-key-integration)

	:init
	;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l"), must be set before lsp-mode loads
	(setq lsp-keymap-prefix "C-c l")
	(setq read-process-output-max (* 1024 1024)) ;; 1MiB, for lsp to be faster
	:config
	(setq lsp-idle-delay 0.1)
	(setq lsp-lens-enable t)
	(setq lsp-completion-show-detail t)
	(setq lsp-completion-show-kind t)
	(setq lsp-keep-workspace-alive nil)

	(setq lsp-headerline-breadcrumb-enable nil) ; annoying!
	(setq lsp-intelephense-multi-root nil)			; don't scan unnecessary projects
	(with-eval-after-load 'lsp-intelephense
		(setf (lsp--client-multi-root (gethash 'iph lsp-clients)) nil))

	;; ;; python-related
	;; (setq lsp-pyls-plugins-pycodestyle-enabled nil)
	(setq lsp-pylsp-plugins-pycodestyle-enabled          nil )     
	(setq lsp-pylsp-plugins-yapf-enabled                 nil )     
	(setq lsp-pylsp-plugins-black-enabled                nil )     
	(setq lsp-pylsp-plugins-flake8-enabled               nil )     
	(setq lsp-pylsp-plugins-mccabe-enabled               nil )     
	(setq lsp-pylsp-plugins-pylint-enabled               nil )     
	;; (setq lsp-pylsp-plugins-preload-enabled              nil )     
	(setq lsp-pylsp-plugins-autopep8-enabled             nil )     
	(setq lsp-pylsp-plugins-pyflakes-enabled             nil )     
	(setq lsp-pylsp-plugins-jedi-hover-enabled           nil )     
	(setq lsp-pylsp-plugins-pydocstyle-enabled           nil )     
	;; (setq lsp-pylsp-plugins-jedi-symbols-enabled         nil )     
	;; (setq lsp-pylsp-plugins-jedi-completion-enabled      nil )    
	;; (setq lsp-pylsp-plugins-jedi-definition-enabled      nil )      
	;; (setq lsp-pylsp-plugins-jedi-references-enabled      nil )      
	;; (setq lsp-pylsp-plugins-rope-completion-enabled      nil )      
	;; (setq lsp-pylsp-plugins-jedi-signature-help-enabled  nil )      
	)

(elpaca-leaf lsp-ui
	:after lsp-mode
	:hook lsp-mode-hook
	:bind
	(lsp-ui-mode-map
	 ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	 ([remap xref-find-references] . lsp-ui-peek-find-references))
	:init
	(setq lsp-ui-doc-enable t)
	(setq lsp-ui-doc-header t)
	(setq lsp-ui-doc-include-signature t)
	(setq lsp-ui-doc-border (face-foreground 'default))
	(setq lsp-ui-doc-delay 1.5
				lsp-ui-doc-position 'bottom
				lsp-ui-doc-max-width 100
				)
	(setq lsp-ui-sideline-show-code-actions nil)
	(setq lsp-ui-sideline-delay 0.05)

	(setq lsp-ui-doc-show-with-cursor t)
	(setq lsp-ui-doc-show-with-mouse t)
	(setq lsp-ui-sideline-show-diagnostics nil)

	)

;; (leaf lsp-treemacs
;; 	:after lsp-mode
;; 	:require t
;; 	:config (setq treemacs-space-between-root-nodes nil)
;; 	:commands lsp-treemacs-errors-list)

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

;; Debug Adaptor Protocol(DAP)-related
(elpaca-leaf dap-mode
	:after lsp-mode
	:config
	(leaf dap-cpptools
		:require t													; afterwards run dap-cpptools-setup
		:doc "for C and C++"
		)
	(leaf dap-python
		:require t													; requires pip install "ptvsd>=4.2"
		)
	:global-minor-mode dap-auto-configure-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; ;; for CS1010S picture language
;; (add-hook 'python-mode-hook (lambda ()
;; 															(bind-keys :map python-mode-map
;; 																				 ("M-c" . (lambda () (interactive) (python-shell-send-string "clear_all()")))
;; 																				 :map inferior-python-mode-map
;; 																				 ("M-c" . (lambda () (interactive) (python-shell-send-string "clear_all()"))))
;; 															))

(leaf elec-pair
	:hook
	((python-mode-hook
		java-mode-hook
		) . electric-pair-local-mode))

(elpaca-leaf hackernews)

(elpaca-leaf (eaf :type git :host github :repo "emacsmirror/eaf")
	;; check out this guy's config for EAF: https://github.com/MatthewZMD/.emacs.d#eaf
	)

(use-package detached
	;; copied from https://old.reddit.com/r/emacs/comments/104ud8k/how_do_you_use_the_terminal_during_a_coding/j3cn11r/
	;; https://emacsconf.org/2022/talks/detached/
	:disabled t
  :init
  (detached-init)
  :bind (;; Replace `async-shell-command' with `detached-shell-command'
         ([remap async-shell-command] . detached-shell-command)
         ;; Replace `compile' with `detached-compile'
         ([remap compile] . detached-compile)
         ([remap recompile] . detached-compile-recompile)
         ;; Replace built in completion of sessions with `consult'
         ([remap detached-open-session] . detached-consult-session))
  :custom ((detached-show-output-on-attach t)
           (detached-terminal-data-command system-type)))

(elpaca-leaf matlab-mode
	;; ref: https://swang251.github.io/2021/12/03/Matlab-in-Emacs/
	;; karthink's setup: ~/stuff/compro/karthink/.emacs.d/lisp/setup-matlab.el 
	:if +apexless
	:init
	(setq matlab-shell-command "/Applications/MATLAB_R2022a.app/bin/matlab")
	(setq matlab-shell-command-switches '("-nodesktop" "-nosplash"))
	:config
	(matlab-cedet-setup))


(elpaca-leaf csv-mode)


;; Forth Programming
(elpaca-leaf forth-mode
	:pre-setq
	`(forth-executable . ,+forth-executable)
	:config
	(require 'forth-block-mode)
	(require 'forth-interaction-mode)
	)

;; Factor Programming
(elpaca-leaf fuel
	:config
	(setq fuel-factor-root-dir +fuel-factor-root-dir))

(elpaca-leaf (verilog-ext :host github :repo "gmlarumbe/verilog-ext"
													:files ("verilog-ext.el" "verilog-ts-mode.el" "snippets")) 
	:after verilog-mode
	:hook verilog-mode-hook
	:config
	(verilog-ext-mode-setup))

;; Org Website!? https://www.youtube.com/watch?v=AfkrzFodoNw
;; set up with github/sourcehut pages: https://www.youtube.com/watch?v=za99DwdZEyg
(elpaca-leaf htmlize)



(elpaca-leaf solidity-mode)

(elpaca-leaf djvu)

(leaf doc-view
	;; doc-view-mode tips
	;; change doc-view-resolution to increase resolution
	;; 
	;; here are some helpful functions to change fit
	;; doc-view-fit-page-to-window
	;; doc-view-fit-width-to-window
	;; doc-view-fit-height-to-window
	;; ...-fit-... functions
	;; u probably need to change resolution and fit that to certain dimensions if u want to rescale or display on another screen.
	:setq
	(doc-view-resolution . 300)						; default is 100, too low, really
	)


(leaf lilypond-mode
	:config
	(add-to-list 'load-path "/opt/homebrew/Cellar/lilypond/2.24.1/share/emacs/site-lisp/lilypond/")
	(load "lilypond-init"))
;; TODO SET IT UP WTF
(elpaca-leaf (lyqi :host github :repo "nsceaux/lyqi") ; you need to "make lyqi.elc" after downloading, follow instructions in README: https://github.com/nsceaux/lyqi
	:hook
	LilyPond-mode-hook
	:defer-config
	(add-to-list 'auto-mode-alist '("\\.ly$" . lyqi-mode))
	(add-to-list 'auto-mode-alist '("\\.ily$" . lyqi-mode))

	;; lilypond code from https://old.reddit.com/r/emacs/comments/k0mld2/using_lilypond_in_emacs/gdjonva/
	:custom
	(lyqi:preferred-languages . '("English"))
	(lyqi:midi-backend . 'osx)
	(lyqi:pdf-command . "open")

	:config
	(defun ap/lilypond-make ()
		"Run Lilypond and play MIDI file."
		(interactive)
		(lyqi:compile-ly)
		(lyqi:open-midi))
	) 


;; ;; lilypond code from https://old.reddit.com/r/emacs/comments/k0mld2/using_lilypond_in_emacs/gdjonva/
;; (use-package lyqi-mode
;;   :load-path "~/src/archive/emacs/lyqi/"
;;   :custom
;;   (lyqi:midi-command "timidity")
;;   (lyqi:pdf-command "okular")
;;   (lyqi:prefered-languages '("English"))

;; ;; from https://github.com/nsceaux/lyqi
;; (custom-set-variables
;;  '(lyqi:prefered-languages '(italiano nederlands))
;;  '(lyqi:prefered-octave-mode 'absolute)
;;  '(lyqi:keyboard-mapping 'azerty)
;;  '(lyqi:custom-key-map '(("w" "\\trill")
;;                          ("x" (space-around . "\\appoggiatura"))
;;                          ("n" "~")
;;                          ("," "(")
;;                          (";" ")")
;;                          (":" "[")
;;                          ("=" "]")
;;                          ("\C-m" lyqi:insert-pipe-and-return)
;;                          ("$" lyqi:insert-pipe-and-return)))
;;  '(lyqi:projects-language '(("~/src/lilypond" nederlands)
;;                             ("~/Documents/LilyPond" italiano)
;;                             ("~/Documents/LilyPond/Haendel/Opera/GiulioCesare" nederlands)))
;;  '(lyqi:midi-backend 'osx)
;;  '(lyqi:pdf-command "open")
;;  '(lyqi:midi-command "open")
;;  ;; ...
;;  )



;; ;; commented out 'cuz I wanna try the "amazing" lsp-java for working with lsp-mode
;; (leaf eglot-java
;; 	;; installed jdtls again, to "/Users/s/.emacs.d/share/eclipse.jdt.ls", the other version is installed with homebrew
;; 	:straight t
;; 	:hook java-mode-hook
;; 	:config
;; 	(setq eglot-java-workspace-folder "/Users/s/stuff/university/CS2040 Data Structures and Algorithms/AY2023ST2Src/")
;; 	)


;; gnus guide : https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/blob/master/gnus-guide-en.org


;; ;; 'cuz idk how to use it yet, under too much time pressure, gonna stick to basics
;; ;; commented out 'cuz in favor of more modern solutions, this is the oldest one...
;; homepage: https://jdee.sourceforge.net/rootpage.html
;; user guide: https://jdee.sourceforge.net/jdedoc/html/jde-ug/jde-ug.html
;; (leaf jdee
;; 	:straight t
;; 	:config
;; 	(setq jdee-server-dir "~/myJars/")
;; 	)
(elpaca-leaf gptel
	:init
	(setq gptel-api-key +openai-api-key-kor) ;this shit needs paid account...
	(setq gptel-model "gptel-3.5-turbo"))


(elpaca-leaf flycheck
	;; :global-minor-mode global-flycheck-mode
	)

(elpaca-leaf lsp-java
	;; guide linked from lsp-java github page: https://xpressrazor.wordpress.com/2020/11/04/java-programming-in-emacs/
	;; man tried all sorts of other packages, as shown at the start of this video: https://www.youtube.com/watch?v=Bbjxn9yVNJ8
	:require t
	:after lsp-mode
	:init
	(setq lsp-java-java-path +lsp-java-java-path)
	(leaf dap-java
		:require t)	
	)


(elpaca-leaf mix
	:doc "provides a minor mode for integration with Mix, a build tool that ships with Elixir."
	:hook
	(elixir-mode-hook . mix-minor-mode))

(elpaca-leaf exunit
	:doc "ExUnit integration."
	:hook elixir-mode-hook)

(elpaca-leaf web-mode)


(elpaca-leaf elixir-mode
	;; elixir-mode doesn't use treesitter, and so is obsolete since emacs 29 since 29 comes with treesitter
	:emacs< 29
	:hook
	+elixir-prettify-symbols-setup
	)

;; elixir development setup: https://medium.com/@victor.nascimento/elixir-emacs-development-2023-edition-1a6ccc40629
(elpaca-leaf elixir-ts-mode
	:doc "run (elixir-ts-install-grammar) to install grammar"
	:emacs>= 29
	:hook 
	+elixir-prettify-symbols-setup
	)
(add-to-list 'exec-path +elixir-ls-build-output-path t)

(defun +elixir-prettify-symbols-setup ()
	(push '(">=" . ?\u2265) prettify-symbols-alist)
	(push '("<=" . ?\u2264) prettify-symbols-alist)
	(push '("!=" . ?\u2260) prettify-symbols-alist)
	(push '("==" . ?\u2A75) prettify-symbols-alist)
	(push '("=~" . ?\u2245) prettify-symbols-alist)
	(push '("<-" . ?\u2190) prettify-symbols-alist)
	(push '("->" . ?\u2192) prettify-symbols-alist)
	(push '("<-" . ?\u2190) prettify-symbols-alist)
	(push '("|>" . ?\u25B7) prettify-symbols-alist))

(elpaca-leaf '(biome :host github :repo "SqrtMinusOne/biome")
	:doc "interface to Open Meteo Free Forecasting API. Docs here: https://open-meteo.com/en/docs#latitude=1.2897&longitude=103.8501&hourly=temperature_2m,visibility&daily=temperature_2m_min,winddirection_10m_dominant&current_weather=true&timezone=Asia%2FSingapore"
	:require t
	:config (setq biome-query-coords
								'(("Helsinki, Finland" 60.16952 24.93545)
									("Berlin, Germany" 52.52437 13.41053)
									("Dubai, UAE" 25.0657 55.17128)
									("Singaopre, SG" 1.280270 103.851959))))


(elpaca-leaf sicp
	:doc "TeXInfo version of SICP (same format as info manual), via your package manager straight from MELPA!"
	:require t)

;; this sets up all ocaml-related stuff already! go read the loaded file for more details.

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line




(elpaca-leaf scala-mode
	:hook
	lsp
	lsp-format-before-save
	:config
	)
(elpaca-leaf lsp-metals)
(elpaca-leaf sbt-mode
	:preface
	(defun +start-sbt-here ()
		(interactive)
		(require 'sbt-mode)
		(setq sbt:buffer-project-root default-directory)
		(sbt-start)
		)
	:doc "for usage with Scala Build Tool(sbt).
If the directory found by sbt-start finds the wrong directory,
set sbt:buffer-project-root to nil and try again from the scala file."
	:commands sbt-start sbt-command
	:bind
	(sbt-mode-map
	 ("M-t" . comint-previous-input)
	 )
	(scala-mode-map
	 ("C-c C-c" . sbt-do-run))
	:config
	;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
	;; allows using SPACE when in the minibuffer
	(substitute-key-definition
	 'minibuffer-complete-word
	 'self-insert-command
	 minibuffer-local-completion-map)

	;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
	(defvar sbt:program-options '("-Dsbt.supershell=false"))
	)

(elpaca-leaf (ammonite-term-repl :host github :repo "zwild/ammonite-term-repl") )


;; ELPACA TUTORIAL WITH WEIRD ESOLANG AS EXAMPLE LMAO
;; after the initial config has run,
;; to queue something, give elpaca a recipe, ":repo" can be just the full url to the git repo, or a combination of ":host github" and ":repo wkirschbaum/surface-ts-mode". Note it doesn't have the ".git" behind. This will turn into some URL which I don't know how to view tbh... just know that if it works, elpaca-browse <insert package name here> will work and direct to the correct website.
;; TODO how to check what's in the queues?
;; after running the elpaca macro, u will see "*elpaca log*" buffer. This buffer means there's no error in processing the recipe(TODO find out for sure).
;; then (elpaca-process-queues) to process. This will return a long-arse elisp thing, mentioning the recipe given, and the steps generated that just ran.
;; check .emacs.d/elpaca/repos and .emacs.d/elpaca/build to see if it worked.
;; it will link the package as well, so if you "M-x load-library RET surface-ts-mode", you should see it.
;; The recipe given can be quoted or unquoted, unlike in straight, as "elpaca" is a macro. To keep compatibility with straight, I choose to quote, and also in the recipe, use ":host" instead of ":fetcher".
;; elpaca-fetch git-fetches the repos, requiring the git repos to already be inside .emacs.d/elpaca/repos folder. elpaca-update is hence probably git-pull TODO check if my guess is correct, that elpaca-update does git-pull?
(elpaca-leaf '(surface-ts-mode :repo "https://github.com/wkirschbaum/surface-ts-mode.git")
	:doc "the installation is fucked, go look at the git repo"
	:require t)
;; (elpaca-process-queues)

;; if you evaluate something like
;; (elpaca random-name)
;; it may not give u an error immediately, and fail when processing this order. DO check.

(defun +elpaca-process-queues ()
	(interactive)
	(elpaca-process-queues))



;; COMMENTED OUT 'cuz I need to reconfig to make the LSP and stuff work, anyways no benefit for now.
;; (elpaca-leaf '(nix-ts-mode :host github :repo "remi-gelinas/nix-ts-mode"))
;; (elpaca-leaf '(ocaml-ts-mode :host github :repo "dmitrig/ocaml-ts-mode"))
;; (leaf python-ts-mode
;; 	:doc "remember to install grammar, it will clone and build for u if you press enter and reply yes enough"
;; 	:config
;; 	(setq major-mode-remap-alist
;; 				'((python-mode . python-ts-mode)))
;; 	)
;; (elpaca-leaf clojure-ts-mode
;; 	:config
;; 	(push '(clojure-mode . clojure-ts-mode) major-mode-remap-alist)
;; 	(push '(clojurec-mode . clojurec-ts-mode) major-mode-remap-alist)
;; 	(push '(clojurescript-mode . clojurescript-ts-mode) major-mode-remap-alist))




;; ;; COMMENTED OUT 'cuz org-heatmap is a very cool package, but low priority to set up
;; (use-package org-habit
;; 	:elpaca t
;; 	:custom
;; 	(org-habit-graph-column 1)
;; 	(org-habit-preceding-days 10)
;; 	(org-habit-following-days 1)
;; 	(org-habit-show-habits-only-for-today nil))
;; (use-package org-heatmap
;; 	:init
;; 	(add-to-list 'load-path "/path-to/emacsql/")
;; 	(add-to-list 'load-path "/path-to/org-heatmap/")
;; 	(require 'org-heatmap)
;; 	:after (org)
;; 	:custom
;; 	;; (org-agenda-files '("/path-to/org-heatmap/examples/examples.org"))
;; 	(org-heatmap-db-location "/path-to/org-heatmap/examples/org-heatmap.db")
;; 	:config
;; 	(org-heatmap-mode))


;; from https://web.archive.org/web/20220605183815/https://with-emacs.com/posts/tips/quit-current-context/

(defun +keyboard-quit-context ()
  "Quit current context.

This function is a combination of `keyboard-quit' and
`keyboard-escape-quit' with some parts omitted and some custom
behavior added."
  (interactive)
  (cond ((region-active-p)
         ;; Avoid adding the region to the window selection.
         (setq saved-region-selection nil)
         (let (select-active-regions)
           (deactivate-mark)))
        ((eq last-command 'mode-exited) nil)
        (current-prefix-arg
         nil)
        (defining-kbd-macro
         (message
          (substitute-command-keys
           "Quit is ignored during macro defintion, use \\[kmacro-end-macro] if you want to stop macro definition"))
         (cancel-kbd-macro-events))
        ((active-minibuffer-window)
         (when (get-buffer-window "*Completions*")
           ;; hide completions first so point stays in active window when
           ;; outside the minibuffer
           (minibuffer-hide-completions))
         (abort-recursive-edit))
        (t
         ;; if we got this far just use the default so we don't miss
         ;; any upstream changes
         (keyboard-quit))))
;; (global-set-key [remap keyboard-quit] #'keyboard-quit-context+)


(elpaca-leaf hyperspace
	:doc "in between QuickSilver and keyword URLs. Try M-x hyperspace RET wp James Clerk Maxwell")


(defun +compile-and-execute-in-vterm ()
  (interactive)
  (+execute-in-vterm
   "cd \"/Users/s/stuff/university/CS2100 Computer Organisation/AY2022_23/Labs/Lab2/\" && gcc -g -o lab2b-modified lab2b-modified.c && ./lab2b-modified"))


;; god knows how to fix my life.org file
;; (setq org-element-use-cache nil)



(elpaca-leaf mips-mode
	:doc "for use with spim, as QTspim is too fucked in user interface.
Doesn't have commands for stepping by n values, nor printing all register values... nope! can't use! no time to make my own, will stick with QTspim.")


;; for browsing Common Lisp HyperSpec locally. in slime repl, do (ql:quickload "clhs"), then (clhs:print-emacs-setup-form), and follow instructions.
;; from https://stackoverflow.com/a/25656974, then just "C-c C-d C-h" when point is over a symbol to quick-lookup
(load "/Users/s/.quicklisp/clhs-use-local.el" t)
(define-key global-map (kbd "<f11>") #'+toggle-undecorated-frame-maximised)

(elpaca-leaf (wat-ts-mode :host github :repo "nverno/wat-ts-mode")
	:require t
	:config
	;; wat
	(add-to-list
	 'treesit-language-source-alist
	 '(wat "https://github.com/wasm-lsp/tree-sitter-wasm" nil "wat/src"))
	;; wast
	(add-to-list
	 'treesit-language-source-alist
	 '(wast "https://github.com/wasm-lsp/tree-sitter-wasm" nil "wast/src"))
	)

(elpaca-leaf (iota :host sourcehut :repo "mango/iota.el")
	:doc
	"Iota is a very simple package that provides an easy way to replace strings in
   a region with an incrementing count.  This can be useful in a variety of
   settings for doing things such as creating numbered lists, creating
   enumerations, and more.  The substrings to match are defined by the variable
   ‘iota-regexp’ while the functions ‘iota’ and ‘iota-complex’ offer simple- and
   advanced methods of enumeration.  You probably do not want to use these
   functions in your emacs-lisp code; they are intended for interactive use.")

;; ;; COMMENTED OUT 'cuz I don't need it, it stays because it's so hard to find anyways; googling "powershell emacs" doesn't surface this
;; (elpaca-leaf koopa-mode
;; 	:doc "mode for powershell lol!")

(elpaca-leaf ascii-table
	:doc "godsend, who needs https://asciitable.com when you have
 this offline table right in emacs?")
(elpaca-leaf crystal-mode)
(elpaca-leaf literate-calc-mode)

;; usage of define-skeleton : https://codeberg.org/uncomfyhalomacro/erudite-macs


(elpaca-leaf smalltalk-mode
	:doc "just for reading smalltalk files")


(elpaca-leaf logos)


