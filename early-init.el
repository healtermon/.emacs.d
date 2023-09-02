;;; early-init.el --- Emacs pre package.el & GUI configuration -*- lexical-binding:t -*-
;;; Code:
(setq
 gc-cons-threshold most-positive-fixnum ; Inhibit garbage collection during startup
 gc-cons-percentage 0.6 ; Portion of heap used for allocation.  Defaults to 0.1. don't need to change during startup when gc-cons-threshold is so high, it never triggered even on 0.1
 package-quickstart nil ; Prevent package.el loading packages prior to their init-file
 package-enable-at-startup nil
 ad-redefinition-action 'accept     ; Disable warnings from legacy advice system
 ;; inhibit-startup-screen t           ; Reduce noise at startup
 inhibit-startup-echo-area-message user-login-name
 inhibit-default-init t                 ; don't load default.el file
 initial-scratch-message nil
 auto-mode-case-fold nil  ; Use case-sensitive `auto-mode-alist' for performance
 ;; fast-but-imprecise-scrolling t ; More performant rapid scrolling over unfontified regions
 ffap-machine-p-known 'reject    ; Don't ping things that look like domain names
 frame-inhibit-implied-resize t  ; Inhibit frame resizing for performance
 idle-update-delay 1.0           ; slow down UI updates down, default is 0.5
 inhibit-compacting-font-caches t ; speed up unicode loading, but uses more memory
 read-process-output-max (* 1024 1024) ; Increase how much is read from processes in a single chunk.
 redisplay-skip-fontification-on-input t ; Inhibits it for better scrolling performance.
 command-line-x-option-alist nil ; Remove irreleant command line options for faster startup
 ;; select-active-regions 'only ; Emacs hangs when large selections contain mixed line endings. ; Sam: What is this?
 ;; auto-save-list-file-prefix nil ; Disable auto-save
 ;; create-lockfiles nil ; Disable lockfiles
 ;; make-backup-files nil ; Disable backup files
 vc-follow-symlinks t                   ; Do not ask about symlink following
 ;; user-emacs-directory (expand-file-name "~/.cache/emacs/") ; No littering
 custom-file (concat user-emacs-directory "custom.el") ; Place all "custom" code in a temporary file
 use-short-answers t                                   ; y/n for yes/no
 load-prefer-newer t
 ;; native-comp-async-report-warnings-errors 'silent ; I don't think I can use these
 ;; native-comp-async-jobs-number     10
 )

;; Skipping a bunch of regular expression searching in the file-name-handler-alist should improve start time.
;; it'll be set back after startup.
(unless (daemonp)
  (let ((original file-name-handler-alist))
    (setq-default file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook
              (lambda ()
                ;; Startup might have changed it, so preserve those additions
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist
                                           original)))))))

;; Modified from Doom Emacs. `load-file' sends the "Loading X..."
;; message, which triggers a redisplay, which has an appreciable
;; effect on startup times. This supresses the message.
(define-advice load-file (:override (file) silence)
  (load (expand-file-name file) nil 'nomessage 'nosuffix))
(add-hook 'emacs-startup-hook
          (lambda ()
            (advice-remove 'load-file #'load-file@silence)))


(defun +gc-after-focus-change ()
  "Run GC when frame loses focus."
  (run-with-idle-timer
   5 nil
   (lambda () (unless (frame-focus-state) (garbage-collect)))))
(defun +reset-early-init-values ()
  (run-with-idle-timer
   1 nil
   (lambda ()
     (setq
      ;; Set gc threshold back to normal
      ;; if pauses are too long, decrease the threshold
      ;; if pauses are too frequent, increase the threshold
      gc-cons-threshold (* 128 1024 1024) ; increase garbage collection limit to 100MiB, default is 0.8MB, measured in bytes
      )
     (message "gc_th&fh-alist ^_^, Emacs ready in %s with %d garbage collections."
              (format "%.2f seconds"
                      (float-time
                       (time-subtract after-init-time before-init-time)))
              gcs-done)
     (when (boundp 'after-focus-change-function)
       (add-function :after after-focus-change-function #'+gc-after-focus-change)))))
(add-hook 'emacs-startup-hook #'+reset-early-init-values)

;; This config supports a few computers, mostly broken by now though.
(defun +system-name? (name-string)
	(string= system-name name-string))
(defvar +apexless (and (eq system-type 'darwin)) "Whether Emacs is running on my macbook pro 14-inch m1 pro") ;; system-name Apexless/Apexless.local/???
(defvar +termux   (and (+system-name? "localhost")) "Whether Emacs is running on termux (probably on my phone)")
(defvar +mango    (and (+system-name? "mango")) "Whether Emacs is running on my linux desktop running NixOS")
(defvar +asses    (and (+system-name? "ASSES-UX310UQK")) "Whether Emacs is running on ASSES-UX310UQK (my poly laptop)")
(defvar +durian   (and (+system-name? "DURIAN")) "Whether Emacs is running on kor's poly laptop running Manjaro")
(defvar +nix-on-droid  (+system-name? "nix-on-droid-placeholder-name") "Whether emacs is running on nix-on-droid")

;; should they be here? idk. Why did alexlugit put them in early-init?
(defvar +font-size 141)
(defvar +default-font "mononoki Nerd Font")
(defvar +fixed-font "mononoki Nerd Font") ; for info
(defvar +variable-font "Sarasa Mono SC")  ; variable-pitch font
(defvar +CJK-font "LXGW WenKai Mono") ; Chinese, Japanese, Korean characters

;; don't need 'em UI, disable here to hopefully start-up faster, replaces code below which was in init.el
(push '(tool-bar-lines . 0) default-frame-alist)
;; (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(push '(menu-bar-lines . 0) default-frame-alist)
;; (menu-bar-mode -1)
(push '(vertical-scroll-bars) default-frame-alist)
;; (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ;; scroll bar not useful as its behaviour is weird(too lazy to learn), and there's a percentage to show vertical position so...

(when +apexless
	;; ;; COMMENTED OUT 'cuz I copied functions that add and remove these from frame now, more convenient.
	;; (push '(undecorated . t) default-frame-alist)
	;; ;; https://www.reddit.com/r/emacs/comments/b2r2oj/is_it_possible_to_disable_or_hide_the_titlebar_in/
	;; (push '(drag-internal-border . 1) default-frame-alist)
	;; (push '(internal-border-width . 5) default-frame-alist)

	;; ;; COMMENTED OUT 'cuz toggle-frame-maximized works now (emacs 29 railwaycat/emacsmacport)
	;; ;; for fullscreen
	;; (setq default-frame-alist	;; no (toggle-frame-maximized) as you can't move or resize the window without undoing it, and no fullscreen 'cuz stupid notch
	;; 			(append ;; these parameters perfectly fit my screen, like (toggle-frame-maximized), gotten by (frame-height)+1 and (frame-width)
	;; 			 '((top . 0)
	;; 				 (left . 0)
	;; 				 (width . 187)
	;; 				 (height . 63))
	;; 			 default-frame-alist))
	)


(message "%s" default-frame-alist) ;; i guess since this is in early-init.el, this message goes straight to stdout (run emacs via command line in foreground)!!!
;; After early-init.el, the frame is generated. So for example, the below line is useless:
;; (toggle-frame-fullscreen)


;; Local Variables:
;; no-byte-compile: t
;; End:
