;;; early-init.el --- Emacs pre package.el & GUI configuration -*- lexical-binding:t -*-
;;; Code:
(setq
 gc-cons-threshold most-positive-fixnum ; Inhibit garbage collection during startup
 gc-cons-percentage 1.0
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
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun +gc-after-focus-change ()
  "Run GC when frame loses focus."
  (run-with-idle-timer
   5 nil
   (lambda () (unless (frame-focus-state) (garbage-collect)))))
(defun +reset-early-init-values ()
  (run-with-idle-timer
   1 nil
   (lambda ()
     (setq file-name-handler-alist default-file-name-handler-alist
           ;; Set gc threshold back to normal
           ;; if pauses are too long, decrease the threshold
           ;; if pauses are too frequent, increase the threshold
           gc-cons-threshold (* 128 1024 1024) ; increase garbage collection limit to 100MiB, default is 0.8MB, measured in bytes
           gc-cons-percentage 0.6 ; Portion of heap used for allocation.  Defaults to 0.1.
           )
     (message "gc-cons-threshold & file-name-handler-alist restored, Emacs ready in %s with %d garbage collections."
              (format "%.2f seconds"
                      (float-time
                       (time-subtract after-init-time before-init-time)))
              gcs-done)
     (when (boundp 'after-focus-change-function)
       (add-function :after after-focus-change-function #'+gc-after-focus-change)))))
(add-hook 'emacs-startup-hook #'+reset-early-init-values)

;; should they be here? idk. Why did alexlugit put them in early-init?
(defvar +font-size 141)
(defvar +default-font "mononoki Nerd Font")
(defvar +fixed-font "mononoki Nerd Font") ; for info
(defvar +variable-font "Sarasa Mono SC")  ; variable-pitch font
(defvar +CJK-font "LXGW WenKai Mono") ; Chinese, Japanese, Korean characters

;; Local Variables:
;; no-byte-compile: t
;; End:
