(setq
 gc-cons-threshold most-positive-fixnum ; Inhibit garbage collection during startup
 package-quickstart nil ; Prevent package.el loading packages prior to their init-file
 package-enable-at-startup nil
 ad-redefinition-action 'accept ; Disable warnings from legacy advice system
 ;; inhibit-startup-message t ; Reduce noise at startup
 inhibit-startup-echo-area-message user-login-name
 ;; inhibit-startup-screen t
 ;; inhibit-default-init t
 initial-scratch-message nil
 auto-mode-case-fold nil ; Use case-sensitive `auto-mode-alist' for performance
 ;; fast-but-imprecise-scrolling t ; More performant rapid scrolling over unfontified regions
 ffap-machine-p-known 'reject ; Don't ping things that look like domain names
 frame-inhibit-implied-resize t ; Inhibit frame resizing for performance
 idle-update-delay 1.0  ; slow down UI updates down, default is 0.5
 inhibit-compacting-font-caches t 
 read-process-output-max (* 1024 1024) ; Increase how much is read from processes in a single chunk.
 redisplay-skip-fontification-on-input t ; Inhibits it for better scrolling performance.
 command-line-x-option-alist nil ; Remove irreleant command line options for faster startup
 ;; select-active-regions 'only ; Emacs hangs when large selections contain mixed line endings. ; Sam: What is this?
 ;; auto-save-list-file-prefix nil ; Disable auto-save
 ;; create-lockfiles nil ; Disable lockfiles
 ;; make-backup-files nil ; Disable backup files
 vc-follow-symlinks t ; Do not ask about symlink following
 ;; user-emacs-directory (expand-file-name "~/.cache/emacs/") ; No littering
 ;; custom-file (concat user-emacs-directory "custom.el") ; Place all "custom" code in a temporary file
 use-short-answers t ; y/n for yes/no
 )

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Set gc threshold back to normal
            ;; if pauses are too long, decrease the threshold
            ;; if pauses are too frequent, increase the threshold
            (setq gc-cons-threshold (* 128 1024 1024)) ; increase garbage collection limit to 100MiB, default is 0.8MB, measured in bytes
            (setq gc-cons-percentage 0.6);; Portion of heap used for allocation.  Defaults to 0.1.
            ))

;; Local Variables:
;; no-byte-compile: t
;; End:
