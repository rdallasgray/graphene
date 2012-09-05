;; Defaults for the Emacs environment.

;; Use Smex for recent M-x commands a la ido.
(require 'smex)
(smex-initialize)

;; Mark word, sexp, line, ...
(require 'expand-region)

;; No startup splash screen.
(setq inhibit-startup-message t)

;; Color theme everywhere.
(setq color-theme-is-global t)

;; Add directory info to distinguish buffers.
(setq uniquify-buffer-name-style 'forward)

;; Don't make me type out 'yes' and 'no'
(fset 'yes-or-no-p 'y-or-n-p)

;; Don't create backup~ files.
(setq make-backup-files nil)

;; Don't create #autosave# files
(setq auto-save-default nil)

;; Defaults for Ido.
(setq ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-everywhere t)
(ido-mode 1)

;; Allow commands which would be disabled by default.
(put 'ido-complete 'disabled nil)
(put 'ido-exit-minibuffer 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'autopair-newline 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(provide 'graphene-env)
