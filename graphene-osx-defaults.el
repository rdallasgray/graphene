(require 'graphene-helper-functions)
(require 'graphene-speedbar)

;; Default fonts
(defvar graphene-default-font "Menlo-12")
(defvar graphene-variable-pitch-font "Lucida Sans-12")
(defvar graphene-fixed-pitch-font "Menlo-12")

;; Delete files by moving them to the OS X trash
(setq delete-by-moving-to-trash t)

(global-set-key (kbd "s-w") 'kill-default-buffer)
(global-set-key (kbd "s-n") 'create-new-buffer)
(global-set-key (kbd "s-N") 'new-emacs-instance)

(define-key speedbar-mode-map (kbd "<kp-enter>") 'speedbar-item-rename)
(define-key speedbar-mode-map (kbd "<s-backspace>") 'speedbar-item-delete)
(define-key speedbar-mode-map (kbd "<s-i>") 'speedbar-item-info)
(define-key speedbar-mode-map (kbd "<s-r>") 'speedbar-refresh)

(provide 'graphene-osx-defaults)
