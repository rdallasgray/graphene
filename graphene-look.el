;; Defaults for Graphene visual settings.

(require 'graphene-speedbar)

;; Scroll bars off
(scroll-bar-mode -1)

;; Toolbar off
(tool-bar-mode -1)

;; Non-blinking cursor
(blink-cursor-mode -1)

;; Soft-wrap lines
(visual-line-mode 1)

;; Get graphene font defaults or use system defaults.
(unless (boundp 'graphene-default-font)
  (defvar graphene-default-font (face-font 'default)))
(unless (boundp 'graphene-variable-pitch-font)
  (defvar graphene-variable-pitch-font (face-font 'variable-pitch)))
(unless (boundp 'graphene-fixed-pitch-font)
  (defvar graphene-fixed-pitch-font (face-font 'fixed-pitch)))

;; Set graphene default fonts
(set-frame-font graphene-default-font)
(set-face-font 'variable-pitch graphene-variable-pitch-font)
(set-face-font 'fixed-pitch graphene-fixed-pitch-font)

;; In GUI mode, use a bigger frame, larger line spacing, and sr-speedbar
(if window-system
    (progn
      (add-to-list 'default-frame-alist '(width .  170))
      (add-to-list 'default-frame-alist '(height .  60))
      (add-to-list 'default-frame-alist '(line-spacing . 2))
      ;; Seems to fix graphical glitches with linum
      (set-fringe-mode '(8 . 0))
      (sr-speedbar-open))
    ;; Menu bar off in text mode
  (menu-bar-mode -1))

;; Load theme extensions
(add-hook 'after-init-hook
          (lambda () (load-theme 'graphene t)))

(provide 'graphene-look)
