;; Defaults for Graphene visual settings.

;; Scroll bars off
(scroll-bar-mode -1)

;; Toolbar off
(tool-bar-mode -1)

;; Non-blinking cursor
(blink-cursor-mode -1)

;; Soft-wrap lines
(visual-line-mode 1)

;; Default fonts
(set-default-font graphene-default-font)
(set-face-attribute 'variable-pitch nil :family graphene-variable-pitch-family)
(set-face-attribute 'fixed-pitch nil :family graphene-fixed-pitch-family)

;; In GUI mode, use a bigger frame, larger line spacing, and sr-speedbar
(if window-system
    (progn
      (add-to-list 'default-frame-alist '(width .  180))
      (add-to-list 'default-frame-alist '(height .  70))
      (add-to-list 'default-frame-alist '(line-spacing . 2))
      ;; Seems to fix graphical glitches with linum
      (set-fringe-mode '(8 . 0))
      (load-theme 'solarized-light t)
      (sr-speedbar-open))
  (progn
    ;; Dark theme and menu bar off in text mode
    (load-theme 'solarized-dark t)
    (menu-bar-mode -1)))

;; Load theme extensions
(load-theme 'graphene t)

(provide 'graphene-look)
