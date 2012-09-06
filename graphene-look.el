;; Defaults for Graphene visual settings.

;; Scroll bars off
(scroll-bar-mode -1)

;; Toolbar off
(tool-bar-mode -1)

;; Non-blinking cursor
(blink-cursor-mode -1)

;; Soft-wrap lines
(visual-line-mode 1)

;; In GUI mode, use a bigger frame, larger line spacing, and sr-speedbar
(if window-system
    (progn
      (add-to-list 'default-frame-alist '(width .  180))
      (add-to-list 'default-frame-alist '(height .  70))
      (add-to-list 'default-frame-alist '(line-spacing . 2))
      ;; Seems to fix graphical glitches with linum
      (set-fringe-mode '(8 . 0))
      (sr-speedbar-open))
    ;; Menu bar off in text mode
  (menu-bar-mode -1))

;; Load theme extensions
(load-theme 'graphene t)

(provide 'graphene-look)
