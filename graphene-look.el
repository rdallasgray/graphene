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

;; Default frame geometry
(defvar graphene-frame-geometry '(160 70 0 0))

;; Name of the file to which to load and save geometry.
(defvar graphene-geometry-file
  (concat user-emacs-directory ".emacs.geometry"))

;; Load geometry settings from file if it exists.
(defun graphene-load-frame-geometry ()
  (if (file-readable-p graphene-geometry-file)
      (with-temp-buffer
        (insert-file-contents graphene-geometry-file)
        (setq graphene-frame-geometry
              (mapcar
               'string-to-number
               (split-string (buffer-string) " "))))))

;; Save geometry settings.
(defun graphene-save-frame-geometry ()
  (with-temp-file graphene-geometry-file
    (let ((geometry-string (format "%d %d %d %d" (frame-width) (frame-height) 0 0)))
          (message geometry-string)
          (insert geometry-string))))

;; In GUI mode, use a bigger frame, larger line spacing, and sr-speedbar.
(if window-system
    (progn
      (graphene-load-frame-geometry)
      (let ((f-width (car graphene-frame-geometry)) (f-height (car (cdr graphene-frame-geometry))))
        (message (format "%d %d" f-width f-height))
        (add-to-list 'default-frame-alist (cons 'width f-width))
        (add-to-list 'default-frame-alist (cons 'height f-height)))
      (add-to-list 'default-frame-alist '(line-spacing . 2))
      (set-frame-font graphene-default-font)
      (set-face-font 'variable-pitch graphene-variable-pitch-font)
      (set-face-font 'fixed-pitch graphene-fixed-pitch-font)
      ;; Seems to fix graphical glitches with linum
      (set-fringe-mode '(8 . 0))
      (add-hook 'kill-emacs-hook 'graphene-save-frame-geometry))
    ;; Menu bar off in text mode
  (menu-bar-mode -1))

;; Load theme extensions
(add-hook 'after-init-hook
          (lambda () (load-theme 'graphene t)))

(provide 'graphene-look)
