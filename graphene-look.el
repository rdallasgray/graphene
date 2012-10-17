;;; graphene-look.el --- Graphene defaults for the UI
;;
;; Copyright (c) 2012 Robert Dallas Gray
;;
;; Author: Robert Dallas Gray <mail@robertdallasgray.com>
;; URL: https://github.com/rdallasgray/graphene
;; Version: 0.1
;; Keywords: defaults

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Graphene is a set of default settings and functionality to make Emacs a little friendlier.
;; This file defines default settings and functionality for UI-centric matters.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code: 

(require 'graphene-speedbar)
(require 'graphene-theme)

;; Less flickery display
(setq redisplay-dont-pause t)

;; Scroll bars off
(scroll-bar-mode -1)

;; Toolbar off
(tool-bar-mode -1)

;; Get graphene font defaults or use system defaults.
(unless (boundp 'graphene-default-font)
  (defvar graphene-default-font (face-font 'default)
    "The universal default font."))
(unless (boundp 'graphene-variable-pitch-font)
  (defvar graphene-variable-pitch-font (face-font 'variable-pitch)
    "The default font for variable-pitch."))
(unless (boundp 'graphene-fixed-pitch-font)
  (defvar graphene-fixed-pitch-font (face-font 'fixed-pitch)
    "The default font for fixed-pitch."))

(defvar graphene-geometry-file
  (concat user-emacs-directory ".graphene-geometry")
  "The file where frame geometry settings are saved.")

(defun graphene-load-frame-geometry ()
  "Load saved frame geometry settings."
  (if (file-readable-p graphene-geometry-file)
      (with-temp-buffer
        (insert-file-contents graphene-geometry-file)
        (read (buffer-string)))
    '(160 70 0 0)))

(defun graphene-save-frame-geometry ()
  "Save current frame geometry settings."
  (with-temp-file graphene-geometry-file
    (print (graphene-get-geometry) (current-buffer))))

(defun graphene-get-geometry ()
  "Get the current geometry of the active frame, subtracting the width of the Speedbar if necessary."
  (list (frame-width) (frame-height) (frame-parameter nil 'top) (frame-parameter nil 'left)))

;; In GUI mode, restore frame geometry, use larger line spacing, set default fonts
(if window-system
    (progn
      (let ((geom (graphene-load-frame-geometry)))
        (let ((f-width (car geom))
              (f-height (cadr geom))
              (f-top (caddr geom))
              (f-left (cadddr geom)))
          (add-to-list 'default-frame-alist (cons 'width f-width))
          (add-to-list 'default-frame-alist (cons 'height f-height))
          (add-to-list 'default-frame-alist (cons 'top f-top))
          (add-to-list 'default-frame-alist (cons 'left f-left))))
      (add-to-list 'default-frame-alist '(line-spacing . 2))
      (set-face-font 'default graphene-default-font)
      (set-face-font 'variable-pitch graphene-variable-pitch-font)
      (set-face-font 'fixed-pitch graphene-fixed-pitch-font)
      ;; Seems to fix some of the graphical glitches with linum
      (set-fringe-mode '(8 . 0))
      (add-hook 'kill-emacs-hook 'graphene-save-frame-geometry))
  ;; Menu bar off in text mode
  (menu-bar-mode -1))

;; Load theme extensions
(add-hook 'after-init-hook
          (lambda () (load-theme 'graphene t)))

(defadvice load-theme
  (after load-graphene-theme (theme &optional no-confirm no-enable) activate)
  "Load the graphene theme extensions after loading a theme."
  (when (not (equal theme 'graphene))
    (load-theme 'graphene t)))

(provide 'graphene-look)
