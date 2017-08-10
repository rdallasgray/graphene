;;; graphene-look.el --- Graphene defaults for the UI
;;
;; Copyright (c) 2017 Robert Dallas Gray
;;
;; Author: Robert Dallas Gray <mail@robertdallasgray.com>
;; URL: https://github.com/rdallasgray/graphene
;; Version: 0.9.8
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

(defcustom graphene-default-font nil
  "The universal default font."
  :type 'string
  :group 'graphene)

(defcustom graphene-variable-pitch-font nil
  "The font to use in the variable-pitch face."
  :type 'string
  :group 'graphene)

(defcustom graphene-fixed-pitch-font nil
  "The font to use in the fixed-pitch face."
  :type 'string
  :group 'graphene)

(let ((sys
       (cond ((eq system-type 'darwin) "osx")
             ((eq system-type 'gnu/linux) "linux")
             ((eq system-type 'windows-nt) "windows")
             (t "other"))))
  (require (intern (format "graphene-%s-defaults" sys))))


;; Work around Emacs frame sizing bug when line-spacing
;; is non-zero, which impacts e.g. grizzl, and allow resizing when
;; vertical modes are enabled or user has customized graphene-resize-minibuffer
(defcustom graphene-resize-minibuffer nil
  "Whether the minibuffer should be resizable."
  :type 'bool
  :group 'graphene)

(defun graphene-resize-minibuffer-p ()
  (or (-any? 'featurep '(ivy grizzl ido-vertical-mode))
      graphene-resize-minibuffer))

(defun graphene-minibuffer-setup-hook ()
  (if (graphene-resize-minibuffer-p)
      (set (make-local-variable 'line-spacing) 0)
    (setq resize-mini-windows nil)))

(add-hook 'minibuffer-setup-hook
          'graphene-minibuffer-setup-hook)

(add-hook 'ido-minibuffer-setup-hook
          'graphene-minibuffer-setup-hook)

(setq redisplay-dont-pause t)

(mapc (lambda (mode)
        (when (fboundp mode) (funcall mode -1)))
      '(scroll-bar-mode tool-bar-mode blink-cursor-mode))

(defvar graphene-geometry-file
  (expand-file-name ".graphene-geometry" user-emacs-directory)
  "The file where frame geometry settings are saved.")

(defun graphene-load-frame-geometry ()
  "Load saved frame geometry settings."
  (if (file-readable-p graphene-geometry-file)
      (with-temp-buffer
        (insert-file-contents graphene-geometry-file)
        (read (buffer-string)))
    '(100 40 0 0)))

(defun graphene-save-frame-geometry ()
  "Save current frame geometry settings."
  (with-temp-file graphene-geometry-file
    (print (graphene-get-geometry) (current-buffer))))

(defun graphene-get-geometry ()
  "Get the current geometry of the active frame."
  (list (frame-width) (frame-height) (frame-parameter nil 'top) (frame-parameter nil 'left)))

(defun graphene-set-geometry ()
  "Set the default frame geometry using the values loaded from graphene-geometry-file."
  (let ((geom (graphene-load-frame-geometry)))
    (let ((f-width (nth 0 geom))
          (f-height (nth 1 geom))
          (f-top (nth 2 geom))
          (f-left (nth 3 geom)))
      (setq default-frame-alist
            (append default-frame-alist
                    `((width . ,f-width)
                      (height . ,f-height)
                      (top . ,f-top)
                      (left . ,f-left)))))))

(defun graphene-set-fonts ()
  "Set up default fonts."
  (unless graphene-default-font
    (setq graphene-default-font (face-font 'default)))
  (unless graphene-variable-pitch-font
    (setq graphene-variable-pitch-font (face-font 'variable-pitch)))
  (unless graphene-fixed-pitch-font
    (setq graphene-fixed-pitch-font (face-font 'fixed-pitch))))

(defun graphene-look-startup-after-init ()
  "Load defaults for the overall Graphene look -- to be called after loading the init file so as to pick up custom settings."
  (if window-system
      (progn
        (graphene-set-geometry)
        (add-hook 'kill-emacs-hook 'graphene-save-frame-geometry)
        (setq-default line-spacing 2)
        (graphene-set-fonts)
        (add-to-list 'default-frame-alist `(font . ,graphene-default-font))
        (set-face-font 'default graphene-default-font)
        (set-face-font 'variable-pitch graphene-variable-pitch-font)
        (set-face-font 'fixed-pitch graphene-fixed-pitch-font)
        (add-to-list 'default-frame-alist '(internal-border-width . 0))
        (set-fringe-mode '(8 . 0))
        (load-theme 'graphene-meta t)
        (defadvice load-theme
          (after load-graphene-meta-theme (theme &optional no-confirm no-enable) activate)
          "Load the graphene theme extensions after loading a theme."
          (when (not (equal theme 'graphene-meta))
            (load-theme 'graphene-meta t))))
    (when (not (eq system-type 'darwin))
      (menu-bar-mode -1))
    ;; Menu bar always off in text mode
    (menu-bar-mode -1)))

(add-hook 'after-init-hook 'graphene-look-startup-after-init)

(provide 'graphene-look)

;;; graphene-look.el ends here
