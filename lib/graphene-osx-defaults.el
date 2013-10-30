;;; graphene-osx-defaults.el --- Graphene defaults for OS X
;;
;; Copyright (c) 2013 Robert Dallas Gray
;;
;; Author: Robert Dallas Gray <mail@robertdallasgray.com>
;; URL: https://github.com/rdallasgray/graphene
;; Version: 0.4.0
;; Keywords: defaults

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Graphene is a set of default settings and functionality to make Emacs a little friendlier.
;; This file defines extra defaults for OS X-based systems.

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

(require 'graphene-helper-functions)
(require 'graphene-speedbar)
(require 'exec-path-from-shell)

;; Standard fonts
(unless graphene-default-font
  (setq graphene-default-font "Menlo-12"))
(unless graphene-fixed-pitch-font
  (setq graphene-variable-pitch-font "Lucida Grande-12"))
(unless graphene-variable-pitch-font
  (setq graphene-fixed-pitch-font "Menlo-12"))

;; Move deleted files to trash
(setq delete-by-moving-to-trash t)

;; Set default path from shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Mac-like key defaults
(global-set-key (kbd "s-n") 'create-new-buffer)
(global-set-key (kbd "s-N") 'new-emacs-instance)
(global-set-key (kbd "s-{") 'previous-buffer)
(global-set-key (kbd "s-}") 'next-buffer)
(global-set-key (kbd "s-<right>") 'end-of-line)
(global-set-key (kbd "s-<left>") 'beginning-of-line)

(define-key speedbar-mode-map (kbd "<kp-enter>") 'speedbar-item-rename)
(define-key speedbar-mode-map (kbd "<s-backspace>") 'speedbar-item-delete)
(define-key speedbar-mode-map (kbd "<s-i>") 'speedbar-item-info)
(define-key speedbar-mode-map (kbd "<s-r>") 'speedbar-refresh)

(provide 'graphene-osx-defaults)
