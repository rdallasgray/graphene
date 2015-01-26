;;; graphene-keys.el --- Graphene keybindings
;;
;; Copyright (c) 2015 Robert Dallas Gray
;;
;; Author: Robert Dallas Gray <mail@robertdallasgray.com>
;; URL: https://github.com/rdallasgray/graphene
;; Version: 0.8.2
;; Keywords: defaults

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Graphene is a set of default settings and functionality to make Emacs a little friendlier.
;; This small set of keybindings enables base Graphene functionality.

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

(global-set-key (kbd "C-x k")
                'kill-default-buffer)
(global-set-key (kbd "C-x C-k")
                'kill-buffer-and-window)
(global-set-key (kbd "C-c n")
                'create-new-buffer)
(global-set-key (kbd "C-c N")
                'new-emacs-instance)
(global-set-key (kbd "C-;")
                'insert-semicolon-at-end-of-line)
(global-set-key (kbd "M-RET")
                'newline-anywhere)
(global-set-key (kbd "C-M-;")
                'comment-current-line-dwim)
(global-set-key (kbd "C->")
                'increase-window-height)
(global-set-key (kbd "C-<")
                'decrease-window-height)
(global-set-key (kbd "C-,")
                'decrease-window-width)
(global-set-key (kbd "C-.")
                'increase-window-width)
(global-set-key (kbd "M-x")
                'smex)
(global-set-key (kbd "M-X")
                'smex-major-mode-commands)
(global-set-key (kbd "C-c s")
                'sr-speedbar-select-window)

(provide 'graphene-keys)
