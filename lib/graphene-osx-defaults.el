;;; graphene-osx-defaults.el --- Graphene defaults for OS X
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

(unless graphene-default-font
  (setq graphene-default-font "Menlo-12"))
(unless graphene-variable-pitch-font
  (setq graphene-variable-pitch-font "Lucida Grande-12"))
(unless graphene-fixed-pitch-font
  (setq graphene-fixed-pitch-font "Menlo-12"))

(setq delete-by-moving-to-trash t)

(when (memq window-system '(mac ns))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(provide 'graphene-osx-defaults)

;;; graphene-osx-defaults.el ends here
