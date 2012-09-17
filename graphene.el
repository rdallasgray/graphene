;;; graphene.el --- Graphene bootstrap
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

;; Add the theme directory to custom-theme-load-path
(let ((root (file-name-directory (if load-file-name load-file-name buffer-file-name))))
  (add-to-list 'custom-theme-load-path (concat root "theme/")))

;; Set up the system-based defaults
(let ((graphene-sys
  (cond ((eq system-type 'darwin) "osx")
        ((eq system-type 'gnu/linux) "linux")
        ((eq system-type 'windows-nt) "windows")
        (t "other"))))
  (defvar graphene-sys-defaults (intern (format "graphene-%s-defaults" graphene-sys))
    "Symbol for the specific system-based defaults file."))

;; Define custom settings
(defgroup graphene nil
  "Graphene custom settings.")

(defcustom graphene-use-sr-speedbar t
  "Whether graphene should use sr-speedbar."
  :type 'boolean
  :group 'graphene)

(defcustom graphene-speedbar-always t
  "Whether graphene should open sr-speedbar at launch."
  :type 'boolean
  :group 'graphene)

;; Require necessary files
(require 'graphene-helper-functions)
(require 'graphene-editing)
(require 'graphene-env)
(require 'graphene-speedbar)
(require 'graphene-projects)
(require 'graphene-keys)
(require graphene-sys-defaults)
(require 'graphene-look)

(provide 'graphene)
