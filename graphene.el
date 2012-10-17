;;; graphene.el --- Newbie-friendly defaults
;;
;; Copyright (c) 2012 Robert Dallas Gray
;;
;; Author: Robert Dallas Gray <mail@robertdallasgray.com>
;; URL: https://github.com/rdallasgray/graphene
;; Version: 0.1
;; Keywords: defaults

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Graphene is a collection of default settings and functionality to make Emacs a little friendlier.

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

(defcustom graphene-speedbar-auto t
  "Whether graphene should open sr-speedbar when a project is loaded."
  :type 'boolean
  :group 'graphene)

(defcustom graphene-linum-auto t
  "Whether graphene should enable linum-mode with prog-modes."
  :type 'boolean
  :group 'graphene)

(defcustom graphene-autopair-auto t
  "Whether graphene should enable autopair-mode with prog-modes."
  :type 'boolean
  :group 'graphene)

(defcustom graphene-autocomplete-auto t
  "Whether graphene should enable autocomplete-mode with prog-modes."
  :type 'boolean
  :group 'graphene)

(defcustom graphene-parens-auto t
  "Whether graphene should enable show-paren-mode."
  :type 'boolean
  :group 'graphene)

(defcustom graphene-prog-mode-hooks '(prog-mode-hook)
  "List of hooks to be treated as prog-mode."
  :type 'sexp
  :group 'graphene)

(defvar graphene-prog-mode-hook nil
  "A hook to be run on entering a de facto prog mode.")

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
