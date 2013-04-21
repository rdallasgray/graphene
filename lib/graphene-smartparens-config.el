;;; graphene-smartparens-config.el --- Graphene configuration for smartparens
;;
;; Copyright (c) 2013 Robert Dallas Gray
;;
;; Author: Robert Dallas Gray <mail@robertdallasgray.com>
;; URL: https://github.com/rdallasgray/graphene
;; Version: 0.1.17
;; Keywords: defaults

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Graphene is a set of default settings and functionality to make Emacs a little friendlier.
;; This file provides extra configuration for Smartparens (https://github.com/Fuco1/smartparens).

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

;; Newline and indent inside {} and []

(defvar gp/sp/post-command-count 0
  "Number of commands called after a pair has been opened.")

(defun gp/sp/create-newline-and-enter-sexp ()
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (previous-line)
  (indent-according-to-mode))

(defun gp/sp/await-newline-post-command ()
  (if (> gp/sp/post-command-count 0)
      (progn
        (remove-hook 'post-command-hook 'gp/sp/await-newline-post-command)
        (setq gp/sp/post-command-count 0))
    (progn
      (setq gp/sp/post-command-count (+ gp/sp/post-command-count 1))
      (when (or (eq this-command 'newline) (eq this-command 'newline-and-indent))
        (gp/sp/create-newline-and-enter-sexp)
        (setq gp/sp/post-command-count 0)))))

(defun gp/sp/await-newline (id action context)
  (when (eq action 'insert)
    (add-hook 'post-command-hook 'gp/sp/await-newline-post-command)))

(sp-pair "{" nil :post-handlers '(:add gp/sp/await-newline))
(sp-pair "[" nil :post-handlers '(:add gp/sp/await-newline))

(provide 'graphene-smartparens-config)
