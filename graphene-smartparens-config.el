;;; graphene-smartparens-config.el --- Graphene configuration for smartparens
;;
;; Copyright (c) @YEAR Robert Dallas Gray
;;
;; Author: Robert Dallas Gray <mail@robertdallasgray.com>
;; URL: https://github.com/rdallasgray/graphene
;; Version: @VERSION
;; Keywords: defaults
;;
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

(let ((pairs '(("{" nil) ("[" nil))))
  (mapc
   (lambda (pair)
     (sp-pair (-first-item pair)
              (-last-item pair)
              :post-handlers
              '(:add ("||\n[i]" "RET"))))
   pairs))

;; Fix for ruby-mode, which appears to override handlers
(add-hook 'ruby-mode-hook
          (lambda ()
            (sp-local-pair 'ruby-mode
                           "{"
                           nil
                           :post-handlers
                           '(:add ("||\n[i]" "RET")))))

(sp-local-pair
 '(markdown-mode gfm-mode) "*" "*" :unless '(sp-in-string-p) :actions '(insert wrap))

(provide 'graphene-smartparens-config)
