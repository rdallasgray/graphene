;;; graphene-smartparens-config.el --- Graphene configuration for smartparens
;;
;; Copyright (c) 2013 Robert Dallas Gray
;;
;; Author: Robert Dallas Gray <mail@robertdallasgray.com>
;; URL: https://github.com/rdallasgray/graphene
;; Version: 0.4.0
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

(defun gp/sp/pair-on-newline (id action context)
  "Put trailing pair on newline and return to point."
  (save-excursion
    (newline)
    (indent-according-to-mode)))

(defun gp/sp/pair-on-newline-and-indent (id action context)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (gp/sp/pair-on-newline id action context)
  (indent-according-to-mode))

(sp-pair "{" nil :post-handlers
         '(:add ((lambda (id action context)
                   (gp/sp/pair-on-newline-and-indent id action context)) "RET")))
(sp-pair "[" nil :post-handlers
         '(:add ((lambda (id action context)
                   (gp/sp/pair-on-newline-and-indent id action context)) "RET")))

;; Ruby-specific pairs and handlers
(when graphene-autopair-ruby
  (require 'smartparens-ruby))

;; Markdown
(sp-local-pair '(markdown-mode gfm-mode) "*" "*"
               :unless '(sp-in-string-p)
               :actions '(insert wrap))

;; Don't need quotes to pair following words
(sp-pair "\"" nil :unless '(sp-point-after-word-p))
(sp-pair "'" nil :unless '(sp-point-after-word-p))
;; Except in HTML
(sp-local-pair 'html-mode "\"" nil :unless '(:rem sp-point-after-word-p))
;; CoffeeScript PyStrings
(push 'coffee-mode sp-autoescape-string-quote-if-empty)
(provide 'graphene-smartparens-config)
