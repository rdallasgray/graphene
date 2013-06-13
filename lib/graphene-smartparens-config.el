;;; graphene-smartparens-config.el --- Graphene configuration for smartparens
;;
;; Copyright (c) 2013 Robert Dallas Gray
;;
;; Author: Robert Dallas Gray <mail@robertdallasgray.com>
;; URL: https://github.com/rdallasgray/graphene
;; Version: 0.1.35
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
(defun gp/sp/create-newline-and-enter-sexp (id action context)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (previous-line)
  (indent-according-to-mode))

(defun gp/sp/newline-indent-and-return (id action context)
  "Post command, put trailing pair on newline and return to point."
  (when (eq action 'insert)
    (save-excursion
      (newline)
      (indent-according-to-mode))))

(defun gp/sp/words-before-p ()
  "Are there words before point?"
  (looking-back "[^\s]"))

(sp-pair "{" nil :post-handlers
         '(:add ((lambda (id action context) (gp/sp/create-newline-and-enter-sexp)) "RET")))
(sp-pair "[" nil :post-handlers
         '(:add ((lambda (id action context) (gp/sp/create-newline-and-enter-sexp)) "RET")))

;; Ruby-specific pairs and handlers
(when graphene-autopair-ruby
  (sp-local-pair 'ruby-mode "class " "end"
                 :unless '(sp-in-string-p gp/sp/words-before-p)
                 :actions '(insert)
                 :post-handlers '(:add gp/sp/newline-indent-and-return))
  (sp-local-pair 'ruby-mode "def " "end"
                 :unless '(sp-in-string-p gp/sp/words-before-p)
                 :actions '(insert)
                 :post-handlers '(:add gp/sp/newline-indent-and-return))
  (sp-local-pair 'ruby-mode "do " "end"
                 :unless '(sp-in-string-p)
                 :actions '(insert)
                 :post-handlers '(:add gp/sp/newline-indent-and-return))
  (sp-local-pair 'ruby-mode "if " "end"
                 :unless '(sp-in-string-p gp/sp/words-before-p)
                 :actions '(insert)
                 :post-handlers '(:add gp/sp/newline-indent-and-return))
  (sp-local-pair 'ruby-mode "begin" "end"
                 :unless '(sp-in-string-p)
                 :actions '(insert)
                 :post-handlers '(:add gp/sp/newline-indent-and-return))
  (sp-local-pair 'ruby-mode "unless " "end"
                 :unless '(sp-in-string-p gp/sp/words-before-p)
                 :actions '(insert)
                 :post-handlers '(:add gp/sp/newline-indent-and-return))
  (sp-local-pair 'ruby-mode "|" "|"
                 :unless '(sp-in-string-p)))

;; Markdown
(sp-local-pair 'markdown-mode "*" "*"
               :unless '(sp-in-string-p)
               :actions '(insert wrap))

;; Don't need c-comments in strings -- they frustrate filename globs
(sp-pair "/*" nil :unless '(sp-in-string-p))

;; Don't need quotes to pair following words
(sp-pair "\"" nil :unless '(sp-point-after-word-p))
(sp-pair "'" nil :unless '(sp-point-after-word-p))

(provide 'graphene-smartparens-config)
