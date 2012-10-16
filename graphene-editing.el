;;; graphene-editing.el --- Graphene editing defaults
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
;; The editing defaults target the text editing environment, with particular relevance to prog modes. 

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

(require 'smart-tab)
(require 'auto-complete)
(require 'auto-complete-config)
(require 'autopair)
(require 'multi-web-mode)

;; Use flymake, autocomplete, paredit, linum in prog modes
;; ESC to get out of autocomplete menu
(ac-config-default)
(define-key ac-completing-map (kbd "ESC") 'ac-stop)
(setq ac-delay 0.2
      ac-auto-show-menu 1.2
      ac-quick-help-delay 2.5
      ac-candidate-limit 30)
(setq-default ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))

(add-hook 'prog-mode-hook
          (lambda ()
            (progn
              (auto-complete-mode t)
              (autopair-mode t)
              (linum-mode t)
              (setq linum-format " %4d ")
              (local-set-key (kbd "RET") 'newline-and-indent))))

;; Fix newline-and-indent in ruby-mode
(add-hook 'ruby-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))

;; Delete marked text on typing (delete-selection-mode not compatible with autopair)
(cua-selection-mode t)

;; Soft-wrap lines
(global-visual-line-mode t)

;; Nicer scrolling with mouse wheel/trackpad.
(global-set-key [wheel-down] (lambda () (interactive) (scroll-up-command 1)))
(global-set-key [wheel-up] (lambda () (interactive) (scroll-down-command 1)))
(global-set-key [double-wheel-down] (lambda () (interactive) (scroll-up-command 2)))
(global-set-key [double-wheel-up] (lambda () (interactive) (scroll-down-command 2)))
(global-set-key [triple-wheel-down] (lambda () (interactive) (scroll-up-command 4)))
(global-set-key [triple-wheel-up] (lambda () (interactive) (scroll-down-command 4)))

;; Use tab for autocomplete.
(global-smart-tab-mode t)

;; Show matching parens immediately.
(show-paren-mode t)
(setq show-paren-delay 0)

;; Non-blinking cursor
(blink-cursor-mode -1)

;; Don't use tabs for indent; replace tabs with two spaces.
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; Character encodings default to utf-8.
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; apply syntax highlighting to all buffers
(global-font-lock-mode t)

;; Use multi-web-mode for editing code embedded in HTML.
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")
                  (ruby-mode "<\\%=\\|<\\% " "\\%>")))
(setq mweb-filename-extensions '("html" "phtml" "rhtml"))
(multi-web-global-mode 1)

(provide 'graphene-editing)
