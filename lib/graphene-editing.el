;;; graphene-editing.el --- Graphene editing defaults
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

;; Delete marked text on typing
(delete-selection-mode t)

;; Soft-wrap lines
(global-visual-line-mode t)

;; Linum format to avoid graphics glitches in fringe
(setq linum-format " %4d ")

;; Don't use tabs for indent; replace tabs with two spaces.
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; Nicer scrolling with mouse wheel/trackpad.
(unless (and (boundp 'mac-mouse-wheel-smooth-scroll) mac-mouse-wheel-smooth-scroll)
  (global-set-key [wheel-down] (lambda () (interactive) (scroll-up-command 1)))
  (global-set-key [wheel-up] (lambda () (interactive) (scroll-down-command 1)))
  (global-set-key [double-wheel-down] (lambda () (interactive) (scroll-up-command 2)))
  (global-set-key [double-wheel-up] (lambda () (interactive) (scroll-down-command 2)))
  (global-set-key [triple-wheel-down] (lambda () (interactive) (scroll-up-command 4)))
  (global-set-key [triple-wheel-up] (lambda () (interactive) (scroll-down-command 4))))

;; Character encodings default to utf-8.
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; apply syntax highlighting to all buffers
(global-font-lock-mode t)

(eval-after-load 'smartparens
  '(progn
     (require 'smartparens-config)
     (require 'graphene-smartparens-config)
     (setq sp-highlight-pair-overlay nil)))

(require 'web-mode)

(push '("php" . "\\.phtml\\'") web-mode-engine-file-regexps)

(dolist (engine-regexp web-mode-engine-file-regexps)
  (when (cdr engine-regexp)
    (add-to-list 'auto-mode-alist `(,(cdr engine-regexp) . web-mode))))

(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-disable-auto-pairing t)))

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "RET") nil)
     (define-key company-active-map (kbd "ESC") 'company-abort)
     (setq company-idle-delay 0.125
           company-minimum-prefix-length 1
           company-require-match nil
           company-transformers '(company-sort-by-occurrence)
           company-dabbrev-ignore-case nil
           company-dabbrev-downcase nil
           company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                               company-preview-frontend
                               company-echo-metadata-frontend))))

(eval-after-load 'flycheck
  '(progn
     (defun graphene--flycheck-display-errors-function (errors)
       (mapc (lambda (err)
               (message "FlyC: %s" (flycheck-error-message err)) (sit-for 1))
             errors))
     (setq flycheck-highlighting-mode nil
           flycheck-display-errors-function 'graphene--flycheck-display-errors-function)))

;; Main hook to be run on entering de facto prog modes, enabling linum, flycheck,
;; and electric-indent
(add-hook 'graphene-prog-mode-hook
          (lambda ()
            (when graphene-linum-auto
              (graphene-linum))
            (when graphene-errors-auto
              (graphene-errors))
            (electric-indent-mode 1)))

(defun graphene-linum ()
  (linum-mode t))

(defun graphene-autocomplete ()
  (require 'company)
  (global-company-mode t))

(defun graphene-autopair ()
  (require 'smartparens)
  (smartparens-global-mode t))

(defun graphene-parens ()
  (show-paren-mode nil)
  (setq blink-matching-paren nil)
  (require 'smartparens)
  (show-smartparens-global-mode t)
  (setq sp-show-pair-delay 0))

(defun graphene-errors ()
  (require 'flycheck)
  (flycheck-mode))

;; auto json-mode
(push '("\\.json\\'" . json-mode) auto-mode-alist)

;; 2-space indent for CSS
(setq css-indent-offset 2)

;; Default Ruby filetypes
(dolist (regex
         '("\\.watchr$" "\\.arb$" "\\.rake$" "\\.gemspec$" "\\.ru$" "Rakefile$"
           "Gemfile$" "Capfile$" "Guardfile$" "Rakefile$" "Cheffile$" "Vagrantfile$"
           "Berksfile$" "\\.builder$"))
  (add-to-list 'auto-mode-alist `(,regex . ruby-mode)))


;; Attach de facto prog mode hooks after loading init file
(add-hook 'after-init-hook
          (lambda ()
            (dolist (hook graphene-prog-mode-hooks)
              (add-hook hook (lambda () (run-hooks 'graphene-prog-mode-hook))))))

;; Global autocomplete, pairing, parens
(when graphene-autocomplete-auto
  (graphene-autocomplete))
(when graphene-autopair-auto
  (graphene-autopair))
(when graphene-parens-auto
  (graphene-parens))

(provide 'graphene-editing)
