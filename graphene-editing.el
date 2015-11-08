;;; graphene-editing.el --- Graphene editing defaults
;;
;; Copyright (c) @YEAR Robert Dallas Gray
;;
;; Author: Robert Dallas Gray <mail@robertdallasgray.com>
;; URL: https://github.com/rdallasgray/graphene
;; Version: @VERSION
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


(defvar graphene-prog-mode-hook nil
  "A hook to be run on entering a de facto prog mode.")

(defcustom graphene-prog-mode-hooks
  '(prog-mode-hook
    csharp-mode-hook
    coffee-mode-hook
    css-mode-hook
    sgml-mode-hook
    html-mode-hook)
  "List of hooks to be treated as prog-mode."
  :type 'sexp
  :group 'graphene)

;; Main hook to be run on entering de facto prog modes
(add-hook 'graphene-prog-mode-hook
          (lambda ()
            (when graphene-indent-auto
              (graphene-indent))
            (when graphene-linum-auto
              (graphene-linum))
            (when graphene-pairs-auto
              (graphene-pairs))
            (when graphene-show-pairs-auto
              (graphene-show-pairs))
            (when graphene-completion-auto
              (graphene-completion))
            (when graphene-errors-auto
              (graphene-errors))))

;; Attach de facto prog mode hooks after loading init file
(add-hook 'after-init-hook
          (lambda ()
            (dolist (hook graphene-prog-mode-hooks)
              (add-hook hook (lambda () (run-hooks 'graphene-prog-mode-hook))))))


;;; indenting

(defcustom graphene-indent-auto t
  "Whether graphene should auto-indent code in prog modes."
  :type 'sexp
  :group 'graphene)

(defun graphene-indent ()
  (electric-indent-mode t))


;;; line numbering

(defcustom graphene-linum-auto t
  "Whether graphene should enable line numbers with prog-modes."
  :type 'sexp
  :group 'graphene)

(defun graphene-linum ()
  (linum-mode t))

(setq linum-format " %4d ")


;;; auto-pairing

(defcustom graphene-pairs-auto 'global
  "Whether graphene should enable pair matching with prog-modes."
  :type 'sexp
  :group 'graphene)

(defcustom graphene-show-pairs-auto t
  "Whether graphene should show matching pairs with prog-modes."
  :type 'sexp
  :group 'graphene)

(defun graphene-pairs ()
  (require 'smartparens)
  (smartparens-mode t))

(when (eq graphene-pairs-auto 'global)
  (require 'smartparens)
  (smartparens-global-mode t))

(defun graphene-show-pairs ()
  (show-paren-mode nil)
  (setq blink-matching-paren nil)
  (require 'smartparens)
  (show-smartparens-mode)
  (setq sp-show-pair-delay 0))

(eval-after-load 'smartparens
  '(progn
     (require 'smartparens-config)
     (require 'graphene-smartparens-config)
     (setq sp-highlight-pair-overlay nil)))


;;; completion

(defcustom graphene-completion-auto 'global
  "Whether graphene should enable autocomplete with prog-modes."
  :type 'sexp
  :group 'graphene)

(defun graphene-completion ()
  (require 'company)
  (company-mode t))

(when (eq graphene-completion-auto 'global)
  (require 'company)
  (global-company-mode t))

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "RET") nil)
     (setq company-idle-delay 0.125
           company-minimum-prefix-length 1
           company-require-match nil
           company-transformers '(company-sort-by-occurrence)
           company-dabbrev-ignore-case nil
           company-dabbrev-downcase nil
           company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                               company-preview-frontend
                               company-echo-metadata-frontend))))


;;; error checking

(defcustom graphene-errors-auto t
  "Whether graphene should highlight errors with prog-modes."
  :type 'sexp
  :group 'graphene)

(defun graphene-errors ()
  (require 'flycheck)
  (flycheck-mode))

(eval-after-load 'flycheck
  '(progn
     (defun graphene--flycheck-display-errors-function (errors)
       (mapc (lambda (err)
               (message "FlyC: %s" (flycheck-error-message err)) (sit-for 1))
             errors))
     (setq flycheck-highlighting-mode nil
           flycheck-display-errors-function 'graphene--flycheck-display-errors-function)))


;;; template editing

(require 'web-mode)

(push '("php" . "\\.phtml\\'") web-mode-engine-file-regexps)

(dolist (engine-regexp web-mode-engine-file-regexps)
  (when (cdr engine-regexp)
    (add-to-list 'auto-mode-alist `(,(cdr engine-regexp) . web-mode))))

(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-disable-auto-pairing t)))


;; Delete marked text on typing
(delete-selection-mode t)

;; Soft-wrap lines
(global-visual-line-mode t)

;; Don't use tabs for indent; replace tabs with two spaces.
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; Better scrolling with mouse wheel/trackpad.
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

(provide 'graphene-editing)

;;; graphene-editing.el ends here
