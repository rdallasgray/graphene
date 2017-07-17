;;; graphene-env.el --- Graphene environment defaults
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
;; The environment defaults target the general Emacs environment, simplifying interactions
;; and enabling discoverability.

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


;; Use smex to complete interactive commands
(require 'smex)
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Use ido for general completion
(ido-mode 1)
(ido-everywhere 1)
(require 'ido-completing-read+)
(ido-ubiquitous-mode t)
(put 'ido-complete 'disabled nil)
(put 'ido-exit-minibuffer 'disabled nil)
(setq ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess)


;; Make buffer names unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)


;; Don't show the startup message
(setq inhibit-startup-message t)


;; Save backup files in the temporary directory
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))


;; Shorten yes/no answers to y/n
(fset 'yes-or-no-p 'y-or-n-p)


;; Automatically update buffers when files change
(global-auto-revert-mode t)


;; Enable 'power user' features
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(provide 'graphene-env)

;;; graphene-env.el ends here
