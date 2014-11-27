;;; graphene-speedbar.el --- Graphene defaults for Speedbar
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
;; This file defines default settings and functionality for the Speedbar.
;; Graphene by default uses sr-speedbar by Sebastian Rose.

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

(eval-after-load 'project-explorer
  '(progn
     (setq pe/omit-regex "^\\(\\.\\)\\|\\(flycheck_\\)\\|\\(#\\)")

     ;; Refresh the speedbar when relevant hooks are run.
     (defvar graphene-drawer-refresh-hooks)
     (defvar graphene-drawer-refresh-hooks-added nil
       "Whether hooks have been added to refresh the drawer.")

     (add-hook 'project-explorer-mode-hook
               '(lambda ()
                  (hl-line-mode 1)
                  (visual-line-mode -1)
                  (setq automatic-hscrolling nil)
                  (let ((drawer-display-table (make-display-table)))
                    (set-display-table-slot drawer-display-table 0 8230)
                    (setq buffer-display-table drawer-display-table))
                  (when (not graphene-drawer-refresh-hooks-added)
                    (lambda ()
                      (mapc (lambda (hook)
                              (add-hook hook 'speedbar-refresh))
                            graphene-drawer-refresh-hooks)
                      (setq graphene-drawer-refresh-hooks-added t)))
                  (define-key project-explorer-mode-map [right] 'pe/tab)
                  (define-key project-explorer-mode-map [left] 'pe/backtab)
                  ))))

(provide 'graphene-speedbar)
