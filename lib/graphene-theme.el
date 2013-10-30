;;; graphene-theme.el --- The Graphene theme -- some simple additions to any theme to improve the look of speedbar, linum, etc.
;;
;; Copyright (c) 2013 Robert Dallas Gray
;;
;; Author: Robert Dallas Gray <mail@robertdallasgray.com>
;; URL: https://github.com/rdallasgray/graphene
;; Version: 0.4.0
;; Keywords: defaults

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Graphene is a set of default settings and functionality to make Emacs a little friendlier.
;; The theme works with any other active theme to provide a pleasing default look. 

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

;; Set relative font heights
(defvar graphene-font-height
  (face-attribute 'default :height)
  "Default font height.")
(defvar graphene-small-font-height
  (floor (* .917 graphene-font-height))
  "Relative size for 'small' fonts.")

(deftheme graphene "The Graphene theme -- some simple additions to any theme to improve the look of speedbar, linum, etc.")

(custom-theme-set-faces
 'graphene
 `(speedbar-directory-face
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit variable-pitch
                    :weight bold
                    :height ,graphene-small-font-height))))
 `(speedbar-file-face
   ((t (:foreground unspecified
                    :inherit speedbar-directory-face
                    :weight normal))))
 `(speedbar-selected-face
   ((t (:background unspecified
                    :foreground unspecified
                    :height unspecified
                    :inherit (speedbar-file-face font-lock-function-name-face)))))
 `(speedbar-highlight-face
   ((t (:background unspecified
                    :inherit region))))
 `(speedbar-button-face
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit file-name-shadow))))
 `(speedbar-tag-face
   ((t (:background unspecified
                    :foreground unspecified
                    :height unspecified
                    :inherit speedbar-file-face))))
 `(speedbar-separator-face
   ((t (:foreground unspecified
                    :background unspecified
                    :inverse-video nil
                    :inherit speedbar-directory-face
                    :overline nil
                    :weight bold))))
 `(linum
   ((t (:height ,graphene-small-font-height
                :foreground unspecified
                :inherit 'shadow
                :slant normal))))
 `(visible-mark-face
   ((t (:foreground unspecified
                    :background unspecified
                    :inverse-video unspecified
                    :inherit 'hl-line))))
 `(hl-sexp-face
   ((t (:bold nil
              :background unspecified
              :inherit 'hl-line))))
 `(fringe
   ((t (:background unspecified))))
 `(vertical-border
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit file-name-shadow))))
 `(font-lock-comment-face
   ((t (:slant normal))))
 `(font-lock-doc-face
   ((t (:slant normal))))
 `(popup-face
   ((t (:background unspecified
                    :foreground unspecified
                    :inherit linum
                    :height ,graphene-font-height))))
 `(popup-scroll-bar-foreground-face
   ((t (:background unspecified
                    :inherit region))))
 `(popup-scroll-bar-background-face
   ((t (:background unspecified
                    :inherit popup-face))))
 `(ac-completion-face
   ((t (:background unspecified
                    :foreground unspecified
                    :inherit popup-face))))
 `(ac-candidate-face
   ((t (:background unspecified
                    :foreground unspecified
                    :inherit linum
                    :height ,graphene-font-height))))
 `(ac-selection-face
   ((t (:background unspecified
                    :foreground unspecified
                    :inherit font-lock-variable-name-face
                    :inverse-video t))))
 `(ac-candidate-mouse-face
   ((t (:background unspecified
                    :foreground unspecified
                    :inherit region))))
 `(ac-dabbrev-menu-face
   ((t (:background unspecified
                    :foreground unspecified
                    :inherit popup-face))))
 `(ac-dabbrev-selection-face
   ((t (:background unspecified
                    :foreground unspecified
                    :inherit ac-selection-face))))
 `(flymake-warnline
   ((t (:background unspecified
                    :foreground unspecified
                    :inherit font-lock-preprocessor-face)))))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'graphene)
