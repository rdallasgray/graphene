(deftheme graphene "The Graphene theme -- some simple additions to any theme to improve the look of speedbar, linum, etc.")

(custom-theme-set-faces
 'graphene
 `(speedbar-directory-face ((t (:foreground unspecified :background unspecified
                                            :inherit variable-pitch :weight bold :height 110))))
 `(speedbar-file-face ((t (:foreground unspecified
                                       :inherit speedbar-directory-face :weight normal))))
 `(speedbar-selected-face ((t (:background unspecified :foreground unspecified :height unspecified
                                           :inherit (speedbar-file-face font-lock-function-name-face)))))
 `(speedbar-highlight-face ((t (:background unspecified :inherit region))))
 `(speedbar-button-face ((t (:foreground unspecified :background unspecified :inherit file-name-shadow))))
 `(speedbar-tag-face ((t (:background unspecified :foreground unspecified :height unspecified
                                      :inherit speedbar-file-face))))
 `(speedbar-separator-face ((t (:foreground unspecified :background unspecified :inverse-video nil
                                            :inherit speedbar-directory-face :overline nil :weight bold))))
 `(linum ((t (:height 110 :foreground unspecified :inherit fringe :slant normal))))
 `(fringe ((t (:background unspecified))))
 `(vertical-border ((t (:foreground unspecified :background unspecified :inherit file-name-shadow))))
 `(font-lock-comment-face ((t (:slant normal))))
 `(font-lock-doc-face ((t (:slant normal))))
 `(popup-face ((t (:background unspecified :foreground unspecified :inherit linum :height 120))))
 `(popup-scroll-bar-foreground-face ((t (:background unspecified :inherit region))))
 `(popup-scroll-bar-background-face ((t (:background unspecified :inherit popup-face))))
 `(ac-completion-face ((t (:background unspecified :foreground unspecified :inherit popup-face))))
 `(ac-candidate-face ((t (:background unspecified :foreground unspecified :inherit linum :height 120))))
 `(ac-selection-face ((t (:background unspecified :foreground unspecified :inherit font-lock-variable-name-face :inverse-video t))))
 `(ac-candidate-mouse-face ((t (:background unspecified :foreground unspecified :inherit region))))
 )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'graphene)
