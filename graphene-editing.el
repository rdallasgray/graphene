;; Defaults for text editing.

(require 'smart-tab)
(require 'auto-complete)
(require 'auto-complete-config)
(require 'autopair)
(require 'multi-web-mode)

;; Use flymake, autocomplete, linum in prog modes
(ac-config-default)
(add-hook 'prog-mode-hook
          (lambda ()
            (progn
              (flymake-mode t)
              (auto-complete)
              (linum-mode 1)
              (setq linum-format " %4d ")
              )))

;; Autopair gives closer-to-textmate functionality than the built-in electric modes.
(autopair-global-mode)

;; Nicer scrolling with mouse wheel/trackpad.
(defun scroll-up-amount (amt) "Scroll up by the given amount."  (scroll-up-command amt))
(defun scroll-down-amount (amt) "Scroll down by the given amount." (scroll-down-command amt))
(global-set-key [wheel-down] (lambda () (interactive) (scroll-up-amount 1)))
(global-set-key [wheel-up] (lambda () (interactive) (scroll-down-amount 1)))
(global-set-key [double-wheel-down] (lambda () (interactive) (scroll-up-amount 2)))
(global-set-key [double-wheel-up] (lambda () (interactive) (scroll-down-amount 2)))
(global-set-key [triple-wheel-down] (lambda () (interactive) (scroll-up-amount 4)))
(global-set-key [triple-wheel-up] (lambda () (interactive) (scroll-down-amount 4)))

;; Autorevert all buffers.
(global-auto-revert-mode t)

;; cua-mode -- only for rectangles, and to have something like delete-selection-mode that's compatible with autopair.
(cua-selection-mode t)

;; Always autoindent new lines.
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Use tab for autocomplete.
(global-smart-tab-mode 1)

;; Show matching parens immediately.
(show-paren-mode 1)
(setq show-paren-delay 0)

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
