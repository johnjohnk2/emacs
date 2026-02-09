;;; core-ui.el --- Apparence & confort -*- lexical-binding: t; -*-

(setq-default
 ring-bell-function 'ignore
 cursor-type 'bar
 x-underline-at-descent-line t)

(global-display-line-numbers-mode 1)
(column-number-mode 1)
(global-hl-line-mode 1)

;; Theme
(use-package doom-themes
  :config
  (load-theme 'doom-one t))

;; Icons (optionnel, dépend de l’installation des polices)
(use-package all-the-icons
  :defer t)

;; Font
(when (member "JetBrains Mono" (font-family-list))
  (set-face-attribute 'default nil :family "JetBrains Mono" :height 110))

;; Modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(provide 'core-ui)
;;; core-ui.el ends here
