;;; core-packages.el --- Dépôts & noyau paquets -*- lexical-binding: t; -*-

(use-package gcmh
  :straight (:type git :host gitlab :repo "koral/gcmh")
  :init
  (gcmh-mode 1)
  :custom
  (gcmh-idle-delay 0.6)
  (gcmh-high-cons-threshold (* 64 1024 1024)))

(use-package diminish
  :straight t)

(use-package bind-key
  :straight t)

(provide 'core-packages)
;;; core-packages.el ends here
