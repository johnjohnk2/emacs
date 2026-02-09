;;; core-project.el --- Projets & navigation -*- lexical-binding: t; -*-

(use-package projectile
  :init
  (projectile-mode 1)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-globally-ignored-directories
        '(".cache" "straight" "var" "node_modules" ".venv")))

(use-package ripgrep)

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; Dired + omit (built-in)
(use-package dired
  :ensure nil
  :straight nil
  :config
  (require 'dired-x)   ;; <-- OBLIGATOIRE
  (setq dired-omit-files
        (rx (or
             (seq bol ".git" eol)
             (seq bol ".idea" eol)
             (seq bol ".venv" eol)
             (seq bol "node_modules" eol)
             (seq bol "__pycache__" eol)
             (seq bol ".cache" eol)
             (seq bol ".pdm-build" eol))))
  :hook
  (dired-mode . dired-omit-mode))


(provide 'core-project)
;;; core-project.el ends here
