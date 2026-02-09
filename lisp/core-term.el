;;; core-term.el -*- lexical-binding: t; -*-

(defun my/open-terminal ()
  "Open a terminal adapted to the current system."
  (interactive)
  (cond
   ;; Windows → eat si dispo, sinon eshell
   ((eq system-type 'windows-nt)
    (if (require 'eat nil t)
        (eat)
      (eshell t)))

   ;; Linux / macOS → vterm si dispo, sinon eshell
   (t
    (if (require 'vterm nil t)
        (vterm)
      (eshell t)))))

;; Windows: eat (terminal moderne)
(use-package eat
  :if (eq system-type 'windows-nt)
  :commands (eat eat-project)
  :config
  (setq eat-kill-buffer-on-exit t))

;; Unix: vterm
(use-package vterm
  :if (not (eq system-type 'windows-nt))
  :commands vterm
  :config
  (setq vterm-max-scrollback 5000))

;; Fallback universel (built-in)
(use-package eshell
  :ensure nil
  :commands (eshell))

(global-set-key (kbd "C-c t") #'my/open-terminal)

(provide 'core-term)
