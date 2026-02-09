;;; core-prog.el --- Python + YAML + Eglot  -*- lexical-binding: t; -*-

(use-package python :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode)
  :hook (python-mode . eglot-ensure)
  :config (setq python-indent-offset 4))

(setq python-shell-interpreter "python")

(use-package python-black
  :after python
  :if (executable-find "black")
  :commands (python-black-buffer python-black-region)
  :hook (python-mode . (lambda ()
                         (add-hook 'before-save-hook #'python-black-buffer nil t))))

(use-package eglot :ensure nil
  :commands (eglot eglot-ensure)
  :config
  (with-eval-after-load 'eglot
    ;; Python : ruff-lsp si dispo, sinon pylsp
    (setf (alist-get 'python-mode eglot-server-programs)
          (list (or (executable-find "ruff-lsp") "pylsp")))
    ;; YAML : yaml-language-server si dispo
    (when (executable-find "yaml-language-server")
      (add-to-list 'eglot-server-programs
                   '(yaml-ts-mode . ("yaml-language-server" "--stdio")))
      (add-to-list 'eglot-server-programs
                   '(yaml-mode    . ("yaml-language-server" "--stdio"))))
    ;; Réglages pylsp (flake8 on)
    (setq-default eglot-workspace-configuration
                  '((:pylsp . (:configurationSources ["flake8"]
                                 :plugins (:pycodestyle (:enabled nil)
                                           :mccabe (:enabled nil)
                                           :flake8 (:enabled t)
                                           :pylint (:enabled nil)
                                           :autopep8 (:enabled nil)
                                           :yapf (:enabled nil))))))))

(cond
 ((and (fboundp 'yaml-ts-mode) (fboundp 'treesit-available-p) (treesit-available-p))
  (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
  (add-hook 'yaml-ts-mode-hook #'eglot-ensure))
 (t
  (use-package yaml-mode
    :mode ("\\.ya?ml\\'" . yaml-mode)
    :hook (yaml-mode . eglot-ensure)
    :config (setq yaml-indent-offset 2))))

(provide 'core-prog)
;;; core-prog.el ends here
