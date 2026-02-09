;;; core-completion.el --- Vertico + Orderless + Consult

(use-package vertico :init (vertico-mode 1))
(use-package savehist :init (savehist-mode 1)
  :custom (savehist-file (expand-file-name "savehist/history" user-cache-directory)))
(use-package marginalia :init (marginalia-mode 1))
(use-package orderless
  :custom (completion-styles '(orderless basic))
          (completion-category-defaults nil)
          (completion-category-overrides '((file (styles basic partial-completion)))))
(use-package consult :bind (("C-s" . consult-line)
                            ("C-x b" . consult-buffer)))

(provide 'core-completion)
;;; core-completion.el ends here
