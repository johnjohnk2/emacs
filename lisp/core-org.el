;;; core-org.el --- Org-mode

(use-package org :ensure t
  :custom
  (org-directory (expand-file-name "org/" (or (getenv "ORG_HOME") (expand-file-name "~"))))
  (org-ellipsis " ▾"))

(use-package org-bullets :hook (org-mode . org-bullets-mode))

(provide 'core-org)
;;; core-org.el ends here
