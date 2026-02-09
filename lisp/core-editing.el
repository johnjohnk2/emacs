;;; core-editing.el --- Édition

(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 100)
(delete-selection-mode 1)
(save-place-mode 1)
(global-auto-revert-mode 1)
(show-paren-mode 1)

;; Backups & autosave vers var/
(setq backup-directory-alist `(("." . ,(expand-file-name "backup/" user-cache-directory))))
(setq auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save/" user-cache-directory) t)))

;; Codage par défaut
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(setq-default buffer-file-coding-system 'utf-8-unix)

(use-package undo-tree
  :init (global-undo-tree-mode 1)
  :diminish)

(use-package multiple-cursors :bind (("C-S-c C-S-c" . mc/edit-lines)))

(provide 'core-editing)
;;; core-editing.el ends here
```elisp
;;; core-editing.el --- Édition

(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 100)
(delete-selection-mode 1)
(save-place-mode 1)
(global-auto-revert-mode 1)
(show-paren-mode 1)

;; Backups & autosave vers var/
(setq backup-directory-alist `(("." . ,(expand-file-name "backup/" user-cache-directory))))
(setq auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save/" user-cache-directory) t)))

(use-package undo-tree
  :init (global-undo-tree-mode 1)
  :diminish)

(use-package multiple-cursors :bind (("C-S-c C-S-c" . mc/edit-lines)))

(provide 'core-editing)
;;; core-editing.el ends here
