;;; core-vcs.el --- Git & VC

(use-package magit :bind (("C-x g" . magit-status)))
(use-package diff-hl :hook ((prog-mode . diff-hl-mode)
                            (text-mode . diff-hl-mode)
                            (magit-post-refresh . diff-hl-magit-post-refresh)))

(provide 'core-vcs)
;;; core-vcs.el ends here
