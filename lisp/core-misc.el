;;; core-misc.el --- Divers utiles

;; TRAMP
(setq tramp-default-method "ssh")

;; Ouvrir récemment
(recentf-mode 1)

;; Chemins temporaires plus propres
(setq url-history-file (expand-file-name "url/history" user-cache-directory))

(provide 'core-misc)
;;; core-misc.el ends here
