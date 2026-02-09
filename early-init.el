;;; early-init.el --- Chargé avant init.el (Emacs 27+)

;; Accélérer le démarrage et éviter le flicker
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      frame-inhibit-implied-resize t
      package-enable-at-startup nil) ; on gère les paquets nous-mêmes

;; GC agressif pendant le chargement
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      read-process-output-max (* 4 1024 1024)) ; 4Mo pour LSP

;; UI minimaliste dès le début
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq frame-resize-pixelwise t)

(provide 'early-init)
;;; early-init.el ends here
