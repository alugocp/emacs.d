;; General setup
(setq next-line-add-newlines nil)
(setq backup-inhibited t)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(global-display-line-numbers-mode)
(setq inhibit-startup-message t)
(tool-bar-mode -1)

;; Use the MELPA package archive
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install necessary packages
(setq packages '(
  typescript-mode
))
(unless package-archive-contents
  (package-refresh-contents))
(dolist (pkg packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Set the UI style
;; https://emacsfodder.github.io/emacs-theme-editor/
(require 'basic-theme)
(defun reapply-themes ()
  (load-theme 'basic))
(add-hook 'after-init-hook 'reapply-themes)

(provide 'startup)