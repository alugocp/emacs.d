;; Miscellaneous variables
(setq next-line-add-newlines nil)
(setq backup-inhibited t)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq inhibit-startup-message t)
(setq transient-mark-mode  (cons 'only transient-mark-mode))

;; Modal function calls
(global-display-line-numbers-mode)
(tool-bar-mode -1)
(set-frame-size (selected-frame) 150 50)
(set-face-attribute 'default nil :height 150)

;; Set the UI style
;; https://emacsfodder.github.io/emacs-theme-editor/
(require 'billw-theme)
(defun reapply-themes ()
  (load-theme 'billw))
(add-hook 'after-init-hook 'reapply-themes)

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

;; Custom key binding functions
(defun highlight-line ()
  "Select the current line"
  (interactive)
  (move-beginning-of-line 1)
  (push-mark nil nil t)
  (forward-line 1)
)

;; Key bindings
(global-set-key (kbd "s-<left>") 'move-beginning-of-line)
(global-set-key (kbd "s-<right>") 'move-end-of-line)
(global-set-key (kbd "s-<up>") 'beginning-of-buffer)
(global-set-key (kbd "s-<down>") 'end-of-buffer)
(global-set-key (kbd "s-l") 'highlight-line)

;; Okay we're done now
(provide 'startup)