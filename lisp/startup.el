;; Miscellaneous variables
(setq backup-inhibited t) ;; Don't create file backups
(setq auto-save-default nil) ;; Don't auto save files
(setq create-lockfiles nil) ;; Don't create lockfiles
(setq inhibit-startup-message t) ;; No startup message
(setq transient-mark-mode  (cons 'only transient-mark-mode)) ;; Allow you to de-select by hitting an arrow key
(setq-default tab-width 2) ;; Set tab length
(setq initial-scratch-message ";; Hello, world!") ;; Set default buffer message

;; Modal function calls
(global-display-line-numbers-mode) ;; Show line numbers
(tool-bar-mode -1) ;; Don't show toolbar
(set-frame-size (selected-frame) 150 50) ;; Set initial frame size
(set-face-attribute 'default nil :height 150) ;; Zoom the text in a little
(global-diff-hl-mode) ;; Use fringe to show edited lines
(delete-selection-mode) ;; Delete selected text on new character
(global-tab-line-mode) ;; Incorporates tabs onto the editor

;; Set the UI style
;; https://emacsfodder.github.io/emacs-theme-editor/
(require 'billw-theme)
(add-hook 'after-init-hook (lambda () (load-theme 'billw)))

;; Use the MELPA package archive
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install necessary packages
(setq packages '(
	typescript-mode ;; TypeScript syntax highlighting
	magit						;; Git support
	diff-hl					;; Show line changes in fringe
))
(unless package-archive-contents
  (package-refresh-contents))
(dolist (pkg packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Package integration setup
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; Custom key binding functions
(defun highlight-line ()
  "Select the current line"
  (interactive)
  (move-beginning-of-line 1)
  (push-mark nil nil t)
  (forward-line 1)
)

;; Key bindings
(global-set-key (kbd "s-<left>") 'move-beginning-of-line) ;; CMD + left moves to beginning of line
(global-set-key (kbd "s-<right>") 'move-end-of-line) ;; CMD + right moves to end of line
(global-set-key (kbd "s-<up>") 'beginning-of-buffer) ;; CMD + up moves to beginning of file
(global-set-key (kbd "s-<down>") 'end-of-buffer) ;; CMD + down moves to end of file
(global-set-key (kbd "s-l") 'highlight-line) ;; CMD + L highlights the current line
(global-set-key (kbd "<tab>") 'tab-to-tab-stop) ;; Tab adds a couple spaces
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; You can use escape key to quit command line
(global-set-key (kbd "s-<return>") 'eshell) ;; CMD + enter opens the Emacs shell
(global-set-key (kbd "s-[") 'previous-buffer) ;; Go to previous tab
(global-set-key (kbd "s-]") 'next-buffer) ;; Go to next tab
(global-set-key (kbd "s-w") 'kill-this-buffer) ;; Close tab

;; Okay we're done now
(provide 'startup)
