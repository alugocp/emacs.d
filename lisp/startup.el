;; Miscellaneous variables
(setq backup-inhibited t)																		 ;; Don't create file backups
(setq auto-save-default nil)																 ;; Don't auto save files
(setq create-lockfiles nil)																	 ;; Don't create lockfiles
(setq inhibit-startup-message t)														 ;; No startup message
(setq transient-mark-mode  (cons 'only transient-mark-mode)) ;; Allow you to de-select by hitting an arrow key
(setq-default tab-width 2)																	 ;; Set tab length
(setq initial-scratch-message ";; Hello, world!")						 ;; Set default buffer message
(setq mouse-wheel-tilt-scroll t)														 ;; Allows you to scroll horizontally
(setq mouse-wheel-flip-direction 1)                          ;; Sets my preferred mouse pad scrolling direction
(setq-default truncate-lines 1)                              ;; Won't wrap long lines
(setq-default tab-line-tabs-function												 ;; Keeps our tab order consistent
	(lambda () (sort (tab-line-tabs-window-buffers)
		(lambda (a b) (string< (buffer-name a) (buffer-name b))))))

;; Modal function calls
(global-display-line-numbers-mode)            ;; Show line numbers
(tool-bar-mode -1)                            ;; Don't show toolbar
(set-frame-size (selected-frame) 150 50)      ;; Set initial frame size
(set-face-attribute 'default nil :height 150) ;; Zoom the text in a little
(global-diff-hl-mode)													;; Use fringe to show edited lines
(delete-selection-mode)												;; Delete selected text on new character
(global-tab-line-mode)												;; Incorporates tabs onto the editor
(diff-hl-margin-mode)													;; Adds funny icons to the git diff margins
(neotree-toggle)															;; Activates the file tree viewer by default

;; Set the UI style
;; https://emacsfodder.github.io/emacs-theme-editor/
(require 'billw-theme)
(add-hook 'after-init-hook (lambda () (load-theme 'billw)))

;; Use the MELPA package archive to install necessary packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(setq packages '(
	typescript-mode ;; TypeScript syntax highlighting
	magit						;; Git support
	diff-hl					;; Show line changes in fringe
	neotree					;; File tree viewer
))
(unless package-archive-contents
  (package-refresh-contents))
(dolist (pkg packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Package integration setup
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; Custom key binding functions and advice
(defun highlight-line ()																				;; Line select function
  "Select the current line"
  (interactive)
	(if (region-active-p)
		(progn
			(setq end (region-end))
			(goto-char (region-beginning))
		  (move-beginning-of-line 1)
		  (push-mark nil nil t)
			(goto-char end)
		  (forward-line 1))
		(move-beginning-of-line 1)
		(push-mark nil nil t)
		(forward-line 1)
	))
(defun no-deactivate-mark (&rest _) (setq deactivate-mark nil)) ;; Copying doesn't deselect the highlighted text
(advice-add 'kill-ring-save :after #'no-deactivate-mark)
(defun global-tab-switch (n)																		;; Switch to a numbered tab
	"Switch the viewer to the nth tab"
	(setq buffers (funcall tab-line-tabs-function))
	(if (< n (length buffers))
		(switch-to-buffer (nth n (mapcar #'buffer-name buffers)))
		nil))
(defun open-empty-buffer ()																			;; Opens a new, empty buffer
	(interactive)
	(switch-to-buffer (get-buffer-create "*scratch*"))
	(lisp-interaction-mode))

;; Key bindings
(global-set-key (kbd "s-<left>") 'move-beginning-of-line)										 ;; CMD + left moves to beginning of line
(global-set-key (kbd "s-<right>") 'move-end-of-line)												 ;; CMD + right moves to end of line
(global-set-key (kbd "s-<up>") 'beginning-of-buffer)												 ;; CMD + up moves to beginning of file
(global-set-key (kbd "s-<down>") 'end-of-buffer)														 ;; CMD + down moves to end of file
(global-set-key (kbd "s-l") 'highlight-line)																 ;; CMD + L highlights the current line
(global-set-key (kbd "<tab>") 'tab-to-tab-stop)															 ;; Tab adds a couple spaces
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)											 ;; You can use escape key to quit command line
(global-set-key (kbd "s-<return>") 'eshell)																	 ;; CMD + enter opens the Emacs shell
(global-set-key (kbd "s-[") 'previous-buffer)																 ;; Go to previous tab
(global-set-key (kbd "s-]") 'next-buffer)																		 ;; Go to next tab
(global-set-key (kbd "s-w") 'kill-this-buffer)															 ;; Close tab
(global-set-key (kbd "s-t") 'open-empty-buffer)															 ;; Open empty buffer
(global-set-key (kbd "s-1") (lambda () (interactive) (global-tab-switch 0))) ;; Switch to tab 1
(global-set-key (kbd "s-2") (lambda () (interactive) (global-tab-switch 1))) ;; Switch to tab 2
(global-set-key (kbd "s-3") (lambda () (interactive) (global-tab-switch 2))) ;; Switch to tab 3
(global-set-key (kbd "s-4") (lambda () (interactive) (global-tab-switch 3))) ;; Switch to tab 4
(global-set-key (kbd "s-5") (lambda () (interactive) (global-tab-switch 4))) ;; Switch to tab 5
(global-set-key (kbd "s-6") (lambda () (interactive) (global-tab-switch 5))) ;; Switch to tab 6
(global-set-key (kbd "s-7") (lambda () (interactive) (global-tab-switch 6))) ;; Switch to tab 7
(global-set-key (kbd "s-8") (lambda () (interactive) (global-tab-switch 7))) ;; Switch to tab 8
(global-set-key (kbd "s-9") (lambda () (interactive) (global-tab-switch 8))) ;; Switch to tab 9

;; Okay we're done now
(provide 'startup)
