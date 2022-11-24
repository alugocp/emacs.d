;; Include dependencies
(require 'redo+)

;; Customizable values
(defcustom library-name/initial-tab-width 2
  "The initial tab width to set when opening this editor")
(defcustom library-name/initial-screen-width 150
  "The initial screen width to set when opening this editor")
(defcustom library-name/initial-screen-height 50
  "The initial screen height to set when opening this editor")
(defcustom library-name/command-key "C"
  "The key to be used in place of CMD for keybindings")
(defcustom library-name/indentation-variables '()
  "A list of indentation variables to keep in sync")

;; Miscellaneous variables
(setq backup-inhibited t)                                    ;; Don't create file backups
(setq auto-save-default nil)                                 ;; Don't auto save files
(setq create-lockfiles nil)                                  ;; Don't create lockfiles
(setq inhibit-startup-message t)                             ;; No startup message
(setq transient-mark-mode (cons 'only transient-mark-mode))  ;; Allows you to de-select by hitting an arrow key
(setq whitespace-style '(tabs tab-mark))                     ;; Displays a mark for tab characters
(setq initial-scratch-message ";; Hello, world!")            ;; Set default buffer message
(setq mouse-wheel-tilt-scroll t)                             ;; Allows you to scroll horizontally
(setq mouse-wheel-flip-direction 1)                          ;; Sets my preferred mouse pad scrolling direction
(setq-default indent-tabs-mode nil)                          ;; Don't use tabs for indentation
(setq-default truncate-lines 1)                              ;; Won't wrap long lines
(setq-default electric-indent-inhibit t)                     ;; Don't indent current line on RET
(setq-default tab-line-tabs-function                         ;; Keeps our tab order consistent
  (lambda () (sort (tab-line-tabs-window-buffers)
    (lambda (a b) (string< (buffer-name a) (buffer-name b))))))

;; Modal function calls
(global-display-line-numbers-mode)            ;; Show line numbers
(set-face-attribute 'default nil :height 150) ;; Zoom the text in a little
(delete-selection-mode)                       ;; Delete selected text on new character
(global-tab-line-mode)                        ;; Incorporates tabs onto the editor
(global-whitespace-mode 1)                    ;; Displays desired whitespace characters
(tool-bar-mode -1)                            ;; Don't show toolbar
(set-frame-size (selected-frame)              ;; Set initial frame size
  library-name/initial-screen-width
  library-name/initial-screen-height)

;; Set the UI style
(require 'billw-theme)
(add-hook 'after-init-hook (lambda () (load-theme 'billw)))

;; Use the MELPA package archive to install necessary packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(setq packages '(
  typescript-mode ;; TypeScript syntax highlighting
  fuzzy-finder    ;; Fuzzy finder package
  diff-hl         ;; Show line changes in fringe
  neotree         ;; File tree viewer
  magit           ;; Git support
))
(unless package-archive-contents
  (package-refresh-contents))
(dolist (pkg packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Package integration setup
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)   ;; Syncs diff-hl and magit
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh) ;; Syncs diff-hl and magit
(global-diff-hl-mode)                                           ;; Use fringe to show edited lines
(neotree-toggle)                                                ;; Activates the file tree viewer by default

;; Miscellaneous hooks
(add-hook 'emacs-startup-hook (lambda ()
      (when (get-buffer "*scratch*")
        (progn
          (kill-buffer "*scratch*")
          (delete-window))
          (when (member "*Messages*" (mapcar (lambda (a) (buffer-name a)) (funcall tab-line-tabs-function)))
              (progn
                  (open-empty-buffer)
                  (kill-buffer "*Messages*"))))))
(defun library-name/no-deactivate-mark (&rest _) (setq deactivate-mark nil)) ;; Copying doesn't deselect the highlighted text
(advice-add 'kill-ring-save :after #'no-deactivate-mark)

;; Custom function definitions

(defun library-name/highlight-line ()
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

(defun library-name/global-tab-switch (n)
  "Switch the viewer to the nth tab"
  (setq buffers (funcall tab-line-tabs-function))
  (if (< n (length buffers))
    (switch-to-buffer (nth n (mapcar #'buffer-name buffers)))
    nil))

(defun library-name/open-empty-buffer ()
  "Opens a new, empty buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (insert initial-scratch-message)
  (lisp-interaction-mode))

(defun library-name/indent-region ()
  "Indents the selected region or current line"
  (interactive)
  (setq start (if (use-region-p) (region-beginning) (point)))
  (setq end (if (use-region-p) (region-end) (point)))
  (goto-char start)
  (setq start (+ start tab-width))
  (while (<= (point) end)
    (progn
        (beginning-of-line)
        (dotimes (_ tab-width) (insert " "))
        (setq end (+ end tab-width))
        (end-of-line)
        (if (and (= (point) end) (= (point-max) end)) (setq end (- end 1)))
        (if (< (point) (point-max)) (forward-char))))
  (set-mark start)
  (goto-char end)
  (setq deactivate-mark nil)
  (activate-mark)
  (setq transient-mark-mode (cons 'only transient-mark-mode)))

(defun library-name/outdent-region ()
  "Outdents the selected region or current line"
  (interactive)
  (setq start (if (use-region-p) (region-beginning) (point)))
  (setq end (if (use-region-p) (region-end) (point)))
  (goto-char start)
  (beginning-of-line)
  (dotimes (_ tab-width) (if (string= " " (string (following-char))) (progn (setq start (- start 1)) (forward-char))))
  (while (<= (point) end)
    (progn
        (beginning-of-line)
        (dotimes (_ tab-width)
            (if (string= " " (string (following-char))) (progn
                (delete-char 1)
                (setq end (- end 1)))))
        (end-of-line)
        (if (and (= (point) end) (= (point-max) end)) (setq end (- end 1)))
        (if (< (point) (point-max)) (forward-char))))
  (set-mark start)
  (goto-char end)
  (setq deactivate-mark nil)
  (activate-mark)
  (setq transient-mark-mode (cons 'only transient-mark-mode)))

(defun library-name/kill-emacs ()
  "Quits Emacs without so many annoying questions"
  (interactive)
  (save-some-buffers nil t)
  (kill-emacs))

(defun library-name/move-beginning-of-line (highlight)
    "Moves to the beginning of content on the current line, or the beginning of the line"
    (interactive)
    (if (and highlight (not (region-active-p))) (set-mark (point)))
    (setq start (point))
    (beginning-of-line)
    (skip-chars-forward "\t ")
    (if (or (= start (point)) (= start (line-beginning-position))) (beginning-of-line))
    (setq transient-mark-mode (cons 'only transient-mark-mode)))

(defun library-name/delete-backward-char ()
    "Deletes up to the tab width if you're only deleting space marks"
    (interactive)
    (if (and (>= (point) (+ (point-min) tab-width)) (string= " " (string (preceding-char))))
        (progn
            (backward-char)
            (if (string= " " (string (preceding-char)))
                (progn
                    (forward-char)
                    (delete-backward-char tab-width))
                (progn
                    (forward-char)
                    (delete-backward-char 1))))
        (delete-backward-char 1)))

(defun library-name/set-indentation-width (n)
  "Sets the editor's indentation width"
  (dolist (variable library-name/indentation-variables) (set-default variable n)))

(defun library-name/set-indent-level ()
    "Sets the editor's indentation width by user input"
    (interactive)
    (library-name/set-indentation-width (string-to-number (read-key-sequence "Indent by"))))

(defun library-name/kbd (shortcut)
  "Wraps the kbd function but replaces the command key with whatever the user has set"
  (if (string= "s-" (substring shortcut 0 2))
    (kbd (concat library-name/command-key (substring shortcut 1)))
    (kbd shortcut)))

(defun library-name/select-all ()
  "Selects the entire buffer"
  (interactive)
  (set-mark (point-min))
  (goto-char (point-max)))

;; Key bindings
(global-set-key (library-name/kbd "s-S-<left>") (lambda () (interactive) (library-name/move-beginning-of-line 1))) ;; CMD + left moves to beginning of line
(global-set-key (library-name/kbd "s-<left>") (lambda () (interactive) (library-name/move-beginning-of-line nil))) ;; CMD + left moves to beginning of line
(global-set-key (library-name/kbd "s-<right>") 'move-end-of-line)                                                  ;; CMD + right moves to end of line
(global-set-key (library-name/kbd "s-<up>") 'beginning-of-buffer)                                                  ;; CMD + up moves to beginning of file
(global-set-key (library-name/kbd "s-<down>") 'end-of-buffer)                                                      ;; CMD + down moves to end of file
(global-set-key (library-name/kbd "s-l") 'highlight-line)                                                          ;; CMD + L highlights the current line
(global-set-key (library-name/kbd "s-<return>") 'eshell)                                                           ;; CMD + enter opens the Emacs shell
(global-set-key (library-name/kbd "s-<enter>") 'eshell)                                                            ;; CMD + enter opens the Emacs shell
(global-set-key (library-name/kbd "s-]") 'library-name/indent-region)                                              ;; Indents the highlighted region
(global-set-key (library-name/kbd "s-[") 'library-name/outdent-region)                                             ;; Outdents the highlighted region
(global-set-key (library-name/kbd "s-f") 'fuzzy-finder)                                                            ;; Opens the fuzzy finder
(global-set-key (library-name/kbd "s-w") 'kill-this-buffer)                                                        ;; Close tab
(global-set-key (library-name/kbd "s-q") 'library-name/kill-emacs)                                                 ;; Just quit Emacs
(global-set-key (library-name/kbd "s-s") 'save-buffer)                                                             ;; Save current buffer
(global-set-key (library-name/kbd "s-S") 'write-file)                                                              ;; Save as
(global-set-key (library-name/kbd "s-o") 'find-file)                                                               ;; Open file
(global-set-key (library-name/kbd "s-t") 'open-empty-buffer)                                                       ;; Open empty buffer
(global-set-key (library-name/kbd "s-i") 'library-name/set-indent-level)                                           ;; Sets indentation width
(global-set-key (library-name/kbd "s-a") 'library-name/select-all)                                                 ;; Selects the entire buffer
(global-set-key (library-name/kbd "s-z") 'undo)                                                                    ;; Undo an action
(global-set-key (library-name/kbd "s-Z") 'redo)                                                                    ;; Redo an action
(global-set-key (library-name/kbd "s-|") 'neotree-toggle)                                                          ;; Toggles the project tree viewer
(global-set-key (library-name/kbd "s-/") 'neotree-dir)                                                             ;; Changes the root directory of project tree viewer
(global-set-key (library-name/kbd "s-1") (lambda () (interactive) (global-tab-switch 0)))                          ;; Switch to tab 1
(global-set-key (library-name/kbd "s-2") (lambda () (interactive) (global-tab-switch 1)))                          ;; Switch to tab 2
(global-set-key (library-name/kbd "s-3") (lambda () (interactive) (global-tab-switch 2)))                          ;; Switch to tab 3
(global-set-key (library-name/kbd "s-4") (lambda () (interactive) (global-tab-switch 3)))                          ;; Switch to tab 4
(global-set-key (library-name/kbd "s-5") (lambda () (interactive) (global-tab-switch 4)))                          ;; Switch to tab 5
(global-set-key (library-name/kbd "s-6") (lambda () (interactive) (global-tab-switch 5)))                          ;; Switch to tab 6
(global-set-key (library-name/kbd "s-7") (lambda () (interactive) (global-tab-switch 6)))                          ;; Switch to tab 7
(global-set-key (library-name/kbd "s-8") (lambda () (interactive) (global-tab-switch 7)))                          ;; Switch to tab 8
(global-set-key (library-name/kbd "s-9") (lambda () (interactive) (global-tab-switch 8)))                          ;; Switch to tab 9
(global-set-key (library-name/kbd "<backspace>") 'library-name/delete-backward-char)                               ;; Overrides backspace to handle space tabs
(global-set-key (library-name/kbd "<escape>") 'keyboard-escape-quit)                                               ;; You can use escape key to quit command line
(global-set-key (library-name/kbd "<tab>") 'tab-to-tab-stop)                                                       ;; Tab adds a couple spaces

;; Okay we're done now
(library-name/set-indentation-width library-name/initial-tab-width)
(provide 'startup)
