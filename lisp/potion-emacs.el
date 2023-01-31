;; Boost GC threshold to minimize lag during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Lower runtime GC threshold to 8 MB after startup (default is 800kB)
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold (expt 2 23))))

;; Prevent new buffers from opening in windows (split screen views)
(add-hook 'emacs-startup-hook (lambda ()
    (setq-default pop-up-windows nil)
    (setq-default pop-up-frames nil)))

;; Include dependencies
(autoload 'redo "redo+" "Redo the last undone change" t)
(autoload 'undo "redo+" "Undo the last change" t)

;; Set the UI style
(require 'billw-theme)
(add-hook 'after-init-hook (lambda () (load-theme 'billw)))

;; Important header values
(defun potion-emacs/stdout (msg)
  "Prints a string message to stdout"
  (princ msg #'external-debugging-output))

;; Customizable values
(defcustom potion-emacs/initial-tab-width 2
  "The initial tab width to set when opening this editor")
(defcustom potion-emacs/initial-screen-width 150
  "The initial screen width to set when opening this editor")
(defcustom potion-emacs/initial-screen-height 50
  "The initial screen height to set when opening this editor")
(defcustom potion-emacs/command-key "C"
  "The key to be used in place of CMD for keybindings")
(defcustom potion-emacs/terminal "/bin/zsh"
  "The terminal command to be run by Emacs")
(defcustom potion-emacs/indentation-variables '()
  "A list of indentation variables to keep in sync")

;; Miscellaneous variables
(setq potion-emacs/main-window (selected-window))           ;; Keep track of the "main" window
(setq backup-inhibited t)                                   ;; Don't create file backups
(setq auto-save-default nil)                                ;; Don't auto save files
(setq create-lockfiles nil)                                 ;; Don't create lockfiles
(setq inhibit-startup-message t)                            ;; No startup message
(setq transient-mark-mode (cons 'only transient-mark-mode)) ;; Allows you to de-select by hitting an arrow key
(setq whitespace-style '(tabs tab-mark))                    ;; Displays a mark for tab characters
(setq initial-scratch-message ";; Hello, world!")           ;; Set default buffer message
(setq mouse-wheel-tilt-scroll t)                            ;; Allows you to scroll horizontally
(setq mouse-wheel-flip-direction 1)                         ;; Sets my preferred mouse pad scrolling direction
(setq-default indent-tabs-mode nil)                         ;; Don't use tabs for indentation
(setq-default truncate-lines 1)                             ;; Won't wrap long lines
(setq-default electric-indent-inhibit t)                    ;; Don't indent current line on RET
(setq-default case-fold-search nil)                         ;; Makes occur case sensitive
(setq-default tab-line-tabs-function                        ;; Keeps our tab order consistent
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
  potion-emacs/initial-screen-width
  potion-emacs/initial-screen-height)

;; Use the MELPA package archive to install necessary packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(setq packages '(
  typescript-mode ;; TypeScript syntax highlighting
  rust-mode       ;; Rust syntax highlighting
  diff-hl         ;; Show line changes in fringe
  neotree         ;; File tree viewer
  magit           ;; Git support
))
(unless package-archive-contents
  (package-refresh-contents))
(dolist (pkg packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Changes diff-hl colors
(defun potion-emacs/reset-hl-colors (diff-hl-dir)
    (setq file (concat diff-hl-dir "/diff-hl.el"))
    (setq contents (with-temp-buffer
        (insert-file-contents file)
        (buffer-string)))
    (setq contents (replace-regexp-in-string "'\(\(default :inherit diff-added\)\n    \(\(\(class color\)\) :foreground \"green4\"\)\)" "'((((class color)) :background \"#00ff00\"))" contents))
    (setq contents (replace-regexp-in-string "'\(\(default :inherit diff-removed\)\n    \(\(\(class color\)\) :foreground \"red3\"\)\)" "'((((class color)) :background \"#ff0000\"))" contents))
    (setq contents (replace-regexp-in-string "'\(\(default :foreground \"blue3\"\)\n    \(\(\(class color\) \(min-colors 88\) \(background light\)\)\n     :background \"#ddddff\"\)\n    \(\(\(class color\) \(min-colors 88\) \(background dark\)\)\n     :background \"#333355\"\)\)" "'((((class color)) :background \"#ffff00\"))" contents))
    (with-temp-buffer
        (insert contents)
        (when (file-writable-p file)
            (write-region (point-min)
                (point-max)
                file)))
    (byte-recompile-directory diff-hl-dir 0))
(potion-emacs/reset-hl-colors (nth 0 (file-expand-wildcards "~/.emacs.d/elpa/diff-hl-*")))

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
                  (potion-emacs/open-empty-buffer)
                  (kill-buffer "*Messages*"))))))
(advice-add 'kill-ring-save :after (lambda (&rest _) (setq deactivate-mark nil)))

;; Custom function definitions

(defun potion-emacs/highlight-line ()
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

(defun potion-emacs/global-tab-switch (n)
  "Switch the viewer to the nth tab"
  (setq buffers (funcall tab-line-tabs-function))
  (if (< n (length buffers))
    (switch-to-buffer (nth n (mapcar #'buffer-name buffers)))
    nil))

(defun potion-emacs/final-tab-switch ()
  "Swith the viewer to the last tab"
  (interactive)
  (potion-emacs/global-tab-switch (- (length (funcall tab-line-tabs-function)) 1)))

(defun potion-emacs/open-empty-buffer ()
  "Opens a new, empty buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (insert initial-scratch-message)
  (lisp-interaction-mode))

(defun potion-emacs/indent-region ()
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

(defun potion-emacs/outdent-region ()
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

(defun potion-emacs/kill-emacs ()
  "Quits Emacs without so many annoying questions"
  (interactive)
  (save-some-buffers nil t)
  (kill-emacs))

(defun potion-emacs/move-beginning-of-line (highlight)
    "Moves to the beginning of content on the current line, or the beginning of the line"
    (interactive)
    (if (and highlight (not (region-active-p))) (set-mark (point)))
    (setq start (point))
    (beginning-of-line)
    (skip-chars-forward "\t ")
    (if (or (= start (point)) (= start (line-beginning-position))) (beginning-of-line))
    (setq transient-mark-mode (cons 'only transient-mark-mode)))

(defun potion-emacs/delete-backward-char ()
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

(defun potion-emacs/set-indentation-width (n)
  "Sets the editor's indentation width"
  (dolist (variable potion-emacs/indentation-variables) (set-default variable n)))

(defun potion-emacs/set-indent-level ()
    "Sets the editor's indentation width by user input"
    (interactive)
    (potion-emacs/set-indentation-width (string-to-number (read-key-sequence "Indent by"))))

(defun potion-emacs/kbd (shortcut)
  "Wraps the kbd function but replaces the command key with whatever the user has set"
  (if (string= "s-" (substring shortcut 0 2))
    (kbd (concat potion-emacs/command-key (substring shortcut 1)))
    (kbd shortcut)))

(defun potion-emacs/select-all ()
  "Selects the entire buffer"
  (interactive)
  (set-mark (point-min))
  (goto-char (point-max)))

(defun potion-emacs/close-tab-or-window ()
    "Closes a tab or split window (but leaves at least 1 tab open)"
    (interactive)
    (if (eq (selected-window) potion-emacs/main-window)
        (if (> (length (tab-line-tabs-window-buffers)) 1) (kill-this-buffer))
        (delete-window)))

(defun potion-emacs/open-terminal ()
    "Opens a pretty terminal tab with no configuration needed"
    (interactive)
    (ansi-term potion-emacs/terminal))

(defun potion-emacs/replace ()
    "Replaces currently searched regex"
    (interactive)
    (setq replacement (read-from-minibuffer "String replacement: "))
    (set-mark (point))
    (goto-char (point-min))
    (while (re-search-forward "hello" nil t)
        (replace-match replacement))
    (goto-char (mark))
    (deactivate-mark))

;; Key bindings
(global-set-key (potion-emacs/kbd "s-S-<left>") (lambda () (interactive) (potion-emacs/move-beginning-of-line 1))) ;; CMD + left moves to beginning of line
(global-set-key (potion-emacs/kbd "s-<left>") (lambda () (interactive) (potion-emacs/move-beginning-of-line nil))) ;; CMD + left moves to beginning of line
(global-set-key (potion-emacs/kbd "s-<right>") 'move-end-of-line)                                                  ;; CMD + right moves to end of line
(global-set-key (potion-emacs/kbd "s-<up>") 'beginning-of-buffer)                                                  ;; CMD + up moves to beginning of file
(global-set-key (potion-emacs/kbd "s-<down>") 'end-of-buffer)                                                      ;; CMD + down moves to end of file
(global-set-key (potion-emacs/kbd "s-l") 'potion-emacs/highlight-line)                                             ;; CMD + L highlights the current line
(global-set-key (potion-emacs/kbd "s-S-<return>") 'potion-emacs/open-terminal)                                     ;; Opens an Emacs terminal shell
(global-set-key (potion-emacs/kbd "s-S-<enter>") 'potion-emacs/open-terminal)                                      ;; Opens an Emacs terminal shell
(global-set-key (potion-emacs/kbd "s-<return>") 'execute-extended-command)                                         ;; Lets you run an Emacs function
(global-set-key (potion-emacs/kbd "s-<enter>") 'execute-extended-command)                                          ;; Lets you run an Emacs function
(global-set-key (potion-emacs/kbd "s-]") 'potion-emacs/indent-region)                                              ;; Indents the highlighted region
(global-set-key (potion-emacs/kbd "s-[") 'potion-emacs/outdent-region)                                             ;; Outdents the highlighted region
(global-set-key (potion-emacs/kbd "s-f") 'occur)                                                                   ;; Search regex match in current file
(global-set-key (potion-emacs/kbd "s-F") 'rgrep)                                                                   ;; Start an Emacs rgrep process (choose directory/filename)
(global-set-key (potion-emacs/kbd "s-r") 'potion-emacs/replace)                                                    ;; Replace currently searched regex
(global-set-key (potion-emacs/kbd "s-w") 'potion-emacs/close-tab-or-window)                                        ;; Close the current tab or window
(global-set-key (potion-emacs/kbd "s-q") 'potion-emacs/kill-emacs)                                                 ;; Just quit Emacs
(global-set-key (potion-emacs/kbd "s-s") 'save-buffer)                                                             ;; Save current buffer
(global-set-key (potion-emacs/kbd "s-S") 'write-file)                                                              ;; Save as
(global-set-key (potion-emacs/kbd "s-o") 'find-file)                                                               ;; Open file
(global-set-key (potion-emacs/kbd "s-t") 'potion-emacs/open-empty-buffer)                                          ;; Open empty buffer
(global-set-key (potion-emacs/kbd "s-i") 'potion-emacs/set-indent-level)                                           ;; Sets indentation width
(global-set-key (potion-emacs/kbd "s-a") 'potion-emacs/select-all)                                                 ;; Selects the entire buffer
(global-set-key (potion-emacs/kbd "s-z") 'undo)                                                                    ;; Undo an action
(global-set-key (potion-emacs/kbd "s-Z") 'redo)                                                                    ;; Redo an action
(global-set-key (potion-emacs/kbd "s-x") 'kill-region)                                                             ;; Cut
(global-set-key (potion-emacs/kbd "s-c") 'kill-ring-save)                                                          ;; Copy
(global-set-key (potion-emacs/kbd "s-v") 'yank)                                                                    ;; Paste
(global-set-key (potion-emacs/kbd "s-|") 'neotree-toggle)                                                          ;; Toggles the project tree viewer
(global-set-key (potion-emacs/kbd "s-/") 'neotree-dir)                                                             ;; Changes the root directory of project tree viewer
(global-set-key (potion-emacs/kbd "s-1") (lambda () (interactive) (potion-emacs/global-tab-switch 0)))             ;; Switch to tab 1
(global-set-key (potion-emacs/kbd "s-2") (lambda () (interactive) (potion-emacs/global-tab-switch 1)))             ;; Switch to tab 2
(global-set-key (potion-emacs/kbd "s-3") (lambda () (interactive) (potion-emacs/global-tab-switch 2)))             ;; Switch to tab 3
(global-set-key (potion-emacs/kbd "s-4") (lambda () (interactive) (potion-emacs/global-tab-switch 3)))             ;; Switch to tab 4
(global-set-key (potion-emacs/kbd "s-5") (lambda () (interactive) (potion-emacs/global-tab-switch 4)))             ;; Switch to tab 5
(global-set-key (potion-emacs/kbd "s-6") (lambda () (interactive) (potion-emacs/global-tab-switch 5)))             ;; Switch to tab 6
(global-set-key (potion-emacs/kbd "s-7") (lambda () (interactive) (potion-emacs/global-tab-switch 6)))             ;; Switch to tab 7
(global-set-key (potion-emacs/kbd "s-8") (lambda () (interactive) (potion-emacs/global-tab-switch 7)))             ;; Switch to tab 8
(global-set-key (potion-emacs/kbd "s-9") 'potion-emacs/final-tab-switch)                                           ;; Switch to last tab
(global-set-key (potion-emacs/kbd "<backspace>") 'potion-emacs/delete-backward-char)                               ;; Overrides backspace to handle space tabs
(global-set-key (potion-emacs/kbd "<escape>") 'keyboard-escape-quit)                                               ;; You can use escape key to quit command line
(global-set-key (potion-emacs/kbd "<tab>") 'tab-to-tab-stop)                                                       ;; Tab adds a couple spaces

;; Okay we're done now
(potion-emacs/set-indentation-width potion-emacs/initial-tab-width)
(provide 'potion-emacs)
