(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("6678ff97b5a734075093a5eb9694bfef55090bc7bcd412943a9f05e869ba9392" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

 (add-to-list 'load-path "~/.emacs.d/lisp")
 (require 'basic-theme)
 (defun reapply-themes ()
   (load-theme 'basic))
 (add-hook 'after-init-hook 'reapply-themes)
