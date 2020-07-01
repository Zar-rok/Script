(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (gruvbox-light-hard)))
 '(custom-safe-themes
   (quote
    ("a06658a45f043cd95549d6845454ad1c1d6e24a99271676ae56157619952394a" default)))
 '(package-selected-packages (quote (ein org gruvbox-theme evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq-default display-line-numbers-type 'visual
              display-line-numbers-current-absolute t
              display-line-numbers-width 3
              display-line-numbers-widen t)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(setq show-paren-delay 0)
(show-paren-mode 1)

(add-to-list 'default-frame-alist '(font . "Hack-10" ))
(set-face-attribute 'default t :font "Hack-10")

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(load-theme 'gruvbox-light-hard)

(require 'evil)
(evil-mode 1)
(evil-ex-define-cmd "q" 'kill-this-buffer)
(evil-ex-define-cmd "quit" 'evil-quit)
