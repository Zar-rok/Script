;; To add at the end of the .doom.d/config.el file

(global-set-key (kbd "C-c C-v") 'clipboard-yank)

(setq show-paren-delay 0)
(setq doom-theme 'doom-gruvbox)
(setq-default display-line-numbers-type 'visual
              display-line-numbers-current-absolute t
              display-line-numbers-width 3
              display-line-numbers-widen t)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(add-to-list 'default-frame-alist '(font . "Hack-12"))

(require 'evil)
(evil-ex-define-cmd "q" 'kill-this-buffer)
(evil-ex-define-cmd "quit" 'evil-quit)
