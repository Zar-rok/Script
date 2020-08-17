;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; General

(setq-default major-mode 'org-mode)

(setq user-full-name "Zar_Rok"
      user-mail-address "zar_rok@live.com")

(setq-default display-line-numbers-type 'visual
              display-line-numbers-current-absolute t
              display-line-numbers-width 3
              display-line-numbers-widen t)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(setq show-paren-delay 0)

;; Doom

(setq doom-theme 'doom-gruvbox)
(setq doom-font (font-spec :family "Hack" :size 16)
      doom-big-font (font-spec :family "Hack" :size 16)
      doom-variable-pitch-font (font-spec :family "Hack" :size 16)
      doom-serif-font (font-spec :family "Hack" :size 16))

;; Org

(setq org-directory "~/org/")

;; Key bindings

(global-set-key (kbd "C-c C-v") 'clipboard-yank)

;; Evil

(require 'evil)
(evil-ex-define-cmd "q" 'kill-this-buffer)
(evil-ex-define-cmd "quit" 'evil-quit)

(after! evil (evil-escape-mode nil))

;; Python

(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

(setq elpy-rpc-python-command "python3")

(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(require 'yasnippet)
(setq yas-triggers-in-field t)

(yas-reload-all)
(add-hook 'python-mode-hook #'yas-minor-mode)
(add-hook 'python-mode-hook 'color-identifiers-mode)
(add-hook 'python-mode-hook 'auto-highlight-symbol-mode)
