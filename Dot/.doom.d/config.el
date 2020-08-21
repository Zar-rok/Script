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

(after! doom-modeline
        (remove-hook 'doom-modeline-mode-hook #'size-indication-mode)
        (setq doom-modeline-buffer-encoding nil))

(setq which-key-idle-delay 0.5)

;; Org

(setq org-directory "~/org/")

;; Key bindings

(map! :leader
      (:prefix "w"
       :desc "Move to the top window" :n "<up>" #'evil-window-up
       :desc "Move to the right window" :n "<right>" #'evil-window-right
       :desc "Move to the bottom window" :n "<down>" #'evil-window-down
       :desc "Move to the left window" :n "<left>" #'evil-window-left))

;; Evil

(require 'evil)
(evil-ex-define-cmd "q" 'kill-this-buffer)
(evil-ex-define-cmd "quit" 'evil-quit)

(after! evil (evil-escape-mode nil))

;; Python

(use-package elpy
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

(setq eldoc-idle-delay 2
      company-idle-delay 0.5
      elpy-rpc-backend "jedi"
      elpy-rpc-python-command "python3")

(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)

(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(require 'yasnippet)
(setq yas-triggers-in-field t)

(yas-reload-all)
(add-hook 'python-mode-hook #'yas-minor-mode)
(add-hook 'python-mode-hook 'color-identifiers-mode)
(add-hook 'python-mode-hook 'auto-highlight-symbol-mode)
