;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; General

(setq-default major-mode 'org-mode)

(setq user-full-name "Zar_Rok"
      user-mail-address "zar_rok@live.com")

(setq-default display-line-numbers-type 'relative
              display-line-numbers-width 3
              display-line-numbers-widen t
              show-paren-delay 0
              tab-width 4
              x-stretch-cursor t)

(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(setq undo-limit 80000000
      evil-want-fine-undo t
      auto-save-default t
      truncate-string-ellipsis "…")

(delete-selection-mode 1)
(global-subword-mode 1)

;; Doom

(setq doom-theme 'doom-gruvbox)
(setq doom-font (font-spec :family "Hack" :size 16)
      doom-big-font (font-spec :family "Hack" :size 16)
      doom-variable-pitch-font (font-spec :family "Hack" :size 16)
      doom-serif-font (font-spec :family "Hack" :size 16))

(after! doom-modeline
        (remove-hook 'doom-modeline-mode-hook #'size-indication-mode))

(defun doom-modeline-conditional-buffer-encoding ()
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))
(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

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

;; Windows

(setq evil-vsplit-window-right t
      evil-split-window-below t)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))

(setq +ivy-buffer-preview t)

(custom-set-variables
 '(zoom-size '(0.618 . 0.618)))

;; Evil

(require 'evil)
(evil-ex-define-cmd "q" 'kill-this-buffer)
(evil-ex-define-cmd "quit" 'evil-quit)

(after! evil (evil-escape-mode nil))

;; Company

(use-package company
  :diminish company-mode
  :init
  (global-company-mode)
  :config
  ;; set default `company-backends'
  (setq company-backends
        '((company-files          ; files & directory
           company-keywords       ; keywords
           company-capf)  ; completion-at-point-functions
          (company-abbrev company-dabbrev))))

(use-package company-statistics
    :init
    (company-statistics-mode))

(use-package company-try-hard
    :bind
    (("C-<tab>" . company-try-hard)
     :map company-active-map
     ("C-<tab>" . company-try-hard)))

(use-package company-quickhelp
    :config
    (company-quickhelp-mode))

;; Python

(use-package elpy
    :bind
    (:map elpy-mode-map
          ("C-M-n" . elpy-nav-forward-block)
          ("C-M-p" . elpy-nav-backward-block))
    :hook ((elpy-mode . flycheck-mode)
           (elpy-mode . (lambda ()
                          (set (make-local-variable 'company-backends)
                               '((elpy-company-backend :with company-yasnippet))))))
    :init
    (elpy-enable)
    :config
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)
          elpy-rpc-python-command "python3"
          elpy-rpc-timeout 2
          eldoc-idle-delay 2
          company-idle-delay 0.5))

(use-package py-pyment
    :config
    (setq py-pyment-options '("--output=numpydoc")))

(use-package py-isort
    :hook (python-mode . py-isort-enable-on-save)
    :config
    (setq py-isort-options '("--line-length=100" "-m=3" "-tc" "-fgw=0" "-ca")))

(use-package py-autoflake
    :hook (python-mode . py-autoflake-enable-on-save)
    :config
    (setq py-autoflake-options '("--expand-star-imports")))

(use-package py-docformatter
    :hook (python-mode . py-docformatter-enable-on-save)
    :config
    (setq py-docformatter-options '("--wrap-summaries=100" "--pre-summary-newline")))

(use-package blacken
    :hook (python-mode . blacken-mode)
    :config
    (setq blacken-line-length '100))

(use-package python-docstring
    :hook (python-mode . python-docstring-mode))

(require 'yasnippet)
(setq yas-triggers-in-field t)
(yas-reload-all)

(add-hook! 'python-mode-hook #'yas-minor-mode)
(add-hook! 'python-mode-hook #'color-identifiers-mode)
(add-hook! 'python-mode-hook #'auto-highlight-symbol-mode)
(add-hook! 'python-mode-hook (modify-syntax-entry ?_ "w"))

(after! python
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter"))
