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
      +doom-dashboard-banner-file (expand-file-name "logo.svg" doom-private-dir)
      +doom-dashboard-banner-padding '(0 . 7))

(defun doom-dashboard-widget-footer () nil)

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

(defadvice evil-inner-word (around underscore-as-word activate)
  (let ((table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?_ "w" table)
    (with-syntax-table table
      ad-do-it)))

(after! evil (evil-escape-mode nil))

;; Dev

(use-package lsp-mode
  :hook (prog-mode . lsp))

(use-package lsp-ui)

(setq lsp-ui-doc-enable nil)

(use-package company
  :config (setq company-idle-delay 0
                company-minimum-prefix-length 1))

(use-package company-statistics
    :init
    (company-statistics-mode))

(setq lsp-signature-auto-activate t lsp-signature-doc-lines 1)

(use-package dap-mode
    :after lsp-mode
    :config
    (dap-mode t)
    (dap-ui-mode t))

(require 'yasnippet)
(yas-reload-all)

;; Python

(add-hook 'python-hook #'yas-minor-mode)

(use-package lsp-python-ms
  :defer 0.3
  :custom (lsp-python-ms-auto-install-server t))

(use-package py-pyment
  :config (setq py-pyment-options '("--output=numpydoc")))

(use-package python
  :delight "π "
  :bind (("M-<up>" . python-nav-backward-block)
         ("M-<down>" . python-nav-forward-block))
  :preface (defun python-remove-unused-imports()
             "Removes unused imports and unused variables with autoflake."
             (interactive)
             (if (executable-find "autoflake")
                 (progn
                   (shell-command (format "autoflake --remove-all-unused-imports -i %s"
                                          (shell-quote-argument (buffer-file-name))))
                   (revert-buffer t t t))
               (warn "python-mode: Cannot find autoflake executable.")))
  :config (setq python-shell-interpreter "jupyter"
                python-shell-interpreter-args "console --simple-prompt"
                python-shell-prompt-detect-failure-warning nil
                python-shell-completion-native-disabled-interpreters '("jupyter")))

(use-package blacken
  :delight
  :hook (python-mode . blacken-mode)
  :custom (blacken-line-length 80))

(add-hook! 'before-save-hook #'blacken-buffer)

(use-package lsp-pyright
  :if (executable-find "pyright")
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

(use-package py-isort
  :after python
  :hook ((python-mode . pyvenv-mode)
         (before-save . py-isort-before-save)))

(use-package direnv
  :config
  (direnv-mode))

(use-package color-identifiers-mode
  :after python
  :hook (python-mode . color-identifiers-mode))

(setq ein:output-area-inlined-images t)
