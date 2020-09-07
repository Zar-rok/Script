;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

;; General

(package! zoom)
(package! color-identifiers-mode)

;; Dev

(package! dap-mode)
(package! lsp-mode)
(package! lsp-ui)
(package! direnv)
(package! company-statistics)
(package! yasnippet)
(package! yasnippet-snippets)

;; Python

(package! blacken)
(package! ein)
(package! lsp-python-ms)
(package! lsp-pyright)
(package! py-isort)

(package! buftra
  :recipe (:host github :repo "humitos/buftra.el"))
(package! py-pyment
  :recipe (:host github :repo "humitos/py-cmd-buffer.el"))
