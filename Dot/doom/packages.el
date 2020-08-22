;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

;; General

(package! zoom)
(package! auto-highlight-symbol)
(package! color-identifiers-mode)

;; Dev

(package! yasnippet)
(package! yasnippet-snippets)

(package! company-try-hard)
(package! company-quickhelp)
(package! company-statistics)

;; Python

(package! ein)
(package! elpy)
(package! blacken)
(package! python-docstring)

(package! buftra
  :recipe (:host github :repo "humitos/buftra.el"))
(package! py-pyment
  :recipe (:host github :repo "humitos/py-cmd-buffer.el"))
(package! py-isrot
  :recipe (:host github :repo "humitos/py-cmd-buffer.el"))
(package! py-autoflake
  :recipe (:host github :repo "humitos/py-cmd-buffer.el"))
(package! py-docformatter
  :recipe (:host github :repo "humitos/py-cmd-buffer.el"))
