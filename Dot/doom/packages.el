;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

;; General

(package! info-colors)
(package! color-identifiers-mode)

;; Notes

(package! org-ref)
(package! org-roam-bibtex)
(package! org-noter-pdftools)

;; Dev

(package! company-statistics)

;; Python

(package! buftra
  :recipe (:host github :repo "humitos/buftra.el"))
(package! py-pyment
  :recipe (:host github :repo "humitos/py-cmd-buffer.el"))
