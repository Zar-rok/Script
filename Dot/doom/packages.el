;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

;; General

(package! modus-themes)
(package! keycast)
(package! org-modern)
(package! nov)
(package! olivetti)
(package! jinx)
(package! osm)
(package! qrencode)
(package! fish-mode)
(package! topspace)
(package! evil-matchit)

(package! evil-escape :disable t) ;; https://tecosaur.github.io/emacs-config/config.html#evil

;; Dev

(package! gitconfig-mode
  :recipe (:host github :repo "magit/git-modes"
           :files ("gitconfig-mode.el")))

(package! screenshot
  :recipe (:host github :repo "tecosaur/screenshot"
           :files ("screenshot.el")))

(package! ultra-scroll
  :recipe (:host github :repo "jdtsmith/ultra-scroll"
           :files ("ultra-scroll.el")))

(package! treesit-auto)
(package! envrc)

;; Python

(package! ipython-shell-send)

(package! numpydoc)
