;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

;; General

(package! modus-themes)
(package! keycast)
(package! org-modern)
(package! org-ql)
(package! olivetti)
(package! jinx)
(package! osm)
(package! qrencode)
(package! fish-mode)
(package! topspace)
(package! casual-suite)
(package! all-the-icons-ibuffer)

(package! ultra-scroll
  :recipe (:host github :repo "jdtsmith/ultra-scroll"
           :files ("ultra-scroll.el")))

(package! evil-escape :disable t) ;; https://tecosaur.github.io/emacs-config/config.html#evil
(package! elfeed-goodies :disable t)
(package! doom-themes :disable t) ;; https://github.com/doomemacs/doomemacs/issues/8250

;; Dev

(package! envrc)

(package! gitconfig-mode
  :recipe (:host github :repo "magit/git-modes"
           :files ("gitconfig-mode.el")))

;; Python

(package! ipython-shell-send)
(package! numpydoc)
