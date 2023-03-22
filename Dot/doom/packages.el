;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

;; General

(package! modus-themes)

(package! keycast)

;; Dev

(package! ipython-shell-send)

(package! gitconfig-mode
  :recipe (:host github :repo "magit/git-modes"
           :files ("gitconfig-mode.el")))
(package! gitignore-mode
  :recipe (:host github :repo "magit/git-modes"
           :files ("gitignore-mode.el")))

;; Python

(package! buftra
  :recipe (:host github :repo "humitos/buftra.el"))
(package! py-autoflake
  :recipe (:host github :repo "humitos/py-autoflake.el"))

(package! numpydoc)
