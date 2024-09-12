;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Local var

(setq user-full-name nil
      user-mail-address nil
      current-home (getenv "HOME")
      default-directory current-home
      org-notes-path (concat current-home nil)
      org-meetings-path (concat org-notes-path nil)
      org-dailies-path (concat org-notes-path nil)
      biblio-path (concat current-home nil)
      personal-dictionnary (concat current-home nil)
      plantuml-path (concat current-home nil)
      epdfinfo-path (concat current-home nil))

;; General

(setq initial-major-mode #'org-mode
      initial-scratch-message
      "#+BEGIN_SRC python
#+END_SRC

#+BEGIN_SRC calc
#+END_SRC"
      org-agenda-skip-deadline-prewarning-if-scheduled 3
      undo-limit 80000000
      evil-want-fine-undo t
      auto-save-default t
      truncate-string-ellipsis "…"
      system-time-locale "C"
      calendar-week-start-day 1
      select-enable-primary t
      select-enable-clipboard t
      kill-do-not-save-duplicates t
      save-interprogram-paste-before-kill t
      +modeline-height 16
      +format-with-lsp nil
      fill-column 80
      warning-fill-column fill-column)

(setq-default display-line-numbers-type 'relative
              display-line-numbers-width 2
              display-line-numbers-widen t
              tab-width 2
              x-stretch-cursor t)

(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override "main"))

(add-hook 'org-mode-hook (lambda () (org-next-visible-heading 1)))

(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(delete-selection-mode 1)
(global-subword-mode 1)

(modify-syntax-entry ?< "(>" )
(modify-syntax-entry ?> ")<" )

(use-package! display-fill-column-indicator
  :defer t
  :hook (prog-mode . display-fill-column-indicator-mode)
  :config
  (set-face-attribute 'fill-column-indicator nil :background "gray66"))

(use-package! orderless
  :defer t
  :custom (completion-styles '(orderless)))

(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; Language

(use-package langtool
  :defer t
  :config
  (setq langtool-http-server-host "localhost"
        langtool-http-server-port 8081))

(setenv "DICPATH" "/usr/share/hunspell")
(setenv "DICTIONARY" "en_US,fr-custom,de")
(after! ispell
  (setq ispell-program-name "hunspell"
        ispell-personal-dictionary personal-dictionnary))

;; Eww

(after! eww
  (add-hook 'eww-after-render-hook #'eww-readable))

(after! shr
  ;; Has an impact on evil-easymotion?
  (setq shr-width 80
        shr-max-width 80))

(after! evil-easymotion
  (evilem-make-motion
   evilem-motion-shr-next-link #'shr-next-link
   :scope 'visible
   :initial-point #'window-start))

;; Doom

(setq doom-theme 'modus-operandi
      doom-font (font-spec :family "Iosevka Term" :size 24)
      doom-big-font (font-spec :family "Iosevka Term" :size 34)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 24)
      doom-serif-font (font-spec :family "Libertinus Serif" :size 24))

(use-package! modus-themes
  :defer t
  :config
  (setq modus-themes-completions
        '((matches . (extrabold underline))
          (selection . (semibold italic)))))

;; Keycast

(use-package! keycast
  :defer t)

;; Dired

(after! dired
  (setq fd-dired-program "fdfind"))

;; Org

(defun zar/org-time-stamp ()
  (interactive)
  (if (string= "*" (current-word)) (insert " "))
  (org-time-stamp nil))

(defun zar/org-agenda-find-files ()
  (let* ((cmd (format "find %s -name \"*.org\"" org-notes-path))
         (files (shell-command-to-string cmd))
         (list-files (split-string files "\n")))
    (mapcar 'abbreviate-file-name list-files)))

(after! org
  (setq org-directory org-notes-path
        org-hide-emphasis-markers t
        org-ellipsis "…"
        org-fontify-done-headline t
        org-pretty-entities t
        org-roam-directory org-notes-path
        +org-roam-open-buffer-on-find-file nil
        org-log-done 'time
        org-babel-python-command "python3"
        org-plantuml-jar-path plantuml-path
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "STRT(s)" "HOLD(h)" "|" "DONE(d)"))
        org-agenda-files (zar/org-agenda-find-files)
        org-agenda-show-future-repeats nil
        org-capture-templates '(("m" "Meeting" entry (file org-meetings-path) "* %t %^g %?"))))

(after! org-roam
  (setq org-roam-directory org-notes-path
        +org-roam-open-buffer-on-find-file nil))

(use-package! org-modern
  :defer t
  :hook (org-mode . global-org-modern-mode)
  :config
  (setq org-modern-label-border 0.3
        ;; https://github.com/abougouffa/minemacs/blob/d40fb21fbd2db5810a2e08bec10c4813ecae2597/modules/me-org.el
        org-modern-list '((?+ . "➤") (?- . "–") (?* . "•"))
        org-modern-todo-faces
        '(("TODO" . (:inherit org-verbatim :weight semi-bold
                     :foreground "white" :background "red3"))
          ("NEXT" . (:inherit org-verbatim :weight semi-bold
                     :foreground "white" :background "OrangeRed3"))
          ("STRT" . (:inherit org-verbatim :weight semi-bold
                     :foreground "white" :background "RoyalBlue"))
          ("HOLD" . (:inherit org-verbatim :weight semi-bold
                     :foreground "white" :background "VioletRed"))
          ("DONE" . (:inherit org-verbatim :weight semi-bold
                     :foreground "white" :background "ForestGreen")))))

;; Avy

(defun avy-action-embark (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

(after! avy
  (setq avy-all-windows t)
  (setf (alist-get ?o avy-dispatch-alist) 'avy-action-embark))

;; Treemacs

(after! treemacs
  (setq treemacs-indentation 1
        treemacs-indent-guide-style 'line
        treemacs-follow-mode t
        treemacs-git-commit-diff-mode t))

;; Evil

(defadvice evil-inner-word (around underscore-as-word activate)
  (let ((table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?_ "w" table)
    (with-syntax-table table
      ad-do-it)))

(defun zar/save-then-kill-this-buffer ()
  "Save then kill the current buffer."
  (interactive)
  (basic-save-buffer)
  (kill-current-buffer))

(after! evil
  (evil-ex-define-cmd "wq" #'zar/save-then-kill-this-buffer)
  (evil-ex-define-cmd "q" #'kill-this-buffer)
  (customize-set-variable 'evil-want-minibuffer t)
  (setq +evil-want-o/O-to-continue-comments nil
        evil-move-cursor-back nil
        evil-kill-on-visual-paste nil))

;; PDF

(after! pdf-tools
  (setq pdf-info-epdfinfo-program epdfinfo-path))

;; Deft

(after! deft
  (setq deft-directory org-notes-path)
  (set-evil-initial-state! 'deft-mode 'normal))

;; Latex

(after! reftex
  (setq reftex-default-bibliography nil))

(defun zar/TeX-after-compilation-finished-functions (file-name)
  (shell-command (concat
                  "notify-send \"LatexMK\" \"Compilation finished\!\""
                  " -u low"
                  " -t 3000"
                  " -i /usr/share/icons/hicolor/48x48/apps/emacs28.png"))
  (shell-command "pkill -HUP mupdf || true"))

(after! tex
  (setq-default TeX-engine 'luatex
                +latex-viewers '(pdf-tools mupdf))
  (set-formatter! 'latexindent '("latexindent" "--logfile=/dev/null" "-y=defaultIndent: \"  \"") :modes '(tex-mode))
  (add-hook 'TeX-after-compilation-finished-functions #'zar/TeX-after-compilation-finished-functions)
  (add-to-list 'TeX-command-list '("Pdfsizeopt" "pdfsizeopt %(O?pdf) opt_%(O?pdf)" TeX-run-command nil (plain-tex-mode latex-mode doctex-mode ams-tex-mode texinfo-mode) :help "Optimize PDF size")))

;; Projectile

(after! projectile
  (add-to-list 'projectile-globally-ignored-directories ".direnv"))

(defun zar/projectile-diagnostics-provide-disabler ()
  (if (string= projectile-project-name "thesis-manuscript-gitlab")
      (setq lsp-diagnostics-provider :none)))

(add-hook 'projectile-after-switch-project-hook #'zar/projectile-diagnostics-provide-disabler)

;; Dev

(after! lsp-mode
  (setq lsp-log-io nil
        lsp-use-plists t
        lsp-keep-workspace-alive nil
        lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-enable-diagnostics nil
        lsp-headerline-breadcrumb-icons-enable nil
        lsp-headerline-breadcrumb-segments '(symbols)
        lsp-modeline-diagnostics-mode nil
        lsp-modeline-diagnostics-enable nil
        lsp-modeline-code-actions-enable nil
        lsp-diagnostics-provider :auto
        lsp-imenu-index-symbol-kinds '(Class Method Function Enum))
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.ipynb_checkpoints\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.ruff_cache\\'"))

(after! lsp-ui
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-doc-show-with-mouse nil))

(after! lsp-ruff-lsp
  (setq lsp-ruff-lsp-ruff-args ["--line-length=80"]))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Apheleia

(after! apheleia
  (setq apheleia-formatters-respect-fill-column t)
  (set-formatter! 'ruff-check-format
    '("ruff-check-format.sh" filepath
      (string-join (apheleia-formatters-fill-column "--line-length=")))
    :modes '(python-mode))
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(ruff-check-format)))

;; Python

(after! python
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True"))

(after! lsp-pyright
  (setq lsp-pyright-use-library-code-for-types t
        lsp-pyright-python-executable-cmd "python3"))

(after! dap-mode
  (setq dap-python-executable "python3"
        dap-python-debugger 'debugpy))

;; Coq

(defun zar/coq-buffer-font ()
  (interactive)
  (setq buffer-face-mode-face '(:family "Fira Code"))
  (buffer-face-mode))

(after! coq
  (add-hook 'coq-mode-hook 'zar/coq-buffer-font)
  (add-hook 'coq-goals-mode-hook 'zar/coq-buffer-font)
  (add-hook 'coq-response-mode-hook 'zar/coq-buffer-font))

;; org-protocol

(require 'org-protocol)

(defun mw-start-eww-for-url (plist)
  "raise emacs and call eww with the url in plist."
  (raise-frame)
  (eww (plist-get plist :url))
  (doom/window-maximize-buffer)
  nil)

(add-to-list 'org-protocol-protocol-alist
             '("eww"
               :protocol "eww"
               :function mw-start-eww-for-url))

;; Key bindings

(defun zar/window-move-dwim (direction)
  "Move to the window in the given DIRECTION, within Emacs if it exists,
  otherwise within i3wm. Raise an error if the DIRECTION is not equal to either:
  up, down, left, or right."
  (interactive)
  (if (not (member direction '("up" "down" "left" "right")))
      (error "The direction `%s' must be either equal to: `up', `down', `left', or `right'." direction))
  (condition-case nil
      (funcall (intern (format "evil-window-%s" direction)) 1)
    (error (shell-command (format "i3-msg \"focus %s\" -q" direction) nil nil))))

(defun zar/setup-window-move (direction)
  "Define move functions for the given DIRECTION."
  (with-temp-buffer
    (insert (format "(defun zar/move-%s () (interactive) (zar/window-move-dwim \"%s\"))" direction direction))
    (eval-buffer)))

(mapc #'zar/setup-window-move '("up" "down" "left" "right"))

(map! :leader
      (:n "<up>" #'zar/move-up
       :n "k" #'zar/move-up
       :n "<right>" #'zar/move-right
       :n "l" #'zar/move-right
       :n "<down>" #'zar/move-down
       :n "j" #'zar/move-down
       :n "<left>" #'zar/move-left
       :n "h" #'zar/move-left
       :n "ù" #'evil-switch-to-windows-last-buffer
       :n "DEL" #'kill-buffer-and-window)
      (:prefix "v"
       :desc "Spell check the buffer" :n "v" #'flyspell-buffer)
      (:prefix "o"
       :n "w" #'browse-url-at-point)
      (:prefix "t"
       :n "t" #'modus-themes-toggle))

(map! :prefix "g"
      :n "h" #'evil-avy-goto-char-timer)

(map! :map org-mode-map
      :leader
      (:prefix "m"
               (:prefix "d"
                :n "t" #'zar/org-time-stamp)))

(map! :map pdf-view-mode-map
      :n "<s-wheel-up>" #'image-backward-hscroll
      :n "<s-wheel-down>" #'image-forward-hscroll
      :n "<s-double-wheel-up>" #'image-backward-hscroll
      :n "<s-double-wheel-down>" #'image-forward-hscroll
      :n "<s-triple-wheel-up>" #'image-backward-hscroll
      :n "<s-triple-wheel-down>" #'image-forward-hscroll)

(map! :after cdlatex
      :map cdlatex-mode-map
      :i "<backtab>" #'cdlatex-tab)

(map! :after corfu
      :map corfu-map
      "M-q" #'corfu-quick-complete)

(map! :after numpydoc
      :map python-mode-map
      "C-c C-n" #'numpydoc-generate)

;; (map! :n "<end>" #'end-of-line)
;; (map! :n "<home>" #'beginning-of-line)
;; (evil-global-set-key 'insert (kbd "<end>") #'end-of-line)
;; (evil-global-set-key 'insert (kbd "<home>") #'beginning-of-line)

(setq mouse-avoidance-banish-position '((frame-or-window . frame)
                                        (side . left)
                                        (side-pos . 0)
                                        (top-or-bottom . top)
                                        (top-or-bottom-pos . 0)))
(mouse-avoidance-mode 'banish)

(defun zar/beamer-overlay-incr (beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (if (and beg end)
      (let ((delim-end (search-forward ">"))
            (delim-bend (search-backward "<")))
        (while (< (point) delim-end)
          ))))
