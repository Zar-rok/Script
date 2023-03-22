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
      plantuml-path (concat current-home nil)
      font-name nil
      epdfinfo-path nil)

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
      +modeline-height 16)

(setq-default display-line-numbers-type 'relative
              display-line-numbers-width 3
              display-line-numbers-widen t
              show-paren-delay 0
              tab-width 4
              x-stretch-cursor t)

(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override "main"))

(add-hook 'org-mode-hook (lambda () (org-next-visible-heading 1)))
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(delete-selection-mode 1)
(global-subword-mode 1)

(use-package! display-fill-column-indicator
  :hook (prog-mode . display-fill-column-indicator-mode)
  :config
  (setq fill-column 80)
  (set-face-attribute 'fill-column-indicator nil :background "gray66"))

(use-package! modus-themes
  :init
  (modus-themes-load-themes)
  :config
  (modus-themes-load-operandi))

(use-package! orderless
  :custom (completion-styles '(orderless)))

(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; Language

(use-package langtool
  :config
  (setq langtool-http-server-host "localhost"
        langtool-http-server-port 8081))

;; https://github.com/flycheck/flycheck/issues/1762#issuecomment-750458442
;; (defvar-local zar/flycheck-local-cache nil)

;; (defun zar/flycheck-checker-get (fn checker property)
;;   (or (alist-get property (alist-get checker zar/flycheck-local-cache))
;;       (funcall fn checker property)))

;; (advice-add 'flycheck-checker-get :around 'zar/flycheck-checker-get)

;; (add-hook 'lsp-managed-mode-hook
;;           (lambda ()
;;             (when (derived-mode-p 'tex-mode)
;;               (setq zar/flycheck-local-cache '((lsp . ((next-checkers . (tex-chktex)))))))))

(setenv "DICPATH" "/usr/share/hunspell")
(setenv "DICTIONARY" "en_US,fr-custom,de")
(after! ispell
  (setq ispell-program-name "hunspell"
        ispell-personal-dictionary "~/.hunspell_personal"))

;; Eww

;; Has an impact on evil-easymotion?
(setq shr-width 80
      shr-max-width 80)

(add-hook 'eww-after-render-hook #'eww-readable)
;; (add-hook 'eww-after-render-hook (lambda () (scroll-lock-mode)))

(after! evil-easymotion
  (evilem-make-motion
   evilem-motion-shr-next-link #'shr-next-link
   :scope 'visible
   :initial-point #'window-start))

;; Doom

(setq doom-theme 'modus-operandi)
(setq doom-font (font-spec :family font-name :size 26)
      doom-big-font (font-spec :family font-name :size 26)
      doom-variable-pitch-font (font-spec :family font-name :size 26)
      doom-serif-font (font-spec :family font-name :size 26))

(after! which-key
  (setq which-key-idle-delay 0.05))

;; Org

(defun zar/org-time-stamp ()
  (interactive)
  (if (equal "*" (string (preceding-char)))
      (insert " "))
  (org-time-stamp nil))

(defun zar/org-agenda-find-files ()
  (let* ((cmd (format "find %s -name \"*.org\"" org-notes-path))
         (files (shell-command-to-string cmd))
         (list-files (split-string files "\n")))
    (mapcar 'abbreviate-file-name list-files)))

(after! org
  (setq org-directory org-notes-path
        org-roam-directory org-notes-path
        +org-roam-open-buffer-on-find-file nil
        org-log-done 'time
        org-babel-python-command "python3"
        org-plantuml-jar-path plantuml-path
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "STRT(s)" "HOLD(h)" "|" "DONE(d)"))
        org-agenda-files (zar/org-agenda-find-files)
        org-agenda-show-future-repeats nil
        org-capture-templates
        '(("m" "Meeting" entry (file org-meetings-path)
           "* %t %^g\n"))))

(after! org-roam
  (setq org-roam-directory org-notes-path
        +org-roam-open-buffer-on-find-file nil))

(setq org-fontify-done-headline t)
(custom-set-faces
 '(org-headline-done ((t (:strike-through t)))))

                                        ; (defun my/org-ref-open-pdf-at-point ()
                                        ;   "Open the pdf for bibtex key under point if it exists."
                                        ;   (interactive)
                                        ;   (let* ((results (org-ref-get-bibtex-key-and-file))
                                        ;          (key (car results))
                                        ;          (pdf-file (funcall org-ref-get-pdf-filename-function key)))
                                        ;     (if (file-exists-p pdf-file)
                                        ;         (find-file pdf-file)
                                        ;       (message "No PDF found for %s" key))))

                                        ; (setq org-ref-open-pdf-function #'my/org-ref-open-pdf-at-point)

                                        ; (use-package org-noter-pdftools
                                        ;   :after org-noter
                                        ;   :config
                                        ;   (with-eval-after-load 'pdf-annot
                                        ;     (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

                                        ; (use-package! org-noter
                                        ;   :after (:any org pdf-view)
                                        ;   :config
                                        ;   (setq
                                        ;    org-noter-hide-other nil
                                        ;    org-noter-notes-search-path (list org-notes-path)))

                                        ; (use-package! org-ref
                                        ;   :config
                                        ;   (setq org-ref-get-pdf-filename-function (lambda (key) (car (bibtex-completion-find-pdf key)))
                                        ; 	org-ref-default-bibliography (list biblio-path)
                                        ; 	org-ref-bibliography-notes org-notes-path
                                        ; 	org-ref-note-title-format "* NOTES %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
                                        ; 	org-ref-notes-directory org-notes-path
                                        ; 	org-ref-notes-function #'orb-edit-notes))

                                        ; (after! org-ref
                                        ;   (setq bibtex-completion-notes-path org-notes-path
                                        ; 	bibtex-completion-pdf-open-function (lambda (fpath) (start-process "open" "*open*" "open" fpath))
                                        ; 	bibtex-completion-bibliography biblio-path
                                        ; 	bibtex-completion-pdf-field "file"
                                        ; 	bibtex-completion-notes-template-multiple-files
                                        ; 	(concat "${title}\n"
                                        ; 		"#+ROAM_KEY: cite:${=key=}\n"
                                        ; 		"#+ROAM_TAGS: ${keywords}\n"
                                        ; 		"#+CREATED: %<%Y-%m-%d-%H-%M-%S>\n"
                                        ; 		"* Notes \n"
                                        ; 		":PROPERTIES:\n"
                                        ; 		":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
                                        ; 		":END:\n\n")))

                                        ; (use-package! org-roam-bibtex
                                        ;   :after (org-roam)
                                        ;   :hook (org-roam-mode . org-roam-bibtex-mode)
                                        ;   :config
                                        ;   (setq orb-preformat-keywords '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
                                        ;   (setq orb-templates
                                        ; 	'(("r" "ref" plain (function org-roam-capture--get-point)
                                        ; 	   ""
                                        ; 	   :file-name "literature/%<%Y-%m-%d-%H-%M-%S>-${slug}"
                                        ; 	   :head "#+TITLE: ${=key=}: ${title}
                                        ; #+ROAM_KEY: ${ref}
                                        ; #+ROAM_TAGS:
                                        ; Time-stamp: <>
                                        ; - tags :: ${keywords}

                                        ; * ${title}
                                        ;   :PROPERTIES:
                                        ;   :Custom_ID: ${=key=}
                                        ;   :URL: ${url}
                                        ;   :AUTHOR: ${author-or-editor}
                                        ;   :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")
                                        ;   :NOTER_PAGE:
                                        ;   :END:

                                        ; "
                                        ;            :unnarrowed t))))

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

(add-hook 'LaTeX-mode-hook 'prettify-symbols-mode)
(after! reftex
  (setq reftex-default-bibliography "/home/paul/Documents/thesis-manuscript-gitlab"))

(after! tex
  (setq TeX-engine 'luatex
        +latex-viewers '(pdf-tools mupdf)))

;; Projectile

(after! projectile
  (add-to-list 'projectile-globally-ignored-directories ".direnv"))

;; Dev

(after! lsp-mode
  (setq lsp-keep-workspace-alive nil
        lsp-modeline-diagnostics-mode nil
        lsp-enable-file-watchers nil))

(after! lsp-ui
  (setq lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-delay 0.1))

(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2)
  (add-hook 'evil-normal-state-entry-hook #'company-abort))

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

(use-package numpydoc
  :bind (:map python-mode-map
         ("C-c C-n" . numpydoc-generate)))

(set-formatter! 'black "black -q -l 80 -" :modes '(python-mode))

(setq py-autoflake-options
      '("--remove-all-unused-imports" "--remove-unused-variables"))
(add-hook! 'before-save-hook #'py-autoflake-enable-on-save)

(setq py-isort-options '("-l 80"))
(add-hook! 'before-save-hook #'py-isort-before-save)

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

(mapcar #'zar/setup-window-move '("up" "down" "left" "right"))

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

(map! :after org-mode
      :map org-mode-map
      (:prefix "m"
               (:prefix "d"
                :n "t" #'zar/org-time-stamp)))

(map! :after eww
      :map eww-mode-map
      (:prefix "g"
       :n "<tab>" #'evilem-motion-shr-next-link))

(map! :after pdf-view
      :map pdf-view-mode-map
      :n "<s-wheel-up>" #'image-backward-hscroll
      :n "<s-wheel-down>" #'image-forward-hscroll
      :n "<s-double-wheel-up>" #'image-backward-hscroll
      :n "<s-double-wheel-down>" #'image-forward-hscroll
      :n "<s-triple-wheel-up>" #'image-backward-hscroll
      :n "<s-triple-wheel-down>" #'image-forward-hscroll)

(map! :n "<end>" #'end-of-line)
(map! :n "<home>" #'beginning-of-line)
(evil-global-set-key 'insert (kbd "<end>") #'end-of-line)
(evil-global-set-key 'insert (kbd "<home>") #'beginning-of-line)

