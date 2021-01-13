;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Local var

(setq user-full-name nil
      user-mail-address nil
      current-home (getenv "HOME")
      org-notes-path (concat current-home nil)
      org-roam-directory-path (concat org-notes-path nil)
      biblio-path (concat current-home nil)
      dict-path (concat current-home nil)
      plantuml-path (concat current-home nil)
      font-name nil
      hunspell-path nil
      epdfinfo-path nil)

;; General

(setq-default major-mode 'org-mode)

(setq undo-limit 80000000
      evil-want-fine-undo t
      auto-save-default t
      truncate-string-ellipsis "…"
      ispell-program-name hunspell-path)

(setq-default display-line-numbers-type 'relative
              display-line-numbers-width 3
              display-line-numbers-widen t
              show-paren-delay 0
              tab-width 4
              x-stretch-cursor t)

(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(setenv "DICPATH" dict-path)
(setenv "DICTIONARY" "en-custom,fr")

(defun doom-dashboard-widget-banner () nil)
(defun doom-dashboard-widget-loaded () nil)
(defun doom-dashboard-widget-footer () nil)

(delete-selection-mode 1)
(global-subword-mode 1)

(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)
(add-hook 'Info-mode-hook #'mixed-pitch-mode)

(use-package! display-fill-column-indicator
  :hook (prog-mode . display-fill-column-indicator-mode)
  :config
  (setq fill-column 80)
  (set-face-attribute 'fill-column-indicator nil :foreground "grey25"))

;; Doom

(setq doom-theme 'doom-gruvbox)
(setq doom-font (font-spec :family font-name :size 18)
      doom-big-font (font-spec :family font-name :size 18)
      doom-variable-pitch-font (font-spec :family font-name :size 18)
      doom-serif-font (font-spec :family font-name :size 18))

(after! which-key
  (setq which-key-idle-delay 0.05))

;; Key bindings

(map! :leader
      (:prefix "w"
       :n "<up>" #'evil-window-up
       :n "<right>" #'evil-window-right
       :n "<down>" #'evil-window-down
       :n "<left>" #'evil-window-left
       :n "DEL" #'kill-buffer-and-window)
      (:prefix "v"
       :desc "Spell check the buffer" :n "v" #'flyspell-buffer))

(map! :after pdf-view
      :map pdf-view-mode-map
      :n "<s-wheel-up>" #'image-backward-hscroll
      :n "<s-wheel-down>" #'image-forward-hscroll
      :n "<s-double-wheel-up>" #'image-backward-hscroll
      :n "<s-double-wheel-down>" #'image-forward-hscroll
      :n "<s-triple-wheel-up>" #'image-backward-hscroll
      :n "<s-triple-wheel-down>" #'image-forward-hscroll)

(map! :after evil-org
      :map evil-org-mode-map
      :n "S-<up>" #'org-previous-visible-heading
      :n "S-<down>" #'org-next-visible-heading)

(map! :after python
      :map python-mode-map
      :n "S-<up>" #'python-nav-backward-block
      :n "S-<down>" #'python-nav-forward-block)

(map! :n "<end>" #'end-of-line)
(map! :n "<home>" #'beginning-of-line)
(evil-global-set-key 'insert (kbd "<end>") #'end-of-line)
(evil-global-set-key 'insert (kbd "<home>") #'beginning-of-line)

;; Org

(setq org-directory org-notes-path
      org-roam-directory org-roam-directory-path
      org-log-done 'time
      org-babel-python-command "python3"
      org-plantuml-jar-path plantuml-path
      org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)")))

(defun my/org-ref-open-pdf-at-point ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
         (pdf-file (funcall org-ref-get-pdf-filename-function key)))
    (if (file-exists-p pdf-file)
        (find-file pdf-file)
      (message "No PDF found for %s" key))))

(setq org-ref-open-pdf-function #'my/org-ref-open-pdf-at-point)

(use-package org-noter-pdftools
  :after org-noter
  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(use-package! org-ref
    :config
    (setq org-ref-get-pdf-filename-function (lambda (key) (car (bibtex-completion-find-pdf key)))
          org-ref-default-bibliography (list biblio-path)
          org-ref-bibliography-notes org-notes-path
          org-ref-note-title-format "* NOTES %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
          org-ref-notes-directory org-notes-path
          org-ref-notes-function #'orb-edit-notes))

(after! org-ref
  (setq bibtex-completion-notes-path org-notes-path
        bibtex-completion-pdf-open-function (lambda (fpath) (start-process "open" "*open*" "open" fpath))
        bibtex-completion-bibliography biblio-path
        bibtex-completion-pdf-field "file"
        bibtex-completion-notes-template-multiple-files
        (concat "#+TITLE: ${title}\n"
                "#+ROAM_KEY: cite:${=key=}"
                "#+ROAM_TAGS: ${keywords}"
                "#+CREATED:%<%Y-%m-%d-%H-%M-%S>"
                "Time-stamp: <>\n"
                "- tags :: \n"
                "* NOTES \n"
                ":PROPERTIES:\n"
                ":Custom_ID: ${=key=}\n"
                ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
                ":AUTHOR: ${author-abbrev}\n"
                ":JOURNAL: ${journaltitle}\n"
                ":DATE: ${date}\n"
                ":YEAR: ${year}\n"
                ":DOI: ${doi}\n"
                ":URL: ${url}\n"
                ":END:\n\n")))

(use-package! org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq orb-preformat-keywords
        '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
  (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "literature/%<%Y-%m-%d-%H-%M-%S>-${slug}"
           :head "#+TITLE: ${=key=}: ${title}
#+ROAM_KEY: ${ref}
#+ROAM_TAGS:
Time-stamp: <>
- tags :: ${keywords}

* ${title}
  :PROPERTIES:
  :Custom_ID: ${=key=}
  :URL: ${url}
  :AUTHOR: ${author-or-editor}
  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")
  :NOTER_PAGE:
  :END:

"
           :unnarrowed t))))

(use-package! org-noter
  :after (:any org pdf-view)
  :config
  (setq
   org-noter-hide-other nil
   org-noter-notes-search-path org-notes-path))

;; Windows

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))

(setq +ivy-buffer-preview t)

;; Evil

(defadvice evil-inner-word (around underscore-as-word activate)
  (let ((table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?_ "w" table)
    (with-syntax-table table
      ad-do-it)))

(after! evil
  (evil-ex-define-cmd "q" #'kill-this-buffer))

;; PDF

(after! pdf-tools
  (setq pdf-info-epdfinfo-program epdfinfo-path))

;; Deft

(after! deft
  (setq deft-directory org-notes-path
        deft-recursive t))

;; Dev

(after! lsp-mode
  (setq lsp-keep-workspace-alive nil
        lsp-modeline-diagnostics-mode nil
        lsp-enable-file-watchers nil))

(after! lsp-ui
  (setq lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-delay 0))

(after! company
  (setq company-idle-delay 0.1
        company-show-numbers t)

  ;; https://oremacs.com/2017/12/27/company-numbers/
  (let ((map company-active-map))
  (mapc
   (lambda (x)
     (define-key map (format "%d" x) 'ora-company-number))
   (number-sequence 0 9))
  (define-key map " " (lambda ()
                        (interactive)
                        (company-abort)
                        (self-insert-command 1)))
  (define-key map (kbd "<return>") nil))

  (defun ora-company-number ()
  "Forward to `company-complete-number'.

Unless the number is potentially part of the candidate.
In that case, insert the number."
  (interactive)
  (let* ((k (this-command-keys))
         (re (concat "^" company-prefix k)))
    (if (cl-find-if (lambda (s) (string-match re s))
                    company-candidates)
        (self-insert-command 1)
      (company-complete-number (string-to-number k))))))

(use-package company-statistics
  :init (company-statistics-mode))

(use-package color-identifiers-mode
  :hook (prog-mode . color-identifiers-mode))

;; Python

(after! python
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-completion-native-disabled-interpreters '("jupyter")
        python-shell-prompt-detect-failure-warning nil))

(set-formatter! 'black "black -q --pyi -l 80 -" :modes '(python-mode))

(after! lsp-pyright
  (setq lsp-pyright-use-library-code-for-types t))

(after! dap-mode
  (setq dap-python-debugger 'debugpy))

(use-package py-pyment
  :config (setq py-pyment-options '("--output=numpydoc")))
