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
      epdfinfo-path (concat current-home nil))

;; General

(setq initial-major-mode #'org-mode
      initial-scratch-message
      "#+BEGIN_SRC python
#+END_SRC

#+BEGIN_SRC calc
#+END_SRC"
      org-agenda-skip-deadline-prewarning-if-scheduled 3
      browse-url-browser-function #'browse-url-firefox
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
      warning-fill-column fill-column
      standard-indent 2)

;; (advice-add #'vterm--redraw :around (lambda (fun &rest args) (let ((cursor-type cursor-type)) (apply fun args)))) ;; https://github.com/akermu/emacs-libvterm/issues/313#issuecomment-1183650463

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

(use-package! display-fill-column-indicator
  :hook (prog-mode . display-fill-column-indicator-mode)
  :config (set-face-attribute 'fill-column-indicator nil :background "gray66"))

(add-to-list 'auto-mode-alist '("\\.fish\\'" . fish-mode))
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))
(add-to-list 'auto-mode-alist '("\\.eml\\'" . mail-mode))

(defun zar/embark-ediff-files (fileA)
  (interactive "f")
  (ediff-files fileA (consult--find "File B to compare: " #'consult--locate-builder (file-name-nondirectory fileA))))

(defun zar/dired-ediff-files ()
  "https://oremacs.com/2017/03/18/dired-ediff/"
  (interactive)
  (let ((files (dired-get-marked-files)))
    (if (<= (length files) 2)
        (let* ((fileA (car files))
               (fileB (if (cdr files)
                          (cadr files)
                        (consult--find "File B to compare: " #'consult--locate-builder (file-name-nondirectory fileA)))))
          (ediff-files fileA fileB))
      (error "[!] No more than two files should be marked."))))

(use-package ultra-scroll
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

;; Language

(after! langtool
  (setq langtool-http-server-host "localhost"
        langtool-http-server-port 8081))

(use-package! jinx
  :defer t
  :hook (doom-first-input . global-jinx-mode)
  :config (setq jinx-languages "en_US fr-custom de"))

;; Eww

(after! eww
  (add-hook 'eww-after-render-hook #'eww-readable)
  (add-hook 'eww-after-render-hook #'olivetti-mode)
  (add-hook 'eww-after-render-hook (apply-partially #'fringe-mode 0)))

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
      doom-font (font-spec :family "Iosevka Comfy" :size 24)
      doom-big-font (font-spec :family "Iosevka Comfy" :size 34)
      doom-variable-pitch-font (font-spec :family "Iosevka Comfy Duo" :size 24)
      doom-serif-font (font-spec :family "Iosevka Comfy Motion" :size 24))

(use-package! modus-themes
  :config
  (setq modus-themes-completions
        '((matches . (extrabold underline))
          (selection . (semibold italic)))))

;; Keycast

(use-package! keycast :defer t)

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

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-calendar-want-org-bindings t))

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

(defun zar/latex_update_pdf_buffer (buffer_name)
  "Inspired by https://old.reddit.com/r/emacs/comments/9hpa3g/how_to_get_live_preview_of_pdf_output_of_latex/e6ds838/"
  (let ((buffer (find-buffer-visiting buffer_name)))
    (if buffer
        (with-current-buffer
            (find-buffer-visiting buffer_name)
          (pdf-view-revert-buffer nil t))
      (format "[!] Buffer '%s' not found." buffer_name))))

(after! tex
  (setq-default TeX-shell "zsh"
                TeX-engine 'luatex
                +latex-viewers '(pdf-tools mupdf))
  (set-formatter! 'latexindent '("latexindent" "--logfile=/dev/null" "-y=defaultIndent: \"  \"") :modes '(tex-mode))
  (add-hook 'TeX-after-compilation-finished-functions #'zar/TeX-after-compilation-finished-functions)
  (add-to-list 'TeX-command-list '("Pdfsizeopt" "pdfsizeopt %(O?pdf) opt_%(O?pdf)" TeX-run-command nil (plain-tex-mode latex-mode) :help "Optimize PDF size"))
  (add-to-list 'TeX-command-list '("Preview" "latexmk -pvc -pv- -silent -lualatex -e \"\\$pdf_update_method=4; \\$pdf_update_command=\\\"emacsclient -e '(zar/latex_update_pdf_buffer \\\\\\\"%(O?pdf)\\\\\\\")'\\\"\" %t" TeX-run-command nil (plain-tex-mode latex-mode) :help "Continously preview the current file")))

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
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.ruff_cache\\'")
  (lsp-register-client
   (make-lsp-client
    :new-connection
    (lsp-stdio-connection (list "swipl"
                                "-g" "use_module(library(lsp_server))."
                                "-g" "lsp_server:main"
                                "-t" "halt"
                                "--" "stdio"))
    :major-modes '(prolog-mode)
    :priority 1
    :multi-root t
    :server-id 'prolog-ls)))

(after! lsp-ui
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-doc-show-with-mouse nil))

(after! lsp-ruff-lsp
  (setq lsp-ruff-lsp-ruff-args ["--line-length=80"]))

(use-package! treesit-auto
  :custom (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package envrc
  :hook (doom-first-input . envrc-global-mode))

;; LSP booster

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

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

(after! doom-modeline
  (setq doom-modeline-env-python-executable "python3"))

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

;; Vertico

(after! vertico
  (setq vertico-buffer-display-action
        `(display-buffer-in-direction
          (direction . right)
          (window-height . 0.3))))

(vertico-multiform-mode)

(after! vertico-multiform
  (setq vertico-multiform-commands
        '((consult-imenu buffer)
          (jinx-correct buffer)))
  (setq vertico-multiform-categories
        '((consult-location buffer)
          (consult-grep buffer))))

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
      (:prefix "o"
       :n "w" #'browse-url-at-point)
      (:prefix "t"
       :n "t" #'modus-themes-toggle))

(map! :prefix "g"
      (:prefix "s"
       :n "l" #'evil-avy-goto-line))

(defun zar/forward-sentence-begin-scroll-top-dwim ()
  (interactive)
  (evil-forward-sentence-begin)
  (if (eq 0 (string-match-p "\\`\\s-*$" (thing-at-point 'line)))
      (evil-scroll-line-to-top nil)))

(map! :after eww
      :map eww-mode-map
      :n ")" #' zar/forward-sentence-begin-scroll-top-dwim)

(map! :after scroll-lock
      :map scroll-lock-mode-map
      :n "}" #'scroll-lock-forward-paragraph
      :n "{" #'scroll-lock-backward-paragraph)

(map! :map org-mode-map
      :leader
      (:prefix "m"
               (:prefix "d"
                :n "t" #'zar/org-time-stamp)))

(map! :after shr
      :map shr-map
      "v" nil)

(map! :after embark
      :map embark-file-map
      "=" #'zar/embark-ediff-files)

(map! :after cdlatex
      :map cdlatex-mode-map
      :i "<backtab>" #'cdlatex-tab)

(map! :after vertico
      :map vertico-map
      :i "C-q" #'vertico-quick-exit)

(map! :after corfu
      :map corfu-map
      :i "C-q" #'corfu-quick-complete)

(map! :after numpydoc
      :map python-mode-map
      "C-c C-n" #'numpydoc-generate)

(map! :after jinx
      :map jinx-mode-map
      :prefix "z"
      :n "=" #'jinx-correct
      :n "]" #'jinx-next
      :n "[" #'jinx-previous)

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
