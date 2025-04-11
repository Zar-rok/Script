;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Local var

(load! "private.el")

;; General

(setq-default standard-indent 2
              tab-width 2)

(setopt initial-major-mode #'org-mode
        initial-scratch-message ""
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
        delete-by-moving-to-trash t
        window-combination-resize t
        x-stretch-cursor t
        display-line-numbers-type nil
        warning-fill-column fill-column
        custom-file null-device
        major-mode-remap-alist major-mode-remap-defaults ;; https://github.com/doomemacs/doomemacs/issues/8191#issuecomment-2522039422
        auto-revert-use-notify t
        fill-column 80
        left-fringe-width 15)

(setopt doom-font (font-spec :family "Iosevka Comfy" :size 24)
        doom-big-font (font-spec :family "Iosevka Comfy" :size 34)
        doom-variable-pitch-font (font-spec :family "Iosevka Comfy Duo" :size 24)
        doom-serif-font (font-spec :family "Iosevka Comfy Motion" :size 24))

(delete-selection-mode 1)
(global-subword-mode 1)

(add-to-list 'auto-mode-alist '("\\.fish\\'" . fish-mode))
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))
(add-to-list 'auto-mode-alist '("\\.eml\\'" . mail-mode))

;; UX

(after! doom-modeline
  ;; https://github.com/doomemacs/doomemacs/issues/1817#issuecomment-534179156
  (remove-hook 'doom-modeline-mode-hook #'size-indication-mode)
  (remove-hook 'doom-modeline-mode-hook #'column-number-mode))

(after! persp-mode
  (setopt persp-emacsclient-init-frame-behaviour-override "main"))

(after! display-fill-column-indicator
  (set-face-attribute 'fill-column-indicator nil :background "gray66")  )

;; pdf-tools

(after! pdf-tools
  (setopt tablist-context-window-display-action
          '((+popup-display-buffer-stacked-side-window-fn)
            (side . bottom)
            (slot . 2)
            (inhibit-same-window . t))))

;; Dired

(defun zar/embark-ediff-files (fileA)
  (interactive "f")
  (ediff-files fileA (consult--find "File B to compare: "
                                    #'consult--locate-builder
                                    (file-name-nondirectory fileA))))

(defun zar/dired-ediff-files ()
  "https://oremacs.com/2017/03/18/dired-ediff/"
  (interactive)
  (let ((files (dired-get-marked-files)))
    (if (<= (length files) 2)
        (let* ((fileA (car files))
               (fileB (if (cdr files)
                          (cadr files)
                        (consult--find "File B to compare: "
                                       #'consult--locate-builder
                                       (file-name-nondirectory fileA)))))
          (ediff-files fileA fileB))
      (error "[!] No more than two files should be marked."))))

(after! dired-x
  (setopt dired-omit-files (concat dired-omit-files "\\|^\\.\\w.*$")))

;; Language

(after! langtool
  (setopt langtool-http-server-host "localhost"
          langtool-http-server-port 8081))

(use-package! jinx
  :hook (doom-first-input . global-jinx-mode)
  :custom (jinx-languages "en_US fr-custom de"))

;; Eww

(after! eww
  (add-hook 'eww-after-render-hook #'eww-readable)
  (add-hook 'eww-after-render-hook #'olivetti-mode)
  (add-hook 'eww-after-render-hook (apply-partially #'fringe-mode 0)))

(after! shr
  ;; Has an impact on evil-easymotion?
  (setopt shr-width 80
          shr-max-width 80))

(after! evil-easymotion
  (evilem-make-motion
   evilem-motion-shr-next-link #'shr-next-link
   :scope 'visible
   :initial-point #'window-start))

;; Theme

(use-package! modus-themes
  :init
  (setopt doom-theme 'modus-operandi)
  :custom
  (modus-themes-completions
   '((matches . (extrabold underline))
     (selection . (semibold italic)))))

;; Keycast

(use-package! keycast :defer t)

;; ibuffer

(use-package! all-the-icons-ibuffer
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode)
  :custom (all-the-icons-ibuffer-icon-size 0.75))

;; Org

(defun zar/org-time-stamp ()
  (interactive)
  (if (string= "*" (current-word)) (insert " "))
  (org-time-stamp nil))

(defun zar/org-cbthunderlink-open (link _)
  "Visit the email pointed by a cbthunderlink LINK."
  (call-process "xdg-open" nil 0 nil (concat "cbthunderlink:" link)))

(defun zar/org-cbthunderlink-export (link description format _)
  "Export a cbthunderlink LINK with a DESCRIPTION from an Org file to a FORMAT file."
  (pcase format
    (`html (format "<a target=\"_blank\" href=\"%s\">%s</a>" link description))
    (`latex (format "\\href{%s}{%s}" link description))
    (`texinfo (format "@uref{%s,%s}" link description))
    (`ascii (format "%s (%s)" description link))
    (_ link)))

(defun zar/org-modern-agenda ()
  "Finalize Org agenda highlighting."
  (save-excursion
    (save-match-data
      (let ((case-fold-search)
            (agenda-re-faces '(("^.*-agenda (W[0-9]\\{2\\}):$" . 'org-modern-label)
                               ("^<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [[:word:]]+\\.?>$" . 'org-modern-date-active)
                               ("^[[:space:]]\\{17\\}" . 'org-modern-block-name)
                               ("^[[:space:]]\\{5\\}[0-9:]\\{4\\}[[:space:]]\\{8\\}" . 'org-modern-block-name)
                               ("^[[:space:]]\\{5\\}[0-9:-]\\{10\\}[[:space:]]\\{2\\}" . 'org-modern-block-name)
                               ("^[[:space:]]\\{4\\}[0-9:]\\{5\\}[[:space:]]\\{8\\}" . 'org-modern-block-name)
                               ("^[[:space:]]\\{4\\}[0-9:-]\\{11\\}[[:space:]]\\{2\\}" . 'org-modern-block-name)
                               ("^[[:space:]]+![ 0-9:-]\\{14\\}" . 'org-modern-todo)
                               ("^[[:space:]]+⧖[ 0-9:-]\\{14\\}" . 'org-modern-tag)
                               ("^￫[ 0-9]?[0-9]d[[:space:]]\\{12\\}" . 'org-modern-block-name)
                               ("^￩[ 0-9]?[0-9]d[[:space:]]\\{12\\}" . 'org-modern-priority))))
        (dolist (entry agenda-re-faces)
          (let ((entry-re (car entry))
                (entry-face (cdr entry)))
            (goto-char (point-min))
            (while (re-search-forward entry-re nil 'noerror)
              (let ((beg (match-beginning 0))
                    (end (match-end 0)))
                (when (s-contains? "^<" entry-re)
                  (put-text-property beg (1+ beg) 'display " ")
                  (put-text-property (1- end) end 'display " "))
                (put-text-property beg end 'face entry-face)))))))))

(after! org
  (org-link-set-parameters "cbthunderlink"
                           :follow #'zar/org-cbthunderlink-open
                           :export #'zar/org-cbthunderlink-export)
  (setopt org-directory org-notes-path
          org-hide-emphasis-markers t
          org-ellipsis truncate-string-ellipsis
          org-fontify-done-headline t
          org-pretty-entities t
          org-auto-align-tags nil
          org-tags-column 0
          org-roam-directory org-notes-path
          +org-roam-open-buffer-on-find-file nil
          org-log-done 'time
          org-babel-python-command "python3"
          org-deadline-warning-days 3
          org-log-into-drawer t
          org-latex-compiler "lualatex"
          org-babel-latex-preamble (lambda (_) (concat
                                                "\\documentclass[tikz, preview]{standalone}\n"
                                                "\\def\\pgfsysdriver{pgfsys-tex4ht.def}\n"
                                                "\\usepackage{svg}\n"))
          org-agenda-files org-agenda-files-path
          org-agenda-show-future-repeats nil
          org-agenda-skip-deadline-prewarning-if-scheduled 3
          org-agenda-skip-timestamp-if-done t
          org-agenda-skip-deadline-if-done t
          org-agenda-skip-scheduled-if-done t
          org-agenda-skip-scheduled-if-deadline-is-shown t
          org-agenda-skip-timestamp-if-deadline-is-shown t
          org-agenda-span 'day
          org-agenda-start-day "+0d"
          org-agenda-current-time-string ""
          org-agenda-time-grid '((daily) () "" "")
          org-agenda-hide-tags-regexp "."
          org-agenda-format-date "<%F %a>"
          org-agenda-deadline-leaders '("  ! " "￫%2xd" "￩%2xd")
          org-agenda-scheduled-leaders '("  ⧖ " "")
          org-agenda-prefix-format '((agenda . "%-4s%-11t    ")
                                     (todo   . "   ")
                                     (tags   . "   ")
                                     (search . "   "))
          org-agenda-custom-commands '(("w" "Weekly agenda"
                                        ((agenda "" ((org-agenda-span 'week)
                                                     (org-agenda-start-on-weekday 1))))))
          org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "STRT(s)" "HOLD(h)" "|" "DONE(d)")))
  (add-hook 'org-agenda-finalize-hook #'zar/org-modern-agenda))

(after! org-roam
  (setopt org-roam-directory org-notes-path
          +org-roam-open-buffer-on-find-file nil))

(after! org-modern
  (setopt org-modern-label-border 1
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

;; Elfeed -- Stolen from:
;; https://tecosaur.github.io/emacs-config/config.html#newsfeed
;; https://cundy.me/post/elfeed/

(after! elfeed
  (use-package! elfeed-link)
  (defface elfeed-show-title-face '((t (:weight ultrabold :slant italic :height 1.5)))
    "title face in elfeed show buffer"
    :group 'elfeed)
  (defface elfeed-show-author-face `((t (:weight light)))
    "title face in elfeed show buffer"
    :group 'elfeed)
  (set-face-attribute 'elfeed-search-title-face nil
                      :foreground 'unspecified
                      :weight 'light)

  (defun concatenate-authors (authors-list)
    "Given AUTHORS-LIST, list of plists; return string of all authors
concatenated."
    (mapconcat
     (lambda (author) (plist-get author :name))
     authors-list ", "))

  (defun +rss/elfeed-search-print-entry (entry)
    "Print ENTRY to the buffer."
    (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
           (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (entry-authors (concatenate-authors
                           (elfeed-meta entry :authors)))
           (title-width (/ (window-width) 2))
           (title-column (elfeed-format-column
                          (truncate-string-to-width
                           title title-width nil nil "⁞")
                          title-width
                          :left))
           (authors-width (- title-width 14))
           (authors-column (elfeed-format-column
                            (truncate-string-to-width
                             entry-authors authors-width nil nil "⁞")
                            authors-width
                            :left)))
      (insert " " (propertize title-column 'face title-faces 'kbd-help title) " ")
      (insert (propertize authors-column
                          'face 'elfeed-search-date-face
                          'kbd-help entry-authors) " ")
      (insert (propertize date 'face 'elfeed-search-date-face) " ")))

  (defun elfeed-olivetti (buff)
    "https://emacs.stackexchange.com/a/69363"
    (pop-to-buffer buff)
    (fringe-mode 0)
    (olivetti-mode)
    (olivetti-set-width 100))

  (setopt elfeed-search-filter "@2-week-ago"
          elfeed-search-print-entry-function '+rss/elfeed-search-print-entry
          elfeed-show-entry-switch 'elfeed-olivetti
          elfeed-show-entry-delete #'+rss/delete-pane
          shr-max-image-proportion 0.6))

(add-hook! 'elfeed-show-mode-hook (hide-mode-line-mode 1))
(add-hook! 'elfeed-search-update-hook #'hide-mode-line-mode)
(add-hook! 'after-init-hook #'elfeed-update)

(after! elfeed-search
  (set-evil-initial-state! 'elfeed-search-mode 'normal))
(after! elfeed-show-mode
  (set-evil-initial-state! 'elfeed-show-mode 'normal))

(after! evil-snipe
  (push 'elfeed-show-mode evil-snipe-disabled-modes)
  (push 'elfeed-search-mode evil-snipe-disabled-modes))

;; Avy

(defun avy-action-embark (pt)
  "https://gist.github.com/karthink/af013ffd77fe09e67360f040b57b4c7b#file-avy-actions-el-L146"
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

(after! avy
  (setopt avy-all-windows t)
  (setf (alist-get ?e avy-dispatch-alist) 'avy-action-embark))

;; Evil

(defun zar/save-then-kill-this-buffer ()
  "Save then kill the current buffer."
  (interactive)
  (basic-save-buffer)
  (kill-current-buffer))

(after! evil
  (evil-ex-define-cmd "wq" #'zar/save-then-kill-this-buffer)
  (evil-ex-define-cmd "q" #'kill-current-buffer)
  (setopt +evil-want-o/O-to-continue-comments nil
          evil-want-minibuffer t
          evil-vsplit-window-right t
          evil-split-window-below t
          evil-ex-substitute-global t
          evil-move-cursor-back nil
          evil-kill-on-visual-paste nil))

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))

(after! evil-collection
  (setopt evil-collection-calendar-want-org-bindings t))

;; Deft

(after! deft
  (setopt deft-directory org-notes-path)
  (set-evil-initial-state! 'deft-mode 'normal))

;; Latex

(defun zar/TeX-after-compilation-finished-functions (file-name)
  (call-process "notify-send" nil 0 nil "LatexMK" "Compilation finished!"
                "-u" "low" "-t" "3000"
                "-i" "/usr/share/icons/hicolor/48x48/apps/emacs28.png")
  (call-process "pkill" nil 0 nil "-HUP" "mupdf-gl"))

(defun zar/latex_update_pdf_buffer (buffer_name)
  "Inspired by https://old.reddit.com/r/emacs/comments/9hpa3g/how_to_get_live_preview_of_pdf_output_of_latex/e6ds838/"
  (let ((buffer (find-buffer-visiting buffer_name)))
    (if buffer
        (with-current-buffer
            (find-buffer-visiting buffer_name)
          (pdf-view-revert-buffer nil t))
      (format "[!] Buffer '%s' not found." buffer_name))))

(after! tex
  (setopt TeX-shell "zsh"
          TeX-engine 'luatex
          +latex-viewers '(pdf-tools mupdf))
  (add-hook 'TeX-after-compilation-finished-functions #'zar/TeX-after-compilation-finished-functions)
  (add-to-list 'TeX-command-list '("Pdfsizeopt" "pdfsizeopt %(O?pdf) opt_%(O?pdf)" TeX-run-command nil (plain-tex-mode latex-mode) :help "Optimize PDF size"))
  (add-to-list 'TeX-command-list '("Preview" "latexmk -pvc -pv- -silent -lualatex -e \"\\$pdf_update_method=4; \\$pdf_update_command=\\\"emacsclient -e '(zar/latex_update_pdf_buffer \\\\\\\"%(O?pdf)\\\\\\\")'\\\"\" %t" TeX-run-command nil (plain-tex-mode latex-mode) :help "Continously preview the current file")))

;; Projectile

(after! projectile
  (add-to-list 'projectile-globally-ignored-directories ".direnv"))

;; Dev

(after! lsp-mode
  (setopt lsp-log-io nil
          lsp-use-plists t
          lsp-restart 'ignore
          lsp-enable-file-watchers nil
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
  (setopt lsp-ui-sideline-enable t
          lsp-ui-sideline-show-diagnostics t
          lsp-ui-doc-show-with-mouse nil))

(after! yasnippet
  (setopt yas-triggers-in-field t))

(use-package! envrc
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
  (setopt apheleia-formatters-respect-fill-column t)
  (set-formatter! 'ruff-check '("ruff" "check" "-n" "--select" "I"
                                "--select" "F401" "--select" "F841"
                                "--fix" "--fix-only"
                                "--stdin-filename" filepath "-")
    :modes '(python-mode))
  (set-formatter! 'tex-fmt '("tex-fmt" "--nowrap" "--noconfig" "--quiet" "--stdin")
    :modes '(tex-mode))
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(ruff-check ruff))
  (setf (alist-get 'LaTeX-mode apheleia-mode-alist)
        '(tex-fmt)))

;; Python

(after! python
  (setopt python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True"))

(after! lsp-pyright
  (setopt lsp-pyright-python-executable-cmd "python3"))

(after! dap-mode
  (setopt dap-python-executable "python3"
          dap-python-debugger 'debugpy))

(after! doom-modeline
  (setopt doom-modeline-env-python-executable "python3"))

;; Vertico

(after! vertico
  (setopt vertico-buffer-display-action
          `(display-buffer-in-direction
            (direction . right)
            (window-height . 0.3))))

(vertico-multiform-mode)

;; External

(defun zar/window-move-dwim (direction)
  "Move to the window in the given DIRECTION, within Emacs if it exists,
  otherwise within i3wm. Raise an error if the DIRECTION is not equal to either:
  up, down, left, or right."
  (interactive)
  (if (not (member direction '("up" "down" "left" "right")))
      (error "The direction `%s' must be either equal to: `up', `down', `left', or `right'." direction))
  (condition-case nil
      (funcall (intern (format "evil-window-%s" direction)) 1)
    (error (call-process "i3-msg" nil 0 nil "focus" direction))))

(defun zar/setup-window-move (direction)
  "Define move functions for the given DIRECTION."
  (with-temp-buffer
    (insert (format "(defun zar/move-%s () (interactive) (zar/window-move-dwim \"%s\"))" direction direction))
    (eval-buffer)))

(mapc #'zar/setup-window-move '("up" "down" "left" "right"))

(require 'org-protocol)

(defun mw-start-eww-for-url (plist)
  "raise emacs and call eww with the url in plist."
  (+workspace/switch-to "eww")
  (eww (plist-get plist :url))
  (doom/window-maximize-buffer)
  nil)

(add-to-list 'org-protocol-protocol-alist
             '("eww"
               :protocol "eww"
               :function mw-start-eww-for-url))

(defun zar/update-ade ()
  (interactive)
  (call-process "~/Documents/notes/ade.sh" nil 0 nil))

(add-hook 'emacs-startup-hook #'zar/update-ade)

(defun zar/consult-pdf ()
  (interactive)
  (let ((frame (make-frame '((name . "PDF browser")
                             (minibuffer . only)))))
    (select-frame frame)
    (unwind-protect
        (call-process "xdg-open" nil 0 nil
                      (consult--find "Open: "
                                     (consult--fd-make-builder '("/home/paul/"))
                                     "\.pdf "))
      (delete-frame frame))))

(defun zar/consult-country-code ()
  (interactive)
  (let ((frame (make-frame '((name . "Country code")
                             (minibuffer . only)))))
    (select-frame frame)
    (unwind-protect
        (with-temp-buffer
          (insert-file-contents country-file)
          (read-only-mode)
          (let* ((candidates (split-string (buffer-string) "\n" t))
                 (selected (consult--read candidates
                                          :prompt "Select: "
                                          :require-match t)))
            (kill-new (car (split-string selected " ")))))
      (delete-frame frame))))

;; Key bindings

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
       :n "w" #'browse-url-at-point
       :n "u" #'zar/update-ade)
      (:prefix "t"
       :n "t" #'modus-themes-toggle))

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
      (:leader
       (:prefix "m"
                (:prefix "d"
                 :n "t" #'zar/org-time-stamp))
       (:prefix "t"
        :n "." #'org-toggle-timestamp-type))
      (:prefix "g"
       :n "<" #'org-timestamp-down
       :n ">" #'org-timestamp-up))

(map! :map latex-mode-map
      :leader
      (:prefix "o"
       :n "o" #'TeX-documentation-texdoc))

(map! :after shr
      :map shr-map
      "v" nil)

(map! :after calc
      :map calc-mode-map
      "C-o" #'casual-calc-tmenu)

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

(map! :after lsp
      :map lsp-mode-map
      :leader (:prefix "c"
               :n "D" #'lsp-find-references))

(map! :after numpydoc
      :map python-mode-map
      "C-c C-n" #'numpydoc-generate)

(map! :after jinx
      :map jinx-mode-map
      :prefix "z"
      :n "=" #'jinx-correct
      :n "]" #'jinx-next
      :n "[" #'jinx-previous)

;; Mouse

(setopt mouse-avoidance-banish-position '((frame-or-window . frame)
                                          (side . left)
                                          (side-pos . 0)
                                          (top-or-bottom . top)
                                          (top-or-bottom-pos . 0)))
(mouse-avoidance-mode 'banish)
