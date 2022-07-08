;;; ebn-init.el --- init -*- lexical-binding: t; no-byte-compile: nil; -*-
;;; Commentary:
;;; Code:

;;; Fonts:
;; This is much faster than using set-face-attribute
(add-to-list 'default-frame-alist '(font . "Iosevka-13"))

;;; Better defaults:
(setq ring-bell-function 'ignore
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq-default
 fill-column 80
 left-margin-width 1
 sentence-end-double-space nil
 lisp-backquote-indentation nil
 blink-cursor-blinks 1
 fast-but-imprecise-scrolling t
 auto-save-interval 60
 kill-do-not-save-duplicates t
 bidi-paragraph-direction 'left-to-right
 bidi-inhibit-bpa t)

(global-so-long-mode 1)
(save-place-mode 1)
;(mouse-avoidance-mode 'cat-and-mouse)

;;; Functions:
(defun ebn/--setup-variable-fonts ()
  (interactive)
  (set-face-attribute
   'variable-pitch nil
   :font (font-spec :family "CMU Concrete" :size 19 :weight 'regular :slant 'normal))

  (set-face-attribute
   'fixed-pitch nil
   :font (font-spec :family "Iosevka" :size 17)))

(defun ebn/advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(defun ebn/open-line-below ()
  "Open a newline below current line."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun ebn/open-line-above ()
  "Open a newline above current line."
  (interactive)
  (previous-line)
  (move-end-of-line nil)
  (newline-and-indent))

(defun ebn/kill-dwim ()
  "Run the command `kill-region' on the current region
or `kill-line' if there is no active region'"
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-line)))

(defun ebn/copy-dwim ()
  "Run the command `kill-ring-save' on the current region
or the current line if there is no active region."
  (interactive)
  (if (region-active-p)
      (kill-ring-save nil nil t)
    (kill-ring-save (point-at-bol) (point-at-eol))))

(defun ebn/wikipedia (query)
  "QUERY wikipedia"
  (interactive "sQuery: ")
  (eww (concat "https://en.wikipedia.org/wiki/"
	       (replace-in-string query " " "_"))))

(defun ebn/wikipedia-at-point ()
  "Query wikipedia for thing at point."
  (interactive)
  (ebn/wikipedia (word-at-point)))

(defun ebn/org-open-at-point ()
  (interactive)
  (let ((browse-url-browser-function #'eww-browse-url))
    (org-open-at-point)))

(defun ebn/forward-to-paragraph ()
  (interactive)
  (forward-paragraph)
  (forward-line 1))

(defun ebn/backward-to-paragraph ()
  (interactive)
  (backward-paragraph 2)
  (forward-line 1))

(defun ebn/kill-dir-or-char ()
  "Kill backward by word for directories else by char"
  (interactive)
  (if (looking-back "/")
      (backward-kill-word 1)
    (backward-delete-char 1)))

(defun ebn/shell-command-on-region (command)
  (interactive "sCommand: ")
  (shell-command-on-region (region-beginning) (region-end) command nil t))

(defun ebn/comment-paragraph ()
  (interactive)
  (save-excursion
    (mark-paragraph)
    (comment-or-uncomment-region (region-beginning) (region-end))))

(defun ebn/bury-scratch-buffer ()
  (if (string= (buffer-name) "*scratch*")
      (ignore (bury-buffer))
    t))
(add-hook 'kill-buffer-query-functions 'ebn/bury-scratch-buffer)

(defun ebn/dired-up-directory ()
  "Up directory - killing current buffer."
  (interactive)
  (let ((cb (current-buffer)))
    (progn (dired-up-directory)
	   (kill-buffer cb))))

(defun ebn/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (let ((value (eval (elisp--preceding-sexp))))
    (backward-kill-sexp)
    (insert (format "%S" value))))

(defun ebn/dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (call-process "xdg-open" nil 0 nil file)
    (message "Opening %s done" file)))

(defun ebn/eval-prev-exp ()
  (interactive)
  (unless (null command-history) (eval (car command-history))))

;;; Packages:
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  (setq read-extended-command-predicate
	#'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  (setq tab-bar-close-button nil)
  (setq tab-bar-new-button nil)

  (put 'narrow-to-region 'disabled nil)

  (defun ebn/setup-minibuffer ()
    (interactive)
    (electric-pair-local-mode -1))
  (add-hook 'minibuffer-setup-hook 'ebn/setup-minibuffer)

  (defun ebn/back-to-mark ()
    (interactive)
    (set-mark-command 0)
    (pulse-momentary-highlight-one-line))

  :custom
  (delete-by-moving-to-trash t)
  (gdb-many-windows t)
  (gdb-show-main t)
  (delete-selection-mode t)
  (auto-save-visited-mode t)
  ;(save-place-mode t)
  (initial-scratch-message nil)

  :hook (((prog-mode minibuffer-mode) . superword-mode)
	 ((fundamental-mode prog-mode) . repeat-mode)
	 ((emacs-lisp lisp-mode scheme-mode) . prettify-symbols-mode)
	 (prog-mode . (lambda ()
			(setq show-trailing-whitespace t)
			(electric-pair-local-mode))))

  :bind
  (:map minibuffer-mode-map
	("<DEL>" . ebn/kill-dir-or-char))
  (:map isearch-mode-map
	("TAB" . isearch-toggle-symbol)
	("M-q" . isearch-query-replace))
  (:map emacs-lisp-mode-map
	("C-c C-e" . ebn/eval-and-replace))
  (:map lisp-interaction-mode-map
	("C-c C-e" . ebn/eval-and-replace))
  (:map global-map
	("<f10>" . kmacro-start-macro)
	("<f11>" . kmacro-end-macro)
	("<f12>" . call-last-kbd-macro)
	("<f9>" . kmacro-insert-counter)
	("<f6>" . eval-defun)
	("<f7>" . eval-last-sexp)
	("C-," . xref-go-back)
	("C-." . repeat)
	("C-0" . ebn/back-to-mark)
	("C-8" . backward-sexp)
	("C-9" . forward-sexp)
	("C-6" . mark-sexp)
	("C-<down>" . ebn/forward-to-paragraph)
	("C-<up>" . backward-paragraph)
	("C-b" . backward-to-word)
	("C-c C-c" . ebn/comment-paragraph)
	("C-c d" . flymake-show-buffer-diagnostics)
	("C-f" . forward-to-word)
	("C-h ," . xref-find-definitions)
	("C-h l" . ebn/wikipedia-at-point)
	("C-h r" . xref-find-references)
	("C-h t" . eldoc-doc-buffer)
	("C-h w" . dictionary-search)
	("C-x C-r" . undo-redo)
	("C-j" . join-line)
	("C-k" . ebn/kill-dwim)
	("M-w" . ebn/copy-dwim)
	("C-o" . ebn/open-line-below)
	("C-t" . transpose-lines)
	("C-x C-b" . ibuffer)
	("C-x C-e" . eval-last-sexp)
	("C-x C-f" . find-file-other-window)
	("C-x SPC" . rectangle-mark-mode)
	("C-x f" . find-file)
	("C-x j" . jump-to-register)
	("C-x k" . kill-current-buffer)
	("C-x e" . eval-expression)
	("C-x =" . describe-char)
	("C-c n a" . org-agenda)
	("C-c C-b" . eval-buffer)
	("M-1" . delete-other-windows)
	("M-2" . split-window-below)
	("M-3" . split-window-right)
	("M-4" . delete-window)
	("M-5" . make-frame)
	("M-<f1>" . (lambda () (interactive) (tab-select 1)))
	("M-<f2>" . (lambda () (interactive) (tab-select 2)))
	("M-g M-g" . jump-to-register)
	("M-i" . back-to-indentation)
	("M-o" . ebn/open-line-above)
	("M-u" . upcase-dwim)
	("M-z" . zap-up-to-char)
	("M-§" . end-of-buffer)
	("s-e" . electric-pair-local-mode)
	("s-l" . ebn/org-open-at-point)
	("s-r" . replace-string)
 	("C-<tab>" . hippie-expand)))

(use-package debug
  :ensure nil
  :defer 10
  :bind
    (:map debugger-mode-map
	("<tab>" . debugger-toggle-locals)))

(use-package mindre-theme
  :ensure nil
  :load-path "themes/"
  :custom
  (mindre-use-more-bold nil)
  (mindre-use-faded-lisp-parens t)
  :config
  (load-theme 'mindre t))

(use-package hippie-exp
  :ensure nil
  :bind ([remap dabbrev-expand] . hippie-expand)
  :commands (hippie-expand)
  :config
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line)))

(use-package vterm
  :defer t
  :bind ("C-c C-t" . vterm-other-window))

(use-package save-hist
  :ensure nil
  :defer 10
  :init
  (savehist-mode 1)
  :config
  (setq history-length 10))

(use-package gtags :ensure nil)

(use-package erc
  :commands 'erc-tls
  :config
  (set-face-attribute 'erc-prompt-face nil :background nil :foreground "foreground")
  (setq erc-prompt (lambda () (concat "[" (buffer-name) "]"))))

(use-package popper
  :ensure t
  :config
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
	  "\\*eldoc\\*"
	  "\\*Ibuffer\\*"
	  "\\*vc-git"
	  "\\*Help\\*"
	  "\\*RE-Builder\\*$"
	  flymake-diagnostics-buffer-mode
	  calendar-mode
	  help-mode
	  compilation-mode
	  sage-shell-mode
	  vterm-mode
	  inferior-emacs-lisp-mode))
  (popper-mode)
  (popper-echo-mode)
  :bind* ("C-å" . popper-toggle-type)
         ("C-+" . popper-toggle-latest))

(use-package ibuffer-project
  :config
  (setq ibuffer-truncate-lines nil)
  (defun ebn/ibuffer-setup ()
    (setq ibuffer-filter-groups
	  (ibuffer-project-generate-filter-groups)))
  (setq ibuffer-project-use-cache t)
  :hook ((ibuffer . ebn/ibuffer-setup)))

(use-package eldoc
  :ensure nil
  :custom
  (eldoc-echo-area-prefer-doc-buffer nil))

(use-package dired
  :ensure nil
  :config
  (require 'dired-x)
  (setq dired-recursive-copies t
	dired-recursive-deletes t
	dired-dwim-target t
	dired-omit-files "^\\..*$"
	delete-by-moving-to-trash t)
  (add-hook 'dired-mode-hook #'dired-omit-mode)
  :bind*
  (:map dired-mode-map
	("-" . ebn/dired-up-directory)
	("o" . ebn/dired-open-file)
	(")" . dired-omit-mode)
	("q" . (lambda () (interactive) (quit-window t)))
	("e" . wdired-change-to-wdired-mode)
	("f" . dired-create-empty-file)))

;;; Org:
(use-package org
  :defer t
  :commands (org-agenda
	     org-capture
	     org-cdlatex-mode)
  :init (progn
	  (defun ebn/diary-last-day-of-month (date)
	    "Return `t` if DATE is the last day of the month."
	    (let* ((day (calendar-extract-day date))
		   (month (calendar-extract-month date))
		   (year (calendar-extract-year date))
		   (last-day-of-month
		    (calendar-last-day-of-month month year)))
	      (= day last-day-of-month))))

  :config (progn
	    (require 'org-mouse)
	    (defun ebn/org-eval-block ()
	      "Wrapper around org-ctrl-c-ctrl-c that previews latex."
	      (interactive)
	      (org-ctrl-c-ctrl-c)
	      (when (string= "sage" (plist-get (cadr (org-element-at-point)) :language))
		(org-latex-preview)))

	    ;; Options
	    (setq org-startup-indented t
		  org-startup-with-latex-preview t
		  org-pretty-entities t
		  org-startup-with-inline-images t
		  org-ellipsis " …"
  		  org-export-preserve-breaks t
		  org-highlight-latex-and-related '(native)
		  org-src-fontify-natively t
		  org-fontify-quote-and-verse-blocks t
		  org-startup-folded t
		  org-hide-leading-stars t
		  org-use-speed-commands t
		  org-cycle-separator-lines 2
		  org-catch-invisible-edits 'error
		  org-ctrl-k-protect-subtree t
		  org-image-actual-width nil
		  org-return-follows-link t
		  org-hide-emphasis-markers t
		  org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
		  org-latex-listings 'minted
		  org-latex-packages-alist '(("" "minted"))
		  org-latex-tables-centered t
		  org-insert-heading-respect-content t
		  org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
				      (sequence "BACKLOG(b)" "ACTIVE(a)"
						"REVIEW(v)" "WAIT(w@/!)" "HOLD(h)"
						"|" "DELEGATED(D)" "CANCELLED(c)"))
		  org-agenda-current-time-string "← now ─────────────────"
		  org-latex-pdf-process
		  ;; The reason why this is a list is that it usually takes several
		  ;; runs of ‘pdflatex’, maybe mixed with a call to ‘bibtex’.  Org
		  ;; does not have a clever mechanism to detect which of these
		  ;; commands have to be run to get to a stable result, and it also
		  ;; does not do any error checking.
		  '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
		    "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
		    "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

	    ;; Org-babel languages
	    (org-babel-do-load-languages 'org-babel-load-languages
					 '((latex . t)
					   (emacs-lisp . t)
					   (haskell . t)
					   (lisp . t)
					   (calc . t)
					   (maxima . t)
					   (python . t)
					   (sagemath . t)))

	    ;; Org-agenda
	    (setq org-agenda-files '("gtd.org" "someday.org" "tickler.org")
		  org-agenda-include-diary t
		  org-capture-templates
		  '(("i" "Inbox" entry (file "~/org/inbox.org"))
		    ("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
		     "* TODO %?\n  %i\n  %a")
		    ("s" "Someday" entry (file "~/org/someday.org")
		     "* TODO %?\n  %i\n  %a")
		    ("r" "Roam node" function #'org-roam-capture)
		    ("j" "Journal: Today" function #'org-roam-dailies-capture-today)
		    ("J" "Journal: Tomorrow" function #'org-roam-dailies-capture-tomorrow)
		    ("d" "Journal: Date" function #'org-roam-dailies-capture-date))
		  org-refile-targets
		  '((nil :maxlevel . 3)
		    (org-agenda-files :maxlevel . 3)))

	    (defun ebn/rectangle-number-lines ()
	      (interactive)
	      (rectangle-number-lines (region-beginning) (region-end) 1 "%s. ")))

  :bind*
  (:map org-mode-map
	("C-<return>" . org-meta-return)
	("C-c h" . consult-org-heading)
	("C-j" . join-line)
	("C-x r N" . ebn/rectangle-number-lines)
	("C-x C-e" . ebn/org-eval-block))
  (:map global-map
	("C-c n n" . org-capture))

  :hook ((org-mode . (lambda ()
		       (setq line-spacing .2)
		       (setq cursor-type 'box)
		       (org-cdlatex-mode)
		       (ebn/--setup-variable-fonts)))))

(use-package org-roam
  :defer t
  :commands (org-roam-node-find org-roam-capture)

  :init
  (setq org-roam-v2-ack t) ;; Disable v2-migration-prompt

  :custom
  (org-roam-directory "~/org/org-roam")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   `(("d" "default" plain "%?"
      :if-new (file+head
	       "%<%Y%m%d%H%M%S>-${slug}.org"
	       ,(let ((options '("#+options: _:{}"
				 "#+options: ^:{}"
				 "#+startup: latexpreview"
				 "#+startup: entitiespretty"
				 "#+startup: inlineimages"
				 "#+title: ${title}")))
		  (mapconcat 'identity options "\n")))
      :unnarrowed t)))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n g" . org-roam-graph)
	 ("C-c n c" . org-roam-capture))
  :config
  (org-roam-db-autosync-mode))

(use-package org-download
  :commands (org-mode org-download-clipboard)
  :custom
  (org-download-screenshot-method "flameshot gui -s --raw > %s")
  :bind ("<f8>" . org-download-screenshot))

(use-package ob-sagemath
  :ensure t
  :defer t
  :config
  (progn
    (setq org-babel-default-header-args:sage
	  '((:session . t)
	    (:results . "drawer replace")))

    (with-eval-after-load "org"
      (define-key org-mode-map (kbd "C-c c") 'ob-sagemath-execute-async))
    (setq org-confirm-babel-evaluate nil
	  org-export-babel-evaluate nil
	  org-startup-with-inline-images t)
    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)))

(use-package sage-shell-mode
  :ensure t
  :defer t
  :config
  (setq sage-shell:set-ipython-version-on-startup nil)
  (setq sage-shell-sagetex:auctex-command-name "LaTeX")
  (add-hook 'sage-shell:sage-mode-hook (lambda ()
					 (setq-local prettify-symbols-alist
						     '(("lambda" . 955)
						       ("beta" . 120573)
						       ("alpha" . 120572)))
					 (sage-shell-view-mode))))

;;; Languages:
(use-package sh-mode
  :ensure nil
  :commands sh-mode
  :bind (:map sh-mode-map ("C-x C-e" . sh-execute-region)))

(use-package haskell-mode
  :defer t
  :commands (haskell-mode)
  :init
  (defun ebn/haskell-mode-setup ()
    (interactive)
    (setq-local eldoc-documentation-function #'haskell-doc-current-info
		tab-stop-list '(2)
		indent-line-function #'indent-relative
		tab-width 2)
    (interactive-haskell-mode)
    (haskell-indentation-mode)
    (electric-pair-mode))
  (add-hook 'haskell-mode-hook #'ebn/haskell-mode-setup)

  :custom
  (haskell-process-type 'cabal-repl)
  (haskell-process-load-or-reload-prompt nil)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-log t)
  (haskell-interactive-popup-errors nil)
  (haskell-font-lock-symbols t)

  :config
  (defun haskell-mode-after-save-handler ()
    (let ((inhibit-message t))
      (eglot-format-buffer)))

  :bind (:map haskell-mode-map
	      ("C-h L" . haskell-hoogle-lookup-from-website)
	      ("M-<left>" . backward-sexp)
	      ("M-<right>" . forward-sexp)))

(use-package agda2
  :ensure nil
  :init
  (add-to-list 'load-path (shell-command-to-string "agda-mode locate"))
  :mode ("\\agda\\'" . agda2-mode))

(use-package nix-mode
  :defer t)

(use-package geiser-guile
  :ensure t
  :defer t)

(use-package geiser-racket
  :ensure t
  :disabled t
  :defer t)

(use-package racket-mode
  :config
  (defun setup-racket-eldoc ()
    (eldoc-mode +1)
    (setq eldoc-documentation-function #'racket-xp-eldoc-function))

  (add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
  (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)
  (add-hook 'racket-mode-hook #'setup-racket-eldoc))

(use-package yapfify
  :ensure t
  :defer t)

(use-package cc-mode
  :ensure nil
  :config
  (setq c-default-style "cc-mode")
  :bind
  (:map c-mode-map
	("C-c o" . ff-find-other-file)
	("C-c c" . project-compile))
  :hook (c-mode . electric-pair-mode))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :init (setq markdown-command "multimarkdown")
  :custom
  (markdown-enable-highlighting-syntax t)
  :config
  (set-face-attribute 'markdown-code-face nil :background nil)
  (setq markdown-enable-highlighting-syntax t))

(use-package cdlatex
  :commands 'turn-on-cdlatex)

(use-package tex-mode
  :ensure nil
  :mode (("\\.tex\\'" . LaTeX-mode))
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-master nil)
  (TeX-PDF-mode t)
  :hook
  (LaTeX-mode . (lambda ()
		  (interactive)
		  (visual-line-mode)
		  (flyspell-mode)
		  (LaTeX-math-mode)
		  (turn-on-cdlatex)
		  (yas-minor-mode-on)
		  (eglot-ensure)
		  (ebn/--setup-variable-fonts))))

;;; LSP:
(use-package eglot
  :defer t
  :hook
  (haskell-mode . eglot-ensure)
  (c-mode . eglot-ensure)
  (python-mode . eglot-ensure)
  (js-jsx-mode . eglot-ensure)
  (js-mode . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-autoreconnect nil)
  (eglot-confirm-server-initiated-edits nil)
  (eldoc-idle-delay 1)
  (eldoc-echo-area-display-truncation-message nil)
  (eldoc-echo-area-use-multiline-p 2)

  :config
  (add-to-list 'eglot-server-programs
	       '((tex-mode context-mode texinfo-mode bibtex-mode) . ("texlab")))

  (define-key eglot-mode-map [remap display-local-help] nil)

  :bind (:map eglot-mode-map
	      ("C-c C-a" . eglot-code-actions)
	      ("C-c C-f" . eglot-format-buffer)))

;;; Completion:
(use-package corfu
  :custom
  (corfu-auto-delay 0.2)
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-commit-predicate nil)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
  (corfu-echo-documentation nil)
  :init
  (global-corfu-mode))

(use-package cape
  :after corfu
  :bind (("C-c p i" . cape-ispell)
	 ("C-c p w" . cape-dict)
	 ("C-c p d" . cape-dabbrev)
	 ("C-c p l" . cape-line)
	 ("C-c p \\" . cape-tex))
  :config
  (setq cape-dict-file "~/.local/share/dictionaries/my.dict")
  (setq-local completion-at-point-functions
              (list (cape-super-capf #'cape-dabbrev #'cape-dict #'cape-keyword #'cape-symbol)))

  ;; Silence then pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)

  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package vertico
  :init
  (use-package orderless
    :commands (orderless)
    :custom (completion-styles '(orderless)))

  (use-package consult
    :init
    (setq consult-preview-key nil)
    :bind
    ("C-c r" . consult-recent-file)
    ("C-c f" . consult-ripgrep)
    ("C-c l" . consult-line)
    ("C-c i" . consult-imenu)
    ("C-c t" . gtags-find-tag)
    ("C-x b" . consult-buffer)
    ("C-c x" . consult-complex-command)
    (:map comint-mode-map
	  ("C-c C-l" . consult-history)))
  :config
  (recentf-mode t)
  (vertico-mode t))

(use-package yasnippet
  :defer 10
  :commands yas-minor-mode-on
  :hook ((org-mode prog-mode) . #'yas-minor-mode-on)
  :config
  (yas-reload-all))

;; Load environments (nix-shell)
(use-package envrc
  :diminish
  :config
  (add-hook 'envrc-reload-hook #'eglot-reconnect)
  :hook
  (prog-mode . envrc-mode))

;;; Better editing
(use-package multiple-cursors
  :ensure t
  :bind
  (:map global-map
	("s-<down>" . mc/mark-next-like-this)
	("s-," . mc/mark-all-in-region-regexp)
	("s-." . mc/edit-lines)))

(use-package expand-region
  :ensure t
  :commands 'er/expand-region
  :bind
  ("C-<return>" . er/expand-region))

(use-package paredit
  :ensure t
  :config
  (defun ebn/paredit-semicolon (f &rest args)
    (if (region-active-p)
	(comment-region (region-beginning) (region-end))
      (apply f args)))
  (advice-add 'paredit-semicolon :around #'ebn/paredit-semicolon)
  :hook ((scheme-mode emacs-lisp-mode racket-mode racket-repl-mode) . enable-paredit-mode)
  :bind (:map paredit-mode-map
	      ("M-<left>" . paredit-backward-barf-sexp)
	      ("M-<right>" . paredit-forward-barf-sexp)
	      ("M-7" . paredit-wrap-curly)
	      ("M-8" . paredit-wrap-round)
	      ("C-c w" . (lambda () (interactive) (paredit-wrap-round 4)))))

(use-package avy
  :ensure t
  :defer t
  :commands 'avy-goto-char-timer
  :config
  (setq avy-timeout-seconds 0.4)
  (setq avy-all-windows nil)
  :bind
  ("M-g g" . avy-goto-line)
  ("M-g c" . avy-goto-char-in-line)
  ("M-g m" . avy-move-line)
  ("C-ö" . avy-goto-char-timer))

;;; Misc
(use-package keycast
  :ensure t
  :defer 10
  :commands 'keycast-mode)

(use-package pdf-tools
  :ensure t
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
	TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
	TeX-source-correlate-start-server t)
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

(use-package org-modern
  :commands (org-modern-mode org-modern-agenda)
  :custom
  (org-modern-table-vertical 1)
  (org-modern-table-horizontal 1)
  (org-modern-block t)
  :init
  (setq org-modern-todo t
        org-modern-variable-pitch nil)
  (global-org-modern-mode))

(use-package rainbow-mode
  :commands rainbow-mode)

(use-package package-lint
  :disabled
  :commands package-lint-current-buffer)

(use-package clipmon
  :defer 10
  :commands clipmon-mode-start)

(use-package sv-kalender
  :load-path "lisp/sv-kalender/")

(use-package focus
  :disabled
  :commands focus-mode
  :config
  (add-to-list 'focus-mode-to-thing '(emacs-lisp-mode . list))
  (add-to-list 'focus-mode-to-thing '(scheme-mode . list)))

(use-package imaxima
  :disabled
  :ensure nil
  :commands (maxima imaxima)
  :init
  (setq imaxima-scale-factor 1.5
	maxima-command "maxima"))

(use-package olivetti
  :commands olivetti-mode)

;;; ebn-init.el ends here
