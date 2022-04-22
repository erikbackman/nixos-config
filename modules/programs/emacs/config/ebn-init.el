;;; ebn-init.el --- init -*- lexical-binding: t; no-byte-compile: nil; -*-
;;; Commentary:
;;; Code:
(require 'use-package)
(require 'ebn-core)
(require 'dired)

;; Basic
(set-face-attribute
 'default nil
 :font (font-spec :family "Sarasa Mono CL" :size 14.5 :weight 'medium))

(set-face-attribute
 'fixed-pitch nil
 :font (font-spec :family "Sarasa Mono CL" :size 14.5 :weight 'medium))

(setq ring-bell-function 'ignore
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq-default fill-column 80)
(setq global-mark-ring-max 16)
(setq-default left-margin-width 1)
(setq sentence-end-double-space nil)
(setq kill-whole-line t)

(add-to-list 'load-path (shell-command-to-string "agda-mode locate"))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/lisp")

(add-hook 'lisp-mode-hook
	  (lambda ()
	    (set (make-local-variable 'lisp-indent-function)
		   'common-lisp-indent-function)))

(setq lisp-backquote-indentation nil)

;; Functions
;; TODO: Move to ebn-core
(defun ebn/kill-dwim ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end) nil)
    (kill-line)))

(defun ebn/cht.sh (query)
  "QUERY cht.sh"
  (interactive "sQuery: ")
  (eww (concat "https://cht.sh/" query)))

(defun ebn/forward-to-paragraph ()
  (interactive)
  (forward-paragraph)
  (forward-line 1))

(defun ebn/kill-dir-or-char ()
    "Kill backward by word for directories else by char"
    (interactive)
    (if (looking-back "/")
	(backward-kill-word 1)
      (backward-delete-char 1)))

(defun ebn/shell-command-on-region ()
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) nil t))

(defun ebn/comment-paragraph ()
  (interactive)
  (save-excursion
    (mark-paragraph)
    (comment-or-uncomment-region (region-beginning) (region-end))))

;; Packages
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

  :custom
  (delete-by-moving-to-trash t)
  (gdb-many-windows t)
  (gdb-show-main t)
  (delete-selection-mode t)
  (auto-save-visited-mode t)
  (save-place-mode t)
  (initial-scratch-message nil)

  :hook ((prog-mode . superword-mode)
	 (fundamental-mode . repeat-mode)
	 (prog-mode . repeat-mode)
	 (prog-mode . electric-pair-local-mode)
	 (minibuffer-mode . superword-mode))

  :bind
  (:map minibuffer-mode-map
	("<DEL>" . ebn/kill-dir-or-char))
  (:map isearch-mode-map
	("TAB" . isearch-toggle-symbol))
  (:map global-map 
	("C-j" . join-line)
	("C-t" . transpose-lines)
	("M-u" . upcase-dwim)
	("C-x k" . ebn/kill-current-buffer)
	("C-o" . ebn/open-line-below)
	("M-o" . ebn/open-line-above)
	("M-§" . end-of-buffer)
	("<f7>" . call-last-kbd-macro)
	("M-z" . zap-up-to-char)
	("C-k" . ebn/kill-dwim)
	("C-x f" . find-file)
	("C-x C-f" . find-file-other-window)
	("M-1" . delete-other-windows)
	("M-2" . split-window-below)
	("M-3" . split-window-right)
	("M-4" . delete-window)
	("M-g M-g" . jump-to-register)
	("C-0" . pop-global-mark)
	("C-9" . forward-list)
	("C-8" . backward-list)
	("C-<down>" . ebn/forward-to-paragraph)
	("C-h ," . xref-find-definitions)
	("C-h l" . display-local-help)
	("C-h r" . xref-find-references)
	("C-." . repeat)
	("M-i" . back-to-indentation)
	("<f9>" . kmacro-insert-counter)
	("<f10>" . kmacro-start-macro)
	("<f11>" . kmacro-end-macro)
	("<f12>" . call-last-kbd-macro)
	("s-e" . electric-pair-local-mode)
	("s-r" . replace-string)
	("C-c c" . ebn/comment-paragraph)
	("C-x C-b" . ibuffer)))

(use-package mindre-theme
  :ensure nil
  :load-path "themes/"
  :config
  (setq mindre-use-bold nil)
  (mindre))

(use-package ebn-core
  :ensure nil
  :defer t
  :commands 'ebn/copy-dwim
  :config
  (global-set-key (kbd "M-w") 'ebn/copy-dwim))

(use-package diminish
  :ensure t)

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

(use-package which-key
  :diminish
  :config
  (which-key-mode 1))

(use-package vertico
  :config
  (vertico-mode))

(use-package consult
  :config
  (setq consult-preview-key nil)
  (recentf-mode)
  :bind
  ("C-c r" . consult-recent-file)
  ("C-c f" . consult-ripgrep)
  ("C-c l" . consult-line)
  ("C-c i" . consult-imenu)
  ("C-c t" . gtags-find-tag)
  ("C-x b" . consult-buffer))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	orderless-skip-highlighting nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package eldoc
  :ensure nil
  :diminish)

(use-package dired
  :ensure nil
  :config
  (setq dired-recursive-copies t
	dired-recursive-deletes t
	dired-dwim-target t
	delete-by-moving-to-trash t)
  :bind*			    
  (:map
   dired-mode-map
   ("-" . ebn/dired-up-directory)))

(use-package cdlatex
  :defer t)

(use-package auctex
  :mode
  ("\\.tex\\'" . latex-mode)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq TeX-PDF-mode t)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config (set-face-attribute 'markdown-code-face nil :background nil))

(use-package org
  :defer t
  :commands (my/org-prettify-buffer
	     org-agenda
	     org-capture
	     org-cdlatex-mode)
  :init
  (defun my/org-prettify-buffer ()
    (interactive)
    (when (not org-pretty-entities)
      (org-toggle-pretty-entities))
    (org-latex-preview))
  (defun ebn/diary-last-day-of-month (date)
    "Return `t` if DATE is the last day of the month."
    (let* ((day (calendar-extract-day date))
	   (month (calendar-extract-month date))
	   (year (calendar-extract-year date))
	   (last-day-of-month
	    (calendar-last-day-of-month month year)))
      (= day last-day-of-month)))
  :config
  ;; Faces
  (set-face-attribute
   'variable-pitch nil
   :font (font-spec :family "CMU Concrete" :size 21 :weight 'regular))

  (setq org-export-preserve-breaks t)
  (setq org-ellipsis " …")
  
  ;; Options
  (setq org-startup-indented t
	org-startup-with-latex-preview t
	org-pretty-entities t
	org-fontify-quote-and-verse-blocks t
	org-startup-folded t
	org-hide-leading-stars t
	org-cycle-separator-lines -1
	org-catch-invisible-edits 'error
	org-ctrl-k-protect-subtree t)
  
  ;; Org-babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((latex . t)
     (emacs-lisp . t)
     (python . t)))

  ;; Org-agenda
  (setq org-agenda-files '("gtd.org" "someday.org" "tickler.org")
	org-capture-templates
	'(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
	   "* TODO %?\n  %i\n  %a")
	  ("j" "Journal" entry (file+datetree "~/org/journal.org")
	   "* %?\nEntered on %U\n  %i\n  %a")
	  ("r" "Roam node" function #'org-roam-capture))
	org-image-actual-width nil
	org-return-follows-link t
	org-hide-emphasis-markers t
	org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
	org-latex-listings 'minted
	org-latex-packages-alist '(("" "minted"))
	org-latex-pdf-process
	'("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
	org-latex-tables-centered t
	org-insert-heading-respect-content t)
  
  :bind*
  (:map org-mode-map
	("C-<return>" . org-meta-return)
	("C-c h" . consult-org-heading))
  :hook ((org-mode . (lambda ()
		       (setq line-spacing .2)
		       (setq cursor-type 'box)
		       (org-cdlatex-mode)
		       (variable-pitch-mode)))))

(use-package org-roam
  :defer t
  :commands (org-roam-node-find org-roam-capture)

  :init
  (setq org-roam-v2-ack t) ;; Disable v2-migration-prompt

  :custom
  (org-roam-directory "~/org-roam")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head
	       "%<%Y%m%d%H%M%S>-${slug}.org"
	       "#+title: ${title}\n#+startup: latexpreview\n#+startup: entitiespretty")
      :unnarrowed t))
   )

  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n g" . org-roam-graph)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n c" . org-roam-capture)
	 ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol))

(use-package sage-shell-mode
  :ensure t
  :defer t
  :config
  (setq sage-shell:set-ipython-version-on-startup nil)
  (setq sage-shell-sagetex:auctex-command-name "LaTeX")
  (add-hook 'sage-shell:sage-mode-hook
	    (setq-local prettify-symbols-alist
			'(("lambda" . 955)
			  ("beta" . 120573)
			  ("alpha" . 120572)))))

(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8))))

(use-package yapfify
  :ensure t
  :defer t)

(use-package ob-sagemath
  :ensure t
  :defer t
  :config
  (progn 
    (setq org-babel-default-header-args:sage '((:session . t)
					       (:results . "output replace")))
    (with-eval-after-load "org"
      (define-key org-mode-map (kbd "C-c c") 'ob-sagemath-execute-async))
    (setq org-confirm-babel-evaluate nil
	  org-export-babel-evaluate nil
	  org-startup-with-inline-images t)
    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)))

(use-package haskell-mode
  :defer t
  :commands (haskell-mode)
  :diminish interactive-haskell-mode
 
  :mode
  (("\\.hs\\'" . haskell-mode)
   ("\\.cabal\\'" . haskell-cabal-mode))
  
  :custom
  (haskell-process-type 'cabal-repl)
  (haskell-process-load-or-reload-prompt t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-log t)
  (haskell-font-lock-symbols t)
  
  :config
  (load-library "haskell-mode-autoloads")
  (require 'haskell-interactive-mode)

  (defun haskell-mode-after-save-handler ()
    (let ((inhibit-message t))
      (eglot-format-buffer)))
  (defun ebn/haskell-mode-setup ()
    (haskell-indentation-mode)
    (autoload 'haskell-doc-current-info "haskell-doc")
    (setq-local eldoc-documentation-function
		'haskell-doc-current-info)
    (setq-local tab-stop-list '(2))
    (setq-local haskell-process-path-cabal "cabal")
    (setq indent-line-function 'indent-relative)
    (setq tab-width 2))

  :hook
  ((haskell-mode . ebn/haskell-mode-setup)
   (haskell-mode . interactive-haskell-mode)
   (haskell-mode . electric-pair-mode))
  :bind
  (:map haskell-mode-map
	("M-<left>" . backward-sexp)
	("M-<right>" . forward-sexp)))

(use-package eglot
  :defer t
  :hook ((haskell-mode . eglot-ensure)
	 (c-mode . eglot-ensure)
	 (python-mode . eglot-ensure))
  :custom
  (eglot-autoshutdown t)
  (eglot-autoreconnect nil)
  (eglot-confirm-server-initiated-edits nil)
  (eldoc-idle-delay 1)
  (eldoc-echo-area-display-truncation-message nil)
  (eldoc-echo-area-use-multiline-p 3)
  :config
  (add-to-list 'eglot-server-programs '(LaTeX-mode . ("texlab" "")))
  (define-key eglot-mode-map [remap display-local-help] nil)
  :bind (:map eglot-mode-map
	      ("C-c C-a" . eglot-code-actions)
	      ("C-c C-f" . eglot-format-buffer)))

(use-package envrc
  :diminish
  :config
  (envrc-global-mode))

(use-package corfu
  :defer t
  :custom
  (corfu-auto-delay 0.2)
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-commit-predicate nil)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
  (corfu-echo-documentation nil)
  :hook ((haskell-mode . corfu-mode)
	 (emacs-lisp-mode . corfu-mode)
	 (eshell-mode . corfu-mode)
	 (c-mode . corfu-mode)))

(use-package nix-mode
  :defer t
  :mode ("\\.nix\\'" . nix-mode))

(use-package vterm
  :defer t
  :bind ("C-c C-t" . vterm-other-window))

(use-package cc-mode
  :ensure nil
  :config
  (setq c-default-style "cc-mode")
  :bind
  (:map c-mode-map
	("C-c o" . ff-find-other-file)
	("C-c c" . project-compile))
  :hook (c-mode . electric-pair-mode))

(use-package gtags
  :ensure nil)

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
  ("M-s" . avy-goto-char-in-line)
  ("C-ö" . avy-goto-char-timer))

(use-package multiple-cursors
  :ensure t
  :config
  ; TODO: come up with better bindings for these, use super?
  (global-set-key (kbd "M-m") 'mc/mark-all-like-this-dwim)
  (global-set-key [(super down)] 'mc/mark-next-like-this)
  :bind
  (:map global-map
	("s-<down>" . mc/mark-next-like-this)
	("s-," . mc/mark-all-in-region-regexp)))

(use-package expand-region
  :ensure t
  :defer t
  :commands 'er/expand-region
  :config (unbind-key "C-<return>" python-mode-map)
  :bind
  ("C-," . er/expand-region)
  ("C-<return>" . er/expand-region))

(use-package paredit
  :ensure t
  :defer t
  :diminish
  :mode ("\\.el\\'" . emacs-lisp-mode)
  :hook (emacs-lisp-mode . enable-paredit-mode))

(use-package erc
  :config
  (set-face-attribute 'erc-prompt-face nil :background nil :foreground "foreground")
  (setq erc-prompt (lambda () (concat "[" (buffer-name) "]"))))

(use-package popper
  :ensure t
  :bind (("C-´"   . popper-toggle-latest)
         ("M-´"   . popper-cycle)
         ("C-M-´" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
	  "\\*eldoc\\*"
	  "\\*ibuffer\\*"
	  "\\*vc-git"
          help-mode
          compilation-mode))
  :config
  (popper-mode)
  (popper-echo-mode))

(use-package ibuffer-project
  :config
  (setq ibuffer-truncate-lines nil)
  (defun ebn/ibuffer-setup ()
    (setq ibuffer-filter-groups
	  (ibuffer-project-generate-filter-groups)))
  (setq ibuffer-project-use-cache t)
  :hook ((ibuffer . ebn/ibuffer-setup)))

;; ;;; ebn-init.el ends here
