;;; ebn-init.el --- init -*- lexical-binding: t; no-byte-compile: nil; -*-
;;; Commentary:
;;; Code:
(require 'use-package)
(require 'ebn-core)

;; Basic
;; Setting this lower after early-init for shorter gc-pauses
(setq-default fill-column 80)
(setq gc-cons-threshold (* 2 1000 1000)
      ring-bell-function 'ignore
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
;(add-hook 'after-init-hook #'ebn/display-startup-time)
(add-to-list 'load-path (shell-command-to-string "agda-mode locate"))

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

  :custom
  (delete-by-moving-to-trash t)
  (gdb-many-windows t)
  (gdb-show-main t)
  (delete-selection-mode t)
  (auto-save-visited-mode t)
  (save-place-mode t)
  
  :delight
  (auto-fill-function " AF")
  (visual-line-mode)

  :hook (prog-mode . superword-mode)
  
  :bind
  ("C-j" . join-line)
  ("M-u" . upcase-dwim)
  ("C-x k" . ebn/kill-current-buffer)
  ("C-o" . ebn/open-line-below)
  ("M-o" . ebn/open-line-above)
  ("M-§" . end-of-buffer)
  ("<f7>" . call-last-kbd-macro)
  ("M-z" . zap-up-to-char)
  (:map
   dired-mode-map
   ("-" . ebn/dired-up-directory)))

(use-package ebn-core
  :ensure nil
  :config
  (ebn/font :name "Iosevka Custom" :size 20 :weight 'normal)
  (ebn/font-variable-pitch :name "CMU Concrete" :size 24)
  (global-set-key (kbd "M-w") 'ebn/copy-dwim))
 
(use-package which-key
  :ensure nil
  :diminish)

(use-package modus-themes
 :ensure t
 :init
 (show-paren-mode 1)
 :config
 (setq modus-themes-vivendi-color-overrides
       '((bg-main . "#0C0F12")
	 (bg-focused . "#0C0F12")))
 (setq modus-themes-org-agenda
      '((header-block . (variable-pitch scale-title))
        (header-date . (grayscale bold-today))
        (scheduled . uniform)
        (habit . simplified)))
 (setq modus-themes-org-blocks '(nil))
 (setq modus-themes-headings
       '((1 . (background overline))
         (2 . (background overline))
         (3 . (background rainbow overline))
         (t . (background rainbow no-bold overline))))
 (setq modus-themes-variable-pitch-ui nil
       modus-themes-variable-pitch-headings nil
       modus-themes-scale-headings t
       modus-themes-scale-1 1.1
       modus-themes-scale-2 1.15
       modus-themes-scale-3 1.21
       modus-themes-scale-4 1.27
       modus-themes-scale-title 1.33
       modus-themes-scale-small 0.9
       modus-themes-mixed-fonts t
       modus-themes-mode-line '(3d)
       modus-themes-paren-match '(bold intense)
       modus-themes-syntax '(nil)
       modus-themes-links '(faint))
 (when window-system (set-frame-size (selected-frame) 90 50))
 (modus-themes-load-vivendi))

(use-package diminish
  :ensure t)

(use-package yasnippet
  :diminish
  :config
  (yas-global-mode 1))

(use-package which-key
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
  ("C-c t" . gtags-find-tag))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package eldoc
  :ensure nil
  :diminish)

(use-package envrc
  :commands 'envrc-global-mode
  :config (envrc-global-mode))

(use-package dired
  :ensure nil
  :config
  (setq dired-recursive-copies t
	dired-recursive-deletes t
	dired-dwim-target t))

(use-package cdlatex
  :defer t)

(use-package auctex
  :defer t)

(use-package org
  :defer t
  :commands (my/org-prettify-buffer
	     org-agenda
	     org-capture
	     org-cdlatex-mode)
  ;:hook (org-mode . org-cdlatex-mode)
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
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "n"  '(:ignore t :which-key "Notes")
   "na" '(org-agenda :which-key "Agenda")
   "no"  '(org-capture :which-key "Capture"))

  (setq org-agenda-files '("gtd.org" "someday.org" "tickler.org"))
  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
	   "* TODO %?\n  %i\n  %a")
	  ("j" "Journal" entry (file+datetree "~/org/journal.org")
	   "* %?\nEntered on %U\n  %i\n  %a")
	  ("r" "Roam node" function #'org-roam-capture)))
  (setq org-image-actual-width nil)
  (setq org-return-follows-link t)
  (setq org-hide-emphasis-markers t)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.4))

  (setq org-latex-listings 'minted
	org-latex-packages-alist '(("" "minted"))
	org-latex-pdf-process
	'("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
	org-latex-tables-centered t)
  (setq org-insert-heading-respect-content t)
  :bind* (:map org-mode-map ("C-<return>" . org-meta-return))
  :hook (org-mode . variable-pitch-mode))

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
  :config
  (setq sage-shell:set-ipython-version-on-startup nil))

(use-package ob-sagemath
  :ensure t
  :config
  (setq org-babel-default-header-args:sage '((:session . t)
					     (:results . "output")))
  (with-eval-after-load "org"
    (define-key org-mode-map (kbd "C-c c") 'ob-sagemath-execute-async))
  (setq org-confirm-babel-evaluate nil)
  (setq org-export-babel-evaluate nil)
  (setq org-startup-with-inline-images t)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images))

(use-package haskell-mode
  :defer t
  :commands (haskell-mode) 
  :init
  (load-library "haskell-mode-autoloads")
  
  :mode
  (("\\.hs\\'" . haskell-mode)
   ("\\.cabal\\'" . haskell-cabal-mode))
  
  :custom
  (haskell-process-type 'cabal-new-repl)
  (haskell-process-load-or-reload-prompt t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-log t)
  (haskell-font-lock-symbols t)
  
  :config
  (require 'haskell-interactive-mode)

  (defun haskell-mode-after-save-handler ())
  (defun ebn/haskell-format-buffer ()
    "Format Haskell buffer using Ormolu."
    (interactive)
    (when-let ((ormolu-path
		(seq-find (lambda (x) (string-match-p (regexp-quote "ormolu") x))
			  exec-path)))
      (make-process
       :name "ormolu"
       :buffer "*ormolu-log*"
       :command `(,(format "%sormolu" ormolu-path) "-m" "inplace" ,(buffer-file-name))
       :sentinel (lambda (proc evt) (revert-buffer-quick nil)))))
  
  (defun haskell-mode-setup ()
    (haskell-indentation-mode)
    (autoload 'haskell-doc-current-info "haskell-doc")
    (setq-local eldoc-documentation-function
		'haskell-doc-current-info)
    (setq-local tab-stop-list '(2 4))
    (setq-local haskell-process-path-cabal "cabal")
    (setq indent-line-function 'indent-relative)
    (setq tab-width 2))

  :hook
  ((haskell-mode . haskell-mode-setup)
   (haskell-mode . interactive-haskell-mode))
  
  :bind
  (:map haskell-mode-map ("C-c C-f" . ebn/haskell-format-buffer)))

(use-package eglot
  :defer t
  :hook ((haskell-mode . eglot-ensure)
	 (c-mode . eglot-ensure))
  :custom
  (eglot-autoshutdown t)
  (eglot-autoreconnect nil)
  (eglot-confirm-server-initiated-edits nil)
  (eldoc-idle-delay 1)
  :config
  :bind (:map eglot-mode-map ("C-c C-a" . eglot-code-actions)))

(use-package envrc
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
  :bind ("C-c C-t" . vterm-other-window))

(use-package project
  :config
  (defun ebn/project-compile ()
    (interactive)
    (if compile-history
	(recompile)
      (call-interactively 'project-compile)))
  (general-define-key
   :prefix "C-c"
   "p" '(:ignore)
   "pc" '(ebn/project-compile :which-key "Compile")
   "pt" '(gtags-find-tag :which-key "gtags-find")
   "pp" '(project-switch-project :which-key "Switch project")
   ;"pb" '(project-switch-buffer :which-key "Switch project buffer")
   "pf" '(project-find-file :which-key "Find project file")))

(use-package cc-mode
  :ensure nil
  :config
  (setq c-default-style "cc-mode")
  :bind
  (:map c-mode-map
	("C-c o" . ff-find-other-file)
	("C-c c" . project-compile)
	("C-c C-c" . comment-or-uncomment-region)))

(use-package gtags
  :ensure nil)

(use-package avy
  :ensure t
  :defer t
  :commands 'avy-goto-char-timer
  :config
  :bind
  ("M-g g" . avy-goto-line)
  ("M-g c" . avy-goto-char-in-line)
  ("M-s" . avy-goto-char-in-line)
  ("C-ö" . avy-goto-char-timer))

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C--") 'mc/mark-all-like-this-dwim)
  (global-set-key (kbd "C-'") 'mc/mark-next-like-this))

(use-package expand-region
  :ensure t
  :defer t
  :commands 'er/expand-region
  :bind ("C-," . er/expand-region))

(use-package dot-mode
  :ensure t
  :diminish
  :bind
  ("C-." . dot-mode-execute))

(use-package paredit
  :ensure t
  :defer t
  :diminish
  :mode ("\\.el\\'" . emacs-lisp-mode)
  :hook (emacs-lisp-mode . enable-paredit-mode))

(use-package hydra
  :ensure t
  :config
  (defhydra move-down (global-map "C-n")
    "Move down"
    ("n" next-line))
  (defhydra move-up (global-map "C-p")
    "Move up"
    ("p" previous-line))
  (defhydra undo-hydra (global-map "C-x u")
    "Undo"
    ("u" undo))
  (defhydra increment/decrement-number-hydra (global-map "C-+")
    "Incremenet or decrement number at point"
    ("+" ebn/increment-number-at-point)
    ("-" ebn/decrement-number-at-point))
  )

(use-package ox-hugo
  :ensure t
  :defer t
  :commands 'org-export-dispatch
  :after 'ox)

;;; ebn-init.el ends here
