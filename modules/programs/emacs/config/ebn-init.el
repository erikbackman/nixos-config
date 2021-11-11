;;; ebn-init.el --- init -*- lexical-binding: t; no-byte-compile: nil; -*-
;;; Commentary:
;;; Code:
(require 'use-package)

(defun ebn/display-startup-time ()
  "Message startup time."
  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))

;; Basic
;; Setting this lower after early-init for shorter gc-pauses
(setq-default fill-column 80)
(setq gc-cons-threshold (* 2 1000 1000)
      ring-bell-function 'ignore
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(add-hook 'after-init-hook #'ebn/display-startup-time)
(add-to-list 'load-path (shell-command-to-string "agda-mode locate"))


;;; Packages
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

  :delight
  (auto-fill-function " AF")
  (visual-line-mode))

(use-package ebn-core
  :ensure nil
  :config
  (ebn/font :name "JetBrains Mono" :size 15 :weight 'normal)
  ;(ebn/font :name "Iosevka Custom" :size 18 :weight 'normal)
  )

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  :config
  (require 'project)
  (evil-mode 1)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  :bind (:map evil-motion-state-map ("RET" . nil)))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package general
  :config
  (general-evil-setup t)
  (general-setq evil-want-Y-yank-to-eol)

  (general-create-definer ebn/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-define-key
   :states 'normal :keymaps 'dired-mode-map
   "m" '(dired-mark :which-key "Dired mark")
   "D" '(dired-do-delete :which-key "Dired delete")
   "c" '(dired-do-copy :which-key "Dired copy")
   "C" '(dired-do-compress :which-key "Dired compress")
   "r" '(dired-do-rename :which-key "Dired rename")
   "u" '(dired-unmark :which-key "Dired unmark")
   "U" '(dired-unmark-all-marks :which-key "Dired umkark all")
   "-" '(ebn/dired-up-directory :which-key "Dired up directory"))

  (ebn/leader-key-def
    "" '(nil :which-key "Leader")

    ;; Basic
    ":"  '(execute-extended-command :which-key "M-x")
    ";"  '(eval-expression :which-key "Eval expr")
    "qq" '(kill-emacs :which-key "Exit")

    ;; File
    "f"  '(:ignore t :which-key "File")
    "ff" '(find-file-other-window :which-key "Find file other-win")
    "fF" '(find-file :which-key "Find file")
    "fs" '(save-buffer :which-key "Save file")
    "fR" '(ebn/rename-current-file :which-key "Rename File")
    "fd" '(dired-jump-other-window :which-key "Find files in dir")
    "sb" '(consult-ripgrep :which-key "Ripgrep Files")
    "fr" '(consult-recent-file :which-key "Recent file")

    ;; Buffer
    "b"  '(:ignore t :which-key "Buffer")
    "bk" '(ebn/kill-current-buffer :which-key "Kill buffer")
    "bb" '(project-switch-to-buffer :which-key "Project buffers")
    "bB" '(consult-buffer :which-key "Buffers")

    ;; Tab
    "t"  '(:ignore t :which-key "Tab")
    "tt" '(tab-new :which-key "New tab")
    "tn" '(tab-next :which-key "Next tab")
    "tp" '(tab-previous :which-key "Previous tab")
    "tl" '(tab-list :which-key "Tab list")
    "td" '(tab-close :which-key "Tab delete")
    "to" '(tab-close-other :which-key "Tab delete other")

    ;; Window
    "w"  '(:ignore t :which-key "Window")
    "ww" '(other-window :which-key "Other window")
    "wd" '(delete-window :which-key "Delete window")
    "wo" '(delete-other-windows :which-key "Delete other windows")
    "ws" '(split-window-below :which-key "Split window below")
    "wv" '(split-window-right :which-key "Split window right")

    ;; Project
    "p"  '(:ignore t :which-key "Project")
    "pp" '(project-switch-project :which-key "Switch project")
    "pf" '(project-find-file :which-key "Find project file")
    "ps" '(ebn/project-rg :which-key "Project rg")

    ;; Other
    "o" '(:ignore t :which-key "Toggle")
    "oo" '(outline-cycle :which-key "Outline cycle")
    "oO" '(outline-cycle-buffer :which-key "Outline cycle")

    "e" '(:ignore t :which-key "Eval")
    "ef" '(eval-defun :which-key "Eval-defun")))

;(use-package modus-themes
;  :ensure t
;  :init
;  (show-paren-mode 1)
;  :config
;  (setq modus-themes-paren-match '(bold intense))
;  ;(modus-themes-load-operandi)
;  (modus-themes-load-vivendi)
;  (when window-system (set-frame-size (selected-frame) 90 50))
;  (setq fancy-startup-text nil))

(use-package kaolin-themes
  :custom-face
  (default ((t (:background "#0C0F12"))))
  (fringe ((t (:background "#0C0F12"))))
  (mode-line ((t (:background "#0C0F12"))))
  :config
  (load-theme 'kaolin-aurora t nil))

(use-package yasnippet
  :config
  (yas-global-mode -1))

(use-package which-key
  :config
  (which-key-mode 1))

(use-package vertico
  :config
  (vertico-mode))

(use-package consult
  :config
  (setq consult-preview-key nil)
  (recentf-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

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
	org-latex-tables-centered t))

(use-package evil-org
  :defer t
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

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
  :mode (("\\.hs\\'" . haskell-mode)
	 ("\\.cabal\\'" . haskell-cabal-mode))
  :custom
  (haskell-process-type 'cabal-new-repl)
  (haskell-process-load-or-reload-prompt t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-log t)
  (haskell-font-lock-symbols t)
  :config
  (require 'haskell-interactive-mode)
  (defun +haskell/evil-open-above ()
    "Opens a line above the current mode"
    (interactive)
    (evil-digit-argument-or-evil-beginning-of-line)
    (haskell-indentation-newline-and-indent)
    (evil-previous-line)
    (haskell-indentation-indent-line)
    (evil-append-line nil))

  (defun +haskell/evil-open-below ()
    "Opens a line below the current mode"
    (interactive)
    (evil-append-line nil)
    (haskell-indentation-newline-and-indent))

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
    (setq tab-width 2)
    (setq-local evil-shift-width 2))

  (general-nmap :keymaps 'haskell-mode-map "o" '+haskell/evil-open-below)

  :hook ((haskell-mode . haskell-mode-setup)
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
	 (c-mode . corfu-mode)
	 ))

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
      (compile)))
  :bind ("C-x p c" . ebn/project-compile))

;;; ebn-init.el ends here
