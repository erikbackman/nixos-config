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
;Setting this lower after early-init for shorter gc-pauses
(setq-default fill-column 80)
(setq gc-cons-threshold (* 2 1000 1000)
      ring-bell-function 'ignore
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(add-hook 'after-init-hook #'ebn/display-startup-time)

;;; Looks
(when (member "JetBrains Mono" (font-family-list))
  (set-frame-font "JetBrains Mono-11" t t))

(add-to-list 'load-path (shell-command-to-string "agda-mode locate"))

;;; Vars
(defvar ebn/formatter nil)

;;; Functions
(defun ebn/kill-current-buffer ()
  "Kill current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun ebn/rename-current-file ()
  "Rename current file to NEWNAME."
  (interactive)
  (let ((fname (buffer-file-name))
	(bname (buffer-name)))
    (if (not (and bname (file-exists-p fname)))
	(error "Buffer '%s' is not visiting a file" bname)
      (let ((new-name (read-file-name "New name: " fname)))
	(if (get-buffer new-name)
	    (error "Buffer with name %s already exists" new-name)
	  (rename-file fname new-name t)
	  (rename-buffer new-name)
	  (set-visited-file-name new-name)
	  (set-buffer-modified-p nil)
	  (message "File renamed to %s" new-name))))))

(defun ebn/dired-up-directory ()
  "Up directory - killing current buffer."
  (interactive)
  (let ((cb (current-buffer)))
    (progn (dired-up-directory)
	   (kill-buffer cb))))

(defun ebn/project-rg ()
  "Run ripgrep in current project"
  (interactive)
  (call-interactively #'consult-ripgrep (project-root (project-current))))

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

  :delight
  (auto-fill-function " AF")
  (visual-line-mode)

  :hook (emacs-lisp-mode . (lambda () (setq-local lisp-indent-function #'common-lisp-indent-function))))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  :config
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
    ;(general-auto-unbind-keys)
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
	"ff" '(find-file :which-key "Find file")
	"fs" '(save-buffer :which-key "Save file")
	"fR" '(ebn/rename-current-file :which-key "Rename File")
	"fd" '(dired-jump :which-key "Find files in dir")
	"sb" '(consult-ripgrep :which-key "Ripgrep Files")
	"fr" '(consult-recent-file :which-key "Recent file")

	;; Buffer
	"b"  '(:ignore t :which-key "Buffer")
	"bk" '(ebn/kill-current-buffer :which-key "Kill buffer")
	"bb" '(consult-buffer :which-key "Buffers")

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
	))

(use-package modus-themes
  :ensure t
  :init
  (show-paren-mode 1)
  :config
  (setq modus-themes-paren-match '(bold intense))
  (modus-themes-load-operandi)
  (when window-system (set-frame-size (selected-frame) 90 50))
  (setq fancy-startup-text nil))

(use-package yasnippet
  :config
  (yas-global-mode 1))

;; Packages
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
	dired-recursive-deletes t))

(use-package org
  :defer t
  :commands (my/org-prettify-buffer org-agenda org-capture)
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
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.2)))

(use-package evil-org
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t) ;; Disable v2-migration-prompt
  :custom
  (org-roam-directory "~/org-roam")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates '(("d" "default" plain "%?" :if-new
				 (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
				  "#+title: ${title}\n#+startup: latexpreview\n#+startup: entitiespretty")
				 :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package sage-shell-mode
    :ensure t)
(use-package ob-sagemath
  :ensure t
  :config
  (setq org-babel-default-header-args:sage '((:session . t)
                                             (:results . "output")))
  ;; C-c c for asynchronous evaluating (only for SageMath code blocks).
  (with-eval-after-load "org"
    (define-key org-mode-map (kbd "C-c c") 'ob-sagemath-execute-async))
  ;; Do not confirm before evaluation
  (setq org-confirm-babel-evaluate nil)
  ;; Do not evaluate code blocks when exporting.
  (setq org-export-babel-evaluate nil)
  ;; Show images when opening a file.
  (setq org-startup-with-inline-images t)
  ;; Show images after evaluating code blocks.
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images))

(use-package haskell-mode
    :defer t
    :commands (haskell-mode) 
    ;:after evil
    :mode ("\\.hs\\'" . haskell-mode)
    :custom
    (haskell-process-type 'cabal-repl)
    (haskell-process-load-or-reload-prompt t)
    (haskell-process-auto-import-loaded-modules t)
    (haskell-process-log t)
    :config
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
      (setq-local tab-stop-list '(2 4))
      (setq indent-line-function 'indent-relative)
      (setq tab-width 2)
      (setq-local evil-shift-width 2))

    (general-nmap :keymaps 'haskell-mode-map "o" '+haskell/evil-open-below)

    (add-hook 'haskell-mode-hook 'haskell-mode-setup)

    :bind
    (:map haskell-mode-map ("C-c C-f" . ebn/haskell-format-buffer)))

(use-package eglot
  :defer t
  :hook (haskell-mode . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-autoreconnect nil)
  (eldoc-idle-delay 1)
  :bind (:map eglot-mode-map ("C-c C-a" . eglot-code-actions)))

(use-package envrc
 :config
 (envrc-global-mode))

(use-package corfu
  :defer t
  :custom
  (corfu-auto-delay 0.2)
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  (corfu-echo-documentation nil) ;; Do not show documentation in the echo area
  :hook ((haskell-mode . corfu-mode)
	 (emacs-lisp-mode . corfu-mode)
         (eshell-mode . corfu-mode)))

(use-package nix-mode
    :defer t
    :mode "\\.nix\\'"
    :ensure t)
;;; ebn-init.el ends here
