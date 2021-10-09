;;; ebn-init.el --- Basic stuff -*- lexical-binding: t; no-byte-compile: nil; -*-
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

;; Looks
(when (member "JetBrains Mono" (font-family-list))
  (set-frame-font "JetBrains Mono-11" t t))

(add-to-list 'load-path (shell-command-to-string "agda-mode locate"))

;; Functions
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
  :ensure t
  :config
  (yas-global-mode 1))

;; Packages
(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(use-package vertico
  :ensure t
  :config
  (vertico-mode))

(use-package consult :ensure t
  :config
  (setq consult-preview-key nil)
  (recentf-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package general
  :ensure t
  :config
  (general-evil-setup t)

  (general-define-key
   :states 'normal
   :keymaps 'dired-mode-map

   "m" '(dired-mark :which-key "Dired mark")
   "D" '(dired-do-delete :which-key "Dired delete")
   "c" '(dired-do-copy :which-key "Dired copy")
   "C" '(dired-do-compress :which-key "Dired compress")
   "r" '(dired-do-rename :which-key "Dired rename")
   "u" '(dired-unmark :which-key "Dired unmark")
   "U" '(dired-unmark-all-marks :which-key "Dired umkark all")
   "-" '(ebn/dired-up-directory :which-key "Dired up directory"))

  (general-define-key
   :keymaps 'normal
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   ""	'(nil :which-key "Leader")

   ;; Basic
   ":"	'(execute-extended-command :which-key "M-x")
   ";"  '(eval-expression :which-key "Eval expr")
   "qq" '(kill-emacs :which-key "Exit")

   ;; File
   "f"	'(:ignore t :which-key "File")
   "ff" '(find-file :which-key "Find file")
   "fr" '(consult-recent-file :which-key "Recent files")
   "fs" '(save-buffer :which-key "Save file")
   "fR" '(ebn/rename-current-file :which-key "Rename File")
   "fd" '(dired-jump :which-key "Find files in dir")

   ;; Buffer
   "b"	'(:ignore t :which-key "Buffer")
   "bk" '(ebn/kill-current-buffer :which-key "Kill buffer")
   "bb" '(consult-buffer :which-key "Switch buffer")
   "sb" '(consult-ripgrep :which-key "Ripgrep dir")

   ;; Tab
   "t" '(:ignore t :which-key "Tab")
   "tt" '(tab-new :which-key "New tab")
   "tn" '(tab-next :which-key "Next tab")
   "tp" '(tab-previous :which-key "Previous tab")
   "tl" '(tab-list :which-key "Tab list")
   "td" '(tab-close :which-key "Tab delete")
   "to" '(tab-close-other :which-key "Tab delete other")

   ;; Window
   "w"	'(:ignore t :which-key "Window")
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
   "z" '(org-capture :which-key "Org capture")
   ))

(use-package envrc
  :commands 'envrc-global-mode
  :config (envrc-global-mode))

(use-package evil
  :ensure t
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
  :ensure t
  :commands 'company-tng-configure-default
  :init (setq evil-collection-company-use-tng nil)
  :after evil
  :config
  (evil-collection-init))

(use-package company
  :ensure t
  :custom
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 2)
  :config
  (add-to-list 'company-backends 'company-capf)
  (add-to-list 'company-backends 'company-elisp)
  :hook ((after-init . global-company-mode)))

(use-package dired
  :ensure nil
  :config
  (setq dired-recursive-copies t
	dired-recursive-deletes t))

(use-package org
  :ensure t
  :commands (my/org-prettify-buffer)
  :init
  (defun my/org-prettify-buffer ()
    (interactive)
    (when (not org-pretty-entities)
      (org-toggle-pretty-entities))
    (org-latex-preview))
  :config 
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
  :ensure t
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t) ;; Disable v2-migration-prompt
  :custom
  (org-roam-directory "~/org-roam")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
    '(("d" "default" plain "%?" :if-new
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

;;; ebn-init.el ends here
