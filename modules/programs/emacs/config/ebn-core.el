;; -*-no-byte-compile: t; -*-
;;; ebn-core.el --- Basic stuff -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'use-package)

(when window-system (set-frame-size (selected-frame) 90 50))
(setq-default fill-column 80)
(setq ring-bell-function 'ignore)
(setq fancy-startup-text nil)
(load-theme 'modus-operandi t)

(add-to-list 'load-path (shell-command-to-string "agda-mode locate")) ;agda2-mode

(defun ebn/kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun ebn/rename-current-file ()
  (interactive)
  (rename-file (buffer-file-name)))

(use-package kaolin-themes
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode 1))

(use-package vertico
  :ensure t
  :config
  (vertico-mode))

(use-package consult :ensure t)

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

   ;; Buffer
   "b"	'(:ignore t :which-key "Buffer")
   "bk" '(ebn/kill-current-buffer :which-key "Kill buffer")
   "bb" '(consult-buffer :which-key "Switch buffer")
   "sb" '(consult-ripgrep :which-key "Search buffer")

   ;; Notes
   "n"	'(:ignore t :which-key "Notes")
   ;"na" '(org-agenda :which-key "Agenda")

   ;; Window
   "w"	'(:ignore t :which-key "Window")
   "ww" '(other-window :which-key "Other window")
   "wd" '(delete-window :which-key "Delete window")
   "wo" '(delete-other-windows :which-key "Delete other windows")
   "ws" '(split-window-below :which-key "Split window below")
   "wv" '(split-window-right :which-key "Split window right")
   ))

(use-package envrc
  :commands 'envrc-global-mode
  :config (envrc-global-mode))

(use-package flycheck
  :commands 'global-flycheck-mode
  :config (global-flycheck-mode))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  :bind (:map evil-motion-state-map ("RET" . nil))
  ;(with-eval-after-load "evil-maps"
  ;  (define-key evil-motion-state-map (kbd "RET") nil))
  )

(use-package company
  :ensure t
  :config
  (add-to-list 'company-backends 'company-capf)
  (global-company-mode))

(use-package evil-collection
  :commands 'company-tng-configure-default
  :after evil
  :ensure t
  :config
  (evil-collection-init)
  )

(use-package org
  :ensure t
  :commands (my/org-prettify-buffer)
  :init
  (defun my/org-prettify-buffer ()
    (interactive)
    (when (not org-pretty-entities)
      (org-toggle-pretty-entities))
    (org-latex-preview))
  :hook ((org-mode . my/org-prettify-buffer))
  :config
  (setq org-image-actual-width nil)
  (setq org-return-follows-link t))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t) ;; Disable v2-migration-prompt
  :custom
  (setq org-roam-directory (file-truename "~/org-roam"))
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

(provide 'ebn-core)
;;; ebn-core.el ends here
