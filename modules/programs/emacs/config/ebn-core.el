;; -*-no-byte-compile: t; -*-
;;; ebn-core.el --- Basic stuff -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'use-package)

(defun ebn/kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun ebn/rename-current-file ()
  (interactive)
  (rename-file (buffer-file-name)))

(use-package kaolin-themes
  :ensure t
  :config (load-theme 'kaolin-aurora t)
  ;;(custom-set-faces
  ;; '(default ((t (:background "#0C0F12" :background "#0C0F12"))))
  ;; '(fringe  ((t (:background "#0C0F12" :background "#0C0F12")))))
  )

;(require agda2-mode)
(add-to-list 'load-path (shell-command-to-string "agda-mode locate"))

(use-package which-key
  :ensure t
  :config (which-key-mode 1))

(use-package counsel
  :ensure t)

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
   ":"	'(counsel-M-x :which-key "M-x")
   "qq" '(kill-emacs :which-key "Exit")

   ;; File
   "f"	'(:ignore t :which-key "File")
   "ff" '(counsel-find-file :which-key "Find file")
   "fr" '(counsel-recentf :which-key "Recent files")
   "fs" '(save-buffer :which-key "Save file")
   "fR" '(ebn/rename-current-file :which-key "Rename File")

   ;; Buffer
   "b"	'(:ignore t :which-key "Buffer")
   "bk" '(ebn/kill-current-buffer :which-key "Kill buffer")
   "bb" '(counsel-ibuffer :which-key "Switch buffer")

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
  (evil-mode 1))

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
  (evil-collection-init))

(use-package org
  :ensure t
  ;:hook (org-mode . #'org-toggle-pretty-entities)
  :config
  (setq org-image-actual-width nil)
  (setq org-return-follows-link t))

(use-package org-roam
  :ensure t
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
  (setq org-roam-v2-ack t) ;; Disable v2-migration-prompt
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
