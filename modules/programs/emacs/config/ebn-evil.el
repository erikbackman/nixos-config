;;; Backup of evil-related stuff.. just in case..
(use-package evil
  :if (ebn/use-evil)
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
  (setcdr evil-insert-state-map nil)
  :bind
  (:map evil-motion-state-map ("RET" . nil))
  (:map evil-normal-state-map ("gd" . #'xref-find-definitions-other-window))
  (:map evil-insert-state-map ("<ESC>" . #'evil-normal-state))

(use-package evil-collection
  :if (ebn/use-evil)
  :after evil
  :config
  (evil-collection-init)
  :bind
  (:map evil-normal-state-map ("S-<down>" . evil-forward-section-begin)
  (:map evil-normal-state-map ("S-<up>" . evil-backward-section-begin))))

(use-package general
  :config
  (when (ebn/use-evil)
    (general-evil-setup t)
    (general-setq evil-want-Y-yank-to-eol))

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

  (when (ebn/use-evil)
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
	   "pt" '(gtags-find-tag :which-key "Find project tag")
	   "pc" '(project-compile :which-key "Compile project")

	   ;; Other
	   "o" '(:ignore t :which-key "Toggle")
	   "oo" '(outline-cycle :which-key "Outline cycle")
	   "oO" '(outline-cycle-buffer :which-key "Outline cycle")

	   "e" '(:ignore t :which-key "Eval")
	   "ef" '(eval-defun :which-key "Eval-defun"))))

(use-package evil-org
  :when (ebn/use-evil)
  :defer t
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))
