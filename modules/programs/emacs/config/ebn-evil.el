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

  (defun ebn/setup-evil-haskell-mode ()
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
    (general-nmap :keymaps 'haskell-mode-map "o" '+haskell/evil-open-below))

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

  :hook
  ((haskell-mode . haskell-mode-setup)
   (haskell-mode . interactive-haskell-mode))
  
  :bind
  (:map haskell-mode-map ("C-c C-f" . ebn/haskell-format-buffer)))
