;;; ebn-init.el --- init -*- lexical-binding: t; no-byte-compile: nil; -*-
;;; Commentary:
;;; Code:
(require 'use-package)
(require 'ebn-core)
(require 'dired)

;; Basic
(setq ring-bell-function 'ignore
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq-default fill-column 80)
(setq global-mark-ring-max 2)

(add-to-list 'load-path (shell-command-to-string "agda-mode locate"))
(add-to-list 'load-path "~/.emacs.d/lisp")

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

(defun ebn/comment-or-uncomment-fn ()
  (interactive)
  (save-excursion
    (end-of-defun -1)
    (set-mark (point))
    (end-of-defun)  
    (comment-or-uncomment-region
     (region-beginning)
     (region-end))))

(defun ebn/comment-or-uncomment ()
  (interactive)
  ;; Comment or uncomment active region
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning)
				   (region-end))
    (save-excursion
      ;; When cursor is at a function definition
      ;; comment or uncomment the entire functions
      (if (equal (symbol-at-point) 'defun)
	  (ebn/comment-or-uncomment-fn)
	;; Else comment or uncomment the line
	(move-beginning-of-line nil)
	(set-mark (point))
	(move-end-of-line nil)
	(comment-or-uncomment-region (region-beginning)
				     (region-end))))))

(defun ebn/haskell-mode-lines-to-list ()
  (interactive)
  (skip-chars-forward "[:space:]"))

(defun ebn/setup-modeline ()
  (set-face-attribute
   'mode-line nil
   :background "#000"
   :foreground "white"
   :box '(:line-width 6 :color "#000")
   :overline nil
   :underline nil)

  (set-face-attribute
   'mode-line-inactive nil
   :background "#090a0b"
   :foreground "white"
   :box '(:line-width 6 :color "#090a0b")
   :overline nil
   :underline nil))

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
  (initial-scratch-message nil)

  :delight
  (auto-fill-function " AF")
  (visual-line-mode)

  :hook ((prog-mode . superword-mode)
	 (fundamental-mode . repeat-mode)
	 (prog-mode . repeat-mode))

  :bind
  (:map global-map 
	("C-j" . join-line)
	("M-u" . upcase-dwim)
	("C-x k" . ebn/kill-current-buffer)
	("C-o" . ebn/open-line-below)
	("M-o" . ebn/open-line-above)
	("M-§" . end-of-buffer)
	("<f7>" . call-last-kbd-macro)
	("M-z" . zap-up-to-char)
	("C-k" . ebn/kill-dwim)
	("M-1" . delete-other-windows)
	("M-3" . split-window-right)
	("C-0" . pop-global-mark)
	("C-9" . forward-list)
	("C-8" . backward-list)
	("C-<down>" . ebn/forward-to-paragraph)
	("C-h l" . display-local-help)
	("M-i" . back-to-indentation)
	("<f8>" . kmacro-insert-counter)
	("C-x C-b" . ibuffer)))

(use-package ebn-core
  :ensure nil
  :config
  (ebn/font :name "Victor Mono" :size 18 :weight 'medium)
  (ebn/font-variable-pitch :name "CMU Concrete" :size 21)
  (global-set-key (kbd "M-w") 'ebn/copy-dwim))

(use-package kaolin-themes
  :ensure t
  :config
  (load-theme 'kaolin-aurora t nil)
  (set-face-attribute 'fringe nil :background nil)
  (set-face-attribute 'fringe nil :background nil)
  (set-face-attribute 'mode-line nil :background nil :box nil :overline "darkgray")
  (set-face-attribute 'mode-line-inactive nil :foreground "darkgray" :overline "darkgray")
  (set-face-attribute 'font-lock-keyword-face nil :italic nil))

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
  (defun ebn/consult-buffer ()
    (interactive)
    (push-mark (point) t nil)
    (consult-buffer))
  :bind
  ("C-c r" . consult-recent-file)
  ("C-c f" . consult-ripgrep)
  ("C-c l" . consult-line)
  ("C-c i" . consult-imenu)
  ("C-c t" . gtags-find-tag)
  ("C-x b" . ebn/consult-buffer))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
	completion-category-defaults nil
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
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch :background nil)
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
	org-format-latex-options (plist-put org-format-latex-options :scale 1.4)
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
  :hook ((org-mode . variable-pitch-mode)
	 (org-mode . org-cdlatex-mode)))

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

(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

(use-package yapfify
  :ensure t)

(use-package ob-sagemath
  :ensure t
  :config
  (setq org-babel-default-header-args:sage '((:session . t)
					     (:results . "output")))
  (with-eval-after-load "org"
    (define-key org-mode-map (kbd "C-c c") 'ob-sagemath-execute-async))
  (setq org-confirm-babel-evaluate nil
	org-export-babel-evaluate nil
	org-startup-with-inline-images t)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images))

(use-package haskell-mode
  :defer t
  :commands (haskell-mode) 
 
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
  (set-face-attribute 'haskell-interactive-face-result nil :foreground "White" :weight 'medium)

  ;(add-hook 'after-save-hook (lambda () (funcall-interactively 'haskell-process-load-or-reload)))
  (require 'haskell-interactive-mode)

  (defun haskell-mode-after-save-handler ())
  (defun ebn/haskell-format-buffer ()
    "Format Haskell buffer using Ormolu."
    (interactive)
    (when-let ((ormolu-path
		(seq-find (lambda (x) (string-match-p (regexp-quote "ormolu") x))
			  exec-path)))
      (make-process
       :name "ormolxu"
       :buffer "*ormolu-log*"
       :command `(,(format "%sormolu" ormolu-path) "-m" "inplace" ,(buffer-file-name))
       :sentinel (lambda (proc evt) (revert-buffer-quick nil)))))
  
  (defun haskell-mode-setup ()
    (haskell-indentation-mode)
    (autoload 'haskell-doc-current-info "haskell-doc")
    (setq-local eldoc-documentation-function
		'haskell-doc-current-info)
    (setq-local tab-stop-list '(2))
    (setq-local haskell-process-path-cabal "cabal")
    (setq indent-line-function 'indent-relative)
    (setq tab-width 2))

  :hook
  ((haskell-mode . haskell-mode-setup)
   (haskell-mode . interactive-haskell-mode))
  
  :bind
  (:map haskell-mode-map
	("C-c C-f" . ebn/haskell-format-buffer)
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
  (define-key eglot-mode-map [remap display-local-help] nil)
  (add-to-list 'eglot-server-programs
               `(sage-shell:sage-mode . ("pyls" "-v" "--tcp" "--host"
					 "localhost" "--port" :autoport)))
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

(use-package cc-mode
  :ensure nil
  :config
  (setq c-default-style "cc-mode")
  :bind
  (:map c-mode-map
	("C-c o" . ff-find-other-file)
	("C-c c" . project-compile)
	("C-c u" . ebn/comment-or-uncomment)))

(use-package gtags
  :ensure nil)

(use-package avy
  :ensure t
  :defer t
  :commands 'avy-goto-char-timer
  :config
  (setq avy-timeout-seconds 0.4)
  :bind
  ("M-g g" . avy-goto-line)
  ("M-g c" . avy-goto-char-in-line)
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
	("s-<down>" . mc/mark-next-like-this)))

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

(use-package ox-hugo
  :ensure t
  :defer t
  :commands 'org-export-dispatch
  :after 'ox)

(use-package erc
  :config
  (set-face-attribute 'erc-prompt-face nil :background nil :foreground "Green")
  (setq erc-prompt (lambda () (concat "[" (buffer-name) "]"))))

(use-package popper
  :ensure t
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
	  "\\*eldoc\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package rainbow-mode
  :ensure t
  :mode "\\.css\\'")

;;; ebn-init.el ends here
