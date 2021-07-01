;;; -*- lexical-binding: t; -*-

(use-package ivy
  :ensure t
  :hook (after-init . ivy-mode)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy))

(use-package counsel
  :ensure t
  :after ivy
  :bind (("C-c C-r" . ivy-resume))
  :config
  ;; make mnemonic alias for how I want to bind it
  (defalias 'my/counsel-rg-directory 'counsel-rg)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (setq counsel-git-cmd "rg --files"
        counsel-grep-base-command
          "rg --column --line-number --no-heading --smart-case --no-ignore --hidden --follow --color never %s %s"
        counsel-rg-base-command
          "rg --column --line-number --no-heading --smart-case --no-ignore --hidden --follow --color never %s ."))

;; Used by Ivy to sort commands by frequency.

;; increasing recentf max items for better ivy-switch-buffer completion


(use-package counsel-projectile
  ;; When this loads, projectile will also load. It would be nice if I could use
  ;; `:commands projectile-command-map'. However, commands expects a function
  ;; and not a key map. `:keymap' exists for this case, but I can't figure out
  ;; how to bind it they way I want and where I want (not here). So just load
  ;; after init.
  :hook (after-init . counsel-projectile-mode))

(use-package company
  :ensure t
  :config
  (add-to-list 'company-backends 'company-capf)
  (global-company-mode))

(use-package projectile
  :commands projectile-mode
  :config
  (projectile-mode +1))

;; (use-package lsp-mode
;;   :ensure t
;;   :commands lsp-install-server
;;   )
;; 
;; (use-package lsp-ui
;;   :ensure t
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :config
;;   (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;;   (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
;;   (setq lsp-ui-sideline-enable nil
;;         lsp-ui-doc-enable t
;;         lsp-ui-flycheck-enable nil
;;         lsp-ui-imenu-enable t
;;         lsp-ui-sideline-ignore-duplicate t))

;;(use-package company-capf)
;;(use-package company
;;  :ensure t
;;  :hook (after-init . global-company-mode)
;;  :bind (:map company-active-map
;;         ("C-n" . company-select-next)
;;         ("C-p" . company-select-previous))
;;  :config
;;  (setq company-idle-delay 0
;;        company-minimum-prefix-length 1
;;        company-show-numbers t))


(provide 'ebn-completion)
