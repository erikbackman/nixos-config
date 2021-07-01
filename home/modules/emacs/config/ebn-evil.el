(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :commands 'company-tng-configure-default
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(provide 'ebn-evil)
