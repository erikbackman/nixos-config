;;; ebn-core.el --- Basic stuff -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package envrc
  :commands 'envrc-global-mode
  :config (envrc-global-mode))

(use-package kaolin-themes
  :ensure t
  :config (load-theme 'kaolin-aurora t))

(use-package flycheck
  :commands 'global-flycheck-mode
  :config (global-flycheck-mode))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(provide 'ebn-core)
;;; ebn-core.el ends here
