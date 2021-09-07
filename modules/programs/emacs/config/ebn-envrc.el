;;; init-direnv.el --- Integrate with direnv -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun sanityinc/maybe-enable-envrc-global-mode ()
  "Enable `envrc-global-mode' if `direnv' is installed."
  (when (executable-find "direnv")
    (envrc-global-mode)))
(use-package envrc
  :ensure t
  :config
  (add-hook 'after-init-hook #'sanityinc/maybe-enable-envrc-global-mode))

(provide 'ebn-envrc)

;;; init-direnv.el ends here
