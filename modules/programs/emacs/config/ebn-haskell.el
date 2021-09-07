;;; ebn-haskell.el --- Haskell -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;###autoload
(defun +haskell/open-repl (&optional arg)
  "Opens a Haskell REPL."
  (interactive "P")
  (if-let (window
           (display-buffer
            (haskell-session-interactive-buffer (haskell-session))))
      (window-buffer window)
    (error "Failed to display Haskell REPL")))

;;;###autoload
(defun +haskell/evil-open-above ()
  "Opens a line above the current mode"
  (interactive)
  (evil-digit-argument-or-evil-beginning-of-line)
  (haskell-indentation-newline-and-indent)
  (evil-previous-line)
  (haskell-indentation-indent-line)
  (evil-append-line nil))

;;;###autoload
(defun +haskell/evil-open-below ()
  "Opens a line below the current mode"
  (interactive)
  (evil-append-line nil)
  (haskell-indentation-newline-and-indent))

(use-package haskell-mode
  :config
  (load "haskell-mode-autoloads"))

(use-package dante
  :after haskell-mode
  :commands 'dante-mode
  :init
  (defun ebn-init-haskell ()
    (interactive)
    (progn
      (dante-mode 1)
      ))
  (add-hook 'haskell-mode-hook #'ebn-init-haskell))
  
(provide 'ebn-haskell)
;;; ebn-haskell.el ends here
