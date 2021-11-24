;;; ebn-evil.el --- evil -*- lexical-binding: t; no-byte-compile: nil; -*-
;;; Commentary:
;;; Code:
(use-package evil
  :if use-evil
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
  (:map evil-insert-state-map ("<ESC>" . #'evil-normal-state)))

(use-package evil-collection
  :if use-evil
  :after evil
  :config
  (evil-collection-init)
  :bind
  (:map evil-normal-state-map ("S-<down>" . evil-forward-section-begin)
  (:map evil-normal-state-map ("S-<up>" . evil-backward-section-begin))))
