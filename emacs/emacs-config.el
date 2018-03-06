(setq frame-title-format "Emacs: %b (%f)")
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)

(require 'package)

;; make unpure packages archives unavailable
(setq package-archives nil)

(package-initialize 'noactivate)

(eval-when-compile
  (require 'use-package))

(use-package winner-mode
  :bind
  (("s-TAB" . winner-undo)
   ("C-S-s-i" . winner-redo))
  :config
  (winner-mode 1))

(use-package company
  :ensure t
  :bind ("<C-tab>" . company-complete)
  :diminish company-mode
  :commands (company-mode global-company-mode)
  :defer 1
  :config
  (global-company-mode))

(use-package counsel
  :ensure t
  :commands (counsel-descbinds)
  :bind (([remap execute-extended-command] . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate)
         ("M-y" . counsel-yank-pop)))

(use-package flycheck
  :ensure t
  :defer 2
  :config (global-flycheck-mode))

(use-package ivy
  :ensure t
  :defer 1
  :bind (("C-c C-r" . ivy-resume)
         ("C-x C-b" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("C-j" . ivy-call))
  :diminish ivy-mode
  :commands ivy-mode
  :config
  (ivy-mode 1))

(use-package magit
  :ensure t
  :defer
  :if (executable-find "git")
  :bind (("s-g" . magit-status)
         ("s-G" . magit-dispatch-popup))
  :init
  (setq magit-completing-read-function 'ivy-completing-read)
  :config
  (fullframe magit-status magit-mode-quit-window)
  )

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :commands projectile-global-mode
  :defer 5
  :bind
  ("C-s-p" . projectile-find-file)
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-global-mode))


(use-package solarized-theme
  :ensure t
  :init
  (setq solarized-distinct-fringe-background t)
  (setq solarized-use-variable-pitch nil)
  (setq solarized-high-contrast-mode-line t)
  (load-theme 'solarized-light))

(use-package avy
  :ensure t
  :config
  (setq avy-timeout-seconds 0.25)
  (defun mh/avy-goto-word-or-subword-or-char (prefix)
    (interactive "P")
    (cond ((equal (list 4) prefix) (avy-goto-word-or-subword-1))
          (t (avy-goto-char-timer))))
  :bind (("s-l" . mh/avy-goto-word-or-subword-or-char)
         ("s-:" . avy-goto-line)
         :map isearch-mode-map
         ("C-'" . avy-isearch)))

(use-package fasd
  :ensure t
  :config
  (global-fasd-mode))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config (global-undo-tree-mode)
  :bind ("s-/" . undo-tree-visualize))

(use-package goto-chg
  :ensure t
  :commands goto-last-change
  :bind (("C-." . goto-last-change)
         ("C-," . goto-last-change-reverse)))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :config (yas-reload-all))

(use-package indent-guide
  :ensure t
  :init
  (setq indent-guide-char "┋")
  :config
  (indent-guide-global-mode 1))

(use-package git-link
  :ensure t
  :bind
  ("C-c Y" . git-link)
  :config
  (defun mh/git-link-github (hostname dirname filename branch commit start end)
    (format "https://%s/%s/blob/%s/%s"
            hostname
            dirname
            commit
            (concat filename
                    (when start
                      (concat "#"
                              (if end
                                  (format "L%s-L%s" start end)
                                (format "L%s" start)))))))
  (setq git-link-remote-alist
        '(("github.com" mh/git-link-github)
          ("bitbucket" git-link-bitbucket)
          ("gitorious" git-link-gitorious)
          ("gitlab" git-link-gitlab))))

;;;
