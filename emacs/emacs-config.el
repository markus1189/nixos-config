(require 'package)
(require 'quick-yes) ;; added via load path...

(desktop-save-mode 1)

;; (custom-set-faces
;;  '(helm-selection ((t (:background "#eee8d5" :foreground "red" :weight bold))))
;;  '(default ((t (:inherit nil :stipple nil :background "#fdf6e3" :foreground "#657b83" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "ADBO" :family "Source Code Pro"))))
;;  '(magit-hash ((t (:foreground "#268bd2"))))
;;  '(sp-show-pair-match-face ((t (:background "#d33682" :foreground "black" :weight bold)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#242424" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "adobe" :family "Source Code Pro"))))
 '(Man-overstrike ((t (:inherit bold :foreground "#ddaa6f"))))
 '(Man-underline ((t (:foreground "medium spring green" :underline "medium spring green"))))
 '(ac-selection-face ((t (:background "dark orange" :foreground "gray20"))))
 '(ace-jump-face-foreground ((t (:foreground "dark orange" :underline nil))))
 '(agda2-highlight-datatype-face ((t (:foreground "deep sky blue"))))
 '(agda2-highlight-function-face ((t (:foreground "deep sky blue"))))
 '(avy-lead-face ((t (:background "orange1" :foreground "black"))))
 '(avy-lead-face-0 ((t (:background "pale green" :foreground "black"))))
 '(avy-lead-face-1 ((t (:background "sky blue" :foreground "black"))))
 '(avy-lead-face-2 ((t (:background "IndianRed2" :foreground "black"))))
 '(bmkp-*-mark ((t (:foreground "orange"))))
 '(bmkp-D-mark ((t (:foreground "red"))))
 '(bmkp-a-mark ((t (:foreground "yellow"))))
 '(bmkp-bad-bookmark ((t (:foreground "Red" :underline t :slant italic))))
 '(bmkp-local-directory ((t (:foreground "Pink"))))
 '(col-highlight ((t (:inherit hl-line))))
 '(company-preview ((t (:foreground "dim gray"))))
 '(company-preview-common ((t (:inherit company-preview :foreground "dark orange"))))
 '(company-scrollbar-bg ((t (:inherit company-tooltip :background "dark gray"))))
 '(company-scrollbar-fg ((t (:background "dark orange"))))
 '(company-tooltip ((t (:background "gray17" :foreground "light gray"))))
 '(company-tooltip-annotation ((t (:inherit company-tooltip :foreground "dark orange"))))
 '(company-tooltip-common ((t (:inherit company-tooltip :foreground "dark orange"))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :foreground "black"))))
 '(company-tooltip-selection ((t (:inherit company-tooltip :background "orange1" :foreground "black"))))
 '(cursor ((t (:background "white smoke" :inverse-video t))))
 '(diredp-compressed-file-suffix ((t (:foreground "steel blue"))))
 '(diredp-date-time ((t (:foreground "pale goldenrod"))))
 '(diredp-deletion ((t (:foreground "red"))))
 '(diredp-dir-heading ((t (:foreground "pale green" :height 1.15))))
 '(diredp-dir-priv ((t (:foreground "#8ac6f2"))))
 '(diredp-exec-priv ((t (:foreground "yellow"))))
 '(diredp-file-name ((t (:foreground "white"))))
 '(diredp-file-suffix ((t (:foreground "light gray"))))
 '(diredp-flag-mark ((t (:foreground "DarkOrange1"))))
 '(diredp-flag-mark-line ((t (:background "gray20"))))
 '(diredp-ignored-file-name ((t (:foreground "dark gray"))))
 '(diredp-link-priv ((t (:foreground "dodger blue"))))
 '(diredp-mode-line-marked ((t (:foreground "#6B6BFFFF2C2C"))))
 '(diredp-number ((t (:foreground "pale goldenrod"))))
 '(diredp-rare-priv ((t (:background "red" :foreground "black"))))
 '(diredp-read-priv ((t (:foreground "tomato"))))
 '(diredp-symlink ((t (:foreground "pale green"))))
 '(diredp-write-priv ((t (:foreground "spring green"))))
 '(ediff-current-diff-C ((t (:background "RoyalBlue4"))))
 '(ediff-fine-diff-A ((t (:background "#aa2222" :foreground "black"))))
 '(ediff-fine-diff-B ((t (:background "#22aa22" :foreground "black"))))
 '(ediff-fine-diff-C ((t (:background "RoyalBlue2" :foreground "black"))))
 '(ensime-implicit-highlight ((t (:underline (:color "dim gray" :style wave)))))
 '(eshell-prompt ((t (:foreground "light sky blue" :weight bold))))
 '(flyspell-duplicate ((t (:underline "red"))))
 '(flyspell-incorrect ((t (:foreground "red" :underline "red"))))
 '(guide-key/highlight-command-face ((t (:foreground "spring green"))))
 '(guide-key/key-face ((t (:foreground "dark orange"))))
 '(guide-key/prefix-command-face ((t (:foreground "dark orange"))))
 '(helm-candidate-number ((t (:inherit helm-source-header :height 0.7))))
 '(helm-ff-directory ((t (:foreground "#8ac6f2"))))
 '(helm-ff-executable ((t (:foreground "chartreuse2"))))
 '(helm-selection ((t (:background "gray24" :weight bold))))
 '(helm-source-header ((t (:foreground "DarkOrange2" :weight bold :height 1.3 :family "Sans Serif"))))
 '(helm-visible-mark ((t (:background "dark orange" :foreground "black"))))
 '(hl-line ((t (:background "gray20"))))
 '(hl-paren-face ((t (:weight ultra-bold))) t)
 '(linum ((t (:inherit (shadow default) :height 0.9))))
 '(magit-item-highlight ((t (:inherit hl-line))))
 '(match ((t (:background "dark green" :foreground "white"))))
 '(minesweeper-1 ((t (:foreground "steel blue"))))
 '(minesweeper-2 ((t (:foreground "light green"))))
 '(minesweeper-3 ((t (:foreground "medium violet red"))))
 '(minesweeper-marked ((t (:foreground "yellow"))))
 '(minesweeper-neighbor ((t (:background "black"))))
 '(newsticker-treeview-selection-face ((t (:inherit hl-line))))
 '(org-level-4 ((t (:foreground "dark orange"))))
 '(org-todo ((t (:foreground "indian red" :weight bold))))
 '(popup-isearch-match ((t (:background "orange" :foreground "black"))))
 '(popup-menu-selection-face ((t (:background "dark orange" :foreground "black"))))
 '(region ((t (:background "#444444" :foreground "#f6f3e8" :weight bold))))
 '(shm-current-face ((t (:background "gray18"))))
 '(sp-show-pair-match-face ((t (:inherit match))))
 '(term-color-blue ((t (:background "cornflower blue" :foreground "cornflower blue"))))
 '(term-color-green ((t (:background "light green" :foreground "light green"))))
 '(term-color-magenta ((t (:background "orchid" :foreground "orchid"))))
 '(term-color-red ((t (:background "firebrick1" :foreground "firebrick1"))))
 '(term-color-yellow ((t (:background "yellow" :foreground "yellow"))))
 '(trailing-whitespace ((t (:underline "dark red"))))
 '(visible-mark-face ((t (:background "dim gray"))))
 '(vr/group-0 ((t (:background "RoyalBlue4"))))
 '(vr/group-1 ((t (:background "#335533"))))
 '(vr/group-2 ((t (:background "brown4"))))
 '(vr/match-0 ((t (:background "gray30"))))
 '(vr/match-1 ((t (:background "black"))))
 '(wgrep-delete-face ((t (:background "firebrick4"))))
 '(wgrep-face ((t (:background "dark green"))))
 '(window-number-face ((t (:foreground "white"))))
 '(writegood-duplicates-face ((t (:inherit font-lock-warning-face :background "DeepPink" :foreground "black"))))
 '(writegood-passive-voice-face ((t (:inherit font-lock-warning-face :background "LemonChiffon" :foreground "black"))))
 '(writegood-weasels-face ((t (:inherit font-lock-warning-face :background "DarkOrange" :foreground "black")))))

(custom-set-variables
 '(custom-enabled-themes (quote (wombat)))
 '(helm-split-window-default-side (quote right))
 '(helm-for-files-preferred-list
   (quote
    (helm-source-buffers-list helm-source-fasd helm-source-recentf helm-source-bookmarks helm-source-file-cache helm-source-files-in-current-dir helm-source-locate))))

;; make unpure packages archives unavailable
(setq package-archives nil)
(setq package-archives nil)

(package-initialize 'noactivate)

(eval-when-compile
  (require 'use-package))

(use-package company
  :ensure t
  :bind ("<C-tab>" . company-complete)
  :diminish company-mode
  :commands (company-mode global-company-mode)
  :defer 1
  :config
  (global-company-mode))

(use-package flycheck
  :ensure t
  :defer 2
  :config (global-flycheck-mode))

(use-package fullframe
  :ensure t
  :demand t
  :config
  (fullframe magit-status magit-mode-quit-window))

(use-package magit
  :ensure t
  :defer
  :if (executable-find "git")
  :bind (("s-g" . magit-status)
         ("s-G" . magit-dispatch-popup)
         :map magit-process-mode-map
         ("k" . magit-process-kill))
  :init
  (defun mh/magit-log-edit-mode-hook ()
    (flyspell-mode)
    (set-fill-column 72))
  :hook
  ((magit-log-edit-mode . mh/magit-log-edit-mode-hook)))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :commands projectile-global-mode
  :defer 5
  :bind
  ("C-s-p" . projectile-find-file)
  :config
  (use-package helm-projectile
    :bind ("C-s-p" . projectile-find-file)
    :config
    (setq projectile-completion-system 'helm)
    (helm-projectile-on))
  (projectile-global-mode))

(use-package s
  :ensure t)

(use-package f
  :ensure t)

(use-package helm
  :ensure t
  :bind (("s-;" . helm-for-files)
         ("s-'" . helm-M-x)
         ("s-f" . helm-find-files)
         ("M-y" . helm-show-kill-ring)
         ("s-RET" . 'helm-man-woman)
         :map helm-map
         ("C-w" . backward-kill-word)
         ("C-S-n" . mh/helm-next-line-fast)
         ("C-S-p" . mh/helm-previous-line-fast))
  :config

  (defun mh/helm-next-line-fast ()
    (interactive)
    (progn (helm-next-line 5)))

  (defun mh/helm-previous-line-fast ()
    (interactive)
    (progn (helm-previous-line 5)))


  (defun helm-fasd ()
    "Preconfigured helm to search using fasd."
    (interactive)
    (helm :sources '(helm-source-fasd)
          :buffer "*helm async fasd source*"))

  (defvar helm-source-fasd
    (helm-build-sync-source "helm-source-fasd"
      :volatile 't
      :candidates (lambda () (s-lines (shell-command-to-string "fasd -lR"))) ; TODO: use mutate
      :action 'helm-type-file-actions))
  (helm-mode 1))

(use-package helm-swoop
  :ensure t
  :bind ("M-s o" . helm-swoop))

(use-package wgrep-helm
    :ensure t)

(use-package solarized-theme
  :ensure t
  :init
  (setq solarized-distinct-fringe-background t)
  (setq solarized-use-variable-pitch nil)
  (setq solarized-high-contrast-mode-line t)
  :config
  ;; (load-theme 'solarized-light)
  )

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
  :bind (("s-/" . undo-tree-visualize)
         ("C-/" . undo-tree-undo)
         ("C-?" . undo-tree-redo)))

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
  (defun mh/git-link-aareal-gogs (hostname dirname filename branch commit start end)
    (format "http://%s/%s/src/%s/%s"
            hostname
            dirname
            commit
            (concat filename
                    (when start
                      (concat "#"
                              (if end
                                  (format "L%s-L%s" start end)
                                (format "L%s" start)))))))

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
          ("gitlab" git-link-gitlab)
          ("gogs.default.dev.aareality.aareal.org" mh/git-link-aareal-gogs))))

(use-package nix-mode
  :ensure t)

(use-package prog-mode
  :init
  (defmacro Λ (&rest body)
    `(lambda ()
       (interactive)
       ,@body))

  (defun smart-open-line-above ()
    "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
    (interactive)
    (move-beginning-of-line nil)
    (ignore-errors (newline-and-indent))
    (forward-line -1)
    (indent-according-to-mode))

  (defun smart-open-line ()
    "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
    (interactive)
    (move-end-of-line nil)
    (ignore-errors (newline-and-indent)))

  (defun mh/other-window-backward ()
    (interactive)
    (other-window -1))

  (defun mh/other-window-forward ()
    (interactive)
    (other-window 1))

  (global-set-key (kbd "M-i") 'back-to-indentation)
  (global-set-key (kbd "s-[") 'mh/other-window-backward)
  (global-set-key (kbd "s-]") 'mh/other-window-forward)
  (global-set-key (kbd "s-v") 'view-mode)
  (global-set-key (kbd "C-s-y") 'bury-buffer)
  (global-set-key (kbd "C-S-s-y") 'unbury-buffer)
  (global-set-key (kbd "C-s-o") (Λ (split-window-right) (other-window 1)))
  (global-set-key (kbd "C-s-u") 'split-window-below)
  (global-set-key (kbd "C-s-u") (Λ (split-window-below) (other-window 1)))
  (global-set-key (kbd "s-i") 'delete-other-windows)
  (global-set-key (kbd "M-s-i") 'delete-other-frames)
  (global-set-key (kbd "C-S-n") (Λ (ignore-errors (forward-line 5))))
  (global-set-key (kbd "C-S-p") (Λ (ignore-errors (forward-line -5))))
  (global-set-key (kbd "C-S-f") (Λ (ignore-errors (forward-char 10))))
  (global-set-key (kbd "C-S-b") (Λ (ignore-errors (backward-char 10))))
  (global-set-key "\M-o" 'smart-open-line)
  (global-set-key "\C-o" 'smart-open-line-above)
  (global-set-key (kbd "<f5>") 'recompile)

  (winner-mode 1)
  (global-set-key (kbd "s-TAB") 'winner-undo)
  (global-set-key (kbd "C-S-s-i") 'winner-redo)

  (setq frame-title-format "Emacs: %b (%f)")
  (put 'narrow-to-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'set-goal-column 'disabled nil)
  (put 'downcase-region 'disabled nil)

  (global-auto-revert-mode 1)
  (delete-selection-mode)

  (require 'dired-x)

  ;; https://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
  (setq version-control t   ;; Use version numbers for backups.
      kept-new-versions 10  ;; Number of newest versions to keep.
      kept-old-versions 0   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them.

  (setq vc-make-backup-files t)

  (setq backup-directory-alist '(("" . "~/.emacs.d/backup/per-save")))

  (defun force-backup-of-buffer ()
    ;; Make a special "per session" backup at the first save of each
    ;; emacs session.
    (when (not buffer-backed-up)
      ;; Override the default parameters for per-session backups.
      (let ((backup-directory-alist '(("" . "~/.emacs.d/backup/per-session")))
            (kept-new-versions 3))
        (backup-buffer)))
    ;; Make a "per save" backup on each save.  The first save results in
    ;; both a per-session and a per-save backup, to keep the numbering
    ;; of per-save backups consistent.
    (let ((buffer-backed-up nil))
      (backup-buffer)))

(add-hook 'before-save-hook  'force-backup-of-buffer)

  (if window-system
      (progn
        (tool-bar-mode -1)
        (menu-bar-mode -1)
        (toggle-scroll-bar -1)))

  (add-hook 'focus-out-hook (lambda () (interactive) (save-some-buffers t))))

(use-package quick-yes
  :bind (
         :map query-replace-map
         ("M-y" . act)
         ("M-n" . skip)))

(use-package expand-region
  :bind (("C-M-w" . er/expand-region)
         ("C-!" . er/contract-region)))

(use-package scala-mode
  :ensure t)

(use-package sbt-mode
  :ensure t)

(use-package smartparens
  :demand t
  :ensure t
  :bind
  (("C-k" . sp-kill-hybrid-sexp)
   ("M-D" . sp-unwrap-sexp)
   ("C-s-s" . sp-split-sexp))
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode t)
  (smartparens-global-mode))

(use-package move-text
  :ensure t
  :bind
  (("M-œ" . move-text-up)
   ("M-ï" . move-text-down)))

(use-package find-temp-file
  :ensure t
  :bind
  ("C-s-f" . find-temp-file)
  :config
  (setq find-temp-file-prefix (list "etf-alpha"
                                    "etf-bravo"
                                    "etf-charlie"
                                    "etf-delta"
                                    "etf-echo"
                                    "etf-foxtrot"
                                    "etf-golf"
                                    "etf-hotel"
                                    "etf-india"
                                    "etf-juliet"
                                    "etf-kilo"
                                    "etf-lima"
                                    "etf-mike"
                                    "etf-november"
                                    "etf-oscar"
                                    "etf-papa"
                                    "etf-quebec"
                                    "etf-romeo"
                                    "etf-sierra"
                                    "etf-tango"
                                    "etf-uniform"
                                    "etf-victor"
                                    "etf-whiskey"
                                    "etf-x-ray"
                                    "etf-yankee"
                                    "etf-zulu")))

(use-package haskell-mode
  :ensure t)

(use-package ox-jira
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :init
  (defun mh/mc/create-fake-cursor-at-point () (interactive) mc/create-fake-cursor-at-point)
  (defun mh/multiple-cursors-mode () (interactive) (multiple-cursors-mode 1))
  :bind
  (("C-M->" . mh/mc/create-fake-cursor-at-point)
   ("C-M-<" . mh/multiple-cursors-mode)
   ("C-#" . mc/mark-all-like-this-dwim)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)))

(use-package compile
  :ensure t
  :demand t
  :config
  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (toggle-read-only))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))
;;;
