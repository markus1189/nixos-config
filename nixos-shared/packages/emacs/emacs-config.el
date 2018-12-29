(require 'package)
(require 'quick-yes) ;; added via load path...
(require 'dired+) ;; added via load path...

(if window-system
    (progn
      (tool-bar-mode -1)
      (menu-bar-mode -1)
      (scroll-bar-mode -1)
      (toggle-scroll-bar -1)))

;; (desktop-save-mode 1)

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
 '(ediff-current-diff-C ((t (:background "RoyalBlue4"))))
 '(ediff-fine-diff-A ((t (:background "#aa2222" :foreground "black"))))
 '(ediff-fine-diff-B ((t (:background "#22aa22" :foreground "black"))))
 '(ediff-fine-diff-C ((t (:background "RoyalBlue2" :foreground "black"))))
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
 '(ediff-merge-split-window-function (quote split-window-horizontally))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(dired-auto-revert-buffer (quote dired-directory-changed-p))
 '(dired-dwim-target t)
 '(dired-filter-saved-filters (quote (("custom-filters" (omit)))))
 '(dired-guess-shell-alist-user
   (quote
    (("\\.hp\\'" "hp2pretty" "hp2ps")
     ("\\.\\(svg\\)\\|\\(png\\)\\|\\(jpg\\)\\'" "imv")
     ("\\.\\(mp4\\)\\|\\(avi\\)\\|\\(mkv\\)\\|\\(m4v\\)\\'" "mplayer -really-quiet")
     ("\\.pdf\\'" "zathura" "pdftotext ? /dev/stdout" "evince")
     ("\\.ps\\'" "evince")
     ("\\.\\(ods\\)\\|\\(odf\\)\\'" "libreoffice")
     ("\\.\\(zip\\)\\|\\(rar\\)\\'" "file-roller")
     ("\\.jar\\'" "java -jar"))))
 '(dired-isearch-filenames (quote dwim))
 '(dired-listing-switches "-al --block-size=M --group-directories-first")
 '(diredp-hide-details-initially-flag nil)
 '(custom-enabled-themes (quote (wombat)))
 '(helm-split-window-default-side (quote right))
 '(magit-default-tracking-name-function (quote magit-default-tracking-name-branch-only))
 '(magit-diff-options (quote ("--minimal" "--patience")))
 '(magit-diff-refine-hunk nil)
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n100")))
 '(magit-log-auto-more t)
 '(magit-merge-arguments (quote ("--no-ff")))
 '(magit-mode-hook (quote (magit-load-config-extensions)))
 '(magit-process-log-max 50)
 '(magit-process-popup-time -1)
 '(magit-rebase-arguments (quote ("--autostash")))
 '(magit-remote-ref-format (quote remote-slash-branch))
 '(magit-repo-dirs (quote ("~/repos")))
 '(magit-repository-directories (quote ("~/repos")))
 '(magit-restore-window-configuration t)
 '(magit-revert-buffers (quote silent) t)
 '(magit-server-window-for-rebase (quote pop-to-buffer))
 '(magit-set-upstream-on-push t)
 '(magit-tag-arguments (quote ("--annotate")))
 '(magit-use-overlays t)
 '(helm-for-files-preferred-list
   (quote
    (helm-source-buffers-list helm-source-fasd helm-source-recentf helm-source-bookmarks helm-source-file-cache helm-source-files-in-current-dir helm-source-locate)))
 '(whitespace-action (quote (auto-cleanup))))

;; make unpure packages archives unavailable
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
  :config (global-flycheck-mode)
  :bind (("C-s-SPC" . flycheck-next-error)
         ("C-S-s-SPC" . flycheck-previous-error)))

(use-package fullframe
  :ensure t
  :demand t
  :config
  (fullframe magit-status magit-mode-quit-window))

(use-package flyspell
  :ensure t
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode))

(use-package helm-flyspell
  :ensure t
  :demand t
  :config
  (defun mh/switch-ispell-dictionary()
    (interactive)
    (let* ((dic ispell-current-dictionary)
           (change (if (string= dic "deutsch8") "english" "deutsch8")))
      (ispell-change-dictionary change)
      (message "Dictionary switched from %s to %s" dic change)))

  :bind (("C-c s" . flyspell-buffer)
         ("C-c S" . mh/switch-ispell-dictionary)
         :map flyspell-mode-map
         ("C-." . helm-flyspell-correct)))

(use-package magit
  :ensure t
  :defer
  :if (executable-find "git")
  :bind (("s-g" . magit-status)
         ("s-G" . magit-commit)
         ("C-s-g" . magit-log-buffer-file)
         ("C-S-s-g" . magit-blame)
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
  :bind (("C-s-p" . projectile-find-file))

  :config
  (use-package helm-projectile
    :demand t
    :bind (("C-s-p" . projectile-find-file)
           ("s-h" . helm-projectile-grep))
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
      :candidates (lambda () (s-lines (shell-command-to-string "@fasd@/bin/fasd -lR")))
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
  :demand t
  :diminish yas-minor-mode
  :hook
  (prog-mode . yas-minor-mode)
  (text-mode . yas-minor-mode)
  :config
  (yas-reload-all))

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
  :demand t
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

  (defun mh/copy-file-and-line ()
    (interactive)
    (let ((name buffer-file-name))
      (message "Copied: %s" name)
      (kill-new name)))

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

  (winner-mode 1)
  (global-set-key (kbd "s-TAB") 'winner-undo)
  (global-set-key (kbd "C-S-s-i") 'winner-redo)

  (global-set-key (kbd "C-c y") 'mh/copy-file-and-line)
  (global-set-key (kbd "C-w") 'backward-kill-word)
  (global-set-key (kbd "C-s-k") 'kill-region)
  (global-set-key (kbd "C-c t") 'make-frame)
  (global-set-key (kbd "<f7>") (Λ (switch-to-buffer "*scratch*")))
  (global-set-key (kbd "<f9>") 'calc)
  (global-set-key (kbd "M-SPC") 'cycle-spacing)
  (global-set-key (kbd "C-h") 'backward-delete-char)

  (global-set-key (kbd "<f5>") 'compile-dwim)

  (defvar get-buffer-compile-command (lambda (file) (cons file 1)))
  (make-variable-buffer-local 'get-buffer-compile-command)

  (setq-default compile-command "")

  (defun compile-dwim (&optional arg)
    "Compile Do What I Mean.
    Compile using `compile-command'.
    When `compile-command' is empty prompt for its default value.
    With prefix C-u always prompt for the default value of
    `compile-command'.
    With prefix C-u C-u prompt for buffer local compile command with
    suggestion from `get-buffer-compile-command'.  An empty input removes
    the local compile command for the current buffer."
    (interactive "P")
    (cond
     ((and arg (> (car arg) 4))
      (let ((cmd (read-from-minibuffer
                  "Buffer local compile command: "
                  (funcall get-buffer-compile-command
                           (or (file-relative-name (buffer-file-name)) ""))
                  nil nil 'compile-history)))
        (cond ((equal cmd "")
               (kill-local-variable 'compile-command)
               (kill-local-variable 'compilation-directory))
              (t
               (set (make-local-variable 'compile-command) cmd)
               (set (make-local-variable 'compilation-directory)
                    default-directory))))
      (when (not (equal compile-command ""))
        ;; `compile' changes the default value of
        ;; compilation-directory but this is a buffer local
        ;; compilation
        (let ((dirbak (default-value 'compilation-directory)))
          (compile compile-command)
          (setq-default compilation-directory dirbak))))
     ((or (and arg (<= (car arg) 4))
          (equal compile-command ""))
      (setq-default compile-command (read-from-minibuffer
                                     "Compile command: "
                                     (if (equal compile-command "")
                                         "make " compile-command)
                                     nil nil 'compile-history))
      (setq-default compilation-directory default-directory)
      (when (not (equal (default-value 'compile-command) ""))
        (compile (default-value 'compile-command))))
     (t
      (recompile))))

  (defun mh/comment-or-uncomment-current-line-or-region (prefix)
    "Comments or uncomments current current line or whole lines in region."
    (interactive "P")
    (cond ((equal (list 4) prefix) (kill-comment nil))
          (t (save-excursion
               (let (min max)
                 (if (region-active-p)
                     (setq min (region-beginning) max (region-end))
                   (setq min (point) max (point)))
                 (comment-or-uncomment-region
                  (progn (goto-char min) (line-beginning-position))
                  (progn (goto-char max) (line-end-position))))))))

  (global-set-key (kbd "M-;") 'mh/comment-or-uncomment-current-line-or-region)

  (global-set-key (kbd "C-c o") 'org-open-at-point-global)

  (defun mh/duplicate-current-line-below ()
    "duplicate current line, position cursor at the top."
    (interactive)
    (save-excursion
      (let ((current-line (thing-at-point 'line)))
        (when (or (= 1 (forward-line 1)) (eq (point) (point-max)))
          (insert "\n"))
        (insert current-line))))

  (defun mh/duplicate-current-line-above ()
    "duplicate current line, position cursor at the bottom."
    (interactive)
    (mh/duplicate-current-line-below)
    (forward-line 1))

  (global-set-key (kbd "C-M-ï") 'mh/duplicate-current-line-above)
  (global-set-key (kbd "C-M-œ") 'mh/duplicate-current-line-below)
  (global-set-key (kbd "C-z") 'eshell)

  (global-set-key (kbd "C-c j") (Λ (mh/scala-open-in-intellij)))

  (defun mh/delete-or-kill-window (prefix)
    "Without prefix, delete-window, with prefix, kill the buffer."
    (interactive "P")
    (cond ((equal (list 4) prefix) (kill-buffer (current-buffer)))
          ((equal (list 16) prefix) (let ((kill-buffer-query-functions nil)) (kill-buffer (current-buffer))))
          (t (delete-window))))

  (global-set-key (kbd "s-y") 'mh/delete-or-kill-window)

  (defun mh/delete-horizontal-space-forward ()
    "*Delete all spaces and tabs after point."
    (interactive "*")
    (delete-region (point) (progn (skip-chars-forward " \t") (point))))

  (global-set-key (kbd "C-M-\\") 'mh/delete-horizontal-space-forward)

  (setq frame-title-format "Nixmacs: %b (%f)")
  (put 'narrow-to-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'set-goal-column 'disabled nil)
  (put 'downcase-region 'disabled nil)

  (set-mouse-color "OrangeRed")

  (global-auto-revert-mode 1)
  (delete-selection-mode)
  (setq-default indent-tabs-mode nil)

  (require 'dired-x)

  ;; https://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
  (setq version-control t    ;; Use version numbers for backups.
        kept-new-versions 10 ;; Number of newest versions to keep.
        kept-old-versions 0  ;; Number of oldest versions to keep.
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
   ("C-s-s" . sp-split-sexp)
   ("C-s-d" . sp-splice-sexp-killing-around)
   ("C-M-u" . sp-up-sexp)
   ("C-M-d" . sp-down-sexp)
   ("C-M-SPC" . sp-mark-sexp)
   ("C-s-j" . sp-join-sexp))
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
  :demand t
  :config
  (defun mh/haskell-mode-organize-imports ()
    (interactive)
    (haskell-sort-imports)
    (haskell-align-imports))
  (defun mh/haskell-set-compilation-command ()
    (set (make-local-variable 'compile-command) "stack --nix build "))
  (defun mh/haskell-import-qualified-p ()
    (interactive)
    (save-excursion
      (beginning-of-line)
      (search-forward "qualified" (line-end-position) t)))

  (defun mh/haskell-import-line-p ()
    (interactive)
    (save-excursion
      (beginning-of-line)
      (re-search-forward "^import" (line-end-position) t)))

  (defun mh/haskell-import-toggle-qualified ()
    (interactive)
    (if (not (mh/haskell-import-line-p))
        (message "Not at import line.")
      (cond ((mh/haskell-import-qualified-p)
             (beginning-of-line)
             (search-forward "qualified")
             (backward-kill-word 1)
             (just-one-space)
             (forward-sexp)
             (unless (equal (point) (line-end-position)) (kill-line)))
            (t
             (beginning-of-line)
             (search-forward "import" (line-end-position) t)
             (insert " qualified")
             (just-one-space)
             (forward-sexp)
             (unless (equal (point) (line-end-position)) (kill-line))
             (insert " as ")))))

  (defun mh/haskell-insert-undefined (prefix)
    (interactive "P")
    (cond ((equal (list 4) prefix) (insert "_undefined"))
          (t (insert "undefined"))))

  :bind (("C-c C-a" . mh/haskell-mode-organize-imports)
         ("C-c C-q" . mh/haskell-import-toggle-qualified)
         ("C-c ?" . mh/haskell-insert-undefined))
  :ensure t
  :hook
  (haskell-mode . mh/haskell-set-compilation-command))

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
  :bind (("<f5>" . recompile))
  :config

  (defun play-sound-file-async (file)
    "Play FILE asynchronously"
    (start-process-shell-command "appt-notify" nil "@mplayer@/bin/mplayer" "-really-quiet" file))

  (defun mh/compilation-start-sound (proc)
    (interactive)
    (play-sound-file-async "@popSound@"))

  (defun mh/compilation-play-sound-after-finish (buffer string)
    "Play a sound after compilation finished"
    (if (s-prefix? "finished" string)
        (play-sound-file-async "@yesSound@")
      (play-sound-file-async "@noSound@")))

  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (toggle-read-only))

  (add-hook 'compilation-finish-functions 'mh/compilation-play-sound-after-finish)
  :hook
  (compilation-filter . colorize-compilation-buffer)
  (compilation-start . mh/compilation-start-sound))

(use-package whitespace
  :ensure t
  :hook
  ((before-save . whitespace-cleanup)))

(use-package persistent-scratch
  :ensure t
  :demand t
  :config
  (persistent-scratch-setup-default))

(use-package iedit
  :ensure t
  :demand t
  :bind ("C-s-;" . iedit-mode))

(use-package evil-numbers
  :ensure t
  :bind
  (("C-s-+" . evil-numbers/inc-at-pt)
   ("C-s-=" . evil-numbers/inc-at-pt)
   ("C-s--" . evil-numbers/dec-at-pt)))

(use-package git-commit
  :ensure t
  :init
  (defun mh/commit-insert-branch ()
    "Insert the current branch name at point."
    (interactive)
    (insert
     (replace-regexp-in-string "^\\(bugfix\\|feature\\)/"
                               ""
                               (concat (magit-get-current-branch) ": "))))
  :bind
  (("C-c b" . mh/commit-insert-branch)))

(use-package yaml-mode
  :ensure t
  :demand t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode)))

(use-package flycheck-yamllint
  :ensure t)

(use-package iy-go-to-char
  :ensure t
  :bind (("M-m" . iy-go-up-to-char)
         ("M-M" . iy-go-up-to-char-backward)))

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-goto-error ()
                  "hydra-errors"
                  ("DEL" kill-whole-line "delete-line")
                  ("j" next-error "next")
                  ("k" previous-error "prev")
                  ("g" first-error "first")
                  ("o" (lambda () (interactive) (switch-to-buffer-other-window next-error-last-buffer)) "error buf")
                  ("q" nil "cancel"))
  :bind (("M-g" . hydra-goto-error/body)))

(use-package jq-mode
  :ensure t)

(use-package which-key
  :ensure t
  :demand t
  :config
  (which-key-mode))

(use-package restclient
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.restclient\\'" . restclient-mode)))

;; TODO: figure out how to customize build
;; (use-package pdf-tools
;;   :init
;;   (setq pdf-info-epdfinfo-program "/tmp/epdfinfo")
;;   :config
;;   (pdf-tools-install))

(use-package hippie-exp
  :ensure t
  :demand t
  :bind (("M-/" . hippie-expand)
         ("C-M-/" . hippie-expand-lines))
  :config
  (defvar he-search-loc-backward (make-marker))
  (defvar he-search-loc-forward (make-marker))

  (defun try-expand-dabbrev-closest-first (old)
    "Try to expand word \"dynamically\", searching the current buffer.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible expansions of the same
string). It returns t if a new expansion is found, nil otherwise."
    (let (expansion)
      (unless old
        (he-init-string (he-dabbrev-beg) (point))
        (set-marker he-search-loc-backward he-string-beg)
        (set-marker he-search-loc-forward he-string-end))

      (if (not (equal he-search-string ""))
          (save-excursion
            (save-restriction
              (if hippie-expand-no-restriction
                  (widen))

              (let (forward-point
                    backward-point
                    forward-distance
                    backward-distance
                    forward-expansion
                    backward-expansion
                    chosen)

                ;; search backward
                (goto-char he-search-loc-backward)
                (setq expansion (he-dabbrev-search he-search-string t))

                (when expansion
                  (setq backward-expansion expansion)
                  (setq backward-point (point))
                  (setq backward-distance (- he-string-beg backward-point)))

                ;; search forward
                (goto-char he-search-loc-forward)
                (setq expansion (he-dabbrev-search he-search-string nil))

                (when expansion
                  (setq forward-expansion expansion)
                  (setq forward-point (point))
                  (setq forward-distance (- forward-point he-string-beg)))

                ;; choose depending on distance
                (setq chosen (cond
                              ((and forward-point backward-point)
                               (if (< forward-distance backward-distance) :forward :backward))

                              (forward-point :forward)
                              (backward-point :backward)))

                (when (equal chosen :forward)
                  (setq expansion forward-expansion)
                  (set-marker he-search-loc-forward forward-point))

                (when (equal chosen :backward)
                  (setq expansion backward-expansion)
                  (set-marker he-search-loc-backward backward-point))

                ))))

      (if (not expansion)
          (progn
            (if old (he-reset-string))
            nil)
        (progn
          (he-substitute-string expansion t)
          t))))

  (defun try-expand-line-closest-first (old)
    "Try to complete the current line to an entire line in the buffer.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible completions of the same
string). It returns t if a new completion is found, nil otherwise."
    (let ((expansion ())
          (strip-prompt (and (get-buffer-process (current-buffer))
                             comint-use-prompt-regexp
                             comint-prompt-regexp)))
      (unless old
        (he-init-string (he-line-beg strip-prompt) (point))
        (set-marker he-search-loc-backward he-string-beg)
        (set-marker he-search-loc-forward he-string-end))

      (if (not (equal he-search-string ""))
          (save-excursion
            (save-restriction
              (if hippie-expand-no-restriction
                  (widen))

              (let (forward-point
                    backward-point
                    forward-distance
                    backward-distance
                    forward-expansion
                    backward-expansion
                    chosen)

                ;; search backward
                (goto-char he-search-loc-backward)
                (setq expansion (he-line-search he-search-string
                                                strip-prompt t))

                (when expansion
                  (setq backward-expansion expansion)
                  (setq backward-point (point))
                  (setq backward-distance (- he-string-beg backward-point)))

                ;; search forward
                (goto-char he-search-loc-forward)
                (setq expansion (he-line-search he-search-string
                                                strip-prompt nil))

                (when expansion
                  (setq forward-expansion expansion)
                  (setq forward-point (point))
                  (setq forward-distance (- forward-point he-string-beg)))

                ;; choose depending on distance
                (setq chosen (cond
                              ((and forward-point backward-point)
                               (if (< forward-distance backward-distance) :forward :backward))

                              (forward-point :forward)
                              (backward-point :backward)))

                (when (equal chosen :forward)
                  (setq expansion forward-expansion)
                  (set-marker he-search-loc-forward forward-point))

                (when (equal chosen :backward)
                  (setq expansion backward-expansion)
                  (set-marker he-search-loc-backward backward-point))

                ))))

      (if (not expansion)
          (progn
            (if old (he-reset-string))
            ())
        (progn
          (he-substitute-string expansion t)
          t))))


  ;; Create own function to expand lines (C-S-.)
  (defun hippie-expand-lines ()
    (interactive)
    (let ((hippie-expand-try-functions-list '(try-expand-line-closest-first
                                              try-expand-line-all-buffers)))
      (end-of-line)
      (hippie-expand nil))))

(use-package goto-chg
  :ensure t
  :demand t
  :config
  (defhydra hydra-goto-changes ()
                  "hydra-goto-changes"
                  ("SPC" goto-last-change "goto-last-change")
                  ("C-SPC" goto-last-change "goto-last-change")
                  ("C-x C-SPC" goto-last-change "goto-last-change")
                  ("DEL" goto-last-change-reverse "goto-last-change-reverse")
                  ("q" nil "cancel"))
  :bind (("C-x C-SPC" . hydra-goto-changes/body)))

(use-package beacon
  :demand t
  :ensure t
  :config
  (beacon-mode 1))

(use-package pabbrev
  :ensure t
  :config
  (global-pabbrev-mode))

(use-package groovy-mode
  :ensure t)

(use-package plantuml-mode
  :ensure t
  :config
  (setq plantuml-jar-path "@plantuml@/lib/plantuml.jar"))

(use-package mu4e
  :config
  (require 'mu4e-utils)
  (setq mu4e-sent-folder "/[Google Mail].All Mail"
      mu4e-drafts-folder "/[Google Mail].Drafts"
      mu4e-trash-folder "/[Google Mail].Trash"
      mu4e-refile-folder "/[Google Mail].All Mail"
      mu4e-maildir "~/Mail/personal"
      mu4e-html2text-command "@pandoc@/bin/pandoc -f html -t org"
      mu4e-view-auto-mark-as-read nil)
  (add-to-list 'mu4e-bookmarks
       (make-mu4e-bookmark
         :name  "Inbox"
         :query "NOT flag:thrashed AND maildir:/INBOX"
         :key ?b))
  (setq mu4e-bookmarks `(("\\\\Inbox" "Inbox" ?i)
                         ("flag:flagged" "Flagged messages" ?f)
                         (,(concat "flag:unread AND "
                                   "NOT flag:trashed AND "
                                   "NOT maildir:/[Google Mail].Spam AND "
                                   "NOT maildir:/[Google Mail].Bin")
                          "Unread messages" ?u))) )

(use-package terraform-mode
  :ensure t)

(use-package dired-filter
  :ensure t)

(use-package magit-todos
  :config (magit-todos-mode)
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

(use-package string-inflection
  :ensure t
  :commands ()
  :bind (("C-c C-u" . string-inflection-all-cycle)))

(use-package lua-mode
  :ensure t)

(use-package dyalog-mode
  :ensure t)
;;;