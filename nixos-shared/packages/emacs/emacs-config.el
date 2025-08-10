(require 'package)
(require 'quick-yes) ;; added via load path...
(require 'dired+) ;; added via load path...
(require 'iy-go-to-char)
(require 'dired-x)

;;; Code:

(progn
  (defun mh/secrets/raindrop/testToken () "@raindropTestToken@")
  (defun mh/secrets/gptel/perplexityApiKey () "@gptelPerplexityApiKey@")
  (defun mh/secrets/gptel/geminiApiKey () "@gptelGeminiApiKey@")
  (defun mh/secrets/gptel/openAiApiKey () "@gptelOpenAiApiKey@")
  (defun mh/secrets/gptel/anthropicApiKey () "@gptelAnthropicApiKey@")
  (defun mh/secrets/gptel/deepSeekApiKey () "@gptelDeepSeekApiKey@")
  (defun mh/secrets/gptel/xaiApiKey () "@gptelXAIApiKey@")
  (defun mh/secrets/gptel/openRouterApiKey () "@gptelOpenRouterApiKey@"))

(global-set-key (kbd "M-m") 'iy-go-to-char)

(setq
   split-width-threshold 0
   split-height-threshold nil)

(if window-system
    (progn
      (tool-bar-mode -1)
      (menu-bar-mode -1)
      (scroll-bar-mode -1)
      (toggle-scroll-bar -1)))

(global-display-line-numbers-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(async-shell-command-buffer 'new-buffer)
 '(custom-enabled-themes '(wombat))
 '(dired-auto-revert-buffer 'dired-directory-changed-p)
 '(dired-dwim-target t)
 '(dired-filter-saved-filters '(("custom-filters" (omit))))
 '(dired-guess-shell-alist-user
   '(("\\.hp\\'" "hp2pretty" "hp2ps")
     ("\\.\\(svg\\)\\|\\(png\\)\\|\\(jpg\\)\\'" "imv")
     ("\\.\\(mp4\\)\\|\\(avi\\)\\|\\(mkv\\)\\|\\(m4v\\)\\'" "mpv")
     ("\\.pdf\\'" "zathura" "pdftotext ? /dev/stdout" "evince")
     ("\\.ps\\'" "evince")
     ("\\.\\(ods\\)\\|\\(odf\\)\\'" "libreoffice")
     ("\\.\\(zip\\)\\|\\(rar\\)\\'" "file-roller")
     ("\\.jar\\'" "java -jar")))
 '(dired-isearch-filenames 'dwim)
 '(dired-listing-switches "-al --block-size=M --group-directories-first")
 '(diredp-hide-details-initially-flag nil)
 '(ediff-merge-split-window-function 'split-window-horizontally)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(gptel-default-mode 'org-mode)
 '(gptel-directives
   '((default . "Use your memory about me when replying, and update it with my confirmation if I share something important.")
     (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
     (writing . "You are a large language model and a writing assistant. Respond concisely.")
     (chat . "You are a large language model and a conversation partner. Respond concisely.")
     (commit . "Based on the code changes, give me a short but well written git commit message describing the change.  Adhere to common commit etiquette and formatting guidelines.  Prefer a bullet list as the body of the message.")))
 '(helm-for-files-preferred-list
   '(helm-source-buffers-list helm-source-fasd helm-source-recentf helm-source-bookmarks helm-source-file-cache helm-source-files-in-current-dir))
 '(helm-split-window-default-side 'right)
 '(initial-major-mode 'org-mode)
 '(magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)
 '(magit-diff-options '("--minimal" "--patience"))
 '(magit-diff-refine-hunk 'all)
 '(magit-log-arguments '("--graph" "--color" "--decorate" "-n100"))
 '(magit-log-auto-more t)
 '(magit-merge-arguments '("--no-ff"))
 '(magit-mode-hook '(magit-load-config-extensions))
 '(magit-process-log-max 50)
 '(magit-process-popup-time -1)
 '(magit-rebase-arguments '("--autostash"))
 '(magit-remote-ref-format 'remote-slash-branch)
 '(magit-repo-dirs '("~/repos"))
 '(magit-repository-directories '("~/repos"))
 '(magit-restore-window-configuration t)
 '(magit-revert-buffers 'silent t)
 '(magit-server-window-for-rebase 'pop-to-buffer)
 '(magit-set-upstream-on-push t)
 '(magit-tag-arguments '("--annotate"))
 '(magit-use-overlays t)
 '(org-babel-load-languages '((emacs-lisp . t) (python . t)))
 '(package-selected-packages
   '(iy-go-to-char iy-goto-char visual-regexp vertico-buffer consult-extra-project counsel-jq yaml-mode which-key vertico verb use-package undo-tree typescript-mode treemacs-projectile transpose-frame terraform-mode systemd string-inflection strace-mode solarized-theme smartparens sbt-mode rust-mode rg restclient rainbow-mode protobuf-mode plantuml-mode persistent-scratch pdf-tools ox-jira ormolu org-drill orderless nix-mode mvn move-text markdown-preview-mode marginalia magit lsp-ui lsp-metals lsp-haskell log4j-mode liso-theme just-mode jsonnet-mode json-mode js2-refactor jq-mode itail indent-guide iedit ibuffer-vc ibuffer-projectile hledger-mode hl-anything groovy-mode gptel go-complete go-autocomplete git-link fullframe format-all flycheck-yamllint flycheck-haskell find-temp-file fasd expand-region evil-numbers eros embark-consult editorconfig dyalog-mode dumb-jump doom-themes dogears dockerfile-mode docker direnv dired-filter diff-hl dhall-mode deadgrep csv-mode company beacon annotate))
 '(whitespace-action '(auto-cleanup)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#242424" :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "adobe" :family "Sauce Code Pro Nerd Font "))))
 '(Man-overstrike ((t (:inherit bold :foreground "#ddaa6f"))))
 '(Man-underline ((t (:foreground "medium spring green" :underline "medium spring green"))))
 '(ac-selection-face ((t (:background "dark orange" :foreground "gray20"))))
 '(ace-jump-face-foreground ((t (:foreground "dark orange" :underline nil))))
 '(agda2-highlight-datatype-face ((t (:foreground "deep sky blue"))))
 '(agda2-highlight-function-face ((t (:foreground "deep sky blue"))))
 '(annotate-highlight-secondary ((t (:underline "dim gray"))))
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
 '(company-scrollbar-bg ((t (:inherit company-tooltip :background "dark gray"))) t)
 '(company-scrollbar-fg ((t (:background "dark orange"))) t)
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
 '(diredp-omit-file-name ((t (:inherit diredp-ignored-file-name))))
 '(diredp-rare-priv ((t (:background "red" :foreground "black"))))
 '(diredp-read-priv ((t (:foreground "tomato"))))
 '(diredp-symlink ((t (:foreground "pale green"))))
 '(diredp-write-priv ((t (:foreground "spring green"))))
 '(ediff-current-diff-C ((t (:background "RoyalBlue4"))))
 '(ediff-fine-diff-A ((t (:background "#aa2222" :foreground "black"))))
 '(ediff-fine-diff-B ((t (:background "#22aa22" :foreground "black"))))
 '(ediff-fine-diff-C ((t (:background "RoyalBlue2" :foreground "black"))))
 '(embark-keybinding ((t (:foreground "dark orange" :inherit success))))
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


;; make unpure packages archives unavailable
(setq package-archives nil)

(package-initialize)

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

(use-package flyspell
  :ensure t
  ;; :hook
  ;; (text-mode . flyspell-mode)
  ;; (prog-mode . flyspell-prog-mode)
  )

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
  (setq log-edit-maximum-comment-ring-size 9999)
  (defun mh/magit-log-edit-mode-hook ()
    ;; (flyspell-mode)
    (set-fill-column 72))
  (setq magit-display-buffer-function
        #'magit-display-buffer-fullframe-status-v1)
  (setq magit-bury-buffer-function
        #'magit-restore-window-configuration)
  :hook
  ((magit-log-edit-mode . mh/magit-log-edit-mode-hook)
   (magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
   (magit-post-refresh-hook . diff-hl-magit-post-refresh)))

;; (use-package projectile
;;   :ensure t
;;   :diminish projectile-mode
;;   :commands projectile-global-mode
;;   :defer 5
;;   :bind (("C-s-p" . projectile-find-file))

;;   :config
;;   (use-package helm-projectile
;;     :demand t
;;     :bind (("C-s-p" . projectile-find-file)
;;            ("s-h" . helm-projectile-grep))
;;     :config
;;     (setq projectile-completion-system 'helm)
;;     (helm-projectile-on))
;;   (projectile-global-mode))

(use-package s
  :ensure t)

(use-package f
  :ensure t)

(use-package solarized-theme
  :ensure t
  :init
  (setq solarized-distinct-fringe-background t)
  (setq solarized-use-variable-pitch nil)
  (setq solarized-high-contrast-mode-line t)
  :config
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
  (indent-guide-global-mode 0))

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

(use-package git-timemachine
  :ensure t
  :bind (("C-x v t" . git-timemachine)))

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
    (let ((name (or buffer-file-name default-directory)))
      (message "Copied: %s" name)
      (kill-new name)))

  (defun mh/insert-random-uuid ()
    (interactive)
    (insert (s-trim (shell-command-to-string "uuidgen"))))

  (global-set-key (kbd "M-*") 'pop-tag-mark)
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

  (global-set-key (kbd "C-<f5>") 'compile)
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

  (global-set-key (kbd "C-c j") (Λ (mh/open-in-intellij)))

  (defun mh/open-in-intellij ()
    (interactive)
    (start-process-shell-command "jump-to-intellij"
                                 nil
                                 (s-join " " (list "idea-community"
                                                   "--line"
                                                   (format "%s" (line-number-at-pos))
                                                   buffer-file-name)))
    (message "Opened in IntelliJ IDEA."))

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
  (add-hook 'focus-out-hook (lambda () (interactive) (save-some-buffers t)))

  (defun mh/restore-marker (marker)
    (switch-to-buffer (marker-buffer marker))
    (goto-char (marker-position marker)))


  (defun mh/copy-marker-to-register ()
    (interactive)
    (set-register :mh/copy-and-paste-marker
                  (point-marker))
    (message "Copied buffer."))

  (defun mh/restore-marker-from-register ()
    (interactive)
    (mh/restore-marker
     (get-register :mh/copy-and-paste-marker)))

  (defun mh/move-window-to-other-and-winner-undo ()
    (interactive)
    (mh/copy-marker-to-register)
    (winner-undo)
    (other-window 1)
    (mh/restore-marker-from-register))

  (defun mh/cut-window-to-register()
    (interactive)
    (mh/copy-marker-to-register)
    (delete-window))

  (global-set-key (kbd "s-©") 'mh/copy-marker-to-register)
  (global-set-key (kbd "s-®") 'mh/restore-marker-from-register)
  (global-set-key (kbd "s-ó") 'mh/move-window-to-other-and-winner-undo)
  (global-set-key (kbd "s-œ") 'mh/cut-window-to-register)

  (setq gc-cons-threshold 100000000)

  (setq erc-autojoin-channels-alist
        '(("irchighway.net" "#ebooks")))

  (global-subword-mode)
  )

(use-package quick-yes
  :bind (
         :map query-replace-map
         ("M-y" . act)
         ("M-n" . skip)))

(use-package expand-region
  :bind (("C-M-w" . er/expand-region)
         ("C-!" . er/contract-region)))

(use-package scala-ts-mode
  :ensure t)

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
  :hook ((haskell-mode . interactive-haskell-mode)))

(use-package ox-jira
  :ensure t)

(use-package ox-clip
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


  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (toggle-read-only))

  :hook
  (compilation-filter . colorize-compilation-buffer))

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
  :bind (("C-s-;" . iedit-mode)
         :map iedit-mode-keymap
         ("M-i" . iedit-restrict-function)))

(use-package evil-numbers
  :ensure t
  :bind
  (("C-s-+" . evil-numbers/inc-at-pt)
   ("C-s-=" . evil-numbers/inc-at-pt)
   ("C-s--" . evil-numbers/dec-at-pt)))

(use-package evil
  :ensure t)

(use-package yaml-mode
  :ensure t
  :demand t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode)))

(use-package flycheck-yamllint
  :ensure t)

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
  :bind
  ;; (("M-g" . hydra-goto-error/body))
  )

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

(use-package pdf-tools
  :init
  ;; (setq pdf-info-epdfinfo-program "/tmp/epdfinfo")
  :config
  (pdf-tools-install))

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

;; (use-package goto-chg
;;   :ensure t
;;   :demand t
;;   :config
;;   (defhydra hydra-goto-changes ()
;;                   "hydra-goto-changes"
;;                   ("SPC" goto-last-change "goto-last-change")
;;                   ("C-SPC" goto-last-change "goto-last-change")
;;                   ("C-x C-SPC" goto-last-change "goto-last-change")
;;                   ("DEL" goto-last-change-reverse "goto-last-change-reverse")
;;                   ("q" nil "cancel"))
;;   :bind (("C-x C-SPC" . hydra-goto-changes/body)))

(use-package beacon
  :demand t
  :ensure t
  :config
  (beacon-mode 1))

;; (use-package pabbrev
;;   :ensure t
;;   :config
;;   (global-pabbrev-mode)
;;   (setq pabbrev-idle-timer-verbose nil))

(use-package groovy-mode
  :ensure t)

(use-package plantuml-mode
  :ensure t
  :config
  (setq plantuml-jar-path "@plantuml@/lib/plantuml.jar"))

;; (use-package mu4e
;;   :config
;;   (require 'mu4e-utils)
;;   (setq
;;       mu4e-sent-folder "/[Google Mail].All Mail"
;;       mu4e-drafts-folder "/[Google Mail].Drafts"
;;       mu4e-trash-folder "/[Google Mail].Trash"
;;       mu4e-refile-folder "/[Google Mail].All Mail"
;;      mu4e-maildir "~/mail"
;;       mu4e-html2text-command "@pandoc@/bin/pandoc -f html -t org"
;;       mu4e-view-auto-mark-as-read nil
;;      )
;;   (add-to-list 'mu4e-bookmarks
;;        (make-mu4e-bookmark
;;          :name  "Inbox"
;;          :query "NOT flag:thrashed AND maildir:/INBOX"
;;          :key ?b))
;;   (setq mu4e-bookmarks `(("\\\\Inbox" "Inbox" ?i)
;;                          ("flag:flagged" "Flagged messages" ?f)
;;                          (,(concat "flag:unread AND "
;;                                    "NOT flag:trashed AND "
;;                                    "NOT maildir:/[Google Mail].Spam AND "
;;                                    "NOT maildir:/[Google Mail].Bin")
;;                           "Unread messages" ?u)))
;;  )

(use-package terraform-mode
  :ensure t)

(use-package dired-filter
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package string-inflection
  :ensure t
  :commands ()
  :bind (("C-c C-u" . string-inflection-all-cycle)))

(use-package lua-mode
  :ensure t)

(use-package ibuffer
  :ensure t
  :bind (("C-x C-b" . ibuffer)))

(use-package ibuffer-vc
  :ensure t)

;; (use-package ibuffer-projectile
;;   :ensure t)

(use-package json-mode
  :bind (("C-c j" . (lambda () (interactive) (jsons-print-path-jq))))
  :ensure t)

(use-package deadgrep
  :ensure t
  :bind (:map dired-mode-map
              ("C-c r" . deadgrep)))

(use-package go-mode
  :ensure t)

(use-package go-complete
  :ensure t
  :config
  (add-hook 'completion-at-point-functions 'go-complete-at-point))

(use-package markdown-mode
  :ensure t)

(use-package markdown-preview-mode
  :ensure t)

(use-package csv-mode
  :ensure t)

;; (use-package dyalog-mode
;;   :ensure t)

(use-package direnv
  :ensure t
  :config
  (direnv-mode))

(use-package protobuf-mode
  :ensure t)

(use-package rainbow-mode
  :ensure t)

(use-package jsonnet-mode
  :ensure t)

(use-package typescript-mode
  :ensure t)

(use-package log4j-mode
  :ensure t)

(use-package docker
  :ensure t)

;; https://writequit.org/articles/working-with-logs-in-emacs.html#dealing-with-large-files
(use-package hl-anything
  :ensure t
  :diminish hl-highlight-mode
  :commands hl-highlight-mode)

(use-package itail
  :ensure t)

;; (use-package auctex
;;   :ensure t)

;; (use-package kubel
;; :ensure t)

(use-package ormolu
  :ensure t)

(use-package flycheck-haskell
  :ensure t
  :hook (haskell-mode . flycheck-haskell-setup))

(use-package strace-mode
  :ensure t)

(use-package format-all
  :ensure t
  :bind
  ("C-c C-r" . format-all-buffer)
  :config
  (define-format-all-formatter jq
    (:executable "jq")
    (:install)
    (:languages "JSON")
    (:features)
    (:format (format-all--buffer-easy executable ".")))
  (setq-default format-all-formatters
                '(("Nix" (nixfmt))
                  ("JSON" (jq)))))

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "s-\\")

(use-package lsp-mode
  :ensure t
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ;; (XXX-mode . lsp)
         ;; if you want which-key integration
         (scala-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-lens-mode))
  :commands
  lsp
  :config
  (setq lsp-prefer-flymake nil))

(use-package lsp-haskell
  :ensure t)

(use-package lsp-metals
  :ensure t
  :config
  (setq lsp-metals-treeview-show-when-views-received t))

(use-package lsp-treemacs
  :ensure t)

;; optional if you want which-key integration
(use-package which-key
  :config
  (which-key-mode))
(use-package lsp-ui :commands lsp-ui-mode)
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package js2-mode
  :ensure t)

(use-package js2-refactor
  :ensure t)

(use-package annotate
  :ensure t)

(use-package org-drill
  :ensure t)

(use-package verb
  :ensure t)

(use-package org
  :ensure t
  :init
  (eval-after-load "org"
    '(require 'ox-md nil t))
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(use-package transpose-frame
  :ensure t)

(use-package eros
  :ensure t
  :hook
  (prog-mode . eros-mode))

(use-package systemd
  :ensure t)

;; (use-package helm-rg
;;   :ensure t)

(use-package hledger-mode
  :ensure t
  :mode ("\\.journal\\'" "\\.hledger\\'")
  :commands hledger-enable-reporting
  :preface
  (defun hledger/next-entry ()
    "Move to next entry and pulse."
    (interactive)
    (hledger-next-or-new-entry)
    (hledger-pulse-momentary-current-entry))

  (defface hledger-warning-face
    '((((background dark))
       :background "Red" :foreground "White")
      (((background light))
       :background "Red" :foreground "White")
      (t :inverse-video t))
    "Face for warning"
    :group 'hledger)

  (defun hledger/prev-entry ()
    "Move to last entry and pulse."
    (interactive)
    (hledger-backward-entry)
    (hledger-pulse-momentary-current-entry))

  :bind (("C-c j" . hledger-run-command)
         :map hledger-mode-map
         ("C-c e" . hledger-jentry)
         ("M-p" . hledger/prev-entry)
         ("M-n" . hledger/next-entry))
  :init

  :config
  (add-hook 'hledger-view-mode-hook #'hl-line-mode)
  (add-hook 'hledger-view-mode-hook #'center-text-for-reading)

  (add-hook 'hledger-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (add-to-list 'company-backends 'hledger-company))))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-org-config))

(use-package treemacs
  :ensure t)

;; (use-package treemacs-projectile
;;   :ensure t)

(use-package just-mode
  :ensure t)

(use-package dhall-mode
  :ensure t
  :mode "\\.dhall\\'")

(use-package rg
  :ensure t
  :init
  (rg-enable-menu)
  )

(use-package rust-mode
  :ensure t)

(use-package copilot
  :config
  ;; keep it disabled by default
  ;; (add-hook 'prog-mode-hook 'copilot-mode)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (setq copilot-enable-predicates '(copilot--buffer-changed)))

(use-package gptel
  :ensure t
  :hook
  (gptel-post-stream . gptel-auto-scroll)
  :bind (
         ("C-c C-<return>" . gptel-send)
         ("C-c g g" . gptel)
         ("C-c g o" . 'mh/gptel-ocr-screenshot))
  :init
  (require 'gptel-integrations)
  :config

  (defun mh/gptel-ocr-screenshot ()
    (interactive)
    (let ((buf-name "*gptel-ocr-screenshot*")
          (gptel-backend (gptel-get-backend "OpenRouter"))
          (gptel-model 'openai/gpt-4.1-nano)
          (gptel-context--alist nil)
          (gptel-track-media t)
          (gptel-post-request-hook nil)
          (path (format
                 "~/Screenshots/%s"
                 (car
                  (-filter (lambda (path)
                             (s-match "^[[:digit:]][[:digit:]][[:digit:]][[:digit:]]-" path))
                           (reverse
                            (directory-files
                             (expand-file-name "~/Screenshots")
                             nil
                             "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))))))
          )
      (add-hook 'gptel-post-request-hook (lambda () (switch-to-buffer (get-buffer-create buf-name))))
      (with-current-buffer (get-buffer-create buf-name) (erase-buffer))
      (and
       (gptel-context--add-binary-file path)
       (gptel-request
           "Please extract all text from the attached screenshot in this one-time
request. Preserve any table structures and format the output for easy
copy-pasting. Highlight or clearly indicate important information such
as messages, UUIDs, code snippets, or other relevant identifiers. The
extraction should be suitable for use with programming tools and
documentation, capturing details from sites like JIRA, Miro, Kibana,
etc. This is a single, standalone request, no follow-up needed."
         :buffer   (get-buffer-create "*gptel-ocr-screenshot*")
         :position (point-max)
         ;; :callback (lambda (response _) (message response))
         ))))

  (setq gptel-use-tools nil)

  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "* user\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "* assistant\n")

  (gptel-make-perplexity "Perplexity"
    :key 'mh/secrets/gptel/perplexityApiKey
    :stream t)

  (gptel-make-gemini "Gemini" :key 'mh/secrets/gptel/geminiApiKey :stream t)

  (setq gptel-api-key 'mh/secrets/gptel/openAiApiKey)

  (gptel-make-anthropic "Claude"
    :stream t
    :key 'mh/secrets/gptel/anthropicApiKey)

  (gptel-make-anthropic "Claude-thinking" ;; Temporarily until gptel has support
    :key 'mh/secrets/gptel/anthropicApiKey
    :stream t
    :models '(claude-3-7-sonnet-20250219)
    :header (lambda () (when-let* ((key (gptel--get-api-key)))
                         `(("x-api-key" . ,key)
                           ("anthropic-version" . "2023-06-01")
                           ("anthropic-beta" . "pdfs-2024-09-25")
                           ("anthropic-beta" . "output-128k-2025-02-19")
                           ("anthropic-beta" . "prompt-caching-2024-07-31"))))
    :request-params '(:thinking (:type "enabled" :budget_tokens 2048)
                                :max_tokens 4096))

  (gptel-make-openai "DeepSeek"
    :host "api.deepseek.com"
    :endpoint "/chat/completions"
    :stream t
    :key 'mh/secrets/gptel/deepSeekApiKey
    :models
    '(deepseek-chat deepseek-coder))

  (gptel-make-openai "xAI"
    :host "api.x.ai"
    :key 'mh/secrets/gptel/xaiApiKey
    :endpoint "/v1/chat/completions"
    :stream t
    :models '(grok-2-latest
              (grok-2-vision-latest :capabilities (media)
                                    :mime-types ("image/jpg" "image/jpeg" "image/png"))))

  (gptel-make-openai "OpenRouter"
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key 'mh/secrets/gptel/openRouterApiKey
    :models '((openai/gpt-4.1 :capabilities (media tool) :mime-types ("image/jpg" "image/jpeg" "image/png"))
              (openai/gpt-4.1-mini  :capabilities (media tool) :mime-types ("image/jpg" "image/jpeg" "image/png"))
              (openai/gpt-4.1-nano  :capabilities (media tool) :mime-types ("image/jpg" "image/jpeg" "image/png"))
              ;;
              openai/gpt-4o
              openai/gpt-4o-mini
              ;;
              (anthropic/claude-opus-4 :capabilities (media tool) :mime-types ("image/jpg" "image/jpeg" "image/png" "application/pdf"))
              (anthropic/claude-sonnet-4 :capabilities (media tool) :mime-types ("image/jpg" "image/jpeg" "image/png" "application/pdf"))
              anthropic/claude-3.7-sonnet
              anthropic/claude-3.7-sonnet:thinking
              ;;
              mistralai/codestral-2501
              mistralai/codestral-2501
              ;;
              deepseek/deepseek-r1:free
              deepseek/deepseek-chat-v3-0324
              deepseek/deepseek-chat-v3-0324:free
              ;;
              meta-llama/llama-3.3-70b-instruct
              ;;
              google/gemini-2.5-pro-preview
              google/gemini-2.5-flash-preview))


  (add-to-list 'gptel-directives '(expertMatrix . "Generate solutions from the perspectives of 5 different experts in the relevant field (e.g. doctor, psychologist, entrepreneur, engineer, etc.)."))
  (add-to-list 'gptel-directives '(userScript . "Write a greasemonkey userscript with debug logging enabled that can be disabled via a boolean.  For the debug logging, always use a consistent prefix to identify the script. If it makes sense, use a mutation observer on the dom. Start with important design points to consider and explain your plan before the implementation.  Write clean code and add important comments. Fill the metadata section of the userscript."))
  (add-to-list 'gptel-directives '(pareto . "You are a university professor at a top university. You have become an expert in the Pareto principle (80/20 rule). Please identify the 20% of the subject I'm asking about that will yield 80% of the best results. Use your academic resources to provide a well identified and focused learning program to master this subject."))
  (add-to-list 'gptel-directives '(questions . "To start, ask me up to 5 questions to improve your understanding of what I'm trying to do here"))
  (add-to-list 'gptel-directives '(brainstorm . "Ask me one question at a time so we can develop a thorough, step-by-step spec for this idea. Each question should build on my previous answers, and our end goal is to have a detailed specification. Let’s do this iteratively and dig into every relevant detail. Remember, only one question at a time."))
  (add-to-list 'gptel-directives '(followup . "Finally, provide a numbered list of 3-5 actionable next steps I could take related to this response. These next steps should be diverse and may include, but are not limited to: further research questions, concrete actions, alternative perspectives to consider, potential challenges to anticipate, or resources to consult for further information.  Be specific and concise in each suggestion."))
  (add-to-list 'gptel-directives '(memory . "Use your memory about me when replying, and update it with my confirmation if I share something important."))

  (defun mh/add-gptel-tool (tool)
    (add-to-list 'gptel-tools tool t (lambda (tool1 tool2) (string= (aref tool1 2) (aref tool2 2))))
    gptel-tools)

  (gptel-make-tool
   :name "read_buffer"
   :description "Return the contents of an Emacs buffer"
   :args (list '(:name "buffer"
                       :type string
                       :schema
                       :description "The name of the buffer whose contents are to be retrieved"))
   :category "emacs"
   :function (lambda (buffer)
               (unless (buffer-live-p (get-buffer buffer))
                 (error "Error: buffer %s is not live" buffer))
               (with-current-buffer  buffer
                 (buffer-substring-no-properties (point-min) (point-max)))))

  (gptel-make-tool
   :name "list_directory_recursively"
   :description "List the contents of a given directory recursively and return files."
   :args (list '(:name "directory"
                       :type string
                       :description "The path to the directory to list files in (recursively)")
               '(:name "regexp"
                       :type string
                       :description "A valid emacs regular expression to match file names with"))
   :category "filesystem"
   :function (lambda (directory regexp)
               (encode-coding-string (mapconcat #'identity
                                                (directory-files-recursively directory regexp nil t)
                                                "\n") 'utf-8)))
  (gptel-make-tool
   :name "read_file"
   :description "Read and display the contents of a file"
   :args (list '(:name "filepath"
                       :type string
                       :description "Path to the file to read.  Supports relative paths and ~."))
   :category "filesystem"
   :function (lambda (filepath)
               (with-temp-buffer
                 (insert-file-contents (expand-file-name filepath))
                 (buffer-string))))
  (gptel-make-tool
   :name "rename_file"
   :description "Rename a file from OLD-PATH to NEW-PATH."
   :args (list '(:name "old-path"
                       :type string
                       :description "The current path of the file to rename.")
               '(:name "new-path"
                       :type string
                       :description "The new path of the file after renaming."))
   :category "filesystem"
   :confirm t
   :function (lambda (old-path new-path)
               (if (file-exists-p old-path)
                   (progn
                     (f-move old-path new-path)
                     (format "Successfully renamed file from %s to %s" old-path new-path))
                 (format "Error: %s does not exist." old-path))))

  (gptel-make-tool
   :name "write_file"
   :description "Write CONTENT to a file at FPATH"
   :args (list '(:name "fpath"
                       :type string
                       :description "The path of the file to write to.")
               '(:name "content"
                       :type string
                       :description "Content of the file."))
   :category "filesystem"
   :confirm t
   :function (lambda (fpath content)
               (if (file-exists-p fpath)
                   (progn
                     (f-write content 'utf-8 fpath)
                     (format "Successfully wrote to file %s" fpath))
                 (format "Error: %s does not exist." fpath))))

  (gptel-make-tool
   :function (lambda (old-path new-path)
               (if (file-exists-p old-path)
                   (progn
                     (f-move old-path new-path)
                     (format "Renamed file from %s to %s" old-path new-path))
                 (format "Error: %s does not exist." old-path)))
   :name "rename_file"
   :confirm t
   :description "Rename a file from OLD-PATH to NEW-PATH."
   :args (list '(:name "old-path"
                       :type string
                       :description "The current path of the file to rename.")
               '(:name "new-path"
                       :type string
                       :description "The new path of the file after renaming."))
   :category "filesystem")

  (defvar mh/llm-memory-file (expand-file-name "~/Syncthing/Inbox/llm-memory.txt")
    "The file used to store LLM memory.")

  (gptel-make-tool
   :name "read_memory"
   :description "Retrieves and returns the entire current content of your persistent memory. Use this to recall information saved previously, especially at the beginning of a conversation or when context from past interactions is needed."
   :args nil
   :confirm nil
   :category "memory"
   :function (lambda ()
               (if (file-exists-p mh/llm-memory-file)
                   (with-temp-buffer
                     (insert-file-contents mh/llm-memory-file)
                     (buffer-string))
                 (error "Memory storage inaccessible.  Cannot read"))))

  (gptel-make-tool
   :name "write_memory"
   :description "Appends the provided text as a new entry to your persistent memory. Ensures separation from previous entries. Use this to save specific facts, user preferences, summaries, or instructions for future reference. Consult the user before doing this unless explicitly asked to do it."
   :args (list '(:name "memory"
                       :type string
                         :description "Content to append to memory. Will be added as a distinct entry."
                       :minLength 1))
   :confirm t
   :category "memory"
   :function (lambda (content)
               (if content
                   (if (file-exists-p mh/llm-memory-file)
                       (progn

                         (with-temp-buffer
                           (insert-file-contents mh/llm-memory-file)
                           (unless (or (zerop (point-max))
                                       (eq (char-before (point-max)) ?\n))
                             (goto-char (point-max))
                             (insert "\n")))
                         (append-to-file content nil mh/llm-memory-file)
                         "Success: Information appended to memory.")
                     (error "Memory storage inaccessible.  Cannot write"))
                 "Content argumentis required")))

  (gptel-make-tool
   :name "replace_memory"
     :description "Completely replaces the *entire* content of your persistent memory  with the provided text. WARNING: All previously stored information will be permanentlyerased. Use this to reset memory, load a specific state, or start freshwith a clean slate."
   :args (list '(:name "memory"
                       :type string
                       :description "New content to completely overwrite the memory  with."
                       :minLength 0)) ; Allow empty memory
   :confirm t
   :category "memory"
   :function (lambda (content)
               (if content
                   (progn
                     (write-region content nil mh/llm-memory-file nil 0) ; Use write-region for overwriting
                     "Success: Memory replaced.")
                 "Error:Content argument is required")))
  (gptel-make-tool
   :name "get_current_datetime"
   :description "Returns the current date and time in ISO 8601 format."
   :args nil
   :confirm nil
   :category "utility"
   :function (lambda ()
               (format-time-string "%Y-%m-%dT%H:%M:%S%z")))

  (gptel-make-tool
   :name "move_item"
   :description "Move a file or directory from OLD-PATH to NEW-PATH."
   :args (list '(:name "old_path"
                       :type string
                       :description "The current path of the file or directory to move.")
               '(:name "new_path"
                       :type string
                       :description "The new path for the file or directory."))
   :category "filesystem"
   :confirm t
   :function (lambda (old_path new_path)
               (let ((expanded_old_path (expand-file-name old_path))
                     (expanded_new_path (expand-file-name new_path)))
                 (if (not (file-exists-p expanded_old_path))
                     (format "Error: Source path %s does not exist." expanded_old_path)
                   (if (file-exists-p expanded_new_path)
                       (format "Error: Target path %s already exists. Please use a different target path or delete it first." expanded_new_path)
                     (progn
                       (f-move expanded_old_path expanded_new_path) ; f-move is from f.el
                       (format "Successfully moved %s to %s" expanded_old_path expanded_new_path)))))))

  (gptel-make-tool
   :name "execute_bash_command"
   :description (concat "Executes an arbitrary bash command and returns its exit status, standard output, and standard error. "
                        "CRITICAL SAFETY PROTOCOL: Before you decide to use this tool, you MUST explicitly ask the user for their permission to execute *any* bash command for the current task, explain why it's necessary, and what kind of command you are considering. "
                        "Only proceed to formulate and call this tool if they explicitly agree. "
                        "When calling the tool, state the exact command you intend to run. "
                        "Use with EXTREME CAUTION as this can modify your system or expose sensitive data."
                        "When using `rm' or `mv' always add the `-v' flag.")
   :args (list '(:name "command"
                       :type string
                       :description "The bash command to execute. This should only be formulated after user pre-approval."))
   :category "system"
   :confirm t
   :function (lambda (command_string)
               (let ((output_buffer_name "*gptel-bash-output*")
                     (output_buffer (get-buffer-create "*gptel-bash-output*"))
                     (exit-status nil)
                     result)
                 (unwind-protect
                     (progn
                       (with-current-buffer output_buffer (erase-buffer))
                       (setq exit-status (call-process-shell-command
                                          (format "(%s) 2>&1" command_string)
                                          nil
                                          output_buffer
                                          nil))
                       (with-current-buffer output_buffer
                         (setq result (format "Exit Status: %s\nOutput (stdout & stderr):\n%s"
                                              exit-status
                                              (s-trim (buffer-string))))))
                   (when (buffer-live-p output_buffer)
                     (kill-buffer output_buffer)))
                 result)))

  ;; Tools end
  )

(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode)
  :config
  (set-face-attribute 'diff-hl-change nil
                      :background "orange1"
                      :foreground "orange")
  (set-face-attribute 'diff-hl-insert nil
                      :background "SpringGreen3"
                      :foreground "medium spring green")
  (set-face-attribute 'diff-hl-delete nil
                      :background "red3"
                      :foreground "red1"))

(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package savehist
  :ensure t
  :init
  (savehist-mode)
  :config
  (add-to-list 'savehist-additional-variables 'log-edit-comment-ring))

(use-package project
  :ensure t)

(use-package marginalia
  :ensure t
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light))
  :init
  (marginalia-mode))

(use-package consult-project-extra
  :ensure t
  :after consult
  :bind
  ("C-s-p" . consult-project-extra-find))

(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; CUSTOM
         ("s-;" . consult-buffer)
         ("s-f" . find-file)
         ("s-RET" . consult-man)
         ("s-F" . mh/consult-fasd)
         ("s-'" . execute-extended-command)
         ;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  (defun mh/consult-fasd ()
    (interactive)
    (find-file (consult--read
                (consult--async-command (lambda (input) (list "fasd" "-Rl" (string-trim input))))
                :prompt "fasd: "
                :category 'file)))

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )

(defun mh/embark-kagi-search (term)
  (interactive "sSearch Term: ")
  (browse-url-xdg-open
   (format "http://kagi.com/search?q=%s" term)))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-s-." . embark-dwim)      ;; good alternative: M-.
   ("C-h B" . embark-bindings)  ;; alternative for `describe-bindings'
   :map embark-general-map
   ("k" . mh/embark-kagi-search)
   ("g" . gptel-context-add)
   :map embark-buffer-map
   ("g" . gptel-context-add)
   :map embark-file-map
   ("g" . gptel-context-add-file)
   :map embark-region-map
   ("g" . gptel-context-add))

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package recentf
  :ensure t
  :init
  (recentf-mode 1)
  (setq recentf-max-menu-items 20)
  (setq recentf-max-saved-items nil)
  (run-at-time nil (* 5 60) 'recentf-save-list))

(use-package dogears
  :ensure t
  :bind (:map global-map
              ("M-g M-r" . dogears-remember)
              ("M-g M-d" . dogears-go)
              ("M-g M-b" . dogears-back)
              ("M-g M-f" . dogears-forward)
              ("M-g M-D" . dogears-list)
              ;; ("M-g M-D" . dogears-sidebar)
              )
  :init (dogears-mode))

(use-package string-inflection
  :after embark
  :bind (:map embark-identifier-map
              ("-" . #'string-inflection-all-cycle))
  :init
  (add-to-list 'embark-repeat-actions #'string-inflection-all-cycle))

(use-package eat
  :ensure t
  :bind (("C-c e" . eat))
  :init
  (defun mh/display-line-numbers--turn-off ()
    (display-line-numbers-mode -1))
  :hook
  (eat-mode . mh/display-line-numbers--turn-off))

(use-package visual-regexp
  :ensure t
  :bind
  (("C-c r" . vr/replace)
   ("C-c q" . vr/query-replace)
   ("C-c m" . vr/mc-mark)))

(use-package ediff
  :ensure t
  :bind
  (("M-s =" . mh/ediff-compare-with-kill))
  :config
  (defun mh/ediff-compare-with-kill ()
    "Compare current buffer with kill ring using ediff."
    (interactive)
    (let ((cur-buffer (current-buffer))
          (temp-buffer (generate-new-buffer "*Kill Ring Content*")))
      (with-current-buffer temp-buffer
        (yank)
        (push-mark (point-min) t t))
      (with-current-buffer temp-buffer
        (setq reg-A-beg (region-beginning)
              reg-A-end (region-end))
        (set-buffer cur-buffer)
        (setq reg-B-beg (region-beginning)
              reg-B-end (region-end)))
      (ediff-regions-internal
       (get-buffer temp-buffer) reg-A-beg reg-A-end
       (get-buffer cur-buffer) reg-B-beg reg-B-end
       nil 'ediff-regions-wordwise 'word-mode nil))))

(use-package web-mode
  :ensure t
  :mode
  "\\.ftl\\'"
  "\\.ftlh\\'"
  :config
  (setq web-mode-engines-alist '(("freemarker" . "\\.ftlh\\'"))))

(use-package rainbow-delimiters
  :ensure t
  :hook (elisp-mode . rainbow-delimiters-mode))

(use-package elfeed
  :ensure t
  :bind
  (("C-x w" . elfeed)
   :map elfeed-search-mode-map
   ("j" . #'next-line)
   ("k" . #'previous-line))
  :hook
  (elfeed-new-entry-parse . mh/elfeed-extract-comments-link)

  :init
  ;; (add-hook 'elfeed-new-entry-hook
  ;;           (elfeed-make-tagger :entry-title "llm|LLM|gemini|Gemini|claude|Claude|Anthropic|anthropic|OpenAI|openai"))
  (defun mh/elfeed-extract-comments-link (_type xml entry)
    "If ENTRY is tagged with special tag, prefer comments link from XML and store it as link."
    (when (elfeed-tagged-p 'pref-comment entry)
      (when-let ((comments-link (xml-query '(comments *) xml)))
        (when (and comments-link (not (string-empty-p comments-link)))
          (elfeed-meta--put entry :original-link (elfeed-entry-link entry))
          (setf (elfeed-entry-link entry) comments-link)))))

  (setq mh/elfeed-search-stack '(llm hackernews hackernews2 hackernews3 youtube news newsletter github sport analog reading programming reddit nil))

  (defun mh/raindrop-add-url-api (url tags)
    "Add a URL to Raindrop.io API.
Returns t on success, nil on failure.
Provides more detailed messages on failure."
    (interactive)
    (let* ((bearer-token (format "Bearer %s" (mh/secrets/raindrop/testToken)))
           (request-data
            `(("link" . ,url)
              ("pleaseParse" . #s(hash-table))
              ("tags" . ,tags)))
           (url-request-method "POST")
           (url-request-extra-headers `(("Content-Type" . "application/json")
                                        ("X-Accept" . "application/json")
                                        ("Authorization" . ,bearer-token)))
           (url-request-data (json-encode request-data))
           (response-buffer nil)
           (success nil))
      (unwind-protect
          (progn
            (setq response-buffer
                  (url-retrieve-synchronously
                   "https://api.raindrop.io/rest/v1/raindrop" t t 1))
            (if response-buffer
                (with-current-buffer response-buffer
                  (goto-char (point-min))
                  (if (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\) \\(.*\\)$" nil t)
                      (let ((status-code (string-to-number (match-string 1)))
                            (status-message (match-string 2)))
                        (if (= status-code 200)
                            (progn
                              (re-search-forward "\n\n" nil t)
                              (let ((json-response-string (buffer-substring-no-properties (point) (point-max))))
                                (condition-case err
                                    (let ((json-data (json-read-from-string json-response-string)))
                                      (if (and (eq (cdr (assoc 'result json-data)) t)
                                               (assoc '_id (cdr (assoc 'item json-data))))
                                          (progn
                                            (message "Raindrop: URL added successfully (%s)." url)
                                            (setq success t))
                                        (message "Raindrop: API reported success but response format unexpected for %s. Body: %s" url json-response-string)))
                                  (error
                                   (message "Raindrop: Failed to parse JSON response for %s. Error: %s. Body: %s" url err json-response-string)))))
                          (message "Raindrop: API request failed for %s. Status: %d %s. Body: %s"
                                   url status-code status-message (buffer-string))))
                    (message "Raindrop: Could not parse HTTP status from response for %s. Buffer: %s" url (buffer-string))))
              (message "Raindrop: Failed to retrieve response from API for %s." url)))
        (when response-buffer
          (kill-buffer response-buffer)))
      success))

  (defun mh/elfeed-raindrop-add-url ()
    "Add the elfeed selection to raindrop."
    (interactive)
    (let ((buffer (current-buffer))
          (entries (elfeed-search-selected)))
      (cl-loop for entry in entries
               when (elfeed-entry-link entry)
               do (let
                      ((tags (delete 'unread (elfeed-entry-tags entry))))
                    (when (mh/raindrop-add-url-api it tags)
                      (elfeed-untag entry 'unread)
                      (elfeed-tag entry 'mh/pocketed))))
      (with-current-buffer buffer
        (mapc #'elfeed-search-update-entry entries)
        (unless (or elfeed-search-remain-on-entry (use-region-p))
          (forward-line)))))
  :config
  (defun mh/elfeed-search-stack-next ()
    (interactive)
    (letrec ((head (car (setq mh/elfeed-search-stack (-rotate -1 mh/elfeed-search-stack))))
             (next-filter (if head (format "@6-months-ago +unread +%s" head) "@6-months-ago +unread")))
      (elfeed-search-set-filter next-filter)
      (if (and head (not (consp elfeed-search-entries))) (mh/elfeed-search-stack-next))))

  (progn
    (define-key elfeed-search-mode-map (kbd "l") (lambda () (interactive) (switch-to-buffer (elfeed-log-buffer))))
    (define-key elfeed-search-mode-map (kbd ", m") 'mh/elfeed-search-stack-next)
    (define-key elfeed-search-mode-map (kbd ", ,") (lambda ()
                                                     (interactive)
                                                     (mark-whole-buffer)
                                                     (elfeed-search-untag-all-unread)
                                                     (elfeed-search-update--force)
                                                     (mh/elfeed-search-stack-next)))
    (define-key elfeed-search-mode-map (kbd ", .") 'mh/elfeed-raindrop-add-url)
    (define-key elfeed-search-mode-map (kbd "SPC") (lambda()
                                                    (interactive)
                                                    (delete-other-windows)
                                                    (split-window-right)
                                                    (other-window 1)
                                                    (elfeed-search-show-entry (elfeed-search-selected :ignore-region))
                                                    (other-window 1))))

  (defface mh/elfeed-reddit-tag-face
    '((t :foreground "#1CE"))
    "Marks reddit tags.")
  (defface mh/elfeed-youtube-tag-face
    '((t :foreground "#E40"))
    "Marks youtube tags.")
  (defface mh/elfeed-hackernews-tag-face
    '((t :foreground "#FC0"))
    "Marks hackernews tags.")
  (defface mh/elfeed-github-tag-face
    '((t :foreground "#AAA"))
    "Marks github tags.")
  (defface mh/elfeed-newsletter-tag-face
    '((t :foreground "#0E9"))
    "Marks newsletter tags.")
  (defface mh/elfeed-special-tag-face
    '((t :foreground "#A0A"))
    "Marks special tags.")

  (setq elfeed-search-face-alist '((unread elfeed-search-unread-title-face)
                                   (reddit mh/elfeed-reddit-tag-face)
                                   (youtube mh/elfeed-youtube-tag-face)
                                   (github mh/elfeed-github-tag-face)
                                   (hackernews mh/elfeed-hackernews-tag-face)
                                   (hackernews2 mh/elfeed-hackernews-tag-face)
                                   (hackernews3 mh/elfeed-hackernews-tag-face)
                                   (newsletter mh/elfeed-newsletter-tag-face)
                                   (qtl mh/elfeed-special-tag-face)))
  (setq elfeed-search-title-max-width 120)
  (setq elfeed-feeds
        (append
         (let ((serverUrl "http://localhost:9999"))
           (mapcar
            (lambda (feed-spec)
              (let* ((subreddit (plist-get feed-spec :subreddit))
                     (threshold (or (plist-get feed-spec :threshold) 60))
                     (tags (cons 'pref-comment (cons 'reddit (plist-get feed-spec :tags)))))
                (cons (format "%s/?subreddit=%s&threshold=%d&view=rss"
                              serverUrl subreddit threshold)
                      tags)))
            '((:subreddit "ExperiencedDevs")
              (:subreddit "AdvancedRunning" :tags (sport))
              (:subreddit "books" :tags (reading))
              (:subreddit "booksuggestions" :tags (reading))
              (:subreddit "bulletjournal" :tags (analog))
              (:subreddit "claudeai")
              (:subreddit "claudecode")
              (:subreddit "commandline")
              (:subreddit "compsci")
              (:subreddit "cycling" :tags (sport))
              (:subreddit "electronics")
              (:subreddit "emacs")
              (:subreddit "esp32")
              (:subreddit "eupersonalfinance" :threshold 80)
              (:subreddit "fantasy")
              (:subreddit "flexibility")
              (:subreddit "frankfurt")
              (:subreddit "Foodforthought" :threshold 90)
              (:subreddit "functionalprogramming")
              (:subreddit "garminfenix")
              (:subreddit "gridfinity" :threshold 90)
              (:subreddit "haskell")
              (:subreddit "internetisbeautiful")
              (:subreddit "ironsworn")
              (:subreddit "litrpg")
              (:subreddit "malazan")
              (:subreddit "netsec")
              (:subreddit "nixos" :tags (programming))
              (:subreddit "notebooks" :tags (analog))
              (:subreddit "NoteTaking" :tags (analog))
              (:subreddit "osr")
              (:subreddit "penandpaper")
              (:subreddit "ProgressionFantasy")
              (:subreddit "ReverseEngineering")
              (:subreddit "rpg")
              (:subreddit "rss")
              (:subreddit "scala" :threshold 100)
              (:subreddit "solo_roleplaying")
              (:subreddit "starforged")
              (:subreddit "startrek" :threshold 80)
              (:subreddit "singularity")
              (:subreddit "stocks")
              (:subreddit "swn")
              (:subreddit "theglasscannonpodcast")
              (:subreddit "tools")
              (:subreddit "trailrunning" :tags (sport))
              (:subreddit "ultramarathon" :tags (sport))
              (:subreddit "traveller")
              (:subreddit "ultrarunning" :tags (sport))
              (:subreddit "usbchardware")
              (:subreddit "usenet")
              (:subreddit "worldnews" :threshold 70)
              (:subreddit "writingprompts" :threshold 80)
              (:subreddit "wwn")
              (:subreddit "zwift")
              (:subreddit "ai_agents"))))

         (mapcar
          (lambda (feed-spec)
            (let* ((owner (plist-get feed-spec :owner))
                   (repo (plist-get feed-spec :repo))
                   (tags (cons 'github (plist-get feed-spec :tags))))
              (cons (format "https://github.com/%s/%s/releases.atom" owner repo) tags)))
          '((:owner "CMB" :repo "edbrowse")
            (:owner "pwmt" :repo "zathura")
            (:owner "mpv-player" :repo "mpv")
            (:owner "mwh" :repo "dragon")
            (:owner "jarun" :repo "ddgr")
            (:owner "solemnwarning" :repo "rehex")
            (:owner "eXeC64" :repo "imv")
            (:owner "BestImageViewer" :repo "geeqie")
            (:owner "Duncaen" :repo "OpenDoas")
            (:owner "nushell" :repo "nushell")
            (:owner "elves" :repo "elvish")
            (:owner "tmux" :repo "tmux")
            (:owner "kmonad" :repo "kmonad")
            (:owner "saulpw" :repo "visidata")
            (:owner "kopecmaciej" :repo "vi-mongo")
            (:owner "johnwarne" :repo "reddit-top-rss")
            (:owner "mbnuqw" :repo "sidebery")
            (:owner "ast-grep" :repo "ast-grep")
            (:owner "martinvonz" :repo "jj")
            (:owner "dunst-project" :repo "dunst")
            (:owner "karthink" :repo "gptel")
            (:owner "aider-ai" :repo "aider")))

         (mapcar
          (lambda (feed-spec)
            (let* ((channelId (plist-get feed-spec :channelId))
                   (tags (cons 'youtube (plist-get feed-spec :tags))))
              (cons (format "https://www.youtube.com/feeds/videos.xml?channel_id=%s" channelId) tags)))
          (seq-filter
           (lambda (feed-spec) (not (or (plist-get feed-spec :disabled) nil)))
           '((:channelId "UCl_5s2WDFi38LiXUQA7CTWQ" :title "Chris Kaula" :disabled t)
             (:channelId "UCt8REhn8USXlxhiTc9H_F3w" :title "Iron Home" :tags (rpg))
             (:channelId "UCHMfbHsJZCqRtSCVLV-RnYA" :title "Daily MTB Rider" :tags (sport))
             (:channelId "UC4hJTeF0QvnsC7bIshU_K2w" :title "Aiko Sukdolak" :disabled t)
             (:channelId "UCDIr0UgrBJ3lGfs0eeKV6Tw" :title "Steve Mattheis")
             (:channelId "UCI1aF4MNqSzKIS2t0KHS1gw" :title "Figboot on Pens" :tags (fp))
             (:channelId "UCJV7ONWjegVFOlHpAGgjGMQ" :title "Trond Westby" :tags (photography))
             (:channelId "UCKq3tXnvXnA0feJYmOx9MPw" :title "Stefano Ianiro Wildlife" :tags (photography))
             (:channelId "UCa51ED7iENUjtadDnqPuoWw" :title "SuperTragopan" :tags (photography))
             (:channelId "UCcGPU4A6xJ1OYOkvfMoo25w" :title "Simon Baxter" :tags (photography))
             (:channelId "UCimiUgDLbi6P17BdaCZpVbg" :title "exurb1a")
             (:channelId "UCzbbkYQUqeGNKSRwoyWB9IA" :title "Simon Wantling" :tags (photography))
             (:channelId "UCRHCA0iiOZ_tbOowedaeK-A" :title "Skyrunning Federation" :tags (sport))
             (:channelId "UC8szqVDJF60HueoqJrD50qw" :title "GOLDEN TRAIL SERIES" :tags (sport))
             (:channelId "UCDw36yB-ZXJ_FnqEH7o2HfQ" :title "Saul Pwanson - Visidata")
             (:channelId "UCqeVdbCP-fVjSGEVnY-3lWQ" :title "MotionTwin (Dead Cells)")
             (:channelId "UC9-y-6csu5WGm29I7JiwpnA" :title "Computerphile")
             (:channelId "UCIfRR1N2Gm1vjj9X955iWSQ" :title "NorCal Cycling" :tags (sport))
             (:channelId "UCvYwePdbWSEwUa-Pk02u3Zw" :title "Questing Beast")
             (:channelId "UCQs8-UJ7IHsrzhQ-OQOYBmg" :title "Seth Skorkowsky")
             (:channelId "UC6mIxFTvXkWQVEHPsEdflzQ" :title "Great Scott")
             (:channelId "UCtM5z2gkrGRuWd0JQMx76qA" :title "BigCliveDotCom")
             (:channelId "UCosVFjW2FecfJE3I12-fZag" :title "ProblemLoeser")
             (:channelId "UCUQo7nzH1sXVpzL92VesANw" :title "DIY Perks")
             (:channelId "UCFX1Z9N6aPWuCN_KR8UZ2vg" :title "Learn Electronics Repair")
             (:channelId "UCNKMpnM_Yvf6E-Hhf9btYqA" :title "Jeff Pelletier")
             (:channelId "UC2DjFE7Xf11URZqWBigcVOQ" :title "EEVblog")
             (:channelId "UCn4Ifss-t3wMT6VBzQoKPUA" :title "Pine Hollow Auto Diagnostics")
             (:channelId "UCVbn813ctsoChuTT4LuLqrA" :title "Jetpens")
             (:channelId "UCsZNco1cxrspWzlPX3Kim9g" :title "Basti HW")
             (:channelId "UC_zyfHGL6MWzm36l9TEWFRg" :title "Sally McRae")
             (:channelId "UCTo55-kBvyy5Y1X_DTgrTOQ" :title "MKMe Lab" :tags (electronics))
             (:channelId "UChwnFBBtasi2kn2TDK5OsWg" :title "Buy it Fix it" :tags (electronics))
             (:channelId "UChY9Cgv-iyPDvf1Bkyx20OQ" :title "My Mate Vince" :tags (electronics))
             (:channelId "UCqZQlzSHbVJrwrn5XvzrzcA" :title "NBC Sports" :tags (sport))
             (:channelId "UCSoOJTknGqXQSeamRjEE8aA" :title "TechDregs")
             (:channelId "UCNQJqvSXfDBOd9spve8doWw" :title "Worm Girl CDDA")
             (:channelId "UCt1ES-_FMXQfM3JeO_FrOXw" :title "ParkNotes" :tags (analog))
             (:channelId "UCSHZfmwfiIxCpKrQFrr7YyQ" :title "The Bad Spot")
             (:channelId "UC2rzsm1Qi6N1X-wuOg_p0Ng" :title "Project Farm")
             (:channelId "UCo6hpY_BpeDt72PEJicyVOQ" :title "Techisode TV")
             (:channelId "UCgGbvwkKfnt1V-CvdzXo7OQ" :title "How-2-repair.com")
             (:channelId "UCUwGYfvGvmqMnvQTOY8E_qg" :title "Göran Winblad - Running" :tags (sport))
             (:channelId "UCbBVRJq3H6yRvC9G6xBLJZw" :title "UTMB World Series" :tags (sport))
             (:channelId "UC33MjuRroWlm7vzWCEHKXMw" :title "obsessed mushroom pickers")
             (:channelId "UCWMsoao_uuuVkzuXDDMjFdg" :title "Adidas Terrex")
             (:channelId "UCNJ1Ymd5yFuUPtn21xtRbbw" :title "AI Explained")
             (:channelId "UCXUPKJO5MZQN11PqgIvyuvQ" :title "Andrej Karpathy")
             (:channelId "UCw7R5moYo-DNLw4BKjFX3bg" :title "Bradford Redpath Zwift Racing" :tags (sport))
             (:channelId "UCgcykADGx7tXw7m0NOoUNwA" :title "Ida-Sophie Hegemann")
             (:channelId "UCsBjURrPoezykLs9EqgamOA" :title "Fireship")
             (:channelId "UCrPpaC5uLPx03XHVCfufiTQ" :title "Tim Cannon" :tags (sport))
             (:channelId "UCNS-y3tEoPmBy9Q-ZYJf9QQ" :title "Kelp and Fern" :tags (sport))
             (:channelId "UCIX0OOS3khVxwpxuoqkLWMw" :title "Pfefferminz Film" :tags (sport))
             (:channelId "UCTjmLCEVb9r1OpFDJMMWyIw" :title "StezStix Fix?" :tags (electronics))
             (:channelId "UClsFdM0HzTdF1JYoraQ0aUw" :title "Brick Experiment Channel"))))

         (mapcar
          (lambda (feed-spec)
            (let* ((id (plist-get feed-spec :id))
                   (title (plist-get feed-spec :title))
                   (tags (cons 'newsletter (plist-get feed-spec :tags))))
              (cons (format "https://kill-the-newsletter.com/feeds/%s.xml" id) tags)))
          '((:id "bgfqc0awgchwumud" :title "Bike Components")
            (:id "o2bsigatmzdvp2t2" :title "Chaosium")
            (:id "sbpv9agei99dsegl" :title "Tor.com")
            (:id "appeyt9qbgbukkqh" :title "Money Stuff")
            (:id "0edslwsaoudstyp9" :title "5-Bullet Friday")
            (:id "tc8vjbiw33og592y" :title "Thinking About Things")
            (:id "1ema4onuqfab8l4o" :title "Leuchtturm1917")
            (:id "fxshdjutxkvhp4uq" :title "Lamy")
            (:id "xb9ujr6s9d3ed1w1" :title "Salomon DE" :tags (running))
            (:id "6zr0oawurrjiw4saxcnu" :title "LOWA Newsletter" :tags (running))
            (:id "a5c6xo3x2mts4urw57pm" :title "Dungeon Crawler Carl")))

         '(("http://localhost:9998/?action=display&bridge=CssSelectorComplexBridge&home_page=https%3A%2F%2Fwww.fnp.de%2Flokales%2Fmain-taunus%2Fhofheim-ort74520%2F&cookie=&title_cleanup=&entry_element_selector=.id-LinkOverlay&url_selector=a&url_pattern=&limit=&use_article_pages=on&article_page_content_selector=article&content_cleanup=script%2C+.id-Story-interactionBar%2C+.id-StoryElement-inArticleReco%2C+.id-DonaldBreadcrumb&title_selector=h1&category_selector=&author_selector=.id-Story-authors-link&time_selector=time&time_format=Y-m-d+H%3Ai&remove_styling=on&format=Atom" news)
           ("http://localhost:9998/?action=display&bridge=CssSelectorComplexBridge&home_page=https%3A%2F%2Fwww.fnp.de%2Flokales%2Fmain-taunus%2Fkelkheim-ort95937%2F&cookie=&title_cleanup=&entry_element_selector=.id-LinkOverlay&url_selector=a&url_pattern=&limit=&use_article_pages=on&article_page_content_selector=article&content_cleanup=script%2C+.id-Story-interactionBar%2C+.id-StoryElement-inArticleReco%2C+.id-DonaldBreadcrumb&title_selector=h1&category_selector=&author_selector=.id-Story-authors-link&time_selector=time&time_format=Y-m-d+H%3Ai&remove_styling=on&format=Atom" news)
           ("http://localhost:9998/?action=display&bridge=CssSelectorComplexBridge&home_page=https%3A%2F%2Fwww.fr.de%2Frhein-main%2Fmain-taunus-kreis%2F&cookie=&title_cleanup=&entry_element_selector=.id-LinkOverlay&url_selector=a%5Btitle%5D&url_pattern=.%2B&limit=&use_article_pages=on&article_page_content_selector=article&content_cleanup=script%2C+.id-Story-interactionBar%2C+.id-StoryElement-inArticleReco%2C+.id-DonaldBreadcrumb&title_selector=h1&category_selector=&author_selector=&time_selector=time&time_format=Y-m-d+H%3Ai&remove_styling=on&format=Atom" news)
           ("http://localhost:9998/?action=display&bridge=CssSelectorComplexBridge&home_page=https%3A%2F%2Fmobil.hessen.de%2Fpresse%3Fmin%3D%26max%3D%26tid1%255B2745%255D%3D2745%26keys%3D%26displayFirst%3Dlist_first&cookie=&title_cleanup=&entry_element_selector=article&url_selector=a%5Btitle%5D&url_pattern=&limit=&use_article_pages=on&article_page_content_selector=.cke-richtext-content&content_cleanup=&title_selector=h1&category_selector=&author_selector=&time_selector=time&time_format=Y-m-d\\TH%3Ai%3As\\Z&remove_styling=on&format=Atom" news)
           ("http://localhost:9998/?action=display&bridge=CssSelectorComplexBridge&home_page=https%3A%2F%2Fwww.hofheim.de%2Fneuigkeiten-und-ausschreibungen%2Faktuelles-aus-hofheim%2F&cookie=&title_cleanup=&entry_element_selector=.teaserbox&url_selector=a&url_pattern=&limit=&use_article_pages=on&article_page_content_selector=.article&content_cleanup=script%2Cimg&title_selector=h1&category_selector=&author_selector=&time_selector=time&time_format=Y-m-d&format=Atom" news)
           ("http://localhost:9998/?action=display&bridge=CssSelectorComplexBridge&home_page=https%3A%2F%2Fwww.strava.com%2Fsegments%2F8923447&cookie=&title_cleanup=&entry_element_selector=.table-leaderboard+tr&url_selector=a&url_pattern=&limit=&article_page_content_selector=&content_cleanup=&title_selector=h1&category_selector=&author_selector=&time_selector=&time_format=&format=Atom" sport)
           ("http://localhost:9998/?action=display&bridge=CssSelectorComplexBridge&home_page=https%3A%2F%2Fwww.hgon.de%2Fentdecken%2F&cookie=&title_cleanup=&entry_element_selector=article&url_selector=a&url_pattern=&limit=&article_page_content_selector=&content_cleanup=img&title_selector=h3&category_selector=&author_selector=&time_selector=time.tagline&time_format=Y.m.d+&remove_styling=on&format=Atom")
           ("http://localhost:9998/?action=display&bridge=GithubIssueBridge&context=Project+Issues&q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc&u=karthink&p=gptel&format=Atom" github)
           ("http://localhost:9998/?action=display&bridge=CssSelectorComplexBridge&home_page=https%3A%2F%2Fwww.eppsteiner-zeitung.de%2Fnachrichten%2Fstadtleben&cookie=&title_cleanup=&entry_element_selector=.field-title+a&url_selector=a&url_pattern=&limit=&use_article_pages=on&article_page_content_selector=.pane-content&content_cleanup=span.kicker&title_selector=h1&category_selector=&author_selector=&time_selector=&time_format=&format=Atom" news)
           ("http://localhost:9998/?action=display&bridge=CssSelectorComplexBridge&home_page=https%3A%2F%2Fwww.npmjs.com%2Fpackage%2F%40anthropic-ai%2Fclaude-code%3FactiveTab%3Dversions&cookie=&title_cleanup=&entry_element_selector=%5Baria-labelledby%3D%22version-history%22%5D+tr&url_selector=a&url_pattern=&limit=&article_page_content_selector=&content_cleanup=&title_selector=h1&category_selector=&author_selector=&time_selector=time&time_format=Y-m-d%5CTH%3Ai%3As.v%5CZ&remove_styling=on&format=Atom"))

         '(("https://liore.com/rss/")
           ("https://samcurry.net/api/feed.rss")
           ("https://fantasy-faction.com/feed")
           ("https://jamierubin.net/feed/" analog)
           ("https://fromthepencup.wordpress.com/feed/" analog)
           ("https://semi-rad.com/feed/" sport)
           ("https://www.irunfar.com/feed" sport)
           ("https://www.quarks.de/feed/")
           ("https://xc-run.de/feed/" sport)
           ("https://sabbatical.derfred.org?feed=rss2")
           ("https://kimi-schreiber.de/feed/")
           ("https://buttondown.com/cassidoo/rss")
           ("https://buttondown.email/hillelwayne/rss")
           ("https://terminaltrove.com/totw.xml" programming)
           ("https://perishablepress.com/feed/atom/")
           ("https://piccalil.li/articles.xml")
           ("https://www.raptitude.com/feed/")
           ("https://rss.p.theconnman.com/r/netbrain/zwift")
           ("https://jillianhess.substack.com/feed" analog)
           ("https://jcjc-dev.com/atom.xml")
           ("http://feeds.grack.com/grack")
           ("https://zwiftinsider.com/category/news/game-updates/feed/" sport)
           ("https://trail-magazin.de/feed/")
           ("https://geekitguide.com/blog/feed")
           ("https://writingatlarge.com/feed/" analog)
           ("https://www.werkzeug-abc.de/feed/")
           ("https://rssbay.net/feed?keyword=vorwerk+260&globalId=EBAY-DE&location=DE&category=30335&auction=1&buyitnow=1&condition=7000&time-frame-type=901&time-frame-value=24")
           ("https://lemmy.dbzer0.com/feeds/c/piracy.xml?sort=TopMonth")
           ("https://lemmy.world/feeds/c/homeimprovement.xml?sort=TopMonth")
           ("https://hackaday.com/blog/feed/")
           ("https://newsboat.org/news.atom")
           ("https://blog.fogus.me/feed/")
           ("https://wolles-elektronikkiste.de/rss")
           ("https://rediscoveranalog.com/feed" analog)
           ("https://www.wellappointeddesk.com/feed")
           ("https://thepoorpenman.com/feed" analog)
           ("https://community.topazlabs.com/c/releases/gigapixel-ai/66.rss")
           ("https://questingbeast.substack.com/feed")
           ("https://controlaltbackspace.org/feed.xml")
           ("https://www.dbreunig.com/feed.xml")
           ("https://www.dcrainmaker.com/feed/" sport)
           ("https://ennie-awards.com/feed")
           ("https://hk-newsletter.de/feed" news)
           ("https://feeds.feedburner.com/AnnaHavron")
           ("https://analogoffice.net/feed.xml")
           ("https://seb.jambor.dev/feed.xml")
           ("https://bulletjournal.com/blogs/bulletjournalist.xml" analog)
           ("https://blog.pragmaticengineer.com/feed/")
           ("https://www.trailandkale.com/feed/" sport)
           ("https://www.takenote.space/blog-posts?format=rss" analog)
           ("https://www.rennrad-news.de/news/rss" sport)
           ("https://diaghilevsdice.blogspot.com/atom.xml")
           ("https://watcherdm.com/newsletter/rss")
           ("https://www.outsideonline.com/rss/all/rss.xml")
           ("https://www.enworld.org/ewr-porta/index.rss")
           ("https://www.reddit.com/search.rss?q=subreddit%3Arpg%20site%3Apodcast&sort=hot&t=week")
           ("https://lonedimension.wordpress.com/feed")
           ("https://theangrygm.com/feed")
           ("https://contributors.scala-lang.org/latest.rss")
           ("https://crawshaw.io/atom.xml" programming)
           ("https://discourse.nixos.org/latest.rss" programming)
           ("https://discourse.haskell.org/latest.rss")
           ("https://carlillustration.wordpress.com/tag/dungeon-world/feed")
           ("https://www.bastionland.com/feeds/posts/default")
           ("https://cannibalhalflinggaming.com/feed")
           ("https://takeonrules.com/index.xml")
           ("https://www.prismaticwasteland.com/blog?format=rss")
           ("https://questingblog.com/rss")
           ("https://thealexandrian.net/feed")
           ("https://elis.nu/blog/index.xml")
           ("https://matklad.github.io/feed.xml")
           ("https://blog.poisson.chat/rss.xml")
           ("https://algorithmsoup.wordpress.com/feed")
           ("https://www.thecramped.com/feed/")
           ("https://blog.sudo.ws/index.xml")
           ("https://fliek.com/blog/rss")
           ("https://www.mountainofink.com/?format=rss")
           ("https://blog.stephsmith.io/rss/")
           ("https://www.fotoforum.de/blog.rss")
           ("https://www.hgon-nabu-mtk.de/rss")
           ("https://hgon-kelkheim.de/feed/")
           ("https://calnewport.com/blog/feed")
           ("https://feeds.feedburner.com/MeltingAsphalt")
           ("https://improvephotography.com/feed/")
           ("https://pixls.us/feed.xml")
           ("https://feeds.feedburner.com/bmndr")
           ("https://labnotes.org/rss/")
           ("https://dariusforoux.com/feed/")
           ("https://thequilltolive.com/feed/" reading qtl)
           ("https://www.raptitude.com/feed/")
           ("https://mathwithbaddrawings.com/feed")
           ("https://photographylife.com/feed")
           ("https://calnewport.com/blog/feed")
           ("https://waitbutwhy.com/feed")
           ("https://failex.blogspot.com/feeds/posts/default")
           ("https://www.locusmap.app/feed/")
           ("https://nixos.org/blog/feed.xml" programming)
           ("https://us10.campaign-archive2.com/feed?u=49a6a2e17b12be2c5c4dcb232&id=ffbbbbd930")
           ("https://www.instaclustr.com/blog/category/technical/feed/")
           ("https://www.drmaciver.com/blog/feed/")
           ("https://www.lihaoyi.com/feed.xml")
           ("https://feeds.feedburner.com/incodeblog")
           ("https://www.gridsagegames.com/blog/feed/")
           ("https://hnrss.org/newest?points=150&comments=20&link=comments&count=25" hackernews)
           ("https://hnrss.org/bestcomments" hackernews)
           ("https://hnrss.org/newest?link=comment&comments=10" hackernews)
           ("https://hnrss.org/user?id=simonw" hackernews llm)
           ("https://typesandkinds.wordpress.com/feed/")
           ("https://feeds.feedburner.com/BlackCover")
           ("https://www.penaddict.com/blog?format=rss" analog)
           ("https://meta.plasm.us/atom.xml")
           ("https://feeds.feedburner.com/incodeblog")
           ("https://nullprogram.com/feed/")
           ("https://www.masteringemacs.org/feed/")
           ("https://emacsredux.com/atom.xml")
           ("http://xahlee.info/emacs/emacs/blog.xml")
           ("https://apocalisp.wordpress.com/feed/")
           ("https://blog.8thlight.com/feed/atom.xml")
           ("https://blog.jessitron.com/feeds/posts/default")
           ("https://blog.mgdproductions.com/rss/" hacking)
           ("https://git-blame.blogspot.de/feeds/posts/default")
           ("https://corte.si/rss.xml")
           ("https://feeds.feedburner.com/buckblog")
           ("https://feeds.feedburner.com/ezyang")
           ("https://feeds.feedburner.com/GiantRobotsSmashingIntoOtherGiantRobots")
           ("https://feeds.feedburner.com/TomMoertelsBlog")
           ("https://highlyscalable.wordpress.com/feed/")
           ("https://izbicki.me/blog/feed")
           ("https://jeremykun.wordpress.com/feed/")
           ("https://www.commandlinefu.com/feed/tenup" programming)
           ("https://www.haskellforall.com/feeds/posts/default" programming)
           ("https://byorgey.github.io/blog/rss.xml" programming)
           ("https://byorgey.wordpress.com/feed/" programming)
           ("https://planet.haskell.org/rss20.xml" programming)
           ("https://feeds2.feedburner.com/catonmat")
           ("https://themonadreader.wordpress.com/feed/" programming)
           ("https://irreal.org/blog/?feed=rss2" programming)
           ("https://www.haskellcast.com/feed.xml" programming)
           ("https://jvns.ca/atom.xml" programming)
           ("https://sachachua.com/blog/feed/" emacs programming)
           ("https://blog.zombiesrungame.com/rss/")
           ("https://japgolly.blogspot.com.au/feeds/posts/default")
           ("https://feeds.feedburner.com/codinghorror")
           ("https://jaspervdj.be/rss.xml" programming)
           ("https://joeyh.name/blog/index.rss")
           ("https://funktionale-programmierung.de/rss.xml" programming)
           ("https://okmij.org/ftp/rss.xml")
           ("https://www.oneusefulthing.org/feed" llm)
           ("https://www.recordingthoughts.com/feed/")
           ("https://www.codecentric.de/rss/feed.xml")
           ("https://jeltsch.wordpress.com/feed/")
           ("https://feeds.feedburner.com/NotebookStories" analog)
           ("https://meta.plasm.us/atom.xml")
           ("https://sysdig.com/blog/feed/")
           ("https://rachelbythebay.com/w/atom.xml")
           ("https://danluu.com/atom.xml")
           ("https://www.elidedbranches.com/atom.xml")
           ("https://us2.campaign-archive1.com/feed?u=ba834c562d82d9aba5eaf90ba&id=32cef9ab4e")
           ("https://lethalman.blogspot.com/feeds/posts/default/-/nixpills")
           ("https://begriffs.com/atom.xml")
           ("https://journal-lokal.de/category/hessen/rss" news)
           ("https://chrispenner.ca/atom.xml")
           ("https://blog.humblebundle.com/feed")
           ("https://www.lebensmittelwarnung.de/___LMW-Redaktion/RSSNewsfeed/Functions/RssFeeds/rssnewsfeed_Alle_DE.xml?nn=314268&state=hessen" news)
           ("https://www.tagesschau.de/inland/regional/hessen/index~rss2.xml" news)
           ("https://tinyhack.com/feed/" hacking)
           ("https://daniel.haxx.se/blog/feed/" programming)
           ("https://wearetrailmix.substack.com/feed" sport)
           ("https://simonwillison.net/atom/everything/" programming llm)
           ("https://voidstarsec.com/blog/feeds/all.atom.xml" hacking)
           ("https://blog.vbang.dk/feed.xml")
           ("https://goodmovie.substack.com/feed")
           ("https://varoa.net/feed.xml" programming)
           ("https://www.geoffreylitt.com/feed.xml" programming)
           ("https://www.gleech.org/feed.xml")
           ("https://jmswrnr.com/feed" hacking)
           ("https://terminaltrove.com/blog.xml" programming)
           ("https://newsletter.pessimistsarchive.org/feed")
           ("https://forum.rclone.org/t/rclone-insufficient-authentication-google-photos/50892.rss")
           ("https://www.oreilly.com/radar/topics/ai-ml/feed/")
           ("https://www.oreilly.com/radar/topics/programming/feed/")
           ("https://www.oreilly.com/radar/topics/radar-trends/feed/")
           ("https://feeds.leonid.codes/hacker_news.rss" hackernews2)
           ("https://gophersignal.com/rss#/" hackernews3)
           ("https://github.blog/changelog/feed/" github)
           ("https://aider.chat/feed.xml" programming)
           ("https://hamatti.org/feed/feed.xml" programming)
           ("https://harper.blog/index.xml" programming)
           ("https://lucumr.pocoo.org/feed.atom" llm programming)
           ("https://claudelog.com/rss.xml" llm programming))))
           ;; ^^^^ feeds
  )

(use-package elfeed-summary
  :ensure t
  :config
  (setq elfeed-summary-settings
        '((group
           (:title . "HackerNews")
           (:elements
            (query . hackernews)))
          (group
           (:title . "Reddit")
           (:elements
            (query . reddit)))
          (group
           (:title . "Youtube")
           (:elements
            (query . youtube)))
          (group
           (:title . "Newsletter")
           (:elements
            (query . newsletter)))
          (group
           (:title . "GitHub Releases")
           (:elements
            (query . github)))
          (group
           (:title . "Searches")
           (:elements
            (search
             (:filter . "@7-days-ago +unread")
             (:title . "Unread entries this week"))
            (search
             (:filter . "@6-months-ago emacs")
             (:title . "Something about Emacs"))))
          (group
           (:title . "Auto-Tags")
           (:elements (auto-tags))))))

(use-package elfeed-score
  :ensure t
  :config
  (elfeed-score-enable)
  (define-key elfeed-search-mode-map "=" elfeed-score-map)
  (setq elfeed-search-print-entry-function #'elfeed-score-print-entry))

(use-package pcre2el
  :ensure t)

(use-package ialign
  :ensure t)

(add-to-list 'load-path "@mcp_el@")
(require 'mcp-hub)

(setq mcp-hub-servers
      '(
        ("mongodb-local" . (:command "docker" :args ("run" "--rm" "-i" "--network=host" "furey/mongodb-lens")))
        ("filesystem" . (:command "docker" :args ("run" "--rm" "-i" "--mount" "type=bind,src=/tmp/filesystem-mcp-test,dst=/projects/filesystem-mcp-test" "mcp/filesystem" "/projects")))
        ("sqlite" . (:command "docker" :args ("run" "--rm" "-i" "-v" "mcp-test:/mcp" "mcp/sqlite" "--db-path" "/mcp/test.db")))
        ("intellij" . (:command "npx" :args ("-y" "@jetbrains/mcp-proxy")))))


(defun gptel-mcp-register-tool ()
  (interactive)
  (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
    (mapcar #'(lambda (tool)
                (apply #'gptel-make-tool
                       tool))
            tools)))

(defun gptel-mcp-use-tool ()
  (interactive)
  (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
    (mapcar #'(lambda (tool)
                (let ((path (list (plist-get tool :category)
                                  (plist-get tool :name))))
                  (push (gptel-get-tool path)
                        gptel-tools)))
            tools)))

(defun gptel-mcp-close-use-tool ()
  (interactive)
  (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
    (mapcar #'(lambda (tool)
                (let ((path (list (plist-get tool :category)
                                  (plist-get tool :name))))
                  (setq gptel-tools
                        (cl-remove-if #'(lambda (tool)
                                          (equal path
                                                 (list (gptel-tool-category tool)
                                                       (gptel-tool-name tool))))
                                      gptel-tools))))
            tools)))
;;
