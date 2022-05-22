(setq inhibit-startup-message t) ; no startup message
      (scroll-bar-mode -1) ; Disable scroll bar
      (tool-bar-mode -1) ; Disable tool bar
      (tooltip-mode -1) ; Disable tool tips
      (menu-bar-mode -1) ; Disable the menu bar

;; Display column number in Mode line
(column-number-mode)
;; Display line numbers by default
(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
 (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Sets the size of left and right fringes
  (set-fringe-mode 10)

;; Fix gap in tiling window environments
(setq frame-resize-pixelwise t)

;; Fonts
  (defvar sloth/default-font-size 140)

    (set-face-attribute 'default nil :font "Source code pro" :family "sans" :height 100 :width 'normal)
    (set-face-attribute 'default nil :font "Nimbus Mono PS" :family "monospace" :height 115)

;; Requires package.el so we can get our packages 
  (require 'package)

;; Elisp package repositories
  (setq package-archives '(("org" .  "http://orgmode.org/elpa/") ;; Org mode latest
                             ("elpa" . "http://elpa.gnu.org/packages/") ;; Standard elisp packages
                             ("melpa" . "https://melpa.org/packages/"))) ;;Milkypostman's Emacs Lisp Pacakge Archive

;; Load and activate emacs lisp packages
  (package-initialize)

;; Refresh package contents
(unless package-archive-contents
  (package-refresh-contents))

;; Bootstrap use-pacakge if it's not installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Require the use-package package
(require 'use-package)

;; makes =:ensure t= the default for our =use-package= calls.
(setq use-package-always-ensure t)

(use-package general
;; Creates qeuivalent vim mapping functions
  :config
  (general-evil-setup t)
  ;; Add our leader keys
  :config
  (general-create-definer sloth/leader-keys
    :keymaps '(normal insert visual emacs)
    ;; EVIL spacemacs goodness
    :prefix "SPC"
    :global-prefix "C-SPC"))

(sloth/leader-keys
 "t" '(:ignore t :which-key "toggles")
 "tt" '(counsel-load-theme :which-key "choose theme")
 "f" '(counsel-projectile-grep :which-key "projectile-grep"))

(unless (package-installed-p 'swiper)
  (package-install 'swiper))

(require 'swiper)

;; Ivy for better minibuffer completions
  (use-package ivy
      :diminish
      :bind (("C-s" . swiper)
             :map ivy-minibuffer-map
             ("TAB" . ivy-alt-done)
             ("C-l" . ivy-alt-done)
             ("C-k" . ivy-next-line)
             :map ivy-switch-buffer-map
             ("C-k" . ivy-previous-line)
             ("C-l" . ivy-done)
             ("C-d" . ivy-switch-buffer-kill)
             :map ivy-reverse-i-search-map
             ("C-k" . ivy-previous-line)
             ("C-d" . ivy-reverse-i-search-kill))
      :config
      (ivy-mode 1))

;; Give us some more info in completions
  (use-package ivy-rich
      :init
      (ivy-rich-mode 1))

;; (unless (package-installed-p 'counsel)
;;     (package-install 'counsel))

;; (require 'counsel)

  ;; use counsel and bind some useful keys for switching buffers and finding files
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

;; which-key for when we forget command completions
  (use-package which-key
    :init (which-key-mode)
    :diminish which-key-mode
    :config (setq which-key-idle-delay 0.3))

;; Get Hydra
  (use-package hydra)

  ;; Defining a hydra meny
    (defhydra hydra-text-scale (:timeout 4)
      "scale text"
      ("j" text-scale-increase "in")
      ("k" text-scale-decrease "out")
      ("f" nil "finished" :exit t))

  ;; Add it to our leader-keys
    (sloth/leader-keys
      "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package projectile
    :diminish projectile-mode
    :config (projectile-mode)
    :custom ((projectile-completion-system 'ivy))
    :bind-keymap
    ("C-c p" . projectile-command-map)
    :init
    ;; This lets us rapidly switch between different projects in our
    ;; work directory
    (when (file-directory-p "~/projects")
      (setq projectile-project-search-path '("~/projects")))
    (setq projectile-swith-project-action #'projectile-dired))

;; Play nice with Counsel
  (use-package counsel-projectile
    :config (counsel-projectile-mode))

(use-package magit
  :custom
;; Just show the magit window in the same buffer
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package helpful
    :ensure t
    :custom
    (counsel-describe-function-function #'helpful-callable)
    (counsel-describe-variable-function #'helpful-variable)
    :bind
    ([remap describe-function] . helpful-callable)
    ([remap describe-command] . helpful-command)
    ([remap describe-variable] . helpful-variable)
    ([remap describe-key] . helpful-key))

(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)))

(use-package evil
    :ensure t
    :init
    ;; evil-collections required sets
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    ;; Give us back up from emacs
    (setq evil-want-C-u-scroll t)
    (setq evil-want-C-i-jump nil)
    :config
    (evil-mode 1)
    ;; Exit to evil normal state with C-g instead of having to hit esc
    :config
    (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state))

;; Use visual line motions even outside of visual-line-mode buffers
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)
;;
(evil-set-initial-state 'messages-buffer-mode 'normal)
(evil-set-initial-state 'dashboard-mode 'normal)

(use-package evil-collection
  ;; Only load after evil
  :after evil
  ;; init
  :config
  (evil-collection-init))

(use-package monokai-theme)
(load-theme 'monokai t)

(defun efs/org-mode-setup ()
  ;; Indent according to outline structure
  (org-indent-mode)
  (variable-pitch-mode 1)
  ;; Word Wrap
  (visual-line-mode 1)
  (linum-mode 0))

(use-package org-bullets
    :after org
    :hook (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
			  '(("^ *\\([-]\\) "
			     (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

(use-package org
:hook (org-mode . efs/org-mode-setup)
	   :config (setq org-ellipsis " 🎈"
			 ;; hides formatting markers
			 org-hide-emphasis-markers t)
	   (efs/org-font-setup))

(setq org-tag-alist
      '((:startgroup)
	;; Mutually exclusive tags go here
	(:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@work" . ?W)
       ("planning" . ?p)))

(setq org-refile-targets
        '(("archive.org" :maxlevel . 1)
          ("tasks.org" :maxlevel . 1)))

(advice-add 'org-refile :after `org-save-all-org-buffers)

(setq org-todo-keywords
      '((sequence
	 ;; Active states
	 "TODO(t)" "NEXT(n)"
	 "|"
	 ;; Done state
	 "DONE(d!)")
	(sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

(setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/dump/tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/dump/journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree "~/dump/journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

      ("w" "Workflows")
      ("we" "Checking Email" entry (file+olp+datetree "~/dump/journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ("m" "Metrics Capture")
      ("mw" "Weight" table-line (file+headline "~/dump/metrics.org" "Weight")
       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

(define-key global-map (kbd "C-c j")
(lambda () (interactive) (org-capture nil "jj")))

(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
;; Fold log entries into drawer
(setq org-log-into-drawer t)

(setq org-agenda-files '("~/.emacs.provingGrounds/orgfiles/tasks.org"
                         "~/.emacs.provingGrounds/orgfiles/birthdays.org"
                         "~/.emacs.provingGrounds/orgfiles/habits.org"
                         )) 

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)

(setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))
            (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

          ("n" "Next Tasks"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))))

          ("W" "Work Tasks" tags-todo "+work-email")

          ;; Low-effort next actions
          ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
           ((org-agenda-overriding-header "Low Effort Tasks")
            (org-agenda-max-todos 20)
            (org-agenda-files org-agenda-files)))

          ("w" "Workflow Status"
           ((todo "WAIT"
                  ((org-agenda-overriding-header "Waiting on External")
                   (org-agenda-files org-agenda-files)))
            (todo "REVIEW"
                  ((org-agenda-overriding-header "In Review")
                   (org-agenda-files org-agenda-files)))
            (todo "PLAN"
                  ((org-agenda-overriding-header "In Planning")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "BACKLOG"
                  ((org-agenda-overriding-header "Project Backlog")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "READY"
                  ((org-agenda-overriding-header "Ready for Work")
                   (org-agenda-files org-agenda-files)))
            (todo "ACTIVE"
                  ((org-agenda-overriding-header "Active Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "COMPLETED"
                  ((org-agenda-overriding-header "Completed Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "CANC"
                  ((org-agenda-overriding-header "Cancelled Projects")
                   (org-agenda-files org-agenda-files)))))))

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.provingGrounds/Emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle)))) 

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)
    (clojure . t)
    ))

  (setq org-confirm-babel-evaluate nil)

(setq org-babel-clojure-backend 'cider)

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("clj" . "src clojure"))
(add-to-list 'org-structure-template-alist '("yl" . "src yaml"))
(add-to-list 'org-structure-template-alist '("conf" . "src conf"))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 125
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package company
:after lsp-mode
:hook (lsp-mode . company-mode)
:bind (:map company-active-map
       ("<tab>" . company-complete-selection))
      (:map lsp-mode-map
       ("<tab>" . company-indent-or-complete-common))
:custom
(company-minimum-prefix-length 1)
(company-idle-delay 0.0)) 

(use-package company-box
:hook (company-mode . company-box-mode))

(use-package typescript-mode
  :mode "\\.ts\\'" ;; Start up any time we open a fiel with .ts exentsion
  :hook (typescript-mode . lsp-deferred) ;; Don't startup the server until buffer is visible
  :config (setq typescript-indent-level 2))

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  (python-shell-interpreter "python"))

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))
  :init
  (add-hook 'clojure-mode-hook #'yas-minor-mode)         
  (add-hook 'clojure-mode-hook #'linum-mode)             
  (add-hook 'clojure-mode-hook #'subword-mode)           
  (add-hook 'clojure-mode-hook #'smartparens-mode)       
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'eldoc-mode)             
  (add-hook 'clojure-mode-hook #'idle-highlight-mode))

(use-package cider
  :ensure t
  :defer t
  :init (add-hook 'cider-mode-hook #'clj-refactor-mode)
  :diminish subword-mode
  :config
  (setq nrepl-log-messages t                  
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t    
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        nrepl-hide-special-buffers t            
        cider-overlays-use-font-lock t)         
  (cider-repl-toggle-pretty-printing))

;; Rainbow delimiters
(use-package  rainbow-delimiters
:hook (prog-mode . rainbow-delimiters-mode))
