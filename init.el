;;; SlothMacs --- Soul's emacs config
;;; Commentary: Soul slowly creates his own emacs config and blogs about it
;;; Code: ?

(defvar sloth/default-font-size 140)

;;
;; Disable Basic crap
(setq inhibit-startup-message t) ; no startup message
(scroll-bar-mode -1) ; Disable scroll bar
(tool-bar-mode -1) ; Disable tool bar
(tooltip-mode -1) ; Disable tool tips
(menu-bar-mode -1) ; Disable the menu bar

;;
;; fringes
(set-fringe-mode 10) ; Left and right fringes

;;
;; escape escapes mini buffer
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;
;; Fonts


(set-face-attribute 'default nil :font "Source code pro" :family "sans" :height 100 :width 'normal)
(set-face-attribute 'default nil :font "Nimbus Mono PS" :family "monospace" :height 115)


;;
;; Initialize package sources
(require 'package)

;; Package Sources
(setq package-archives '(("org" .  "http://orgmode.org/elpa/")
                         ("elpa" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Bootstrap use-pacakge if it's not installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

;; Ensure our packages are installed before we run
(setq use-package-always-ensure t)

;;
;; Theme

;; For trying some stuff out
;;(use-package doom-themes)

(use-package monokai-theme)
(load-theme 'monokai t)

;;
;; General.el
(use-package general
  :config
  (general-evil-setup t)
  :config
  (general-create-definer sloth/leader-keys
    :keymaps '(normal insert visual emacs)
    ;; EVIL spacemacs goodness
    :prefix "SPC"
    :global-prefix "C-SPC"))

;; Our global leader keys
(sloth/leader-keys
 "t" '(:ignore t :which-key "toggles")
 "tt" '(counsel-load-theme :which-key "choose theme"))

;;
;; NOTE: expected that you've switched capslock to lctrl
;; (general-define-key
;;  "C-M-j" 'counsel-switch-buffer
;;  "C-s" 'counsel-grep-or-swiper)

;;
;; Text Compeltion
;;TODO: Add helm



;;
;; swiper
(unless (package-installed-p 'swiper)
  (package-install 'swiper))

(require 'swiper)

;;
;; Counsel
(unless (package-installed-p 'counsel)
  (package-install 'counsel))

(require 'counsel)

;;
;; navigation

;; Get Ivy
;; Some useful minor mode things
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




;; which-key
;; For incomplete commands, displays a menu
;; emacs minor mode
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.3))

;; ivy-rich
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; counsel configuration
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

;; helpful
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

;;
;; Programming stuff

;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)


;; Don't show line numbers in org mode term or eshell
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Rainbow delimeters
(use-package  rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;;
;; Mode line

;; NOTE: Make sure you M-x all-the-icons-install-fonts
(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)))

;;
;; evil mode
;; Vim layer for emacs

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

;; evil-collection
;; Which covers several areas of emacs that evil doesn't
(use-package evil-collection
  ;; Only load after evil
  :after evil
  ;; init
  :config
  (evil-collection-init))

;; Hydra
;; Hydra helps us make keybindings to do things
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

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

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; WOOO BABAY MAGIT TIME
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; TODO: configure this with our github. Seems very useful.
;; https://magit.vc/manual/forge.html
(use-package forge)

;; Org mode
;; manual: https://orgmode.org/manual/

(defun efs/org-mode-setup ()
  ;; Indent according to outline structure
  (org-indent-mode)
  (variable-pitch-mode 1)
  ;; Word Wrap
  (visual-line-mode 1)
  (linum-mode 0)
  ) 


;; Give us some nicer looking BULLETS
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

;; Log the time when a task was complete 
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

(setq org-todo-keywords
      '((sequence
	 ;; Active states
	 "TODO(t)" "NEXT(n)"
	 "|"
	 ;; Done state
	 "DONE(d!)")
	(sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

(setq org-refile-targets
      '(("archive.org" :maxlevel . 1)
	("tasks.org" :maxlevel . 1)))

(advice-add 'org-refile :after `org-save-all-org-buffers) 

(setq org-tag-alist
      '((:startgroup)
	;; Mutually exclusive tags go here
	(:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@work" . ?W)
       ("planning" . ?p))) 

;; Configure custom agenda views
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

;; Journal 
(define-key global-map (kbd "C-c j")
  (lambda () (interactive) (org-capture nil "jj"))) 



(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 125
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

;; TODO:
;; ;; watch the video about magit and projectile!
;; ;; Get comfortable with evil-window! C-w(Not in insert mode, that's kill backwards word)
;; ;; Get comfrotable with kill backwards word( C-w in insert mode), Scroll-up(C-u), and scroll-down(C-d)
;; Ivy completions are opened with M-o

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(cider clojure-mode org-bullets forge evil-magit magit counsel-projectile consel-projectile projectile hydra evil-collections general doom-badger-theme doom-themes helpful ivy-rich which-key rainbow-delimiters doom-modeline counsel swiper ivy evil-collection evil monokai-theme zenburn-theme anti-zenburn-theme atom-dark-theme berrys-theme melancholy-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
