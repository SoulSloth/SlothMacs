;;; SlothMacs --- Soul's emacs config
;;; Commentary: Soul slowly creates his own emacs config and blogs about it
;;; Code: ?

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

;; (use-package evil-magit
;;   :after magit)

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
   '(evil-magit magit counsel-projectile consel-projectile projectile hydra evil-collections general doom-badger-theme doom-themes helpful ivy-rich which-key rainbow-delimiters doom-modeline counsel swiper ivy evil-collection evil monokai-theme zenburn-theme anti-zenburn-theme atom-dark-theme berrys-theme melancholy-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
