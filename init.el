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
(use-package monokai-theme 
  :ensure t)

(load-theme 'monokai t)

;;
;; Text Compeltion
;;TODO: Add helm

;;
;; evil mode

;; evil package proper
(unless (package-installed-p 'evil)
  (package-install 'evil))

(setq evil-want-integration t)
(setq evil-want-keybinding nil)

(require 'evil)
(evil-mode 1)

;; evil-collection
(when (require 'evil-collection nil t)
  (evil-collection-init))

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
;; ivy

;; Get Ivy
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

;;
;; Mode line

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(doom-modeline counsel swiper ivy evil-collection evil monokai-theme zenburn-theme anti-zenburn-theme atom-dark-theme berrys-theme melancholy-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
