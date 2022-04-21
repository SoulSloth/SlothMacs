;;
;; Don't show the startup screen
(setq inhibit-startup-message t)

;;
;; Disable Basic crap

(scroll-bar-mode -1) ; Disable scroll bar
(tool-bar-mode -1) ; Disable tool bar
(tooltip-mode -1) ; Disable tool tips
(menu-bar-mode -1) ; Disable the menu bar

(set-fringe-mode 10) ; Left and right fringes

;;
;; Fonts

(set-face-attribute 'default nil :font "Source code pro" :family "sans" :height 100 :width 'normal)
;;(set-face-attribute 'default nil :font "linux biolinum" :family "sans" :height 125 :width 'normal)
;;(set-face-attribute 'default nil :font "linux libertine" :family "serif" :height 115)
;;(set-face-attribute 'default nil :font "linux biolinum" :family "sans-serif" :height 115)
;; TODO: keep searching for a good mono font
;;(set-face-attribute 'default nil :font "Nimbus Mono PS" :family "monospace" :height 115)

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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(evil-collection evil monokai-theme zenburn-theme anti-zenburn-theme atom-dark-theme berrys-theme melancholy-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
