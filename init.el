(add-to-list 'load-path "~/.emacs.d/misc-elisp/")

(when (string= system-type "darwin")
  (use-package exec-path-from-shell)
    (exec-path-from-shell-initialize))

(if (eq system-type 'darwin)
    (let ((gls (executable-find "gls")))
      (when gls
	(setq insert-directory-program gls
	      dired-listing-switches "-aBhl --group-directories-first"))))

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
		markdown-mode-hook
		term-mode-hook
          shell-mode-hook
          vterm-mode-hook
          eshell-mode-hook
          dired-mode-hook
		nov-mode-hook
		doc-view-mode-hook
		help-mode-hook
		;; on WSL line numbers will show up in magit panes, but I don't experience that problem anywhere else...
                ))
 (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-fringe-mode 10)

(setq frame-resize-pixelwise t)

(defvar sloth/default-font-size 140)
(set-face-attribute 'default nil :font "Source code pro" :family "sans" :height 100 :width 'normal)

;; Haven't found an easy analog to Nimbus Mono PS on Mac
(when (not (string= system-type "darwin"))
 (set-face-attribute 'default nil :font "Nimbus Mono PS" :family "monospace" :height 115))

(require 'package)

(setq package-archives '(("org" .  "http://orgmode.org/elpa/") ;; Org mode latest
                           ("elpa" . "http://elpa.gnu.org/packages/") ;; Standard elisp packages
                           ("melpa" . "https://melpa.org/packages/"))) ;;Milkypostman's Emacs Lisp Pacakge Archive

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(use-package flyspell-correct-ivy)

(use-package no-littering)

(setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(use-package general
;; Creates qeuivalent vim mapping functions
  :config
  (general-evil-setup t)
  ;; see https://github.com/noctuid/general.el#automatic-key-unbinding
  :config
  (general-auto-unbind-keys)
  ;; Add our leader keys
  :config
  (general-create-definer sloth/leader-keys
    :keymaps '(normal insert visual emacs)
    ;; EVIL spacemacs goodness
    :prefix "SPC"
    :global-prefix "C-SPC"))

(defun open-project-file ()
  "Prompts the user for a file in ~/org/projects and opens it"
  (interactive)
  (find-file
   (ivy-read "Open Project File: "
	       (directory-files "~/org/projects/" t "\.org")
	      :require-match "yes" )))


(defun open-emacs-conf ()
  "Opens the emacs literate conf"
  (interactive)
  (find-file "~/.emacs.d/Emacs.org"))

(defun open-tasks-file ()
   "Opens the emacs literate conf"
   (interactive)
   (find-file "~/org/tasks.org"))

(defun open-snippits-file ()
   "Opens the code snippits file"
   (interactive)
   (find-file "~/org/code.org"))

(sloth/leader-keys
 "SPC" '(counsel-M-x :which-key "Execute Command" )
 ;; Opening relevent org files/terminal buffers
 "a" '(:ignore a :which-key "Open file")
 "ad" '(open-tasks-file :which-key "open tasks")
 "ae" '(open-emacs-conf :which-key "open emacs config")
 "ac" '(open-snippits-file :which-key "open code snippits")
 "ap" '(open-project-file :which-key "open project file")
 "at" '(vterm :which-key "open vterm buffer")

 ;; Flyspell
 "d" '(:ignore d :which-key "Flyspell")
 "dd" '(flyspell-correct-wrapper :which-key "Auto-correct word")

 ;; Help items
 "h" '(counsel-describe-symbol :which-key "describe symbol")

 ;; org-roam
 "r" '(:ignore r :which-key "Org-Roam")
 "ri" '(org-roam-node-insert :which-key "Insert Node")
 "re" '(org-roam-node-insert-immediate :which-key "Insert Empty Note")
 "rf" '(org-roam-node-find :which-key "Find Node")
 "rl" '(org-roam-buffer-toggle :which-key "Show Backlinks")
 "rt" '(org-roam-tag-add :which-key "Add tag")
 "ro" '(org-roam-dailies-capture-tomorrow :which-key "Capture a daily for tommorrow")
 "rd" '(org-roam-dailies-goto-today :which-key "Goto Today's Dailies")
 "rm" '(org-roam-insert-image :which-key "Insert Pic")
 "rn" '(org-roam-dailies-goto-next-note :which-key "Go to next daily")
 "rp" '(org-roam-dailies-goto-previous-note :which-key "Go to previous daily")
 
 ;; Magit
 "g" '(:ignore g :which-key "Magit")
 "gs" '(magit-status :which-key "Status")
 "gb" '(magit-blame :which-key "Blame")
 "gl" '(magit-log-current :which-key "Log")
 
 ;; Buffers
 "b" '(:ignore b :which-key "buffers")
 "bb" '(switch-to-buffer :which-key "Switch to buffer")
 "br" '(revert-buffer-quick :which-key "Revert buffer")
 "bk" '(kill-current-buffer :which-key "Kill buffer")

 ;; Bookmarks

 "k" '(:ignore k :which-key "bookmarks")
 "kk" '(bookmark-jump :which-key "Jump to bookmark")
 "km" '(bookmark-set-no-overwrite :which-key "Make Bookmark")
 
 ;; toggles 
 "t" '(:ignore t :which-key "toggles")
 "tt" '(counsel-load-theme :which-key "choose theme")
 
 ;; Clojure CIDER commands
 "s" '(:ignore s :which-key "cider")
 "sj" '(cider-jack-in :which-key "CIDER jack-in")
 "sq" '(cider-quit :which-key "CIDER quit")
 "se" '(cider-eval-region :which-key "cider eval region")
 "ss" '(cider-insert-region-in-repl :which-key "cider send region to repl")
 "sf" '(cider-format-buffer :which-key "cider format buffer")
 "sb" '(cider-load-buffer :which-key "cider load buffer")


 ;; Language management
 "ls" '(lsp-ivy-workspace-symbol :which-key "ivy workspace symbol")
 "ld" '(lsp-find-definition :which-key "find definition")
 "lr" '(lsp-find-references :which-key "find references")
 "li" '(lsp-find-implementations :which-key "find implementations")
 "le" '(lsp-treemacs-errors-list :which-key "Show static code analysis errors")
 
 ;; General Project management
 "li" '(lsp-ivy-workspace-symbol :which-key "ivy workspace symbol")
 "p" '(:ignore p :which-key "Projectile")
 "pp" '(projectile-switch-project :which-key "projectile switch project")
 "pf" '(projectile-find-file :which-key "projectile find file")
 "pb" '(projectile-compile-project :which-key "build project")
 "pt" '(projectile-test-project :which-key "test project")
 "pr" '(projectile-run-project :which-key "run project")
 "pl" '(sloth/view-project-logs  :which-key "view project logs")
 "f" '(counsel-projectile-grep :which-key "projectile-grep")
 
 ;; Org
 "o" '(:ignore o :which-key "org")
 "od" '(org-display-inline-images :which-key "Display Inline Images")
 "of" '(org-footnote-new :which-key "Create Footnote")
 "or" '(org-remove-inline-images :which-key "Remove Inline Images")
 "ol" '(org-store-link :which-key "Store Link")
 "oc" '(org-clock-in :which-key "Clock In")
 "oo" '(org-clock-out :which-key "Clock Out")
 "oe" '(org-set-effort :which-key "Set Effort")

 ;; Org Capture
 "c" '(org-capture :which-key "org capture")

 ;; Yanking
 "y" '(sloth/copy-file-path-to-clipboard :which-key "Yank current path"))

(unless (package-installed-p 'swiper)
  (package-install 'swiper))

(require 'swiper)

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
    ;; Use git grep in source controlled projects
    ;; Invaluable for ignoring temp files
    :config (setq projectile-use-git-grep t)
    :custom ((projectile-completion-system 'ivy))
    :bind-keymap
    ("C-c p" . projectile-command-map)
    :init
    ;; This lets us rapidly switch between different projects in our
    ;; work directory
    (when (file-directory-p "~/projects")
      (setq projectile-project-search-path '("~/projects" "~/org/roam/scripts")))
    (setq projectile-swith-project-action #'projectile-dired))

;; Play nice with Counsel
  (use-package counsel-projectile
    :config (counsel-projectile-mode))

(defun sloth/view-project-logs ()
  "Run the projects log command in another buffer"
  (interactive)
  ;; Check if config is set
  (if (boundp 'project-log-command)
  (let (
	;; Capture the local value of project-log-command before we create a new buffer
	(log-command  project-log-command)
	;; Create the buffer
	(log-buffer (get-buffer-create (format "*%s Logs*" (projectile-project-name)))))
    (with-current-buffer log-buffer
      (read-only-mode 0)
      (erase-buffer)
      (start-process "project-log-process" log-buffer "sh" "-c" log-command)
      (read-only-mode 1))
    (pop-to-buffer log-buffer))
  (message "project-log-command not set")))

;; Mark local variable as safe
(put 'project-log-command 'safe-local-variable #'stringp)

(use-package magit
  :custom
;; Just show the magit window in the same buffer
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package ediff)

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
    ;; Use evil in the minibuffer
    (setq evil-want-minibuffer t)
    ;; Get undo-redo functionality
    (setq evil-undo-system 'undo-redo)
    :config
    (evil-mode 1)
    ;; Exit to evil normal state with C-g instead of having to hit esc
    :config
    (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state))

;; Use visual line motions even outside of visual-line-mode buffers
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)
;; Set the state when we enter certain modes
(evil-set-initial-state 'messages-buffer-mode 'normal)
(evil-set-initial-state 'dashboard-mode 'normal)

(use-package evil-collection
  ;; Only load after evil
  :after evil
  ;; init
  :config
  (evil-collection-init))

(use-package evil-matchit
 :after evil
 :config (global-evil-matchit-mode 1))

(use-package gruvbox-theme)
(load-theme 'gruvbox-dark-medium t)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(defun sloth/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . sloth/lsp-mode-setup)
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
:config (add-to-list 'company-backends 'company-capf)
:custom
(company-minimum-prefix-length 1)
(company-idle-delay 0.0))

(use-package company-box
:hook (company-mode . company-box-mode)
:init (setq company-box-icons-alist 'company-box-icons-all-the-icons)
)

(use-package lua-mode)

(add-hook 'sql-mode-hook 'lsp)
(setq lsp-sqls-workspace-config-path nil)
(setq lsp-sqls-connections
    '(((driver . "postgresql") (dataSourceName . "host=127.0.0.1 port=5432 user=postgres password=passwd dbname=foodb sslmode=disable"))))

(use-package typescript-mode
  :mode "\\.ts\\'" ;; Start up any time we open a fiel with .ts exentsion
  :hook (typescript-mode . lsp-deferred) ;; Don't startup the server until buffer is visible
  :config (setq typescript-indent-level 2))

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  (python-shell-interpreter "python"))

(use-package lsp-java
  :ensure t
  :hook (java-mode . lsp-deferred))

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))
  :hook (clojure-mode . lsp-deferred))

(use-package cider
  :ensure t
  :defer t
  :diminish subword-mode
  :config
  (setq nrepl-log-messages t)
  ;; I don't type things into the repl much
  (setq cider-repl-pop-to-buffer-on-connect nil)
  (setq cider-repl-display-help-banner nil)
  (cider-repl-toggle-pretty-printing))

(use-package scala-mode
  :interpreter
    ("scala" . scala-mode)
  :mode (("\\.scala\\'" . scala-mode))
    )

(use-package clojure-essential-ref)

(use-package go-mode
  :ensure t
  :mode (("\\.go\\'" . go-mode))
  :hook ((before-save . gofmt-before-save) (go-mode . lsp-deferred))
  )

(use-package dockerfile-mode)
(use-package docker-compose-mode)

(use-package scala-mode
   :interpreter
     ("scala" . scala-mode)
   :mode (("\\.scala\\'" . scala-mode))
     )

(use-package terraform-mode)

(use-package protobuf-mode)

;; Rainbow delimiters
(use-package  rainbow-delimiters
:hook (prog-mode . rainbow-delimiters-mode))

(setq compilation-scroll-output t)

(use-package ansi-color)

(defun sloth/colorize-buffer-text ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook 'sloth/colorize-buffer-text)

(defun sloth/org-mode-setup ()
  ;; Indent according to outline structure
  (org-indent-mode)
  (variable-pitch-mode 1)
  ;; Word Wrap
  (visual-line-mode 1)
  (display-line-numbers-mode 0))

(use-package org-bullets
    :after org
    :hook (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun sloth/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1)
							  "•")))))))


(use-package org
  :hook (org-mode . sloth/org-mode-setup)
           :config (setq org-ellipsis " 🎈"
                         ;; hides formatting markers
                         org-hide-emphasis-markers t)
	       ;; Don't auto-indent when we RET after a line
           (setq org-edit-src-content-indentation 0)
	       (setq org-export-with-toc nil)
	       (setq org-html-validation-link nil)
	       ;; I want to actually be able to edit the width and hieghts of iamges in org
	       (setq org-image-actual-width nil)
           (sloth/org-font-setup))

(defun org-html-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-html--textarea-block src-block)
    (let* ((lang (org-element-property :language src-block))
	   (code (org-html-format-code src-block info))
	   (label (let ((lbl (org-html--reference src-block info t)))
		    (if lbl (format " id=\"%s\"" lbl) "")))
	   (klipsify  (and  (plist-get info :html-klipsify-src)
                            (member lang '("javascript" "js"
					   "ruby" "scheme" "clojure" "php" "html")))))
      (if (not lang) (format "<pre class=\"example\"%s>\n%s</pre>" label code)
	(format "<div class=\"org-src-container\">\n%s%s\n</div>"
		;; Build caption.
		(let ((caption (org-export-get-caption src-block)))
		  (if (not caption) ""
		    (let ((listing-number
			   (format
			    "<span class=\"listing-number\">%s </span>"
			    (format
			     (org-html--translate "Listing %d:" info)
			     (org-export-get-ordinal
			      src-block info nil #'org-html--has-caption-p)))))
		      (format "<label class=\"org-src-name\">%s%s</label>"
			      listing-number
			      (org-trim (org-export-data caption info))))))
		;; Contents.
		(if klipsify
		    (format "<pre><code class=\"src src-%s\"%s%s>%s</code></pre>"
			    lang
			    label
			    (if (string= lang "html")
				" data-editor-type=\"html\""
			      "")
			    code)
		  (format "<pre class=\"src src-%s\"%s><code>%s</code></pre>"
                          lang label code)))))))

(custom-theme-set-faces
 'user
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "light pink" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch) :foreground "orange" )))))

(setq org-tag-alist
      '((:startgroup)
	;; Mutually exclusive tags go here
	(:endgroup)
	   ("polish" . ?p)
	   ("marketing" . ?m)
	   ("usability" . ?u)
	   ("defect" . ?d)
	   ("shaders" . ?s)
	   ("narrative" . ?n)
	   ("content" . ?c)
	   ("refactoring" . ?r)))

(setq org-refile-targets
        '(("~/org/archive.org" :maxlevel . 1)
          ("~/org/tasks.org" :maxlevel . 1)))

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
    `(("d" "Daily Planning")
      ;; Our plan for the day
      ;; Currently prompting the user for the datetime just so I can set it to tomarrow,
      ;; But I guess I can plan a couple days in advance
      ("dd" "Daily Todos" entry (file+olp+datetree "~/org/dailies.org" "dailies")
           "* Planned  %?\n  %U\n %a\n %i" :empty-lines 0 :time-prompt t)
      ;; Record what actually happens on the day we're doing things
      ("da" "Daily activities" entry (file+olp+datetree "~/org/dailies.org" "dailies")
           "* %U  %i \ \n" :empty-lines 0)

      ;; Location for useful/interesting code snippits
      ("c" "Capture code snippet"
        entry (file+olp "~/org/code.org" "Snippets")
           "* %^{What is this?} :%^{Language|clojure|emacs-lisp|python}:  %?\n  %U\n  %a\n #+begin_src %\\2 \n %i \n #+end_src\n" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/org/journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
      
      ("k" "Kata Capture")
      ("kk" "Daily Kata" entry
       (file+olp+datetree "~/org/kata.org")
        "\n* %<%I:%M %p> - Kata :kata:\n\n%?\n\n"
        :clock-in :clock-resume
        :empty-lines 1)
      
      ("i" "Improvement Ideas")
      ("ii" "Tooling Improvement" entry (file+olp "~/org/improvement.org" "Tooling")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
      ("il" "Learning Improvement" entry (file+olp "~/org/improvement.org" "Learning")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("m" "Metrics Capture")
      ("ms" "Sleep" table-line (file+headline "~/org/metrics.org" "Sleep")
       "| %U | %^{How Are You Feeling} | %^{Sleep/Wake?} |" :kill-buffer t)
      
      ("jm" "Meeting" entry
           (file+olp+datetree "~/org/journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)
      ("jl" "WorkLogs" entry
           (file+olp+datetree "~/org/journal.org")
           "* %<%I:%M %p> - %a :Logs:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 0)
      
      ("w" "Workflows")
      ("we" "Checking Email" entry (file+olp+datetree "~/org/journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)
      
      
      ("mw" "Weight" table-line (file+headline "~/org/metrics.org" "Weight")
       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)

      ("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/org/tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)))

(define-key global-map (kbd "C-c k")
(lambda () (interactive) (org-capture nil "kk")))

(define-key global-map (kbd "C-c t")
(lambda () (interactive) (org-capture nil "ii")))

(define-key global-map (kbd "C-c j")
(lambda () (interactive) (org-capture nil "jj")))

(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
;; Fold log entries into drawer
(setq org-log-into-drawer t)

(setq org-agenda-files '("~/org/tasks.org"
                         "~/org/birthdays.org"
                         "~/org/habits.org"
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
(defun sloth/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/Emacs.org"))
    
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle)))) 

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'sloth/org-babel-tangle-config)))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)
    (sql . t)
    (clojure . t)
    (shell . t)
    (plantuml . t)
    (go . t)
    (js . t)
    (dot . t)
    (C . t)
    (java . t)
    (lua . t)))

  (setq org-confirm-babel-evaluate nil)

(setq org-babel-clojure-backend 'cider)

(if (string= system-type "darwin")
     (setq org-plantuml-jar-path
       (expand-file-name "/usr/local/bin/plantuml.jar"))
     (setq org-plantuml-jar-path
       (expand-file-name "/usr/bin/plantuml.jar")))

(use-package ob-go)

(setq org-babel-after-execute-hook 'org-display-inline-images)

;; This is needed as of Org 9.2
(require 'org-tempo)

(setq org-structure-template-alist
      '(;; Basic org templates
	      ("a" . "export ascii")
        ("cen" . "center")
        ("C" . "comment")
        ("e" . "example")
        ("E" . "export")
        ("h" . "export html")
        ("l" . "export latex")
        ("q" . "quote")
        ("s" . "src")
        ("v" . "verse")
	      ;; Specific programming languages
	      ("sq" . "src sql :engine postgres :dbhost localhost :dbport 5432 :dbuser postgres :dbpassword passwd :database foodb")
	      ("lu" . "src lua :results output")
        ("sh" . "src shell :results verbatim")
        ("c" . "src C :includes <stdio.h>")
        ("el" . "src emacs-lisp")
        ("py" . "src python :results output")
        ("go" . "src go :imports `(\"fmt\")")
        ("clj" . "src clojure")
        ("js" . "src js")
	      ;; Configuration languages
        ("d" . "src dockerfile")
        ("yl" . "src yaml")
        ("conf" . "src conf")
	      ;; Diagram languages
	      ("pl" . "src plantuml :file diagram.png")
        ("dot" . "src dot :file dotDiagram.png :exports both")))

(defun sloth/org-mode-visual-fill ()
  (setq visual-fill-column-width 125
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . sloth/org-mode-visual-fill))

(use-package org-roam
	     :ensure t
	     :init
	     (setq org-roam-v2-ack t)
	     :custom
	     (org-roam-directory "~/org/roam")
	     (org-roam-completion-everywhere t)
	     :config
	     (org-roam-setup)
	     (setq org-roam-node-display-template
		   (concat "${title:*} "
              (propertize "${tags:50}" 'face 'org-tag)))
	     (setq org-roam-dailies-capture-templates
		    '(("d" "default" entry "* PLAN %<%I:%M %p>: %?"
         :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n* PLAN Are we feeling strong? 💪")))))

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun org-roam-insert-image ()
  "Fuzzy search for a picture in ~/org/roam/pics and insert it into an org document."
  (interactive)
  (let ((image-files (directory-files-recursively "~/org/roam/pics" "\\.\\(png\\|jpg\\|jpeg\\|gif\\|webp\\)$")))
    (ivy-read "Select image to insert: " 
              (mapcar (lambda (path) (cons (file-name-nondirectory path) path)) image-files)
              :action (lambda (x)
                        (insert (format "#+ATTR_ORG: :width 750\n[[file:%s]]" (cdr x)))))))

(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))

(use-package org-roam-ui)

(if (eq system-type 'darwin)
    (custom-set-faces
     '(org-level-1 ((t (:inherit outline-1 :height 1.1))))
     '(org-level-2 ((t (:inherit outline-2 :height 1))))))

(use-package nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package term
:config
(setq explicit-shell-file-name "zsh")
(setq explicit-zsh-args '())
(setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  ;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

(use-package dired
;; dired is part of emacs proper so no need to ensure
  :ensure nil
  :init (setq dired-auto-revert-buffer  (lambda (_dir) (null (cdr dired-subdir-alist))))
  :commands (dired dired-jump)
  ;; Jump out of visited file
  :bind (("C-x C-j" . dired-jump))
  ;; `ls` options passed to dir
  :custom (
	   (dired-listing-switches "-agho --group-directories-first")
	   )
  :config
  ;; navigate our dired buffers as if we were using lf
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer)
  ;; When on mac, tell dired to use gls
  (when (string= system-type "darwin")
  (setq dired-use-ls-dired t
        insert-directory-program "gls")))

(require 'dired-single)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(defun sloth/copy-file-path-to-clipboard ()
  "Copy the filepath for the file open in a buffer/dired to clipboard"
  (interactive)
  (let ((file-path (if (equal major-mode 'dired-mode)
                       (dired-get-file-for-visit)
                     (buffer-file-name))))
    (if file-path
        (progn
          (kill-new file-path)
          (message "Copied: '%s'" file-path))
      (message "No file is associated with this buffer."))))
