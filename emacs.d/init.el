;;; init.el --- My emacs config.

;;; Commentary:

;; Sources
;;
;; Aaron Bedra (https://github.com/abedra/emacs.d/blob/master/abedra.el)
;; Steve Yegge (https://sites.google.com/site/steveyegge2/effective-emacs)
;; Johan Andersson (https://github.com/rejeep/emacs/blob/master/init.el)
;; lunaryorn


;;; Package management

;; Please don't load outdated byte code
(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      ;; Package archives, the usual suspects
      '(("GNU ELPA"     . "http://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      ;; Prefer MELPA Stable over GNU over MELPA.  IOW prefer MELPA's stable
      ;; packages over everything and only fall back to GNU or MELPA if
      ;; necessary.
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 0))
      ;; Pin a couple of packages to MELPA
      package-pinned-packages
      '(;; Last release was a long time ago for these
        ("ivy"       . "MELPA")
        ("ivy-hydra" . "MELPA")
        ("counsel"   . "MELPA")
        ("swiper"    . "MELPA")
	)
      )

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; Requires

(require 'use-package)
(require 'time-date)

;; Paths

(if (eq system-type 'darwin)
    (setq dropbox-path (concat (getenv "HOME") "/Dropbox"))
  (setq dropbox-path "/storage/Dropbox")
  )

(add-to-list 'exec-path (concat (getenv "HOME") "/.npm-global/bin"))
(add-to-list 'exec-path (concat (getenv "HOME") "/.miniconda/bin"))

;; My Additions
;; Includes some functions used downstream in this file
(use-package jmm
  :load-path "lisp/"
  :bind (("C-x C-l" . next-workout-log-file-jmm)
         ))

;; External Package Config
(use-package seq
  :ensure t)

(use-package validate                   ; Validate options
  :ensure t)

(use-package tern
  :ensure t
  :load-path "../.npm-global/lib/node_modules/tern/emacs"
  )

(use-package hl-line
  :config (set-face-background 'hl-line "#073642")
  (global-hl-line-mode 1))

(use-package clojure-mode)


(use-package ivy                        ; Minibuffer completion
  :ensure t
  :init (ivy-mode 1)
  :bind (("C-c b r" . ivy-resume))
  :config
  ;; Include recentf and bookmarks to switch buffer, and tune the count format.
  (validate-setq ivy-use-virtual-buffers t
                 ivy-count-format "(%d/%d) "))

(use-package ivy-hydra                  ; Hydra bindings for ivy buffer
  :ensure t
  :after ivy
  :bind (:map ivy-minibuffer-map
	      ("C-o" . hydra-ivy/body)))

(use-package counsel                    ; Ivy-powered commands
  :ensure t
  :bind (([remap execute-extended-command] . counsel-M-x)
         ([remap find-file] . counsel-find-file)
         ([remap describe-function] . counsel-describe-function)
         ([remap describe-variable] . counsel-describe-variable)
         ([remap info-lookup-symbol] . counsel-info-lookup-symbol)
         ("C-c f L" . counsel-load-library)
         ("C-c i 8" . counsel-unicode-char)
         ("C-c s a" . counsel-ag)
         ("C-c j t" . counsel-imenu)))

(use-package magit                      ; The one and only Git frontend
  :ensure t
  :bind (("C-c g c" . magit-clone)
         ("C-c g s" . magit-status)
         ("C-c g b" . magit-blame)
         ("C-c g l" . magit-log-buffer-file)
         ("C-c g p" . magit-pull))
  :config
  ;; Shut up, Magit
  (validate-setq
   magit-completing-read-function #'ivy-completing-read
   magit-save-repository-buffers 'dontask
   magit-refs-show-commit-count 'all
   ;; Use separate buffers for one-file logs so that we don't need to reset
   ;; the filter everytime for full log view
   magit-log-buffer-file-locked t
   ;; This is creepy, Magit
   magit-revision-show-gravatars nil
   ;; Show status buffer in fullscreen
   magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
   ))

;; monky blows up on the mac in hg server mode
(if (eq system-type 'darwin)
    (use-package monky
      :ensure t
      :config
      (validate-setq monky-process-type nil)
      )
  (use-package monky
    :ensure t
    :config
    (validate-setq monky-process-type 'cmdserver)
    )
  )

(use-package company                    ; Graphical (auto-)completion
  :ensure t
  :defer 1
  :config
  (global-company-mode)

  (validate-setq
   company-tooltip-align-annotations t
   company-tooltip-flip-when-above t
   ;; Easy navigation to candidates with M-<n>
   company-show-numbers t)
  :diminish company-mode)

(use-package company-quickhelp          ; Show help in tooltip
  :ensure t
  :after company
  :config (company-quickhelp-mode))

(use-package company-statistics         ; Sort company candidates by statistics
  :ensure t
  :after company
  :config (company-statistics-mode))

;; (use-package company-jedi
;;   :config
;;   (add-to-list 'company-backends 'company-jedi))

(use-package company-tern
  :ensure t
  :config
  (add-to-list 'company-backends 'company-tern))

(use-package deft
  :ensure t
  :config
  (setq deft-extensions '("txt" "tex" "org")
	deft-directory (concat dropbox-path "/notes")
	deft-recursive t)
  )

(use-package buffer-move
  :ensure t
  :config
  (progn
    (global-set-key (kbd "<s-up>")     'buf-move-up)
    (global-set-key (kbd "<s-down>")   'buf-move-down)
    (global-set-key (kbd "<s-left>")   'buf-move-left)
    (global-set-key (kbd "<s-right>")  'buf-move-right)
    )
  )

(use-package term
  :ensure t
  :config
  (add-hook
   'term-mode-hook
   (lambda() (setq show-trailing-whitespace nil)))
  )

;;(add-hook 'buffer-menu-hook (lambda() (setq show-trailing-whitespace nil)))


(use-package flycheck
  :ensure t
  :config
  (progn
    (setq flycheck-display-errors-function nil)
    (add-hook 'after-init-hook 'global-flycheck-mode))
  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers
			'(javascript-jshint)))
  )

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme)
  (validate-setq powerline-default-separator 'contour)
  )

(use-package elfeed
  :ensure t
  :config
  (progn
    (global-set-key (kbd "C-x w") 'elfeed)
    (setq elfeed-feeds
	'(
	  ("http://lifehacker.com/index.xml" main advice)
	  ("http://fivethirtyeight.com/features/feed/" main politics)
	  ("http://rss.nytimes.com/services/xml/rss/nyt/Upshot.xml" main upshot)

	  ("http://andrewgelman.com/feed/" main data)
	  ("http://feeds.feedburner.com/FlowingData" main data)
	  ("http://norvig.com/rss-feed.xml" main data)
	  ("http://matt.might.net/articles/feed.rss" main data)
	  ("http://andrew.gibiansky.com/feed.rss" main data)
	  ("http://sabermetricinsights.blogspot.com/feeds/posts/default?alt=rss" main data)
	  ("http://www.johndcook.com/blog/feed/" main data)
	  ("http://www.randalolson.com/feed/" main data)
	  ("http://radfordneal.wordpress.com/feed/" main data)
	  ("http://healthyalgorithms.wordpress.com/feed/" main data)

	  ("http://www.masteringemacs.org/feed/" main emacs)
	  ("http://pragmaticemacs.com/feed/" main emacs)


	  ("http://blog.megafaunasoft.com/feeds/posts/default" main brian)
	  ("http://blog.booleanbiotech.com/feeds/all.atom.xml" main brian data)

	  ("http://swannodette.github.com/atom.xml" main clojure)

	  ("http://feeds2.feedburner.com/MarksDailyApple/" main fitness diet)
	  ("http://eatingacademy.com/feed" main diet)
	  ("http://rawfoodsos.com/feed/" main diet)

	  ("http://www.catalystathletics.com/rss/index.php" main fitness)
	  ("http://mentalitywod.com/feed/" main fitness)

	  ("http://feeds.feedburner.com/zenhabits" main advice)
	  ("http://www.scottberkun.com/feed/" main advice)


	  ("http://datawod.com/feed.atom" main mine data)
	  ("http://blog.macphunk.net/feed.xml" main mine)
	  )
	(setq-default elfeed-search-filter "@1-week-ago +unread ")
	)))

;; (use-package flyspell)
;; ;; flyspell config
;; (setq flyspell-issue-welcome-flag nil)
;; (if (eq system-type 'darwin)
;;     (setq-default ispell-program-name "/usr/local/bin/aspell")
;;   (setq-default ispell-program-name "/usr/bin/aspell"))
;; (setq-default ispell-list-command "list")

;; (use-package markdown-mode
;;   :mode (("README\\.md\\'" . gfm-mode)
;;          ("\\.md\\'" . markdown-mode)
;;          ("\\.markdown\\'" . markdown-mode))


(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :config
  (progn
    (bind-key "M-n" 'open-line-below markdown-mode-map)
    (bind-key "M-p" 'open-line-above markdown-mode-map))
  :mode (("\\.markdown$" . markdown-mode)
         ("\\.mdown$" . markdown-mode)
         ("\\.md$" . markdown-mode)
	 ("\\.txt$" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  ;; :init (setq markdown-command "pandoc --smart -f markdown -t html")
  )
(add-hook 'markdown-mode-hook
          (lambda ()
            (visual-line-mode t)
            (writegood-mode t)
            (flyspell-mode t)))
;; (defvar mikemac/vendor-dir (expand-file-name "vendor" user-emacs-directory))
;; (add-to-list 'load-path mikemac/vendor-dir)
;; (setq markdown-css-paths `(,(expand-file-name "markdown.css" mikemac/vendor-dir)))

;; configure smex
;; (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
;; (smex-initialize)
;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)

(use-package ess
  :ensure t
  :defer
  )

;; (use-package smex)

(use-package elpy
  :ensure t
  :config
  (progn
    (elpy-enable)
    (remove-hook 'elpy-modules 'elpy-module-flymake)
    ))

;; (use-package blackboard-theme)
;; (use-package solarized-theme)
;; (use-package zenburn-theme)

;; (use-package color-theme-cobalt)

(use-package web-mode
  :ensure t
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))
  :config (progn
            (add-hook 'web-mode-hook
                      (lambda ()
                        (setq web-mode-enable-css-colorization t)
                        (setq web-mode-markup-indent-offset 2)
                        (setq web-mode-style-padding 2)
                        (setq web-mode-script-padding 2)))))

(use-package writegood-mode
  :ensure t)

;; (use-package uniquify
;;   :ensure t
;;   :config (setq uniquify-buffer-name-style 'forward))

;; (electric-pair-mode +1)
;; (setq
;;  electric-pair-preserve-balance t
;;  electric-pair-delete-adjacent-pairs t
;;  electric-pair-open-newline-between-pairs t
;;  electric-pair-skip-whitespace t)


;; (use-package flycheck-cask
;;   :init (add-hook 'flycheck-mode-hook 'flycheck-cask-setup))

;; use js-mode for json files
;; (use-package js-mode
;;   :ensure t
;;   :mode ("\\.json$" . js-mode)
;;   :init
;;   (progn
;;     (add-hook 'js-mode-hook (lambda () (setq js-indent-level 2)))))

(use-package js2-mode
  :ensure t
  :mode (("\\.js$" . js2-mode)
	 ("\\.json$" . js2-mode)
         ("Jakefile$" . js2-mode))
  :interpreter ("node" . js2-mode)
  :bind (;;("C-a" . back-to-indentation-or-beginning-of-line)
         ("C-M-h" . backward-kill-word))
  :config
  (progn
    (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))
    (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
    (add-hook 'js2-mode-hook (lambda ()
                               (bind-key "M-j" 'join-line-or-lines-in-region js2-mode-map)))
    ))

;; (use-package dired-x)

;; (use-package ido
;;   :init (progn (ido-mode 1)
;;                (ido-everywhere 1))
;;   :config
;;   (progn
;;     (setq ido-case-fold t)
;;     (setq ido-everywhere t)
;;     (setq ido-enable-prefix nil)
;;     (setq ido-enable-flex-matching t)
;;     (setq ido-create-new-buffer 'always)
;;     (setq ido-max-prospects 10)
;;     (setq ido-use-faces nil)
;;     (setq ido-file-extensions-order '(".rb" ".el" ".coffee" ".js"))
;;     (add-to-list 'ido-ignore-files "\\.DS_Store")))

;; (use-package ace-jump-mode
;;   :bind ("C-c SPC" . ace-jump-mode))

(use-package windmove
  :ensure t
  :config (windmove-default-keybindings 'shift))

;;; OS X support
(when (eq system-type 'darwin)
  (validate-setq
   mac-option-modifier 'meta            ; Option is simply the natural Meta
   mac-command-modifier 'meta           ; But command is a lot easier to hit
   mac-right-command-modifier 'left
   ;; FIXME: Temporarily bind right alt to control until karabiner works again
   ;; on macOS sierra and gives me by beloved return/control key combination
   ;; back
   mac-right-option-modifier 'control   ; Keep right option for accented input
   ;; Just in case we ever need these keys
   mac-function-modifier 'hyper))

;; (use-package ns-win                     ; OS X window support
;;   :defer t
;;   :if (eq system-type 'darwin)
;;   :ensure t
;;   :config
;;   (validate-setq
;;    ;; Don't pop up new frames from the workspace
;;    ns-pop-up-frames nil))

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :ensure t
  :init (exec-path-from-shell-initialize))

(use-package osx                        ;  OS X tools
  :if (eq system-type 'darwin)
  :load-path "lisp/"
  :defer t)

(use-package osx-trash                  ; Trash support for OS X
  :if (eq system-type 'darwin)
  :ensure t
  :init (osx-trash-setup))

;; (use-package flx-ido
;;   :init (flx-ido-mode 1))

;; (use-package ido-vertical-mode
;;   :init (ido-vertical-mode 1))

;; (use-package idomenu
;;   :bind ("M-i" . idomenu))

;; (use-package swoop
;;   :bind ("C-o" . swoop))

;; (use-package counsel
;;   :config
;;   (progn
;;     (ivy-mode 1)
;;     (setq ivy-use-virtual-buffers t)
;;     ;; (global-set-key "\C-s" 'swiper)
;;     (global-set-key (kbd "C-c C-r") 'ivy-resume)
;;     (global-set-key (kbd "<f6>") 'ivy-resume)
;;     (global-set-key (kbd "M-x") 'counsel-M-x)
;;     (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;;     (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;;     (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;;     (global-set-key (kbd "<f1> l") 'counsel-load-library)
;;     (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;;     (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;;     (global-set-key (kbd "C-c g") 'counsel-git)
;;     (global-set-key (kbd "C-c j") 'counsel-git-grep)
;;     ;; (global-set-key (kbd "C-c k") 'counsel-ag)
;;     (global-set-key (kbd "C-x l") 'counsel-locate)
;;     ;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;;     ))

;; (use-package swiper
;;   :bind (("\C-s" . swiper)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings


;; no splash screen, no scratch message
(setq inhibit-splash-screen t
      initial-scratch-message nil
      )


;; remap kb shortcuts as I like them (thanks to Yegge)
(global-set-key "\C-x\C-m" 'execute-extended-command)
;;(global-set-key "\C-x-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; disable this stuff
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; turn off that shit
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; common behavior fixes
(delete-selection-mode t)
(global-font-lock-mode 1)
(transient-mark-mode t)

;; always end a file with a newline
(setq require-final-newline 'query)

;; make emacs use the clipboard
;;(setq x-select-enable-clipboard t)
;;(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Don't pollute my init file!
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; show trailing whitespace
(setq-default show-trailing-whitespace t)

;; delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; indicate empty lines
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; try to fix tabs globally
(setq indent-tabs-mode nil
      tab-width 2)

;; no backup files
(setq make-backup-files nil)

;; just y or n, don't make me type 'yes'
(defalias 'yes-or-no-p 'y-or-n-p)

;; quicker to show keystrokes, disable dialog boxes
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(show-paren-mode t)

;; turn on column numbers
(setq column-number-mode t)

;; no auto save files
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; whitespace cleanup from emacs starter kit
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun cleanup-region (beg end)
  "Remove tmux artifacts from region."
  (interactive "r")
  (dolist (re '("\\\\│\·*\n" "\W*│\·*"))
    (replace-regexp re "" nil beg end)))

(global-set-key (kbd "C-x M-t") 'cleanup-region)
(global-set-key (kbd "C-c n") 'cleanup-buffer)


;; zsh files are scripts yo
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))

;; safety check
(bind-key
 "C-x C-c"
 (lambda ()
   (interactive)
   (if (y-or-n-p "Quit Emacs? ")
       (save-buffers-kill-emacs))))


;; set color theme
;; (if window-system
;;     ;; (load-theme 'solarized-light t)
;;     (load-theme 'solarized-dark t)
;;   ;; (load-theme 'blackboard t)
;;   ;; (load-theme 'zenburn t)
;;   (load-theme 'wombat t))

;; (add-to-list 'default-frame-alist
;;              '(font . "Inconsolata-12"))
;; (add-to-list 'default-frame-alist
;;              '(font . "Deja Vu Sans Mono-12"))


;; '(elfeed-feeds
;;   (quote
;;    (
;;     "http://www.perceptualedge.com/blog/?feed=rss2"
;;     "http://feeds.feedburner.com/well-formed_data"

;;     "http://www.tom-carden.co.uk/feed/"
;;     "http://anand.typepad.com/datawocky/atom.xml"
;;     "http://strangemaps.wordpress.com/feed/"
;;     "http://feeds.infosthetics.com/infosthetics"
;;     "http://prog21.dadgum.com/atom.xml"
;;     "http://lesswrong.com/.rss"
;;     "http://blog.kaggle.com/feed/"
;;     "http://xkcd.com/atom.xml"
;;     "http://slatestarcodex.com/feed/"
;;     "http://zedshaw.com/feed.xml"
;;     "http://teddziuba.com/atom.xml"
;;     "http://romainfrancois.blog.free.fr/index.php?feed/atom"
;;     "http://emacs-fu.blogspot.com/feeds/posts/default"
;;     "http://feeds.feedburner.com/MarcAndAngel"
;;     "http://benfry.com/writing/feed"
;;     "http://dmbates.blogspot.com/feeds/posts/default"
;;     "http://data-analytics-tools.blogspot.com/feeds/posts/default"

;;     "http://cscs.umich.edu/~crshalizi/weblog/index.rss"
;;     "http://matthewrocklin.com/blog/atom.xml"
;;     "http://feeds.feedburner.com/zurb/blog"

;;     "http://pyre.third-bit.com/blog/feed"
;;     "http://nlpers.blogspot.com/feeds/posts/default"
;;     "http://austinrochford.com/rss.xml"
;;     "http://earningmyturns.blogspot.com/feeds/posts/default"
;;     "http://pulpnoir.com/?feed=rss2"
;;     "http://feeds.feedburner.com/QuantifiedSelf"
;;     "http://feeds.feedburner.com/data-evolution"
;;     "http://feeds.feedburner.com/michaelnielsen/wmna"
;;     "http://feeds.feedburner.com/BrendanOConnorsBlog"
;;     "http://feeds.feedburner.com/ThisNumberCrunchingLife"
;;     "http://feeds.feedburner.com/StatisticalModelingCausalInferenceAndSocialScience"
;;     "http://clemesha.org/feeds/rss"
;;     "http://utcc.utoronto.ca/~cks/space/blog/python/?atom"

;;     "http://blogs.law.harvard.edu/philg/feed/rdf/"
;;     "http://diveintomark.org/feed/"




;;     "http://www.bbc.co.uk/blogs/markkermode/rss.xml"
;;     "http://hunch.net/?feed=rss2"
;;     "http://blog.mdda.net/atom.xml"
;;     "http://steve-yegge.blogspot.com/feeds/posts/default"
;;     "http://emacsworld.blogspot.com/feeds/posts/default"
;;     "http://www.mfasold.net/blog/feed/"
;;     "http://toddwschneider.com/atom.xml"



;;     "http://www.python.org/channews.rdf"
;;     "http://morepypy.blogspot.com/feeds/posts/default"
;;     "http://planet.scipy.org/atom.xml"
;;     "http://www.artima.com/buzz/feeds/python.rss"
;;     "http://www.pythonware.com/daily/rss.xml"


;;     "http://fivethirtyeight.blogs.nytimes.com/feed/"

;;     "http://www.whitehouse.gov/feed/blog"
;;     "http://feeds.feedburner.com/thehealthyskeptic"
;;     "http://chrispconstantlyvaried.blogspot.com/feeds/posts/default"
;;     "http://thatpaleoguy.com/feed/"
;;     "http://robbwolf.libsyn.com/rss?feed=podcast/"
;;     "http://www.archevore.com/panu-weblog/atom.xml"
;;     "http://www.alanaragonblog.com/feed/"
;;     "http://www.cavemandoctor.com/feed/"
;;     "http://garytaubes.com/feed/"
;;     "http://perfecthealthdiet.com/?feed=rss2"
;;     "http://feeds.feedburner.com/drmikenutritionblog"

;;     "http://wholehealthsource.blogspot.com/feeds/posts/default"
;;     "http://anthonycolpo.com/?feed=rss2"
;;     "http://feeds.feedburner.com/RobbWolfThePaleoSolution"
;;     "http://www.crossfit.com/index1.xml"
;;     "http://www.jesliao.com/feeds/posts/default"
;;     "http://www.mobilitywod.com/feed"

;;     "http://www.crossfitactiveperformance.com/index.php/recommended-resources/blog?format=feed&type=rss"
;;     "http://crossfitmobile.blogspot.com/feeds/posts/default"
;;     "http://www.evatstrengthandconditioning.com/category/blog/feed/"
;;     "http://norcalcrossfit-fuel.blogspot.com/feeds/posts/default"
;;     "http://www.tabatatimes.com/feed/"
;;     "http://outlawway.wpengine.com/feed/"
;;     "http://therxreview.com/feed/"
;;     "http://akicloherty.com/feed/"
;;     "http://xfit2011.blogspot.com/feeds/posts/default"
;;     "http://teddykim.com/feed/"

;;     "http://graycook.com/?feed=rss2"
;;     "http://theeternalpursuit.com/feed/"
;;     "http://cfganalysis.blogspot.com/feeds/posts/default"
;;     "http://blog.sevanmatossian.com/?feed=rss2"
;;     "http://journal.crossfit.com/rss.xml"
;;     "http://glennpendlay.wordpress.com/feed/"

;;     "http://feeds.feedburner.com/bigdawgblog"

;;     "http://70sbig.com/feed/"
;;     "http://danjohn.net/feed/"
;;     "http://www.datawod.com/feed"
;;     "http://marcusfilly.com/feed/"
;;     "http://www.crossfitinvictus.com/feed/"

;;     "http://stanfordcehg.wordpress.com/feed/"
;;     "http://www.thegeneticgenealogist.com/feed/"
;;     "http://spittoon.23andme.com/feed/atom/"
;;     "http://feeds.feedburner.com/scienceblogs/gnxp"
;;     "http://www.iq.harvard.edu/blog/sss/atom.xml"
;;     "http://www.wired.com/wiredscience/category/genetic-future/feed"
;;     "http://feeds.feedburner.com/GenomesUnzipped"
;;     "http://www.genomeweb.com/newsletter/69886/feed"
;;     "http://www.genetic-future.com/feeds/posts/default"
;;     "http://www.cell.com/rssFeed/AJHG/rss.NewArticles.xml"
;;     "http://www.nature.com/nrg/current_issue/rss"
;;     "http://www.nature.com/nature/journal/vaop/ncurrent/rss.rdf"
;;     "http://www.genetics.org/rss/Genetics_of_complex_traits.xml"
;;     "http://onlinelibrary.wiley.com/rss/journal/10.1002/(ISSN)1098-2272"
;;     "http://www.genetics.org/rss/Population_and_evolutionary_genetics.xml"
;;     "http://www.plosgenetics.org/article/feed"
;;     "http://genome.cshlp.org/rss/ahead.xml"
;;     "http://www.ploscompbiol.org/article/feed"
;;     "http://mbe.oxfordjournals.org/rss/current.xml"
;;     "http://evmedreview.com/?feed=rss2"
;;     "http://biology.plosjournals.org/perlserv/?request=get-rss&issn=1545-7885&type=new-articles"
;;     "http://www.nature.com/ng/current_issue/rss/"
;;     "http://peakenergy.blogspot.com/feeds/posts/default"
;;     "https://www.dropbox.com/3237645/5150745/kpLzlzxWRKp1np3qkRYRjrQ4gqoTY2kI7bERgeHP/events.xml"
;;     "http://nullprogram.com/feed/"
;;     "http://www.terminally-incoherent.com/blog/feed/"
;;     )))
