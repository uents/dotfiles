;;;;
;;;; @flie  ~/.emacs.d/init.el
;;;; @brief GNU Emacs configuration file (for version 24.1+)
;;;;

;;;;--------------------------------------------------------
;;;; General
;;;;--------------------------------------------------------

;;; Japanese Environment
;;; 他のあらゆる設定よりも必ず前で行う
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)

;;; network proxy
;(setq url-proxy-services
;	  '(("http" . "example.com:8080")
;		("https" . "example.com:8080")
;		("ftp" . "example.com:8080")
;		("no_proxy" . "127.0.0.1")))

;;; inhibit loading "default.el" at startup
(setq inhibit-default-init t)

;;; 起動メッセージを非表示
(setq inhibit-startup-screen t)

;;; バックアップファイルの作成を禁止
(setq backup-inhibited t)

;;; 終了時に自動保存ファイルを削除
(setq delete-auto-save-files t)

;;; バッファ末尾に余計な改行コードを防ぐ
(setq next-line-add-newlines nil)

;;; バッファ保存時に無駄な行末の空白を削除
;(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; デフォルトのタブ幅
(setq-default tab-width 4)

;;;;--------------------------------------------------------
;;;; package management
;;;;--------------------------------------------------------

;;; package
(when (require 'package nil t)
;  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://stable.melpa.org/packages/") t)
  (package-initialize))

(defun my/install-favorite-packages (arg)
  "Install favorite packages."
  (interactive "p")
  (package-refresh-contents)
  (dolist (pkg my/package-recipe)
	(unless (package-installed-p pkg)
	  (package-install pkg)))
  (message "Finished !!"))

;; パッケージを一括取得
(defvar my/package-recipe
  '(
	use-package
	bind-key
	helm
;	helm-company
	helm-ag
	helm-gtags
	company
	yasnippet
	magit
	markdown-mode
;	textile-mode
;	js2-mode
;	flymake-cursor
;	flymake-jshint
;	web-mode
;	slime
;	geiser
	))

;;; quelpa
;(require 'quelpa nil t)

;;; cask
;(when (require 'cask nil t)
;  (cask-initialize))
;; ローカルディレクトリにインストールした場合は以下のようにパスを通す
; (require 'cask "~/.cask/cask.el")


;;;;--------------------------------------------------------
;;;; Look & Feel
;;;;--------------------------------------------------------

;;; 時間を表示
(display-time)

;;; 行番号・列番号を表示
(line-number-mode 1)
(column-number-mode 1)
(global-linum-mode 0)

;;; 対応する括弧を強調表示
(setq show-paren-delay 0) ;; 0秒(遅延なし)で表示
(show-paren-mode t)

;;; 選択領域を可視化
(setq-default transient-mark-mode t)

;;; 長い行は折り返して表示
(setq truncate-lines t)

;; メニューバーを非表示
(menu-bar-mode 0)

(when window-system
  (menu-bar-mode 1)       ;; メニューバーを表示
  (tool-bar-mode 0)       ;; ツールバーを非表示
  (scroll-bar-mode 0)     ;; スクロールバーを非表示
  (setq-default line-spacing .1)  ;; 行間を指定

  ;; タイトル
  (setq frame-title-format (concat "%b"))
  ;; デフォルトのフレームサイズ
  (setq default-frame-alist
		'((width . 108) (height . 40)))

  ;; フォント
  ;; (pp (font-family-list)) でfont-familyの出力可能
  (set-face-attribute 'default nil
					  :family "Go Mono" :height 130)

  (set-fontset-font t 'japanese-jisx0208
					(font-spec :family "Noto Sans Mono CJK JP"))

  ;; テーマ
  (load-theme 'deeper-blue t)
  )

;; 黒背景のターミナル起動で見やすくする
;(add-hook 'tty-setup-hook
;		  '(lambda ()
;			 (set-terminal-parameter nil 'background-mode 'dark)))

;;;;--------------------------------------------------------
;;;; use-package
;;;;--------------------------------------------------------

;; use-package導入前にエラーしないように
(unless (require 'use-package nil t)
  (defmacro use-package (&rest args)))

;;;;--------------------------------------------------------
;;;; bind-key
;;;;--------------------------------------------------------
(use-package bind-key
  :init
  (bind-key "C-h" 'backward-delete-char)
  (bind-key "M-@" 'mark-sexp)
  (bind-key "C-\\" 'ignore)
  (bind-key [?¥] [?\\])  ;; ¥の代わりに\を入力
  )

;;;;--------------------------------------------------------
;;;; input method
;;;;--------------------------------------------------------
(setq default-input-method "")

;;; mozc
(use-package mozc
  :disabled t
  :init
  (setq default-input-method "japanese-mozc")
  (bind-key "C-\\" 'toggle-input-method))

;;; anthy
;(add-to-list 'load-path "/usr/share/emacs/site-lisp/anthy") ;; for vine 5.2
(use-package anthy
;  :disabled t
  :init
  (setq default-input-method "japanese-anthy")
  (bind-key "C-\\" 'toggle-input-method)
  ;; Cant't activate input method 'japanese-anthy' と表示される場合必要
;  (load-library "leim-list")
  )

;;;;--------------------------------------------------------
;;;; whitespace
;;;;--------------------------------------------------------

(use-package whitespace
  :if window-system
  :config
  (setq whitespace-style '(face           ; faceで可視化
						   trailing       ; 行末
						   tabs           ; タブ
						   empty          ; 先頭/末尾の空行
						   space-mark     ; 表示のマッピング
						   tab-mark
						   ))
  (setq whitespace-display-mappings
		'(
		  ;(space-mark ?\u3000 [?\u25a1])
		  (space-mark ?\u3000 [?\uff3f])
		  ;; WARNING: the mapping below has a problem.
		  ;; When a TAB occupies exactly one column, it will display the
		  ;; character ?\xBB at that column followed by a TAB which goes to
		  ;; the next TAB column.
		  ;; If this is a problem for you, please, comment the line below.
		  (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])
		  ))

  ;; スペースは全角のみを可視化
  ;; (setq whitespace-space-regexp "\\(\u3000+\\)")

  ;; カラー設定
  (set-face-attribute 'whitespace-trailing nil
					  :background nil
					  :foreground "DeepPink"
					  :underline nil
					  )
  (set-face-attribute 'whitespace-tab nil
					  :background nil
					  :foreground "LightSkyBlue"
					  :underline nil
					  )
  (set-face-attribute 'whitespace-space nil
					  :background nil
					  :foreground "GreenYellow"
					  :weight 'bold
					  )
  (set-face-attribute 'whitespace-empty nil
					  :background nil
					  )

  (global-whitespace-mode 1))

;;;;--------------------------------------------------------
;;;; helm
;;;;--------------------------------------------------------

;;; helm
(use-package helm-config
  :init
  (helm-mode 1)
  (setq helm-idle-delay 0.1)
  (bind-key "M-x" 'helm-M-x)
  (bind-key "M-i" 'helm-imenu)
  (bind-key "C-x b" 'helm-for-files)
  (bind-key "C-x C-f" 'helm-find-files)
  ;; helm-find-files実行時にTABで手動補完、C-hで文字消去
  (bind-keys :map helm-find-files-map
			 ("TAB" . helm-execute-persistent-action)
			 ("C-h" . delete-backward-char)))

;;; helm-company
(use-package helm-company
  :init)

;;; helm-ag
(use-package helm-ag
  :init
  (bind-key "M-g f" 'helm-do-ag-this-file)
  (bind-key "M-g a" 'helm-do-ag)
  (bind-key "M-g q" 'helm-ag-pop-stack))


;;; helm-gtags
(use-package helm-gtags
  :defer t
  :config
  (setq helm-gtags-ignore-case nil)
  (bind-keys :map helm-gtags-mode-map
			 ("M-t" . helm-gtags-find-tag)
			 ("M-r" . helm-gtags-find-rtag)
			 ("M-s" . helm-gtags-find-symbol)
			 ("C-q" . helm-gtags-pop-stack)
			 ("C-c C-f" . helm-gtags-find-files)))

;; ;;; helm-c-moccur
;; (use-package helm-c-moccur
;;   :config
;;   ;; helm-c-moccur-dmoccur などのコマンドでバッファの情報をハイライトする
;;   (setq helm-c-moccur-higligt-info-line-flag t)
;;   ;; 現在選択中の候補の位置を他のwindowに表示する
;;   (setq helm-c-moccur-enable-auto-look-flag t)
;;   ;; helm-c-moccur-occur-by-moccur の起動時にポイントの位置の単語を初期パターンにする
;;   (setq helm-c-moccur-enable-initial-pattern t)
;;   ;; key bindings
;;   (define-key global-map (kbd "M-o") 'helm-c-moccur-occur-by-moccur) ;バッファ内検索
;;   (define-key global-map (kbd "M-d") 'helm-c-moccur-dmoccur) ;ディレクトリ
;;   (define-key global-map (kbd "s-o") 'helm-c-moccur-buffer-list)
;;   (define-key isearch-mode-map (kbd "M-o") 'helm-c-moccur-from-isearch)
;; ;
;; ;  (defun dired-mode-hook-for-helm-c-moccur ()
;; ;	(local-set-key (kbd "O") 'helm-c-moccur-dired-do-moccur-by-moccur))
;; ;  (add-hook 'dired-mode-hook 'dired-mode-hook-for-helm-c-moccur)
;; )


;;;;--------------------------------------------------------
;;;; company
;;;;--------------------------------------------------------

(use-package company
  :init
  (global-company-mode)

  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)

  ;; face color
  (set-face-attribute 'company-tooltip nil
					  :foreground "black"
					  :background "lightgrey")
  (set-face-attribute 'company-tooltip-common nil
					  :foreground "black"
					  :background "lightgrey")
  (set-face-attribute 'company-tooltip-common-selection nil
					  :foreground "white"
					  :background "steelblue")
  (set-face-attribute 'company-tooltip-selection nil
					  :foreground "black"
					  :background "steelblue")
  (set-face-attribute 'company-preview-common nil
					  :background nil
					  :foreground "lightgrey"
					  :underline t)
  (set-face-attribute 'company-scrollbar-fg nil
					  :background "orange")
  (set-face-attribute 'company-scrollbar-bg nil
					  :background "gray40")

  ;; key bindings
  (bind-keys :map company-active-map
			 ("C-n" . company-select-next)
			 ("C-p" . company-select-previous)
			 ("C-:" . helm-company))
  (bind-keys :map company-search-map
			 ("C-n" . company-select-next)
			 ("C-p" . company-select-previous)
			 ("C-:" . helm-company))
  )

;(setq company-tern-property-marker "")
;(defun company-tern-depth (candidate)
;  "Return depth attribute for CANDIDATE. 'nil' entries are treated as 0."
;  (let ((depth (get-text-property 0 'depth candidate)))
;    (if (eq depth nil) 0 depth)))
;(add-hook 'js2-mode-hook 'tern-mode) ; 自分が使っているjs用メジャーモードに変える
;(add-to-list 'company-backends 'company-tern) ; backendに追加

;;;;--------------------------------------------------------
;;;; yasnippet
;;;;--------------------------------------------------------

(use-package yasnippet
  :init
  (yas-global-mode 1))


;;;;--------------------------------------------------------
;;;; auto complete
;;;;--------------------------------------------------------

;; (use-package auto-complete-config
;;   :config
;;   (add-to-list 'ac-modes 'js2-mode)
;;   (ac-config-default)
;;   (setq ac-auto-start 3)
;;   (setq ac-dwim t)
;;   (setq ac-use-menu-map t) ;; C-n/C-pで候補選択可能
;;   (setq ac-comphist-file "~/.emacs.d/local/ac-comphist.dat") ;; 補完履歴のキャッシュ先
;;   (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")) ;; ユーザー辞書の格納先


;;;;--------------------------------------------------------
;;;; magit
;;;;--------------------------------------------------------

(use-package magit
  :init
  (bind-key "C-x g" 'magit-status)
  (bind-key "C-x M-g" 'magit-dispatch-popup)

  :config
  (defvar my/magit-background-color "202020")
  (set-face-attribute 'magit-diff-added nil
  					  :background my/magit-background-color)
  (set-face-attribute 'magit-diff-removed nil
  					  :background my/magit-background-color)
  (set-face-attribute 'magit-diff-context-highlight nil
  					  :background my/magit-background-color)
  (set-face-attribute 'magit-diff-added-highlight nil
  					  :background my/magit-background-color)
  (set-face-attribute 'magit-diff-removed-highlight nil
  					  :background my/magit-background-color)

  (setq auto-mode-alist (remove (rassoc 'git-rebase-mode auto-mode-alist)
								auto-mode-alist))
  )

;;;;----------------------------------------------------------
;;;; Markdown
;;;;----------------------------------------------------------

;;; markdown-mode
(use-package markdown-mode
  :mode (("\\.md\\'" . gfm-mode)
		 ("\\.markdown\\'" . gfm-mode))
  :config
;  (custom-set-variables
;   '(markdown-command
;	 "pandoc -f markdown -t html -s --mathjax --highlight-style pygments"))
  )

;;;;----------------------------------------------------------
;;;; Textile
;;;;----------------------------------------------------------

;;; textile-mode
(use-package textile-mode
  :mode (("\\.textile\\'" . textile-mode))
  :config
  )


;;;;--------------------------------------------------------
;;;; GDB/GUD
;;;;--------------------------------------------------------

(defvar gud-gdb-history (list "gdb --annotate=3 "))
(setq gdb-many-windows t)
(setq gdb-use-sparate-io-buffer t)

(add-hook 'gdb-mode-hook
		  '(lambda ()
			 (gud-tooltip-mode t) ;; マウスオーバー時に値を表示
			 (gud-break-main nil)
			 (gud-run nil)))

;;;;--------------------------------------------------------
;;;; C
;;;;--------------------------------------------------------

(add-hook 'c-mode-hook
		  '(lambda()
			 ;; helm-gtgsを有効に
			 (helm-gtags-mode t)
			 ;; flymakeを有効に
;			 (flymake-mode t)

			 ;; [TAB]キーの振舞い
			 ;;   t => [TAB]キーでインデントを空ける
			 ;; nil => [TAB]キーで[TAB]コードを入力する
			 (setq c-tab-always-indent . nil)
			 ;; タブ幅の設定
			 (setq tab-width 8)
			 ;; インデント量の設定
			 (setq c-basic-offset 8)
			 (setq c-comment-only-line-offset . 0)
			 ))

;;;;--------------------------------------------------------
;;;; JavaScript
;;;;--------------------------------------------------------

;;; js2-mode
(use-package js2-mode
  :defer t
  :mode (("\\.js\\'" . js2-mode))
  :config
  (bind-keys :map js2-mode-map
			 ("M-s" . js-find-symbol)
;			 ("C-q" . pop-tag-mark)
			 )
  )

;;; flymake-jshint
(use-package flymake-jshint
  :defer t
  :config
  (setq jshint-configuration-path "~/.jshintrc")
  (setq help-at-pt-timer-delay 0.9)
  (setq help-at-pt-display-when-idle '(flymake-overlay))
  (bind-keys :map flymake-mode
			 ("C-c C-n" . flymake-goto-next-error)
			 ("C-c C-p" . flymake-goto-prev-error)))


;;;;--------------------------------------------------------
;;;; HTML/CSS
;;;;--------------------------------------------------------

;;; web-mode
(use-package web-mode
  :defer t
  :mode (("\\.html\\'" . web-mode))
  :config
  (add-hook 'web-mode-hook
		(lambda()
		  ;; indent offset
		  (setq web-mode-markup-indent-offset 2)
		  (setq web-mode-css-indent-offset 2)
		  (setq web-mode-code-indent-offset 2)
		  ;; face color
		   (custom-set-faces
			'(web-mode-doctype-face
			  ((t (:foreground "#00CCCC"))))
			'(web-mode-html-tag-face
			  ((t (:foreground "#6666FF"))))
			'(web-mode-html-attr-name-face
			  ((t (:foreground "#CC3333"))))
			'(web-mode-html-attr-value-face
			  ((t (:foreground "#FF66FF"))))
			'(web-mode-comment-face
			  ((t (:foreground "#00CC00"))))
			'(web-mode-server-comment-face
			  ((t (:foreground "#00CC00"))))
			'(web-mode-css-rule-face
			  ((t (:foreground "#A0D8EF"))))
			'(web-mode-css-pseudo-class-face
			  ((t (:foreground "#FF7F00"))))
			'(web-mode-css-at-rule-face
			  ((t (:foreground "#FF7F00"))))
			)
		   )))

;;;;--------------------------------------------------------
;;;; Common Lisp
;;;;--------------------------------------------------------

;;; slime
(use-package slime
  :disabled t
  :config
  ;; slimeで実行するインタプリタを設定
  (setq inferior-lisp-program "/usr/local/bin/clisp")
  (add-hook 'lisp-mode-hook 'slime-mode))

;;;;--------------------------------------------------------
;;;; Scheme
;;;;--------------------------------------------------------

;;; geiser
(setq geiser-racket-binary "/opt/homebrew-cask/Caskroom/racket/6.1.1/Racket v6.1.1/bin/racket")
(setq geiser-active-implementations '(racket))
(setq geiser-repl-read-only-prompt-p nil)

;;; racket-mode
(use-package racket-mode
  :disabled t
  :mode (("\\.scm\\'" . racket-mode))
  :config
  )

;;;;----------------------------------------------------------
;;;; Ruby
;;;;----------------------------------------------------------

;; ;;; enh-ruby-mode
;; (autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
;; (add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
;; (add-to-list 'auto-mode-alist '("Capfile$" . enh-ruby-mode))
;; (add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
;; (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

;; (add-hook 'enh-ruby-mode-hook
;;           '(lambda ()
;;              (setq tab-width 2)
;;              (setq ruby-indent-level tab-width)
;;              (setq ruby-deep-indent-paren-style nil)
;;              (define-key enh-ruby-mode-map [return]
;; 			   'ruby-reindent-then-newline-and-indent)))

;; ;;; ruby-end
;; (add-hook 'enh-ruby-mode-hook 'ruby-end-mode)

;; ;;; inf-ruby
;; (autoload 'inf-ruby "inf-ruby" "Run an inferior ruby process" t)
;; (add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)

;; ;;; robe-mode
;; (add-hook 'enh-ruby-mode-hook
;; 		  '(lambda ()
;; 			 (robe-mode)
;; 			 (robe-ac-setup)
;; 			 (inf-ruby-keys)))

;; ;;; smart-compile
;; (require 'smart-compile)
;; (setq compilation-window-height 15) ;; default window height is 15
;; (add-hook 'enh-ruby-mode-hook
;;           '(lambda ()
;; 			 (define-key enh-ruby-mode-map (kbd "C-c c")
;; 			   'smart-compile)
;; 			 (define-key enh-ruby-mode-map (kbd "C-c C-c")
;; 			   (kbd "C-c c C-m"))))
