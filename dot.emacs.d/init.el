;;;;
;;;; @flie  ~/.emacs.d/init.el
;;;; @brief GNU Emacs configuration file (ver 23.3 or later)
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
;		("no_proxy" . "127.0.0.1")))

;;; inhibit loading "default.el" at startup
(setq inhibit-default-init t)

;;; 起動メッセージを非表示
;(setq inhibit-startup-screen t)

;;; バックアップファイルの作成を禁止
(setq backup-inhibited t)

;;; 終了時に自動保存ファイルを削除
(setq delete-auto-save-files t)

;;; バッファ末尾に余計な改行コードを防ぐ
(setq next-line-add-newlines nil)

;;; デフォルトのタブ幅
(setq-default tab-width 4)

;;; key bindings
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key "\C-\\" 'ignore)
(global-set-key [?¥] [?\\])  ;; ¥の代わりに\を入力


;;;;--------------------------------------------------------
;;;; input method
;;;;--------------------------------------------------------

;;; mozc
;; (require 'mozc)
;; (load-file "/usr/local/share/emacs/23.4/site-lisp/mozc.el")
;; (setq default-input-method "japanese-mozc")
;; (global-set-key (kbd "C-\\") 'toggle-input-method)

;;; anthy
;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/anthy")
;; (require 'anthy)
;; (setq default-input-method "japanese-anthy")
;; ;; Cant't activate input method 'japanese-anthy' と表示される場合必要
;; (load-library "leim-list")


;;;;--------------------------------------------------------
;;;; Look & Feel
;;;;--------------------------------------------------------

;;; 時間を表示
(display-time)

;;; 行番号・列番号を表示
(line-number-mode 1)
(column-number-mode 1)
(linum-mode -1)

;;; スクロールバーを非表示
(scroll-bar-mode -1)

;;; 対応する括弧を強調表示
(setq show-paren-delay 0) ;; 0秒(遅延なし)で表示
(show-paren-mode t)

;;; 選択リージョン範囲を可視化
(setq-default transient-mark-mode t)

;;; 長い行は折り返す
(setq truncate-lines t)

;; メニューバーを非表示
(menu-bar-mode -1)

(when window-system
  ;; メニューバーを非表示
  (menu-bar-mode 1)
  ;; ツールバーを非表示
  (tool-bar-mode -1)
  ;; 行間を指定
  (setq-default line-spacing 0)
  ;; タイトル
  (setq frame-title-format (concat "%b"))

  (setq default-frame-alist
		'(
		  ;; ウィジェットのサイズ
		  (width . 108)
		  (height . 40)
		  ))
  ;; フォント
  ;; (pp (font-family-list)) でfont-familyの出力可能

  (set-face-attribute 'default nil
					  :family "Hack" :height 120)

  (set-fontset-font t 'japanese-jisx0208
					(font-spec :family "Noto Sans Japanese"))

  ;; theme
;  (load-theme 'dichromacy t)
;  (load-theme 'deeper-blue t)
  )
    

;;;;----------------------------------------------------------
;;;; Cask
;;;;----------------------------------------------------------

;;; Caskをローカルディレクトリにインストールした場合は以下のようにパスを通す
;;; (require 'cask "~/.cask/cask.el")

(when (require 'cask nil t)
  (cask-initialize))


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
;;;; helm
;;;;--------------------------------------------------------
(when (require 'helm-config nil t)
  (helm-mode 1)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-M-z") 'helm-resume)
  ;; helm-find-filesの自動補完を無効にしTABで手動補完する
  (custom-set-variables '(helm-ff-auto-update-initial-value nil))
  (define-key helm-c-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-c-read-file-map (kbd "C-h") 'delete-backward-char))

;;; helm-ack
(when (require 'helm-ack nil t)
  (setq helm-c-ack-version 2)
  ;; does not insert '--type' option
  (setq helm-c-ack-auto-set-filetype nil)
  ;; insert "thing-at-point 'symbol" as search pattern
  (setq helm-c-ack-thing-at-point 'symbol)
  ;; key bindings
  (global-set-key (kbd "C-x g") 'helm-ack))

;;; helm-c-moccur
(when (require 'helm-c-moccur nil t)
  (setq helm-c-moccur-helm-idle-delay 0.1)
  ;; `helm-c-moccur-dmoccur などのコマンドでバッファの情報をハイライトする
  (setq helm-c-moccur-higligt-info-line-flag t)
  ;; 現在選択中の候補の位置を他のwindowに表示する
  (setq helm-c-moccur-enable-auto-look-flag t)
  ;; `helm-c-moccur-occur-by-moccur の起動時にポイントの位置の単語を初期パターンにする
  (setq helm-c-moccur-enable-initial-pattern t)
  ;; key bindings
  (define-key global-map (kbd "M-o") 'helm-c-moccur-occur-by-moccur) ;バッファ内検索
  (define-key global-map (kbd "M-d") 'helm-c-moccur-dmoccur) ;ディレクトリ
  (define-key global-map (kbd "s-o") 'helm-c-moccur-buffer-list)
  (define-key isearch-mode-map (kbd "M-o") 'helm-c-moccur-from-isearch)
;
;  (defun dired-mode-hook-for-helm-c-moccur ()
;	(local-set-key (kbd "O") 'helm-c-moccur-dired-do-moccur-by-moccur))
;  (add-hook 'dired-mode-hook 'dired-mode-hook-for-helm-c-moccur)
)

;;; helm-gtags
(when (require 'helm-gtags nil t)
  (add-hook 'helm-gtags-mode-hook
			'(lambda ()
			   (setq helm-gtags-ignore-case t)
			   ;; key bindings
			   (local-set-key (kbd "M-t") 'helm-gtags-find-tag)
			   (local-set-key (kbd "M-r") 'helm-gtags-find-rtag)
			   (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
			   (local-set-key (kbd "C-q") 'helm-gtags-pop-stack)
			   (local-set-key (kbd "C-c C-f") 'helm-gtags-find-files))))

;;; helm-etags-plus
(when (require 'helm-etags+ nil t)
  (global-set-key (kbd "M-.") 'helm-etags+-select)
  (global-set-key (kbd "M-*") 'helm-etags+-history-go-back))


;;;;--------------------------------------------------------
;;;; yasnippet
;;;;--------------------------------------------------------

(when (require 'yasnippet nil t)
  (setq yas-snippet-dirs
		'("~/.emacs.d/local/yasnippet/snippets"
;		  "~/.emacs.d/local/yasnippet/extras/imported"
		  ))
  (yas-global-mode 1))

;;;;--------------------------------------------------------
;;;; auto complete mode
;;;;  ac-dict/js2-mode
;;;;   curl -L -O https://raw.github.com/sandai/dotfiles/master/.emacs.d/ac-dict/js2-mode
;;;;--------------------------------------------------------

(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-modes 'markdown-mode)
  (add-to-list 'ac-modes 'js2-mode)
  (add-to-list 'ac-modes 'web-mode)
;  (add-to-list 'ac-modes 'racket-mode)
;  (add-to-list 'ac-modes 'racket-repl-mode)
;  (add-to-list 'ac-modes 'enh-ruby-mode)
  (ac-config-default)
  (setq ac-auto-start 4)
  (setq ac-dwim t)
  (setq ac-use-menu-map t) ;; C-n/C-pで候補選択可能
  (setq ac-comphist-file "~/.emacs.d/local/ac-comphist.dat") ;; 補完履歴のキャッシュ先
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")) ;; ユーザー辞書の格納先

;;;;----------------------------------------------------------
;;;; Markdown
;;;;----------------------------------------------------------

;;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(custom-set-variables 
 '(markdown-command
   "pandoc -f markdown -t html -s --mathjax --highlight-style pygments"))


;;;;----------------------------------------------------------
;;;; Textile
;;;;----------------------------------------------------------

;;; textile-mode
(when (require 'textile-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode)))


;;;;--------------------------------------------------------
;;;; C
;;;;--------------------------------------------------------

(add-hook 'c-mode-common-hook
	  '(lambda ()
		 ;; gtagsを有効に
		 (helm-gtags-mode t)
		 ;; flymakeを有効に
;		 (flymake-mode t)

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
(when (require 'js2-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))

;;;;--------------------------------------------------------
;;;; HTML/CSS
;;;;--------------------------------------------------------

;;; web-mode
(when (require 'web-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
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
(when (require 'slime nil t)
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

;; ;;; racket-mode
;; (when (require 'racket-mode nil t)
;;   (add-to-list 'auto-mode-alist '("\\.scm\\'" . racket-mode))
;;   )

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

