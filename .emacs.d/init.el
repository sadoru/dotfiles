;; Emacs 23以前ではuser-emacs-directoryが未定義のため設定を追加しておく
(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d/"))

;; 引数を load-path へ追加
;; load-path を追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
	(dolist (path paths paths)
	  (let ((default-directory
			  (expand-file-name (concat user-emacs-directory path))))
		(add-to-list 'load-path default-directory)
		(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
			(normal-top-level-add-subdirs-to-load-path))))))
;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "elisp"
                  "conf"
                  "public_repos")

;;言語を日本語にする
(set-language-environment 'Japanese)
;;極力UTF-8とする
(prefer-coding-system 'utf-8)

;;フォント設定
;(set-default-font"-*-Osaka-normal-normal-normal-*-10-*-*-*-m-0-iso10646-1")
(when (>= emacs-major-version 23)
 (setq fixed-width-use-QuickDraw-for-ascii t)
 (setq mac-allow-anti-aliasing t)
 (set-face-attribute 'default nil
;                    :family "monaco"
                     :family "ricty"
                     :height 150)
 (set-fontset-font
  (frame-parameter nil 'font)
  'japanese-jisx0208
;  '("Hiragino Maru Gothic Pro" . "iso10646-1"))
  '("ricty" . "iso10646-1"))
 (set-fontset-font
  (frame-parameter nil 'font)
  'japanese-jisx0212
;  '("Hiragino Maru Gothic Pro" . "iso10646-1"))
  '("ricty" . "iso10646-1"))
 ;;; Unicode フォント
 (set-fontset-font
  (frame-parameter nil 'font)
  'mule-unicode-0100-24ff
;  '("monaco" . "iso10646-1")))
  '("monaco" . "iso10646-1")))

;; tab
(setq-default tab-width 4)
;; インデントにタブ文字使用しない
(setq-default indent-tabs-mode nil)

;; デフォルトの透明度を設定する
(add-to-list 'default-frame-alist '(alpha . 95))
;; Color-theme
(when (require 'color-theme nil t)
  (require 'zenburn)
  (color-theme-initialize)
  ;(color-theme-billw)
  ;  (color-theme-clarity)
  (color-theme-zenburn)
  ; https://github.com/credmp/color-theme-zenburn/blob/master/zenburn.el
  )

;; 現在行のハイライト
(defface my-hl-line-face
  ;; 背景がdarkなら背景色を紺に
  '((((class color) (background dark))
     (:background "NavyBlue" t))
    ;;背景がlightならば背景色を緑に
    (((class color) (background light))
     (:background "LightGoldenrodYellow" t))
    (t (:bold t)))
  "hi-line's my face")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode t)

;; リージョンをハイライト
(transient-mark-mode t)
;;対応する括弧のハイライト
(setq show-paren-delay 0) ; 表示までの秒数
(show-paren-mode t)
(setq show-paren-style 'expression)
(set-face-background 'show-paren-match-face nil)
(set-face-underline-p 'show-paren-match-face "yellow")

;; バックアップとオートセーブファイルを一箇所に集める
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))

;; C-mにnewline-and-indent
(global-set-key (kbd "C-m") 'newline-and-indent)
;; 折り返しトグルコマンド
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)
;; "C-t"でウィンドウを切り替える
(define-key global-map (kbd "C-t") 'other-window)

;; タイトルバーにファイルのフルパスを表示
(setq frame-title-format "%f")

;; auto-installの設定
(when (require 'auto-install nil t)
  ;; インストールディレクトリの設定
  (setq auto-install-directory "~/.emacs.d/elisp/")
  ;; Emacs Wikiに登録されているelispの名前を取得する
;  (auto-install-update-emacswiki-package-name t)
  ;; 必要であればプロキシの設定
  ;(setq url-proxy-services '(("http" . "localhost:8339")))
  ;; install-elisp の関数を利用可能にする
  (auto-install-compatibility-setup))

;; auto-complete
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories
               "~/.emacs.d/elisp/auto-complete/ac-dict")
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (ac-config-default))

;; redo+の設定
(when (require 'redo+ nil t)
  ;; C-' にリドゥを割り当てる
  (global-set-key (kbd "C-'") 'redo)
  ;; 日本語キーボードの場合は C-. がいいかも
  ;(global-set-key (kbd "C-.") 'redo)
  )

;; Obj-C
;; <参考>http://sakito.jp/emacs/emacsobjectivec.html#emacs-objective-c
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@implementation" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@interface" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@protocol" . objc-mode))

(ffap-bindings)
;; 探すパスは ffap-c-path で設定する
;; (setq ffap-c-path
;;     '("/usr/include" "/usr/local/include"))
;(setq ffap-c-path
;	  '("/Developer/Platforms"))
;; 新規ファイルの場合には確認する
(setq ffap-newfile-prompt t)
;; ffap-kpathsea-expand-path で展開するパスの深さ
(setq ffap-kpathsea-depth 5)

(setq ffap-other-file-alist
	  '(("\\.mm?$" (".h"))
		("\\.cc$" (".hh" ".h"))
		("\\.hh$" (".cc" ".C"))

		("\\.c$" (".h"))
		("\\.h$" (".c" ".cc" ".C" ".CC" ".cxx" ".cpp" ".m" ".mm"))

		("\\.C$" (".H" ".hh" ".h"))
		("\\.H$" (".C" ".CC"))
		("\\.CC$"  (".HH" ".H"  ".hh" ".h"))
		("\\.HH$"  (".CC"))

		("\\.cxx$" (".hh" ".h"))
		("\\.cpp$" (".hpp" ".hh" ".h"))

		("\\.hpp$" (".cpp" ".c"))))
(add-hook 'objc-mode-hook
		  (lambda ()
			(define-key c-mode-base-map (kbd "C-c o") 'ff-find-other-file)))

;; C++
; ヘッダファイル(.h)をc++モードで開く
(setq auto-mode-alist
      (append '(("\\.h$" . c++-mode))
			  auto-mode-alist))
(add-hook 'c++-mode-hook
	  '(lambda()
	     (c-set-style "cc-mode") ; tab-4
	     (setq indent-tabs-mode nil)))
(setq c-auto-newline t)   ; 全自動インデントを有効

;; auto-insert
(auto-insert-mode)
(require 'autoinsert)

;; テンプレートのディレクトリ
;(setq auto-insert-directory "~/.emacs.d/etc/template/sample/") ;参考にしたサンプル
(setq auto-insert-directory "~/.emacs.d/etc/template/cpp/") ;自分用cppテンプレ
;(setq auto-insert-directory "~/.emacs.d/etc/template/cpp_shigoto/")
;; 各ファイルによってテンプレートを切り替える
(setq auto-insert-alist
      (nconc '(
               ("\\.cpp$" . ["template.cpp" my-template])
               ("\\.h$" . ["template.h" my-template])
               ) auto-insert-alist))
(require 'cl)
;; ここが腕の見せ所
; <参考> http://d.hatena.ne.jp/higepon/20080731/1217491155
(defvar template-replacements-alists
  '(("%file%"             . (lambda () (file-name-nondirectory (buffer-file-name))))
    ("%file-without-ext%" . (lambda () (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
    ("%include-guard%"    . (lambda () (format "%s_H" (upcase (file-name-sans-extension (file-name-nondirectory buffer-file-name))))))))
(defun my-template ()
  (time-stamp)
  (mapc #'(lambda(c)
        (progn
          (goto-char (point-min))
          (replace-string (car c) (funcall (cdr c)) nil)))
    template-replacements-alists)
  (goto-char (point-max))
  (message "done."))
(add-hook 'find-file-not-found-hooks 'auto-insert)

;; flymake
(require 'flymake)
(add-hook 'c++-mode-hook
          '(lambda()
             (flymake-mode t)))

;;; anything
;; (auto-install-batch "anything")
(when (require 'anything nil t)
  (setq
   ;; 候補を表示するまでの時間。デフォルトは0.5
   anything-idle-delay 0.3
   ;; タイプして再描写するまでの時間。デフォルトは0.1
   anything-input-idle-delay 0.2
   ;; 候補の最大表示数。デフォルトは50
   anything-candidate-number-limit 100
   ;; 候補が多いときに体感速度を早くする
   anything-quick-update t
   ;; 候補選択ショートカットをアルファベットに
   anything-enable-shortcuts 'alphabet)

  (when (require 'anything-config nil t)
    ;; root権限でアクションを実行するときのコマンド
    ;; デフォルトは"su"
    (setq anything-su-or-sudo "sudo"))

  (require 'anything-match-plugin nil t)

  (when (and (executable-find "cmigemo")
             (require 'migemo nit t))
    (require 'anything-migemo nil t))

  (when (require 'anything-complete nil t)
    ;; lispシンボルの補完候補の再検索時間
    (anything-lisp-complete-symbol-set-timer 150))

  (require 'anything-show-completion nil t)

  (when (require 'auto-install nil t)
    (require 'anything-auto-install nil t))

  (when (require 'descbinds-anything nil t)
    ;; describe-bindingsをAnythingに置き換える
    (descbinds-anything-install)))
