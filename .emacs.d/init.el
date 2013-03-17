;; load environment value  ; http://d.hatena.ne.jp/syohex/20111117/1321503477
(when (load-file (expand-file-name "~/.emacs.d/shellenv.el"))
  (dolist (path (reverse (split-string (getenv "PATH")":")))
    (add-to-list 'exec-path path))
  )

;; Emacs 23以前ではuser-emacs-directoryが未定義のため設定を追加しておく
(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d/"))

;;言語を日本語にする
(set-language-environment 'Japanese)
;;極力UTF-8とする
(prefer-coding-system 'utf-8)

;; バックアップとオートセーブファイルを一箇所に集める
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))

;;------------------------------------------------------------------
;; packages
;;------------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;------------------------------------------------------------------
;; load-path
;;------------------------------------------------------------------
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
                  "public_repos"
                  "elpa")

;;------------------------------------------------------------------
;; auto-install
;;------------------------------------------------------------------
(when (require 'auto-install nil t)
  ;; インストールディレクトリの設定
  (setq auto-install-directory "~/.emacs.d/elisp/")
  ;; Emacs Wikiに登録されているelispの名前を取得する
;  (auto-install-update-emacswiki-package-name t)
  ;; 必要であればプロキシの設定
  ;(setq url-proxy-services '(("http" . "localhost:8339")))
  ;; install-elisp の関数を利用可能にする
  (auto-install-compatibility-setup))

;;------------------------------------------------------------------
;; フォント設定
;;------------------------------------------------------------------
;; abcdefghijklmnopqrstuvwxyz
;; ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; `1234567890-=\[];',./
;; ~!@#$%^&*()_+|{}:"<>?
;; 壱弐参四五壱弐参四五壱弐参四五壱弐参四五
;; 1234567890123456789012345678901234567890
;; ABCdeＡＢＣｄｅ
;;
;; ┏━━━━━━━━━━━━┓
;; ┃　　　　　罫線          ┃   ;; 全角・半角空白
;; ┗━━━━━━━━━━━━┛

;(set-default-font"-*-Osaka-normal-normal-normal-*-10-*-*-*-m-0-iso10646-1")
(when (>= emacs-major-version 23)
 (setq fixed-width-use-QuickDraw-for-ascii t)
 (setq mac-allow-anti-aliasing t)
 (set-face-attribute 'default nil
;                    :family "monaco"
                     :family "ricty"
                     :height 180)
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
  '("ricty" . "iso10646-1")))

;;------------------------------------------------------------------
;; 外観
;;------------------------------------------------------------------
;; tab
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)  ; インデントにタブ文字使用しない

;; 起動時フレームサイズを設定
(setq initial-frame-alist
      (append (list
               ;; (when (eq system-type 'darwin)   ; Macの設定
               ;;   ('(width . 88)
               ;;   '(height . 51)))
	       ;; )
	       '(width . 88)
	       '(height . 51)
              initial-frame-alist)))
(setq default-frame-alist initial-frame-alist)

;; tool bar
(tool-bar-mode 0)

;; デフォルトの透明度を設定する
(set-frame-parameter (selected-frame) 'alpha '(95 80))

;; Color-theme
(cond
 ((require 'solarized-dark-theme nil t))  ; default color
; ((require 'solarized-light-theme nil t))
 ((require 'zenburn-theme nil t))
 ((require 'molokai-theme nil t))
;;  ((when (require 'color-theme nil t)
;;     (color-theme-subtle-hacker) ; 2nd color
;; ;         (color-theme-billw)
;; ;         (color-theme-clarity)
;;     ))
)

;; タイトルバーにファイルのフルパスを表示
(setq frame-title-format "%f")

;; 行番号を表示
(when (require 'linum nil t)
  (global-linum-mode t)
  (setq linum-format "%5d"))

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
(show-paren-mode t)
(setq show-paren-delay 0) ; 表示までの秒数
;(setq show-paren-style 'expression)  ; とにかく強調表示
(setq show-paren-style 'parenthesis)  ; 強調表示のみ
(set-face-attribute 'show-paren-match-face nil
                    :background nil
                    :foreground nil
                    :underline nil
                    )

;;------------------------------------------------------------------
;; Key bindings
;;------------------------------------------------------------------
;; redo+の設定
(when (require 'redo+ nil t)
  ;; C-' にリドゥを割り当てる
  (global-set-key (kbd "C-'") 'redo)
  ;; 日本語キーボードの場合は C-. がいいかも
  ;(global-set-key (kbd "C-.") 'redo)
  )

;; C-mにnewline-and-indent
(global-set-key (kbd "C-m") 'newline-and-indent)
;; 折り返しトグルコマンド
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)
;; "C-t"でウィンドウを切り替える
(define-key global-map (kbd "C-t") 'other-window)

;;------------------------------------------------------------------
;; auto-complete
;;------------------------------------------------------------------
(when (require 'auto-complete-config nil t)
  (ac-config-default)
  (setq ac-auto-start 1)
  (setq ac-dwim t)
  (setq ac-use-menu-map t) ;; C-n/C-pで候補選択可能
  (add-to-list 'ac-sources 'ac-sources-yasnippet) ;; 常にYASnippetを補完候補に
  (setq ac-dictionary-directories "~/.emacs.d/public_repos/auto-complete/dict") ;;辞書ファイルのディレクトリ
  (setq ac-comphist-file "~/.emacs.d/etc/ac-comphist.dat") ;; 補完履歴のキャッシュ先
)

;;------------------------------------------------------------------
;; yasnippet
;;------------------------------------------------------------------
;(add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-0.8.0")
(when (require 'yasnippet nil t)
;  (yas/initialize) ; error ?
  (yas/global-mode 1)
  (yas/load-directory "~/.emacs.d/public_repos/yasnippet/snippets")
  (yas/load-directory "~/.emacs.d/public_repos/yasnippet/extras/imported"))

;;------------------------------------------------------------------
;; anything
;;------------------------------------------------------------------
;; (auto-install-batch "anything")
(when (require 'anything nil t)
  (global-set-key (kbd "\C-x b") 'anything)
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

;;------------------------------------------------------------------
;; text-mode
;;------------------------------------------------------------------
;;; Tab / BackTab(Tab, Shift+Tab) インデント、逆インデント動作
;;; http://d.hatena.ne.jp/mtv/20110925/p1
(add-hook 'text-mode-hook 
          '(lambda()
;             (define-key text-mode-map "\C-i" 'tab-to-tab-stop)
             (define-key text-mode-map "\C-i" 'tab-to-tab-stop-line-or-region)
;             (define-key text-mode-map [backtab] 'backtab)
             (define-key text-mode-map [backtab] 'backtab-line-or-region)
             (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120 124 128))
             (setq indent-tabs-mode nil)))

(defun tab-to-tab-stop-line-or-region ()
  (interactive)
  (if mark-active (save-excursion
                    (setq count (count-lines (region-beginning) (region-end)))
                    (goto-char (region-beginning))
                    (while (> count 0)
                      (tab-to-tab-stop)
                      (forward-line)
                      (setq count (1- count)))
                    (setq deactivate-mark nil))
    (tab-to-tab-stop)))

(defun backtab()
  "Do reverse indentation"
  (interactive)
  (back-to-indentation)
  (delete-backward-char
   (if (< (current-column) (car tab-stop-list)) 0
     (- (current-column)
        (car (let ((value (list 0)))
               (dolist (element tab-stop-list value) 
                 (setq value (if (< element (current-column)) (cons element value) value)))))))))

(defun backtab-line-or-region ()
  (interactive)
  (if mark-active (save-excursion
                    (setq count (count-lines (region-beginning) (region-end)))
                    (goto-char (region-beginning))
                    (while (> count 0)
                      (backtab)
                      (forward-line)
                      (setq count (1- count)))
                    (setq deactivate-mark nil))
    (backtab)))

;;------------------------------------------------------------------
;; tags
;;------------------------------------------------------------------
;; gtags
;;; http://d.hatena.ne.jp/higepon/20060107/1136628498
;;; http://d.hatena.ne.jp/klon/20110819/1313766145
(when (require 'gtags nil t)
  (when (require 'anything-gtags nil t))
  (autoload 'gtags-mode "gtags" "" t)
  (setq gtags-mode-hook
        '(lambda ()
           (local-set-key "\M-t" 'gtags-find-tag)
           (local-set-key "\M-r" 'gtags-find-rtag)
           (local-set-key "\M-s" 'gtags-find-symbol)
           (local-set-key "\C-t" 'gtags-pop-stack)  ; カーソルの移動とかぶるので他のにしたい

           (setq gtags-path-style 'relative)
           ))

  ;; 自動でgtags-modeにする & 補完リスト作成
  (add-hook 'c-mode-common-hook
            '(lambda ()
               (gtags-mode 1)
               (gtags-make-complete-list)
               ))
  )

;;------------------------------------------------------------------
;; C++
;;------------------------------------------------------------------
(add-hook 'c++-mode-hook
          (lambda ()
            (c-set-style "k&r")
            (setq c-basic-offset 4) ; 字下げは4に変更
            (setq c-auto-newline t) ; 全自動インデントを有効
;             (flyspell-prog-mode)  ; ispellが実行できていない？
            ))

; ヘッダファイル(.h)をc++モードで開く
(setq auto-mode-alist
      (append '(("\\.h$" . c++-mode))
			  auto-mode-alist))

;; auto-insert - 新規作成時のテンプレート挿入設定(C++) 
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
               ("\\.h$"   . ["template.h" my-template])
               ) auto-insert-alist))
(require 'cl)
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

;;------------------------------------------------------------------
;; Obj-C  <参考>http://sakito.jp/emacs/emacsobjectivec.html#emacs-objective-c
;;------------------------------------------------------------------
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


;;------------------------------------------------------------------
;; Flymake
;;------------------------------------------------------------------
(require 'flymake)
;; 全てのファイルでflymakeを有効化
(add-hook 'find-file-hook 'flymake-find-file-hook)
;; M-p/M-nで次の警告、エラー行の移動
(global-set-key "\M-p" 'flymake-goto-prev-error)
(global-set-key "\M-n" 'flymake-goto-next-error)

(add-hook 'c-mode-common-hook
          '(lambda()
             ;; 対応する括弧
             (make-variable-buffer-local 'skeleton-pair)
             (make-variable-buffer-local 'skeleton-pair-on-word)
             (setq skeleton-pair-on-word t)
             (setq skeleton-pair t)
             (make-variable-buffer-local 'skeleton-pair-alist)
             (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
             (local-set-key (kbd "[") 'skeleton-pair-insert-maybe)
             (local-set-key (kbd "{") 'skeleton-pair-insert-maybe)
             (local-set-key (kbd "`") 'skeleton-pair-insert-maybe)
             (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)))

;; Makefileなし(C++)
(defun flymake-cc-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "g++" (list "-Wall" "-Wextra" "-fsyntax-only" local-file))))
(push '("\\.cpp$" flymake-cc-init ) flymake-allowed-file-name-masks)

;; Makefileあり
;; (add-hook 'c++-mode-hook
;;           '(lambda()
;; ;;             (flymake-mode-on)))
;;              (flymake-mode t)))
