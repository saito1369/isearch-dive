;;; isearch-dive.el --- isearch extension         -*- lexical-binding: t; -*-

;; Copyright (C) 2017  saito1369

;; Author: saito1369 <>
;; Keywords: matching, tools, convenience, extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; このプログラムは isearch の拡張です.
;; ある一つのファイル(母艦ファイル)の inclemental search をします.
;; 母艦ファイルにはファイル名が書いてあります.
;; 拡張プログラムは isearch の途中でファイル名を認識し,
;; ファイルが存在すればそのファイル内を検索します.
;; 検索が終わったら母艦ファイルに戻ってきます.
;; これによって, 母艦ファイルに書いてあるファイル名があたかも
;; 母艦ファイルに含まれるかのようにシームレスに検索できます.

;; ;; 設定
;; (setq idive-mother-fpath "/hoge/fuga/mymemo.org")
;; (setq idive-fname-patterns '(".org" ".pl" ".pm" ".org"))
;; (setq idive-visit-dived-file-p t)
;; 得られた fname => 実在のファイル名への変換
;; (defun idive--expand-file-name(fname)
;;     (setq fname (expand-file-name fname))
;;     (if (file-exists-p fname) fname nil)
;;   )

;;; Code:

;;
;; isearch-dive (idive)
;; 

;; 母艦ファイル名
;(defvar idive-mother-fpath (expand-file-name clmemo-file-name))
(defvar idive-mother-fpath (expand-file-name "~/memo.txt"))
;; 母艦ファイル(ex. ChLog)の buffer
(defvar idive-mother-buffer nil)
;; 既に訪問した buffers のリスト(hash)
(defvar idive-dived-files-hash nil)
;; 既に訪問した buffer も訪問する場合は t にする.
;; nil の場合, C-s の最中に戻ったり (C-r) すると変になるかも
(defvar idive-visit-dived-file-p nil)
;; 既に訪問した buffer は訪問しない
;;(setq idive-visit-dived-file-p nil) ;; 戻ったりするときに変になるかも.
;; 母艦ファイルに書いてあるファイル名の中から, dive していくファイル(suffix)のリスト
(defvar idive-fname-patterns '(".howm" ".org"))
;; 検索を行う前から元々開いてた buffers
(defvar idive-already-exist-hash nil)

;;
;; global variables
;;
;; dive するファイルの名前
(defvar idive-fname nil)
;; 元の検索語(isearch-string) が次どこにあるかの場所(mother file 上)
(defvar idive-next-point nil)
;; isearch-repeat 直前にいた mother 上の場所
(defvar idive-present-point nil)
;; isearch-string の退避場所
(defvar idive-isearch-string nil)
;; 今から dive する file 名が書いてある mother 上の場所
(defvar idive-fname-point nil)

;; 今開いている buffer (buffer: current-buffer file: buffer-file-name)が
;; 検索を始める前から既に開いていた buffer かどうか?
(defun idive--already-exist-p()
  (gethash buffer-file-name idive-already-exist-hash))

;; そうでなければ kill-buffer
(defun idive--kill-buffer-not-already-exist()
  (unless (idive--already-exist-p) (kill-buffer (current-buffer))))

;; idive-mother-buffer 以外の buffer にいるかどうか.
;; 他の buffer を訪問中 = t
(defun idive--another-buffer-p()
  (if idive-mother-buffer
      (if (equal (current-buffer) idive-mother-buffer) nil t)
    nil  ;; そもそも mother-buffer が定義されてなければ nil とする
    ))

;; 検索の方向
(defun idive--isdir()
  (if isearch-forward
      1
    -1))

;;
;; そもそも ChangeLog の検索で idive を起動するかどうかを toggle
;;
(defvar idive-toggle t)

(defun idive--toggle()
  (interactive)
  (if idive-toggle (setq idive-toggle nil)
    (setq idive-toggle t)
    )
  (message "idive-toggle=%s" idive-toggle)
  )

;;
;; hook
;; isearch 開始
;;
(add-hook'isearch-mode-hook 'idive--init)

;; 開いている buffer が mother であればその後色々やる.
(defun idive--init()
  (let ((cbuf  (current-buffer)))
    (when (idive--mother-buffer-p)
      ;; is-dive-mother-fname (母艦)の buffer を持っておく
      (setq idive-mother-buffer cbuf)  ;; checked
      ;; 既に開いている buffer の list を hash でとる.
      (idive--get-hash-already-exist-buffers)
      ;; 訪問した buffer を記録する場所を初期化
      (setq idive-dived-files-hash (make-hash-table :test 'equal))
      )
    )
  )

;; 今開いている buffer のファイル名を hash (idive-already-exist-hash) に保存
(defun idive--get-hash-already-exist-buffers()
  (let ((blist (buffer-list))
        (buf   nil)
        (bname nil)
        (fpath nil))
    (setq idive-already-exist-hash (make-hash-table :test 'equal))
    (while blist
      (setq buf   (car blist))
      (setq bname (buffer-file-name buf))
      (if bname   (setq fpath (expand-file-name bname)))
      (if fpath   (puthash fpath t idive-already-exist-hash))
      ;; 取り出しは以下のようにする
      ; (gethash  fpath idive-already-exist-hash) ;=> t  key は fname
      (setq blist (cdr blist))
      )
    )
  )

;;
;; defadvice
;; isearch-repeat の直前に, 抽出 file 名の中を検索する.
(defadvice isearch-repeat (around isearch-repeat-idive-ad activate)
  "Dive another buffer."
  (when idive-toggle
    (if (idive--mother-buffer-p) ;; mother file の検索の場合
        (if idive-fname
            (idive--dive-another-buffer)  ;; idive-fname があればそのファイルに移動 (isearch 実行しない)
          (idive--search-idive-fname)     ;; idive-fname が無ければ探して
          ad-do-it                        ;;             isearch を実行
          )
      ;; mother file でない場合
      ;; idive-fname を訪問していて 検索が終わった場合は mother に戻る
      (if (and (idive--another-buffer-p) (not isearch-success))
          (idive--return-mother)
        ;; そうでなければ isearch を実行(普通はここが動く)
        ad-do-it
        )
      )
    )
  (unless idive-toggle ad-do-it)
  )

;; 今開いてるのが mother-buffer かどうかを確認
(defun idive--mother-buffer-p()
  (let ((fpath nil))
    (if buffer-file-name (setq fpath (expand-file-name buffer-file-name)))
    (if (and fpath idive-mother-fpath (string= fpath idive-mother-fpath))
        t
      nil)
    )
  )

;; 元々の検索文字列を返す
(defun idive--get-isearch-string()
  (let ((str nil))
    ;; idive-isearch-string には元々の isearch-string が入ってる
    (if idive-isearch-string
        ;; idive-isearch-string が定義されてればその文字列を探す
        (setq str idive-isearch-string)
      ;; 定義されてなければ今の isearch-string が元々の isearch-string である.
      (setq str isearch-string))
    str
    )
  )

;;
;; idive-isearch-string <- isearch-string
;; isearch-string <- idive-path
(defun idive--change-isearch-string()
  ;; idive-isearch-string に元々の isearch-string を保存しておく
  (unless idive-isearch-string (setq idive-isearch-string isearch-string))
  ;; search string を file 名に変更
  (setq isearch-string idive-fname)
  )

;; 普通の ChLog 検索に戻す
;; isearch-string <- idive-isearch-string
;; idive-isearch-string <- nil
(defun idive--return-isearch-string()
  (when idive-isearch-string
    ;; 元の検索後を isearch-string に入れる
    (setq isearch-string idive-isearch-string)
    ;; こっちは空にしておく
    (setq idive-isearch-string nil)
    )
  )

(defun idive--get-next-point(str end)
  (let ((pnt nil))
    (if migemo-isearch-enable-p
        (setq pnt (migemo-forward str end t (idive--isdir)))
      (setq (search-forward str end t (idive--isdir)))
      )
    pnt
    )
  )

(defun idive--search-idive-fname()
  (setq idive-present-point (point))
  (idive--return-isearch-string)
  (setq idive-next-point (idive--get-next-point isearch-string nil))
  ;(goto-char idive-present-point)
  (idive--goto-char)
  (idive--get-dive-file)
  ;(if idive-fname (idive--change-isearch-string))
  (when idive-fname
    (idive--change-isearch-string)
    ;; カーソル位置の調整
    (goto-char (if isearch-forward (- idive-fname-point (length isearch-string))
                 (+ idive-fname-point (length isearch-string))))
    )
  )

(defun idive--goto-char()
  (goto-char idive-present-point)
  ;(if isearch-forward (end-of-line) (beginning-of-line))
  )

;; fname に関連するファイルの正確な名前. ファイルが存在しなければ nil を返す
(defun idive--expand-file-name(fname)
  (let ((fpath nil) (hpath nil) (hname nil))
    (if fname (setq fpath (expand-file-name fname)))
    (when (and fname (string-match "\\.howm$" fname))
      (setq hname (quasi-howm-file-name-from-string-safe fname))
      (if hname
          (setq hpath (expand-file-name hname))))
    (if (and fpath (not (file-exists-p fpath))) (setq fpath nil)) ;; file の存在確認
    (if (and fpath (file-directory-p   fpath))  (setq fpath nil)) ;; directory でないことの確認
    (if (and hpath (not (file-exists-p hpath))) (setq hpath nil))
    (if (and hpath (file-directory-p   hpath))  (setq hpath nil))
    (if fpath
        fpath
      (if hpath
          hpath
        nil))
    )
  )

(defun idive--expand-file-exists-p(fname)
  (if (idive--expand-file-name fname) t nil))

;; dive する file 名が書いてある場所を探す
;; 一つだけ get する. 得られなければ idive-fname は nil
;; ここが一番めんどくさい. ;;checked
(defun idive--get-dive-file()
  (let ((suffs       idive-fname-patterns)
        (sff         nil)
        (dpoint      nil)
        (fname       nil)
        (fpath       nil)
        (dive-points (list)) ;; 配列の定義
        (fhash       (make-hash-table :test 'equal))
        (dhash       (make-hash-table :test 'equal))
        (dp          nil)
        (fnames      (list))
        (rep         nil)
        )
    ;; 初期化
    (setq idive-fname nil)
    (save-excursion
      (while suffs
        (setq sff (car suffs))
        (setq rep t)
        (while rep
          ;; 今いる場所(idive-present-point)から idive-next-point の間で sf に合致する場所を探す
          ;(setq dpoint (re-search-forward sff idive-next-point t (idive--isdir)))
          (setq dpoint (idive--get-next-point sff idive-next-point))
          (setq fpath nil)
          (setq fname nil)
          (when dpoint ;; 見つかったら
            ;; file 名の取り出し
            (setq fname (thing-at-point 'filename))
            ;; fpath (ファイルが存在しなければ nil が返る)
            (setq fpath (idive--expand-file-name fname))
            ;; file が存在して, まだ開いていないとき
            (when (and fpath (not (gethash fpath idive-dived-files-hash)))
              ;; dpoint を push する
              (push dpoint dive-points)
              ;; dpoint => fname
              (puthash dpoint fname  fhash)     ;; fhash[dpoint] =fname
              (unless (gethash fname dhash)     ;; 最初に出てきた fname の場所を記録しておく
                (puthash fname  dpoint dhash))  ;; dhash[fname]  =dpoint
              )
            )
          (unless dpoint (setq rep nil))
          ;; 簡易版: ファイルの探索を最も idive--isdir 方向で最も近い一回で終わらせる
          ;; (==> その代わり idive--filtered をかけない)
          ;; 以下をコメントアウトすれば  idive-present-point から idive-next-point
          ;; までにある全てのファイルの検索を行う => 範囲が広ければ結構時間がかかることもある.
          ;(setq rep nil)
          )
        ;(goto-char idive-present-point)
        (idive--goto-char)
        (setq suffs (cdr suffs))
        )
      )
    (if (= (idive--isdir)  1) (setq dive-points (sort dive-points '<)))
    (if (= (idive--isdir) -1) (setq dive-points (sort dive-points '>)))
    ;; ファイル名のリストをつくる
    (while dive-points
      (setq dp          (car dive-points))
      (push (gethash dp fhash) fnames)
      (setq dive-points (cdr dive-points))
      )
    ;; filter をかける場合(検索文字列が無いファイルは開かない)
    (setq idive-fname (idive--filtered fnames))
    ;; もし idive-fname があれば, fname の場所を確保しておく
    (if idive-fname (setq idive-fname-point (gethash idive-fname dhash)))
    ; filter をかけない場合は以下
    ;(setq idive-fname (car fnames))
    ;; ここで dive するファイルを登録する
    ;; (idive-visit-dived-file-p が t ならば登録しない)
    (unless idive-visit-dived-file-p
      (if idive-fname
          (puthash (idive--expand-file-name idive-fname) t idive-dived-files-hash)))
    )
  )

;; ファイルを試しに開いて, 検索文字があるかどうかをざっと確認
;; 検索文字があるものだけを抽出する
(defun idive--filtered(fnames)
  (let ((fname    nil)
        (cnt      nil)
        (filtered nil)
        (file     nil)
        (str      nil)
        )
    (setq filtered (list))
    (while fnames
      (setq fname (car fnames))
      (with-temp-buffer
        (insert-file-contents (idive--expand-file-name fname))
        (setq str (idive--get-isearch-string))
        ;(setq cnt (count-matches str (point-min) (point-max)))
        ;(if (> cnt 0) (push fname filtered))
        (goto-char (if isearch-forward (point-min) (point-max)))
        (setq cnt (idive--get-next-point str nil))
        (if cnt (push fname filtered))
        )
      (setq fnames (cdr fnames))
      )
    (setq file (car filtered))
    file
    )
  )

;; いよいよ別ファイルの中に入っていく
(defun idive--dive-another-buffer()
  ;; mother file(ex. ChLog)内を移動する
  ;; (開く file 名 idive-fname のところまで mother 上の場所を移動しておく)
  ;(re-search-forward idive-fname idive-next-point t (idive--isdir))
  (idive--get-next-point idive-fname idive-next-point)
  ;(if migemo-isearch-enable-p
  ;    (migemo-forward idive-fname idive-next-point t (idive--isdir))
  ;  (search-forward idive-fname idive-next-point t (idive--isdir))
  ;  )
  ;; ファイルの場所を保存しておく
  ;(setq idive-fname-point (point))
  ;;
  ;; dive into the file (idive-fname)
  ;;
  ;; ファイルを開く
  (find-file (idive--expand-file-name idive-fname))
  ;(setq isearch-string idive-isearch-string)
  ; 検索文字列を元に戻す
  (idive--return-isearch-string)
  ; isearch-forward の値に応じてファイルの先頭あるいは最後に移動する
  (goto-char (if isearch-forward (point-min) (point-max)))
  ;; 一つも見つからないエラーが出るので修正
  (setq isearch-case-fold-search t)
  )

(defun idive--return-mother()
  ;; 今開いてる buffer を閉じる(もし元から開いてなければ)
  (idive--kill-buffer-not-already-exist)
  ;; 元々の検索 string を保存
  ;(setq idive-isearch-string isearch-string)
  ;; isearch-string を idive-fname にする.
  ;(setq isearch-string idive-fname)
  ; 検索文字列 isearch-string を idive-fname にして
  (idive--change-isearch-string)
  ; idive-fname は未定義にしておく.
  ; これでこのファイルに関しての探索を終了させる.
  (setq idive-fname nil)
  ;; 母艦ファイルに戻る
  (switch-to-buffer idive-mother-buffer)
  ; 念のため
  (goto-char idive-fname-point)
  (setq idive-fname-point nil)
  ;; 位置の調整
  ;(goto-char (if isearch-forward (- idive-fname-point (length isearch-string))
  ;             (+ idive-fname-point (length isearch-string))))
  )

;; isearch 終了
(add-hook 'isearch-mode-end-hook 'idive--reset)

(defun idive--reset()
  (setq idive-mother-buffer nil)
  (setq idive-dived-files-hash nil)
  (setq idive-already-exist-hash nil)
  (setq idive-fname nil)
  (setq idive-next-point nil)
  (setq idive-present-point nil)
  (setq idive-isearch-string nil)
  (setq idive-fname-point nil)
  )

(provide 'isearch-dive)
;;; isearch-dive.el ends here
