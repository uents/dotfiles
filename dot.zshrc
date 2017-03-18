# -*- mode: shell-script -*-
# @file ~/.zshrc

export LANG=ja_JP.UTF-8

## ヒストリファイルの設定
HISTFILE=$HOME/.zsh-history
HISTSIZE=100000
SAVEHIST=100000

## 開始/終了時刻をヒストリファイルに書き込む
setopt extended_history

## ヒストリを共有
setopt share_history

## 全履歴の一覧を出力する
function history-all { history -E 1 }

## 補完機能の強化
autoload -U compinit
compinit

## コアダンプサイズを制限
limit coredumpsize 102400

## 出力の文字列末尾に改行コードが無い場合でも表示
unsetopt promptcr

## 色を使う
setopt prompt_subst

## ビープを鳴らさない
setopt nobeep

## 内部コマンド jobs の出力をデフォルトで jobs -l にする
setopt long_list_jobs

## 補完候補一覧でファイルの種別をマーク表示
setopt list_types

## サスペンド中のプロセスと同じコマンド名を実行した場合はリジューム
setopt auto_resume

## 補完候補を一覧表示
setopt auto_list

## 直前と同じコマンドをヒストリに追加しない
setopt hist_ignore_dups

## cd 時に自動で push
setopt autopushd

## 同じディレクトリを pushd しない
setopt pushd_ignore_dups

## ファイル名で #, ~, ^ の 3 文字を正規表現として扱う
setopt extended_glob

## TAB で順に補完候補を切り替える
setopt auto_menu

## =command を command のパス名に展開する
setopt equals

## --prefix=/usr などの = 以降も補完
setopt magic_equal_subst

## ヒストリを呼び出してから実行する間に一旦編集
setopt hist_verify

## ファイル名の展開で辞書順ではなく数値的にソート
setopt numeric_glob_sort

## 出力時8ビットを通す
setopt print_eight_bit

## 補完候補のカーソル選択を有効に
zstyle ':completion:*:default' menu select=1

## 補完候補の色づけ
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

## ディレクトリ名だけで cd
setopt auto_cd

## カッコの対応などを自動的に補完
setopt auto_param_keys

## ディレクトリ名の補完で末尾の / を自動的に付加し、次の補完に備える
setopt auto_param_slash

## スペルチェック
setopt correct

## Emacsと同じキー操作を行う
bindkey -e

## ibus-mozc
#mozc_server_start
#export XIM=ibus
#export GTK_IM_MODULE=ibus
#export QT_IM_MODULE=xim
#export XMODIFIERS=@im=ibus
#export XIM_PROGRAM=ibus-daemon
#export XIM_ARGS="-r --daemonize --xim"

## aliases
#alias ls="ls -F --color"
alias ls="ls -F -G"
alias emacs="TERM=xterm-256color XMODIFIERS=@im=none emacs"
alias emacs.app="open /Applicatoins/Emacs.app"
alias drracket="open /opt/homebrew-cask/Caskroom/racket/6.1.1/Racket\ v6.1.1/DrRacket.app"
#alias plantuml="java -jar ${HOME}/opt/plantuml/plantuml.jar"
#alias irb="irb --readline -r irb/completion"
