export GREP_COLOR='1;37;41'
alias grep='grep -E --color=auto'

alias ll='ls -l'
alias lg="ls -G"
alias llg="ls -lG"
alias ey="$HOME/.rbenv/versions/2.0.0-p451/lib/ruby/gems/2.0.0/gems/engineyard-3.0.1/bin/ey"
alias ghi="TERM=xterm-256color ghi"
alias em="emacs"
alias darker="redshift -o -l 0:0 -b 0.8 -t 6500:6500"
alias ibus="ibus-daemon -d"
alias init="ibus && darker"

alias pgstart="pg_ctl start -D /usr/local/pg/data"

autoload -U compinit
compinit

# 色設定
autoload -U colors; colors

# もしかして機能
setopt correct

# PCRE 互換の正規表現を使う
setopt re_match_pcre

# プロンプトが表示されるたびにプロンプト文字列を評価、置換する
setopt prompt_subst

# プロンプト指定
PROMPT="
[%n] %{${fg[yellow]}%}%~%{${reset_color}%}
%(?.%{$fg[green]%}.%{$fg[red]%})%(?!ฅ*•ω•*ฅ <!ฅ(´・ω・｀%)ฅ? <)%{${reset_color}%} "

# ฅ*•ω•*ฅ 

# プロンプト指定(コマンドの続き)
PROMPT2='[%n]> '

# もしかして時のプロンプト指定
SPROMPT="%{$fg[red]%}%{$suggest%}(●ↀωↀ●)✧? < %B%r%b %{$fg[red]%}にゃん? [にゃ!(y), にゃん!(n)]:${reset_color} "

setopt auto_cd
function chpwd() { ls }
