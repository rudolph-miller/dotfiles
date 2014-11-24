export GREP_COLOR='1;37;41'
alias grep='grep -E --color=auto'

PATH=$PATH:/sw/bin:/opt/local/bin:/opt/elastic-mapreduce-ruby:/opt/hadoop/bin:/Users/tomoya/git-lab/pm2/bin:$HOME/.nodebrew/current/bin
export PATH

eval "$(rbenv init -)"

alias ll='ls -l'
alias lg="ls -G"
alias llg="ls -lG"
alias hs="hadoop jar /opt/hadoop/share/hadoop/tools/lib/hadoop-streaming-2.4.0.jar"
alias sakura="ssh -i ~/.ssh/sakura.pem 219.94.255.55"
alias ghost="ssh -i ~/.ssh/sakura.pem ghost@219.94.255.55"
alias ey="/Users/tomoya/.rbenv/versions/2.0.0-p451/lib/ruby/gems/2.0.0/gems/engineyard-3.0.0/bin/ey"
alias tr="/Users/tomoya/.rbenv/versions/2.0.0-p451/bin/trello"

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
eval "$(hub alias -s)"
export NODE_PATH=/usr/local/lib/node_modules
