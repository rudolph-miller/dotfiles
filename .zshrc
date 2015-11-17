export ZSH=$HOME/.oh-my-zsh
ZSH_THEME="mytheme"
plugins=(git)
source $ZSH/oh-my-zsh.sh
export PATH=$HOME/.nodenv/shims:/usr/local/go/bin:$HOME/.plenv/bin:/usr/local/opt/go/libexec/bin:$HOME/.roswell/bin:$HOME/.linuxbrew/bin:$HOME/.nodebrew/current/bin:$HOME/.cask/bin:$HOME/.rvm/bin:/usr/local/heroku/bin:/usr/local/opt/llvm/bin:/usr/local/bin:/sw/bin:/Users/rudolph/.cask/bin:$HOME/Library/Android/sdk/platform-tools:$HOME/dotfiles/bin:/usr/local/sbin:$HOME/bin:/usr/local/bin:$PATH
export LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/usr/local/opt/llvm/lib:$LD_LIBRARY_PATH
export NODE_PATH=/usr/local/lib/node_modules
export GOPATH="/usr/local/go/"
export EDITOR="vim"
export RUBYMOTION_ANDROID_SDK=$HOME/.rubymotion-android/sdk
export RUBYMOTION_ANDROID_NDK=$HOME/.rubymotion-android/ndk
export ANDROID_HOME=/usr/local/opt/android-sdk

eval "$(hub alias -s)"
eval "$(rbenv init -)"
eval "$(plenv init -)"

DIRSTACKSIZE=100
setopt AUTO_PUSHD

autoload -Uz compinit && compinit

zstyle ':completion:*' menu select
zstyle ':completion:*:cd:*' ignore-parents parent pwd
zstyle ':completion:*:descriptions' format '%BCompleting%b %U%d%u'

function exists { which $1 &> /dev/null }

if exists peco; then
  function peco_select_history() {
    local tac
    exists gtac && tac="gtac" || { exists tac && tac="tac" || { tac="tail -r" } }
    BUFFER=$(fc -l -n 1 | eval $tac | peco --query "$LBUFFER")
    CURSOR=$#BUFFER         # move cursor
    zle -R -c               # refresh
  }

  zle -N peco_select_history
  bindkey '^R' peco_select_history
fi

function opengem() {
  local gem_name=$(bundle list | sed -e 's/^ *\* *//g' | peco | cut -d \  -f 1)
  if [ -n "$gem_name" ]; then
    bundle open ${gem_name}
  fi
}

PR_ID_FILE_NAME='.git_pr_id'
function pull-request() {
  local url=$(hub pull-request < `tty` > `tty`)
  local id=$(echo $url | gsed -e 's/http[|s]:\/\/github\.com\/.*\/.*\/pull\///')
  local branch=$(git symbolic-ref --short HEAD)
  touch $PR_ID_FILE_NAME
  gsed "/^$branch .*$/d" $PR_ID_FILE_NAME
  echo "$branch $id" > $PR_ID_FILE_NAME
  echo $url
}
