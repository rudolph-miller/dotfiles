export ZSH=$HOME/.oh-my-zsh
ZSH_THEME="mytheme"
plugins=(git)
source $ZSH/oh-my-zsh.sh
export PYENV_ROOT="${HOME}/.pyenv"
export PATH=$HOME/projects/ubie/eng-tools:/usr/local/opt/openssl@1.1/bin:$HOME/.embulk/bin:$PYENV_ROOT/shims:$PYENV_ROOT/bin:$HOME/.nodenv/shims:/usr/local/go/bin:$HOME/.plenv/bin:/usr/local/opt/go/libexec/bin:$HOME/.roswell/bin:$HOME/.linuxbrew/bin:$HOME/.nodebrew/current/bin:$HOME/.cask/bin:$HOME/.rvm/bin:/usr/local/heroku/bin:/usr/local/opt/llvm/bin:/usr/local/bin:/sw/bin:/Users/rudolph/.cask/bin:$HOME/Library/Android/sdk/platform-tools:$HOME/dotfiles/bin:/usr/local/sbin:$HOME/bin:/usr/local/bin:$HOME/.pyenv/versions/2.7.6/bin:$PATH
export LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/usr/local/opt/llvm/lib:$LD_LIBRARY_PATH
export NODE_PATH=/usr/local/lib/node_modules
export GOPATH="/usr/local/go/"
export EDITOR="vim"
export RUBYMOTION_ANDROID_SDK=$HOME/.rubymotion-android/sdk
export RUBYMOTION_ANDROID_NDK=$HOME/.rubymotion-android/ndk
export ANDROID_HOME=/usr/local/opt/android-sdk
export XDG_CONFIG_HOME=~/.config
export PYENV_VIRTUALENV_DISABLE_PROMPT=1
export CUDA_HOME=/usr/local/cuda
export DYLD_LIBRARY_PATH=$CUDA_HOME/lib:$DYLD_LIBRARY_PATH
export PATH="$CUDA_HOME/bin:$PATH"
export PGDATA=/usr/local/var/postgres
export SDKROOT="$(xcrun --sdk macosx --show-sdk-path)"
export JAVA_HOME=/Library/Java/JavaVirtualMachines/adoptopenjdk-11.jdk/Contents/Home
PATH=${JAVA_HOME}/bin:${PATH}

eval "$(hub alias -s)"
eval "$(rbenv init -)"
eval "$(plenv init -)"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

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

function openpr() {
  local current_branch_name=$(git symbolic-ref --short HEAD | xargs perl -MURI::Escape -e 'print uri_escape($ARGV[0]);')
  hub browse -- pull/${current_branch_name}
}

alias vi=vim
alias ghc='stack ghc --'
alias ghci='stack ghci'

export PATH="$HOME/.yarn/bin:$PATH"

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/rudolph/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/rudolph/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/rudolph/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/rudolph/google-cloud-sdk/completion.zsh.inc'; fi
