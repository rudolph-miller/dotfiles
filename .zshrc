export ZSH=$HOME/.oh-my-zsh
ZSH_THEME="mytheme"
plugins=(git)
source $ZSH/oh-my-zsh.sh
export GOENV_ROOT=$HOME/.goenv
export PYENV_ROOT="${HOME}/.pyenv"
export ANDROID_HOME=$HOME/Library/Android/sdk
export PATH=$PATH:$ANDROID_HOME/emulator
export PATH=$PATH:$ANDROID_HOME/tools
export PATH=$PATH:$ANDROID_HOME/cmdline-tools/latest/bin
export PATH=$PATH:$ANDROID_HOME/platform-tools
export JAVA_HOME=/Applications/Android\ Studio.app/Contents/jbr/Contents/Home
export PATH=$PATH:$JAVA_HOME/bin
export PATH=$HOME/.pub-cache/bin:$HOME/go/bin:$GOENV_ROOT/shims:/opt/homebrew/bin:$GOENV_ROOT/bin:$HOME/projects/ubie/eng-tools:/opt/homebrew/opt/openssl@1.1/bin:$HOME/.embulk/bin:$PYENV_ROOT/shims:$PYENV_ROOT/bin:$HOME/.nodenv/shims:/usr/local/go/bin:$HOME/.plenv/bin:$HOME/.roswell/bin:$HOME/.linuxbrew/bin:$HOME/.nodebrew/current/bin:$HOME/.cask/bin:$HOME/.rvm/bin:/usr/local/heroku/bin:/usr/local/opt/llvm/bin:/usr/local/bin:/sw/bin:/Users/rudolph/.cask/bin:$HOME/Library/Android/sdk/platform-tools:$HOME/dotfiles/bin:/usr/local/sbin:$HOME/bin:/usr/local/bin:$HOME/.pyenv/versions/2.7.6/bin:$PATH
export GOPATH="/usr/local/go/"
export EDITOR="vim"
export RUBYMOTION_ANDROID_SDK=$HOME/.rubymotion-android/sdk
export RUBYMOTION_ANDROID_NDK=$HOME/.rubymotion-android/ndk
export XDG_CONFIG_HOME=~/.config
export PYENV_VIRTUALENV_DISABLE_PROMPT=1
export CUDA_HOME=/usr/local/cuda
export DYLD_LIBRARY_PATH=$CUDA_HOME/lib:$DYLD_LIBRARY_PATH
export PATH="$CUDA_HOME/bin:$PATH"
export PGDATA=/usr/local/var/postgres
export SDKROOT="$(xcrun --sdk macosx --show-sdk-path)"
export PATH="/usr/local/opt/openjdk@11/bin:$PATH"

ARCH=$(uname -m)
if [[ $ARCH == arm64 ]]; then
  echo "Current Architecture: $ARCH"
  eval $(/opt/homebrew/bin/brew shellenv)
elif [[ $ARCH == x86_64 ]]; then
  echo "Current Architecture: $ARCH"
  eval $(/usr/local/bin/brew shellenv)
fi

eval "$(hub alias -s)"
eval "$(rbenv init - zsh)"
eval "$(plenv init -)"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
eval "$(goenv init -)"
eval "$(nodenv init - zsh)"

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
alias gcp='git cherry-pick'

export PATH="$HOME/.yarn/bin:$PATH"
export PATH="$GOROOT/bin:$PATH"
export PATH="$PATH:$GOPATH/bin"
export PATH="$(go env GOENV)/bin:$PATH"

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/rudolph/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/rudolph/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/rudolph/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/rudolph/google-cloud-sdk/completion.zsh.inc'; fi

alias arm="exec arch -arch arm64e /bin/zsh --login"
alias x64="exec arch -arch x86_64 /bin/zsh --login"

export PATH="/opt/homebrew/opt/readline/bin:$PATH"
export LDFLAGS="-L/opt/homebrew/opt/readline/lib"
export CPPFLAGS="-I/opt/homebrew/opt/readline/include"
export PKG_CONFIG_PATH="/opt/homebrew/opt/readline/lib/pkgconfig"

autoload -U +X bashcompinit && bashcompinit
complete -o nospace -C /opt/homebrew/bin/terraform terraform

### MANAGED BY RANCHER DESKTOP START (DO NOT EDIT)
export PATH="/Users/rudolph/.rd/bin:$PATH"
### MANAGED BY RANCHER DESKTOP END (DO NOT EDIT)
