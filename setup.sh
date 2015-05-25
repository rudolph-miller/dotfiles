#!/bin/sh
ln -sf ~/dotfiles/.vimrc ~/.vimrc
ln -sf ~/dotfiles/.vim ~/.vim
ln -sf ~/dotfiles/.zshrc ~/.zshrc 
ln -sf ~/dotfiles/my-setting.zsh ~/.oh-my-zsh/custom/my-setting.zsh
ln -sf ~/dotfiles/swank ~/.swank
ln -sf ~/dotfiles/init.el ~/.emacs.d/init.el
ln -sf ~/dotfiles/Cask ~/.emacs.d/Cask
ln -sf ~/dotfiles/jshintrc ~/.emacs.d/.jshintrc
ln -sf ~/dotfiles/mytheme.zsh-theme ~/.oh-my-zsh/themes/mytheme.zsh-theme
ln -sf ~/dotfiles/.rosrc ~/.rosrc
ln -sf ~/dotfiles/gitignore ~/.gitignore
ln -sf ~/dotfiles/.tmux.conf ~/.tmux.conf
mkdir -p ~/.emacs.d
if [ ! -d ~/.emacs.d/cask ]; then
  git clone http://github.com/cask/cask ~/.emacs.d/cask
fi
mkdir -p ~/.cask
cp ~/.emacs.d/cask/cask.el ~/.cask/cask.el
if [ ! -d ~/.emacs.d/slime ]; then
git clone http://github.com/slime/slime ~/.emacs.d/slime
fi
mkdir -p ~/.vim/neobundle
if [ ! -d ~/.vim/neobundle/neobundle.vim ];then
  git clone https://github.com/Shougo/neobundle.vim ~/.vim/neobundle/neobundle.vim
fi
if [ ! -d ~/.emacs.d/popwin ]; then
  git clone https://github.com/m2ym/popwin-el ~/.emacs.d/popwin
fi
cp ~/.emacs.d/popwin/popwin.el ~/.emacs.d/
if [ ! -d ~/.emacs.d/cl-annot ]; then
git clone https://github.com/m2ym/cl-annot ~/.emacs.d/cl-annot
fi
cp ~/.emacs.d/cl-annot/misc/slime-annot.el ~/.emacs.d/
if [ ! -d ~/.emacs.d/slime-repl-ansi-color ]; then
  git clone https://github.com/deadtrickster/slime-repl-ansi-color ~/.emacs.d/slime-repl-ansi-color
fi
cp ~/.emacs.d/slime-repl-ansi-color/slime-repl-ansi-color.el ~/.emacs.d/
if [ ! -d ~/.emacs.d/helm ]; then
  git clone https://github.com/emacs-helm/helm ~/.emacs.d/helm
fi
if [ ! -d ~/.emacs.d/js2-mode ]; then
  git clone http://github.com/mooz/js2-mode ~/.emacs.d/js2-mode
fi
if [ ! -f /usr/lib/python*/site-packages/setuptools.pth ]; then
  curl https://bootstrap.pypa.io/ez_setup.py -o - | sudo python
fi
sudo mkdir -p /usr/share/kbd/keymaps/i386/dvorak
if [ ! -f /usr/share/kbd/keymaps/i386/dvorak/dvk.map ]; then
  sudo ln -fns $HOME/dotfiles/dvk.map /usr/share/kbd/keymaps/i386/dvorak/dvk.map
fi
