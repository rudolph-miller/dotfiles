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
git clone http://github.com/cask/cask ~/.emacs.d/cask
cp ~/.emacs.d/cask/cask.el ~/.emacs.d/
git clone http://github.com/slime/slime ~/.emacs.d/slime
