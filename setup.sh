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
git clone http://github.com/cask/cask ~/.emacs.d/cask
mkdir -p ~/.cask
cp ~/.emacs.d/cask/cask.el ~/.cask/cask.el
git clone http://github.com/slime/slime ~/.emacs.d/slime
brew tap snmsts/roswell
brew install roswell
roswell setup
roswell install sbcl
mkdir -p ~/.vim/neobundle
git clone https://github.com/Shougo/neobundle.vim ~/.vim/neobundle/neobundle.vim
git clone https://github.com/m2ym/popwin-el ~/.emacs.d/popwin
cp ~/.emacs.d/popwin/popwin.el ~/.emacs.d/
git clone https://github.com/m2ym/cl-annot ~/.emacs.d/cl-annot
cp ~/.emacs.d/cl-annot/misc/slime-annot.el ~/.emacs.d/
git clone https://github.com/deadtrickster/slime-repl-ansi-color
cp ~/.emacs.d/slime-repl-ansi-color/slime-repl-ansi-color.el ~/.emacs.d/
git clone https://github.com/emacs-helm/helm ~/.emacs.d/helm
brew install reattach-to-user-namespace
