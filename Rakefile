require 'rspec/core/rake_task'

HOME = ENV['HOME']

task default: :setup

task setup: [:symlink, 'cask:setup', :neobundle, :emacs, :keymaps, :go, :ruby, 'common-lisp', :perl]

task :symlink do
  [
    { from: "#{HOME}/dotfiles/.vimrc", to: "#{HOME}/.vimrc" },
    { from: "#{HOME}/dotfiles/.vim", to: "#{HOME}/.vim" },
    { from: "#{HOME}/dotfiles/.zshrc", to: "#{HOME}/.zshrc " },
    { from: "#{HOME}/dotfiles/.zprofile", to: "#{HOME}/.zprofile" },
    { from: "#{HOME}/dotfiles/my-setting.zsh", to: "#{HOME}/.oh-my-zsh/custom/my-setting.zsh" },
    { from: "#{HOME}/dotfiles/swank", to: "#{HOME}/.swank" },
    { from: "#{HOME}/dotfiles/init.el", to: "#{HOME}/.emacs.d/init.el" },
    { from: "#{HOME}/dotfiles/Cask", to: "#{HOME}/.emacs.d/Cask" },
    { from: "#{HOME}/dotfiles/jshintrc", to: "#{HOME}/.emacs.d/.jshintrc" },
    { from: "#{HOME}/dotfiles/mytheme.zsh-theme", to: "#{HOME}/.oh-my-zsh/themes/mytheme.zsh-theme" },
    { from: "#{HOME}/dotfiles/.rosrc", to: "#{HOME}/.rosrc" },
    { from: "#{HOME}/dotfiles/init.lisp", to: "#{HOME}/.roswell/init.lisp" },
    { from: "#{HOME}/dotfiles/gitignore", to: "#{HOME}/.gitignore" },
    { from: "#{HOME}/dotfiles/.tmux.conf", to: "#{HOME}/.tmux.conf" },
    { from: "#{HOME}/dotfiles/.rubocop.yml", to: "#{HOME}/.rubocop.yml" },
    { from: "#{HOME}/dotfiles/.gemrc", to: "#{HOME}/.gemrc" }
  ].each do |hash|
    from = hash[:from]
    to = hash[:to]
    puts "ls -sf #{from} #{to}"
    FileUtils.symlink(from, to, force: true)
  end
end

task :neobundle do
  unless Dir.exist?("#{HOME}/.vim/neobundle") && File.exist?("#{HOME}/.vim/neobundle/neobundle.vim")
    Dir.mkdir("#{HOME}/.vim/neobundle")
    sh "git clone https://github.com/Shougo/neobundle.vim #{HOME}/.vim/neobundle/neobundle.vim"
  end
end

task emacs: ['emacs:slime', 'emacs:cl-annot', 'emacs:slime-repl-ansi-color', 'emacs:js2-mode']

namespace :emacs do
  task :slime do
    sh "git clone http://github.com/slime/slime #{HOME}/.emacs.d/slime" unless Dir.exist?("#{HOME}/.cask")
  end

  task 'cl-annot' do
    unless Dir.exist?("#{HOME}/.emacs.d/cl-annot") && File.exist?("#{HOME}/.emacs.d/slime-annot.el")
      sh "git clone https://github.com/m2ym/cl-annot #{HOME}/.emacs.d/cl-annot"
    end
    FileUtils.copy("#{HOME}/.emacs.d/cl-annot/misc/slime-annot.el", "#{HOME}/.emacs.d/")
  end

  task 'slime-repl-ansi-color' do
    unless Dir.exist?("#{HOME}/.emacs.d/slime-repl-ansi-color") && File.exist?("#{HOME}/.emacs.d/slime-repl-ansi-color.el")
      sh "git clone https://github.com/deadtrickster/slime-repl-ansi-color #{HOME}/.emacs.d/slime-repl-ansi-color"
      FileUtils.copy("#{HOME}/.emacs.d/slime-repl-ansi-color/slime-repl-ansi-color.el", "#{HOME}/.emacs.d/")
    end
  end

  task 'helm' do
    unless Dir.exist?("#{HOME}/.emacs.d/helm")
      sh "git clone https://github.com/emacs-helm/helm #{HOME}/.emacs.d/helm"
    end
  end

  task 'js2-mode' do
    unless Dir.exist?("#{HOME}/.emacs.d/js2-mode")
      sh "git clone http://github.com/mooz/js2-mode #{HOME}/.emacs.d/js2-mode"
    end
  end
end

task :brew do
  sh 'cat brewfile | xargs -L1 brew install'
end

namespace :cask do
  task :setup do
    sh "git clone http://github.com/cask/cask #{HOME}/.cask" unless Dir.exist?("#{HOME}/.cask")
    Dir.chdir("#{HOME}/.emacs.d/") do
      sh 'cask install'
    end
  end

  task :update do
    Dir.chdir("#{HOME}/.emacs.d/") do
      sh 'cask update'
    end
  end
end

task :keymaps do
  if Dir.exist?('/Applications/Karabiner.app')
    Rake::Task['karabiner:import'].invoke
  else
    unless Dir.exist?('/usr/share/kbd/keymaps/i386/dvorak') && File.exist?('/usr/share/kbd/keymaps/i386/dvorak/dvk.map')
      sh 'sudo mkdir -p /usr/share/kbd/keymaps/i386/dvorak'
      sh "sudo ln -fns #{HOME}/dotfiles/dvk.map /usr/share/kbd/keymaps/i386/dvorak/dvk.map"
      sh 'sudo loadkeys i386/dvorak/dvk.map'
    end
  end
end

namespace :karabiner do
  task :export do
    sh '/Applications/Karabiner.app/Contents/Library/bin/karabiner export > karabiner.sh'
  end

  task :import do
    if File.exist?('karabiner.sh')
      begin
        sh 'karabiner.sh'
      rescue
        true
      end
    else
      p 'Could not find karabiner.sh.'
    end
  end
end

task :go do
  sh 'go get -u github.com/nsf/gocode'
end

task :ruby do
  %w(rubocop bundler).each do |name|
    sh "gem install #{name}"
  end
end

task 'common-lisp' do
  Dir.mkdir("#{HOME}/.config/common-lisp") unless Dir.exist?("#{HOME}/.config/common-lisp")
  puts 'cp -rf $HOME/dotfiles/skeleton $HOME/.config/common-lisp/'
  FileUtils.copy_entry("#{HOME}/dotfiles/skeleton", "#{HOME}/.config/common-lisp/skeleton")
end

task :perl do
  sh "git clone git://github.com/tokuhirom/plenv.git #{HOME}/.plenv" unless Dir.exist?("#{HOME}/.plenv")
end
