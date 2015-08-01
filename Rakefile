require "rspec/core/rake_task"

task :default => :setup

task :setup do
  sh 'sh setup.sh'
end

task :brew do
  sh 'cat brewfile | xargs -L1 brew install'
end

namespace :cask do
  task :setup do
    Dir.chdir("#{ENV['HOME']}/.emacs.d/") do
      sh 'cask install'
    end
  end

  task :update do
    Dir.chdir("#{ENV['HOME']}/.emacs.d/") do
      sh 'cask update'
    end
  end
end
