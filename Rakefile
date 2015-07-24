require "rspec/core/rake_task"

task :default => :setup

task :setup do
  sh 'sh setup.sh'
end

namespace :setup do
  task :brew do
    sh 'cat brewfile | xargs -L1 brew install'
  end

  task :cask do
    Dir.chdir("#{ENV['HOME']}/.emacs.d/") do
      sh 'cask install'
    end
  end
end
