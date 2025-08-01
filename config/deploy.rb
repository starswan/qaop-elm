#
# $Id$
#
# RVM bootstrap
#
require "capistrano/ext/multistage"
set :stages, %w(alice arthur dell)
set :default_stage, "arthur"
set :linked_dirs, fetch(:linked_dirs, []).push('games')

# main details
set :application, "retroelm"

# server details
set :deploy_via, :copy
set :use_sudo, false

if ENV.key? "BRANCH"
  set :scm, :git
  set :repository, "git@github.com:starswan/qaop-elm.git"
  # set :repository, "https://github.com/starswan/qaop-elm.git"
  set :branch, ENV.fetch("BRANCH")
else
  # repo details
  set :scm, :subversion
  set :repository, "http://arthur/svn/starswan/trunk/projects/retro/retroelm"
end

# runtime dependencies
# depend :remote, :gem, "bundler", ">=1.0.0.rc.2"

# tasks
namespace :deploy do
  task :start, roles: :app do
    run "touch #{current_path}/tmp/restart.txt"
  end

  task :stop, roles: :app do
    # Do nothing.
  end

  desc "Restart Application"
  task :restart, roles: :app do
    run "touch #{current_path}/tmp/restart.txt"
    # Wakey wakey monit, work to do...
    run "monit reload"
  end

  desc "Symlink shared resources on each release"
  task :symlink_shared, roles: :app do
    # run "ln -nfs #{shared_path}/config/database.yml #{release_path}/config/database.yml"
    fetch(:linked_files, []).each do |f|
      run "ln -nfs #{shared_path}/#{f} #{release_path}/#{f}"
    end
    fetch(:linked_dirs, []).each do |f|
      run "ln -nfs #{shared_path}/#{f} #{release_path}/public/#{f}"
    end
  end

  desc "Copy Undebug module"
  task :undebug, roles: :app do
    run "cd #{release_path}/app/javascript && cp ../helpers/Undebug.elm Z80Debug.elm"
  end
end

after "deploy:update_code", "deploy:symlink_shared"
after "bundler:install", "deploy:undebug"

namespace :bundler do
  desc "Symlink bundled gems on each release"
  task :symlink_bundled_gems, roles: :app do
    run "mkdir -p #{shared_path}/bundled_gems"
    run "ln -nfs #{shared_path}/bundled_gems #{release_path}/vendor/bundle"
  end

  desc "Install for production"
  task :install, roles: :app do
    run "cd #{release_path} && bundle config set without 'development test'"
    run "cd #{release_path} && bundle install"
  end
end

after "deploy:update_code", "bundler:symlink_bundled_gems"
before "deploy:assets:precompile", "bundler:install"
after "deploy:update_code", "deploy:migrate"
