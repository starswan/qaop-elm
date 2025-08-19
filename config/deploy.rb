# config valid for current version and patch releases of Capistrano
lock "~> 3.19.2"

set :application, "retroelm"

# Default branch is :master
# ask :branch, `git rev-parse --abbrev-ref HEAD`.chomp
if ENV.key? "BRANCH"
#   set :scm, :git
  set :repo_url, "git@github.com:starswan/qaop-elm.git"
  set :branch, ENV.fetch("BRANCH")
else
#   # repo details
#   set :scm, :subversion
#   set :repository, "http://arthur/svn/starswan/trunk/projects/retro/retroelm"
end

set :passenger_restart_with_touch, true

# Default value for :format is :airbrussh.
# set :format, :airbrussh

# You can configure the Airbrussh format using :format_options.
# These are the defaults.
# set :format_options, command_output: true, log_file: "log/capistrano.log", color: :auto, truncate: :auto

# Default value for :pty is false
# set :pty, true

# Default value for :linked_files is []
# append :linked_files, "config/database.yml", 'config/master.key'

# Default value for linked_dirs is []
# append :linked_dirs, "log", "tmp/pids", "tmp/cache", "tmp/sockets", "public/system", "vendor", "storage"
append :linked_dirs, "log", "tmp/pids", "tmp/cache", "tmp/sockets", "public/system", "vendor", "storage", "node_modules"

# Default value for default_env is {}
# set :default_env, { path: "/opt/ruby/bin:$PATH" }

# Default value for local_user is ENV['USER']
# set :local_user, -> { `git config user.name`.chomp }

# Default value for keep_releases is 5
# set :keep_releases, 5

# Uncomment the following to require manually verifying the host key before first deploy.
# set :ssh_options, verify_host_key: :secure

# require "capistrano/ext/multistage"
# set :stages, %w(alice arthur dell)
# set :default_stage, "arthur"
# set :linked_dirs, fetch(:linked_dirs, []).push('games')
#
# # server details
# set :deploy_via, :copy
# set :use_sudo, false
#
# if ENV.key? "BRANCH"
#   set :scm, :git
#   set :repository, "git@github.com:starswan/qaop-elm.git"
#   # set :repository, "https://github.com/starswan/qaop-elm.git"
#   set :branch, ENV.fetch("BRANCH")
# else
#   # repo details
#   set :scm, :subversion
#   set :repository, "http://arthur/svn/starswan/trunk/projects/retro/retroelm"
# end
#
# # runtime dependencies
# # depend :remote, :gem, "bundler", ">=1.0.0.rc.2"
#
# # tasks
# namespace :deploy do
#   task :start, roles: :app do
#     run "touch #{current_path}/tmp/restart.txt"
#   end
#
#   task :stop, roles: :app do
#     # Do nothing.
#   end
#
#   desc "Restart Application"
#   task :restart, roles: :app do
#     run "touch #{current_path}/tmp/restart.txt"
#     # Wakey wakey monit, work to do...
#     run "monit reload"
#   end
#
#   desc "Symlink shared resources on each release"
#   task :symlink_shared, roles: :app do
#     # run "ln -nfs #{shared_path}/config/database.yml #{release_path}/config/database.yml"
#     fetch(:linked_files, []).each do |f|
#       run "ln -nfs #{shared_path}/#{f} #{release_path}/#{f}"
#     end
#     fetch(:linked_dirs, []).each do |f|
#       run "ln -nfs #{shared_path}/#{f} #{release_path}/public/#{f}"
#     end
#   end
#
#   desc "Copy Undebug module"
#   task :undebug, roles: :app do
#     run "cd #{release_path}/app/javascript && cp ../helpers/Undebug.elm Z80Debug.elm"
#   end
# end
#
# after "deploy:update_code", "deploy:symlink_shared"
# after "bundler:install", "deploy:undebug"
#
# namespace :bundler do
#   desc "Symlink bundled gems on each release"
#   task :symlink_bundled_gems, roles: :app do
#     run "mkdir -p #{shared_path}/bundled_gems"
#     run "ln -nfs #{shared_path}/bundled_gems #{release_path}/vendor/bundle"
#   end
#
#   desc "Install for production"
#   task :install, roles: :app do
#     run "cd #{release_path} && bundle config set without 'development test'"
#     run "cd #{release_path} && bundle install"
#   end
# end
#
# after "deploy:update_code", "bundler:symlink_bundled_gems"
# before "deploy:assets:precompile", "bundler:install"
# after "deploy:update_code", "deploy:migrate"
