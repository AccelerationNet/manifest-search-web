# -*- mode: ruby -*-
require '/opt/lisp/ADWCodeBase/misc/publish_lisp.rb'


#these will probably set for every application.
#What's the name of the app, will be used as a cname essentially.
set :application, "lisp-search.acceleration.net"
set :liveserver, nil

#the ip apache is listening on, name-vhosted on publispweb02, so use that ip.
set :app_ip, "216.155.96.225"

#the server we are sshing to to make this happen.
set :domain, "root@publispweb03"
#the path on the server to deploy to.
set :deploy_to, Proc.new { "/var/www/lisp-search/#{application}" }
#Folders that are shared among every release (e.g. persistent upload folders)
set :share_folders, ["doc-index"]
#Folders from the project root that should be uploaded with every publish
set :copy_folders, ["bin", "www", "etc"]
#Folders int he published root that should be exposed through apache to the web.
set :web_folders, ["www"]
set :a2mods, ["rewrite", "proxy", "filter", "deflate"]

#the port the lisp application should listen on
set :port, 53622

desc "setup the live publish.".cleanup
task :live => ['lisp:clean'] do
  set :application, "lisp-search.acceleration.net"
  set :liveserver, "true"
end


### USE Upstart instead of init.d
Rake.application.remove_task("lisp:create_initd")
Rake.application.remove_task("lisp:restart")
namespace :lisp do
  remote_task :create_initd do
    run "sudo ln -f -s #{current_path}/etc/upstart.conf /etc/init/#{application}.conf"
    run "sudo initctl reload-configuration"
  end

  remote_task :restart do
    run "sudo stop #{application} || echo \"!!! #{application} wasn't running !!!\""
    run "sudo start #{application}"
    run "sudo /usr/sbin/apache2ctl graceful"
    puts "Restarted lisp and apache on the remote server."
  end

  desc "zip the doc index into the www folder".cleanup
  task :do_zips do
    system("rm www/doc-index.tar.gz")
    system("tar -zcf www/doc-index.tar.gz doc-index")
  end
  
  Rake::Task['lisp:publish'].prerequisites.push('lisp:do_zips')

end

### Make a zip of the docs


