from fabric.api import sudo, run, env, cd

env.hosts = ['160.16.105.206']
env.user = 'nitro-idiot'

#
# Usage

#   $ fab server deploy
#   $ fab updater deploy
#   $ fab extracter deploy

#
# Environments

def server():
    env.project_name = 'quickdocs-server'
    env.directory = '/srv/www/quickdocs-server'

def updater():
    env.project_name = 'quickdocs-updater'
    env.directory = '/srv/www/quickdocs-updater'

def extracter():
    env.project_name = 'quickdocs-extracter'
    env.directory = '/srv/www/quickdocs-extracter'


#
# Deployment tasks

def git_pull():
    with cd(env.directory):
        run('git pull')
        run('git submodule update --recursive')

def deploy_server():
    git_pull()
    sudo('supervisorctl restart quickdocs', shell=False)

def deploy_updater():
    git_pull()

def deploy_extracter():
    git_pull()
    with cd(env.directory):
        run('scripts/update-docker-image')

def deploy():
    if env.project_name == "quickdocs-server":
        deploy_server()
    elif env.project_name == "quickdocs-updater":
        deploy_updater()
    elif env.project_name == "quickdocs-extracter":
        deploy_extracter()
