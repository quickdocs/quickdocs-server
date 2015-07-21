from fabric.api import sudo, run, env, cd

env.hosts = ['160.16.105.206']
env.user = 'nitro-idiot'
env.project_name = 'quickdocs'
env.directory = '/srv/www/quickdocs-server'

def update():
    with cd(env.directory):
        run('git pull')

def restart():
    sudo('supervisorctl restart %s' % env.project_name, shell=False)

def deploy():
    update()
    restart()
