pipeline {
    agent any
    environment {
        PATH = "/usr/local/bin:$PATH"
    }
    stages {
        stage('CleanOldBinary') {
            steps {
               catchError {
                 //sh 'rm -rf .stack-work'
                 //sh 'docker stop inventory-repair-login'
                 //sh 'docker rm inventory-repair-login'
                 //sh 'docker images -a | grep "inventory-repair-login" | awk \'{print $3}\' | xargs docker rmi'
               }
            }
        }
        stage('Build') {
            steps {
                sh 'stack build --copy-bins --local-bin-path target'
            }
        }
        stage('DockerBuildImage') {
            steps {
                echo 'Starting to build docker image'
                script {
//                    def customImage = docker.build("inventory-repair-login:1.0 --network=host")
                    def customImage = docker.build("inventory-repair-login:1.0")
                }
            }
        }
        stage('Test') {
            steps {
                echo 'Testing..'
            }
        }

        stage('push docker image') {
                    steps {
                        sh 'echo $CR_PAT | docker login ghcr.io -u USERNAME --password-stdin'
                        sh 'docker push ghcr.io/solutions-for-inventory/inventory-repair-login:1.0'
                    }
        }
        /*
        stage('Deploy') {
            steps {
                echo 'Deploying....'
                script {
                    docker.image("inventory-repair-login:1.0")
                    .run('--name inventory-repair-login --net=host '
                        + '-e APP_PORT=3001 '
                        + '-e DB_USER=inventory_user '
                        + '-e DB_PASSWORD=inventory_password '
                        + '-e DB_HOST=192.168.0.100 '
                        + '-e DB_DATABASE=inventory_repair_db'
                    )
                }
            }
        }
        */
    }
}
