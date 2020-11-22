## Oauth2 provider for inventories solutions

A Haskell implementation of [OpenID Connect](http://openid.net/connect/).

##### Prerequisites
Install Haskell [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)

##### Building
```
$ git clone https://github.com/solutions-for-inventory/inventory-repair-login.git
$ cd inventory-repair-login
```
Do stack setup only the first time
```
$ stack setup
```
Build project
```
$ stack build
```
Or
```
$ stack build --copy-bins --local-bin-path target
```
##### Run
```
$ stack exec inventory-repair-login
```
#### Build Docker Image and push
```
$ docker build -t inventory-repair-login ./
$ docker tag inventory-repair-login docker.pkg.github.com/solutions-for-inventory/inventory-repair-login/inventory-repair-login:1.0
$ docker push docker.pkg.github.com/solutions-for-inventory/inventory-repair-login/inventory-repair-login:1.0
```

#### Build Docker Image and push
```
$ docker run -it --env DB_HOST=192.168.99.100 -p 4200:4200 inventory-repair-login
$ docker run -it --rm  --env DB_HOST=192.168.99.100 -p 4200:4200 inventory-repair-login
```

#### Test Example
http://localhost:3000/oauth/authorize?client_id=app&state=state_code&response_type=code&redirect_uri=http%3A%2F%2Flocalhost:8080/app

