# lncATLAS

Information for lncATLAS container

## Instructions

### Build

```bash
docker build -t lncatlas .
```

### Setup

The container runs the server as the `shiny` user by default. The user has `uid=2001` and `gid=2001`. In order to properly run the container, the mapped volumes should belong to this user. 

### Run

The application folder resides under `/home/shiny` and is called `lncatlas`. It needs to be mounted as a volume within the container at `/srv/shiny-server/`. A mysql configuration file is also needed in order to access the database. We also map port 3838 (not installed in the host).

```bash
docker run -ti --name lncatlas -d -p 3838:3838 -v /home/shiny/lncatlas/:/srv/shiny-server/ -v $(pwd)/my.cnf:/srv/shiny-server/.mysqlconf lncatlas
```

## lncATLAS development

### Server setup

Create a `shiny` user with `git` shell and a specific skel folder:

```bash
$ useradd -u 2001 -s /usr/bin/git-shell -m -k skel shiny
```

The `skel` folder contains an initialized bare git repo with a `post-receive` hook and an authorized key and a pre-created folder for the lncatlas shiny app.

```bash
$ tree skel
skel
├── lncatlas.git
│   ├── branches
│   ├── config
│   ├── description
│   ├── HEAD
│   ├── hooks
│   │   ├── applypatch-msg.sample
│   │   ├── commit-msg.sample
│   │   ├── post-receive
│   │   ├── post-update.sample
│   │   ├── pre-applypatch.sample
│   │   ├── pre-commit.sample
│   │   ├── prepare-commit-msg.sample
│   │   ├── pre-push.sample
│   │   ├── pre-rebase.sample
│   │   └── update.sample
│   ├── info
│   │   └── exclude
│   ├── objects
│   │   ├── info
│   │   └── pack
│   └── refs
│       ├── heads
│       └── tags
└── .ssh
    └── authorized_keys
```

The `post-receive` hook contains the code to update the `lncATLAS` shiny app folder and it is executed every time a `push` is received.

### Client setup

Get the ssh key and add an entry in `.ssh/config`:

```
Host lncatlas-dev
Hostname rodericvm.crg.es
User shiny
IdentityFile ~/.ssh/shiny_id_rsa
```

The private key requires a password to be used. If you don't want to type the key password every time you use it, add the key to the ssh agent:

```
ssh-add ~/.ssh/shiny_id_rsa 
```

Configure a new remote on the client:

```bash
$ git remote add dev lncatlas-dev:lncatlas.git
```

Once you want to test your commited changes, run the following command from the client to trigger the update:

```bash
$ git push dev master
```

## TODO

- [ ] make test container for development
- [x] find a way for David to easily deploy the test container
- [ ] find a solution for logging
