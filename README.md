# lncATLAS

Information for lncATLAS container

## Instructions

### Build

```bash
docker build -t lncatlas .
```

### Setup

The container runs the server as the `shiny` user by default. The user has `uid=2001` and `gid=2001`. In order to properly run the container, the mapped volumes should belong to this user.

```bash
# get lncATLAS application archive
wget LNCATLAS_APP_TARBALL
tar xf LNCATLAS_APP_TARBALL

# make sure the host log folder exists
mkdir -p /var/log/shiny-server

# change permissions to the app and log folders
chown shiny:shiny lncATLAS-app /var/log/shiny-server

# if the user 'shiny' does not exists and you don't want to create a new user ...
chown 2001:2001 lncATLAS-app /var/log/shiny-server
```

### Run

Mapping to port 3838 (not installed in the host)

```bash
docker run -ti --name lncatlas -d -p 3838:3838 -v $(pwd)/lncATLAS-app/:/srv/shiny-server/ -v $(pwd)/my.cnf:/srv/shiny-server/.mysqlconf -v /var/log/shiny-server/:/var/log/shiny-server/ lncatlas
```
