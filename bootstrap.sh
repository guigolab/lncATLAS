#!/bin/bash

# bootstrap packrat and install packages
R --vanilla --slave -f packrat/init.R --args --bootstrap-packrat

# run shiny server
mkdir -p /home/shiny/run/
mkdir -p /home/shiny/log/
mkdir -p /home/shiny/bookmarks/
exec shiny-server --pidfile=/home/shiny/run/shiny-server.pid >> /home/shiny/log/shiny-server.log 2>&1

