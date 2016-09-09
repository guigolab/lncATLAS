#!/bin/bash

# bootstrap packrat and install packages
R --vanilla --slave -f packrat/init.R --args --bootstrap-packrat

# run shiny server
exec shiny-server --pidfile /var/run/shiny-server/shiny-server.pid >> /var/log/shiny-server/shiny-server.log 2>&1

