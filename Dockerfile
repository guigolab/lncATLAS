FROM rocker/shiny@sha256:2a323588645066a69291bc9f2ee0c2e23d56bfbed82d27b1f8f314c4c19374a4

MAINTAINER Emilio Palumbo <emilio.palumbo@crg.eu>

# Install mariadb client libraries
RUN apt-get update && apt-get install -y -t unstable \
    libmariadbclient-dev-compat

# Adjust user permissions
RUN usermod -u 2001 shiny; groupmod -g 2001 shiny
RUN mkdir -p /var/run/shiny-server
RUN chown -R shiny:shiny /var/run/shiny-server /var/lib/shiny-server /srv/shiny-server /var/log/shiny-server

# Copy app
ADD lncATLAS-app /srv/shiny-server/lncATLAS-app

# Fix permissions
RUN chown -R shiny:shiny /srv/shiny-server/lncATLAS-app

# Set working directory
WORKDIR /srv/shiny-server/lncATLAS-app

# Install dependencies
RUN R --no-save --no-restore --no-init-file --no-environ --slave -f packrat/init.R --args --bootstrap-packrat

# update site_dir in shiny server configuration
RUN sed -i '/site_dir/ s:;$:/lncATLAS-app;:' /etc/shiny-server/shiny-server.conf

# Set the user
USER shiny

# Define default command
CMD ["shiny-server", "--pidfile", "/var/run/shiny-server/shiny-server.pid"]
