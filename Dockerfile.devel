FROM rocker/shiny@sha256:2a323588645066a69291bc9f2ee0c2e23d56bfbed82d27b1f8f314c4c19374a4

MAINTAINER Emilio Palumbo <emilio.palumbo@crg.eu>

# Install mariadb client libraries
RUN apt-get update && apt-get install -y -t unstable \
    libmariadbclient-dev

# Adjust user permissions
RUN usermod -u 2001 shiny; groupmod -g 2001 shiny
RUN mkdir -p /var/run/shiny-server
RUN chown -R shiny:shiny /var/run/shiny-server /var/lib/shiny-server /srv/shiny-server /var/log/shiny-server

# Copy bootsrtapping script
COPY bootstrap.sh /usr/bin/bootstrap.sh

# Volumes
VOLUME /srv/shiny-server/

# Set working directory
WORKDIR /srv/shiny-server/

# Set the user
USER shiny

# Define default command
CMD ["/usr/bin/bootstrap.sh"]
