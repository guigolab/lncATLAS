FROM rocker/shiny:latest

MAINTAINER Emilio Palumbo <emilio.palumbo@crg.eu>

# Install mariadb client libraries
RUN apt-get update && apt-get install -y -t unstable \
    libmariadbclient-dev

# Adjust user permissions
RUN usermod -u 2001 shiny; groupmod -g 2001 shiny
RUN mkdir -p /var/run/shiny-server
RUN chown -R shiny:shiny /var/run/shiny-server /var/lib/shiny-server /srv/shiny-server

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
