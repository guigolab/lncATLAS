FROM rocker/shiny:latest

MAINTAINER Emilio Palumbo <emilio.palumbo@crg.eu>

# install mariadb client libraries
RUN apt-get update && apt-get install -y -t unstable \
    libmariadbclient-dev

# adjust user permissions
RUN usermod -u 2001 shiny; groupmod -g 2001 shiny
RUN chown -R shiny:shiny /var/lib/shiny-server /var/run/shiny-server 

# copy bootsrtapping script
COPY bootstrap.sh /usr/bin/bootstrap.sh

# Volumes
VOLUME /home/shiny/
VOLUME /srv/shiny-server/

# Set working directory
WORKDIR /srv/shiny-server/

# Define default command
CMD ["/usr/bin/bootstrap.sh"]
