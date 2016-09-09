# lncATLAS

Information for lncATLAS container

## Instructions

~~~bash
docker build -t lncATLAS .
~~~

Mapping to port 3838 (not installed in the host)

~~~bash
docker run -ti --name lncatlas -d -p 3838:3838 -v $(pwd)/lncATLAS-app:/srv/shiny-server/ -v $(pwd)/my.cnf:/srv/shiny-server/.mysqlconf -u shiny lncATLAS
~~~

To also map log files from the container to te host run the following command:

~~~bash
docker run -ti --name lncatlas -d -p 3838:3838 -v $(pwd)/lncATLAS-app:/srv/shiny-server/ -v $(pwd)/my.cnf:/srv/shiny-server/.mysqlconf -v /var/log/shiny-server/:/var/log/shiny-server/ -u shiny lncATLAS
~~~
