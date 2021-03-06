# lncATLAS

![logo](lncATLAS-app/www/lncatlas.logo2.png)

 lncATLAS is an easy to use web-based visualization tool to obtain useful
information about expression localization of long non-coding class of
RNAs. It is easy to use by just searching your lncRNA of interest in the
search box. You can also use ENSEMBL gene ids to select
them. There are also some reference genes with a clear nuclear or
cytoplasmatic expression profile.


## Data

The data used and displayed in lncATLAS is directly obtained from
GENCODE release [V24](http://www.gencodegenes.org/releases/24.html)
and it is available through our website in the
download all data button from get raw data panel.

## Measuring localisation

The RCI or Relative Concentration Index is a measure based on the
logarithmic quotient between 2 fractions. For most of the
plots, we use the CN RCI or Cytosolic / Nuclear expression in FPKMs.

This is a dynamic measure that permits a quantitative comparison
between multiple genes and also permits analysing distributions of
gene sets.

## Citation

If you find lncATLAS useful for your work please cite us at:

>Mas-Ponte D, Carlevaro-Fita J, Palumbo E, Pulido TH, Guigo R, Johnson R. LncATLAS database for subcellular localization of long noncoding RNAs. Rna. 2017 Jul 1;23(7):1080-7. 

## Docker deployment

A Docker image with the latest released version of the application is available from the [Docker Hub](https://hub.docker.com/r/guigolab/lncatlas/). It can be downloaded with this command:

```bash
docker pull guigolab/lncatlas
```

In order to build a docker image with a specific version of the application run the following command:

```bash
docker build -t lncatlas .
```

A new `lncatlas` image will now show at the top of the images list given by the command `docker images`.

The `lncATLAS` application resides in the folder `/srv/shiny-server/` within the image. Credentials to access the backend database must be specified in a `.mysqlconf` file within the application folder. The shiny-server instance will run on port `3838` inside the container and a port has to be mapped on the host to connect to it. The follwing command runs a container in detached mode mapping port `80` on the host and mounting the `.mysqlconf` file as a volume:

```bash
docker run --name lncatlas -d -p 80:3838 -v /path/to/.mysqlconf:/srv/shiny-server/.mysqlconf lncatlas
```

## Authors

This project has been carried out by [Rory Johnson's Lab](https://gold-lab.org) in the Department of Clinical Research (DCR) of the University of Bern and
[Roderic Guigo's Lab](http://www.crg.eu/roderic_guigo) at the [Centre for Genomic Regulation](http://www.crg.eu) in Barcelona.

If you have questions or need further information please contact us at david.mas@crg.eu or open a issue in this repository.
