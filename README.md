# lncATLAS

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

## Docker deployment

A docker image with the latest released version of the application is available at ...

In order to build a docker image with a specific version of the application run the following command:

```bash
docker build -t lncatlas .
```

A new `lncatlas` image will now show at the top of the images list given by the command `docker images`.

The `lncATLAS` application resides in the folder `/srv/shiny-server/` within the image. Credentials to access the backend database must be specified in a `.mysqlconf` file within the application folder. The shiny-server instance will run on port `3838` inside the container and a port has to be mapped on the host to connect to it. The follwing command runs a container in detached mode mapping port `80` on the host and mounting the `.mysqlconf` file as a volume:

```bash
docker run -d -p 80:3838 -v /path/to/.mysqlconf:/srv/shiny-server/.mysqlconf lncatlas
```

## Authors

This project has been carried out by
<a href="http://www.dkf.unibe.ch/research/research_groups/rna_amp_cancer_nccr_rna_amp_disease/index_eng.html">Rory Johnson's Lab</a>
in the Department of Clinical Research (DCR) of the University of Bern and
<a href="http://www.crg.eu/roderic_guigo">Roderic Guigo's Lab</a>
in the
<a href="http://www.crg.eu">Centre for Genomic Regulation</a>
at Barcelona.

If you have questions or need further information please contact
us at
<a href="mailto:david.mas@crg.eu">david.mas@crg.eu</a>
