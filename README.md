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

A docker container with the latest released version of the application is available at ...

A container can be built at any point using the following command:

```bash
docker build -t lncatlas .
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
