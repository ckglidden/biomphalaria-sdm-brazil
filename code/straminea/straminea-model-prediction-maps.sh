#!/bin/bash
#SBATCH --job-name=straminea_preds
#SBATCH --time=72:00:00
#SBATCH --qos long
#SBATCH -p normal
#SBATCH -c 16
#SBATCH --mem=128GB

ml math
ml gsl/2.7
ml system libressl/2.5.3 curl/7.54.0
ml physics gdal/2.2 udunits proj/4.9.3 geos
ml R/4.0

Rscript 8b-straminea-prediction-maps-AR.R
