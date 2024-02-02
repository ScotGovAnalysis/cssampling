# cssampling
This repository contains the Reproducible Analytical Pipeline (RAP) for the sampling of the three centralised Scottish surveys: the Scottish Household Survey (SHS), the Scottish Health Survey (SHeS) and the Scottish Crime and Justice Survey (SCJS).

## Running the RAP

The only file that needs to be updated before running the cssampling RAP is the config.R file. Values need to be assigned to the following objects:
1. `syear`: Year the sample is taken for (i.e., 20XX).
2. `infilenm.path`: File path of most recent postcode address file. This file is supplied by NRS.
3. `pcd.path`: File path of postcode file with old addresses. This file is updated twice a year. The most recent version should be used. Data can be accessed here: https://www.nrscotland.gov.uk/statistics-and-data/geography/nrs-postcode-extract
4. `dz_simd.path`: File path of SIMD rank information for each datazone. This file includes all datazones and their SIMD rank.
5. `dz.path`: File path of datazone information dataset. This file includes all datazones, their LA, urb/rur status, deprivation and cluster for SHeS. The file is should be updated regularly. The most recent version should be used. 
6. `hh_dz.path`: File path of household estimates by datazone. This file is updated by NRS once a year. The most recent version should be used. Data can be accessed here: https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/households/household-estimates/small-area-statistics-on-households-and-dwellings
8. `scjs.samplesize.path`: File path of SCJS sample size file. This file includes all LA codes and their reserve sample, contractor sample and total sample.
9. `scjs.contractor.sample.previous.path`: File path of previous year's SCJS contractor sample.
10. `previoussamples.path`: File paths of all samples drawn in the last four years.

## Licence

This repository is available under the [Open Government Licence v3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/).
