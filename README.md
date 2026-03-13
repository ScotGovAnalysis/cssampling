# cssampling
This repository contains the Reproducible Analytical Pipeline (RAP) for the sampling of the three centralised Scottish surveys: the Scottish Household Survey (SHS), the Scottish Health Survey (SHeS) and the Scottish Crime and Justice Survey (SCJS).

## Running the RAP

The only file that needs to be updated before running the cssampling RAP is the config.R file. An example of this file with placeholder values can be found in the `scripts` folder.

To draw a sample for one of the centralised surveys, the relevant master script should be run (i.e., `shs_sampling.R`, `scjs_sampling.R` or `shes.sampling.R`). This file sources all required scripts and executes them.

## Licence

Unless stated otherwise, the codebase is released under [the MIT License](LICENSE). This covers both the codebase and any sample code in the documentation.

The documentation is [© Crown copyright](https://www.nationalarchives.gov.uk/information-management/re-using-public-sector-information/uk-government-licensing-framework/crown-copyright/) and available under the terms
of the [Open Government 3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) licence.
