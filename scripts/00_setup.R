#########################################################################
# Name of file - 00_setup.R
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Sets up environment required for running the cssampling RAP.

#########################################################################

### 1 - Load packages ----

library(tidyverse)
library(haven)
library(sampling)
library(janitor)
library(here)
library(openxlsx)
library(readxl)
library(rlang)
library(crayon)

### 2 - Load functions from functions folder of Rproject ----

walk(list.files(here("functions"), pattern = "\\.R$", full.names = TRUE), 
     source)

### 3 - Load config file from code folder of RProject ----

# The config.R script is the only file which needs to be updated before 
# the RAP can be run. 

source(here::here("scripts", "config.R"))

### 4 - Create folders ----

# If output folders for syear specified above 
# don't already exist, create folders

folders <- paste0(
  here("output"), "/", 
  config$syear, " Sampling"
)

subfolders <- paste0(
  folders, "/", 
  c("Scottish Crime and Justice Survey ",
    "Scottish Health Survey ",
    "Scottish Household Survey "), config$syear, "/"
)

# Create subfolder for each survey

walk(subfolders,
  ~ if(!file.exists(.x)) dir.create(.x, recursive = TRUE)
)


# Create lookups folder

walk(here("lookups"),
     ~ if(!file.exists(.x)) dir.create(.x, recursive = TRUE)
)

### 5 - Create paths of individual survey ----

# add path of SCJS output folder
scjs.path <- subfolders[1]

# add path of SHeS output folder
shes.path <- subfolders[2]

# add path of SHS output folder
shs.path <- subfolders[3]

### 6 - Extract PAFs ----

# extract PAF version from file name
paf_v <- str_match(config$infilenm.path, "\\_\\s*(.*?)\\s*\\.txt")[, 2]

# get file names of previous cleaned PAFs
paf_list <- list.files(path = here("lookups"),
                       pattern = "paf")

### 7 - Message style ----

title <- black $ bold

normal <- black
