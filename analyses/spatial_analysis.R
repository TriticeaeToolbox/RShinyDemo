###############################################################################
# Function for Estimating variance component, heritability , RMSE, MAD and correlation for the whole data
###############################################################################
#############################################
# Data preparation for the analysis
library(tidyverse)
library(sommer)
library(caret)
library(dplyr)
library(lme4)
library(rrBLUP)
library(tibble)
library(readr)
library(stringr)
library(genomicMateSelectR)
#########################################
# Import phenotype data
#######################################

source("./analyses/GRM_matrix.R")
source("./analyses/stage1_analysis.R")
source("./analyses/stage2_analysis.R")
source("./utils/getTraitNames.R")
source("./utils/cleanTraitName.R")
##############

spatial_analysis <- function(Traits, data, marker_data) {

    # Clean the trait names (in list of traits and data frame)
    Traits = unlist(lapply(Traits, cleanTraitName))
    data = rename_with(data, cleanTraitName)

    BLUE = stage1(Traits, data)
    GRM = GRM_matix(marker_data = marker_data)
    GEBV = stage2(data = BLUE, GRM = GRM)

    return(list(
        BLUE = BLUE,
        GRM = GRM,
        GEBV = GEBV
      
    ))
 }
