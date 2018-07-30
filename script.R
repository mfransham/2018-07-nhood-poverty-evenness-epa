##################################################
# code files in folder "/code"
##################################################

# code blocks written in R version 3.4.3 (2017-11-30) "Kite-Eating Tree"
source("code/load-packages.R")
source("code/data-retrieval.R") # gets the data, pulls out the right bits, calculates absolute numbers
source("code/poverty-rates-by-date.R") # calculate ID poverty rates by country, date and LA supergroup
source("code/evenness-indices.R") # functions to calculate indices of evenness
source("code/index-calc-all-ages.R") # calculate dissimilarity indices for all ages by country, LA, region
# analysis for children
source("code/index-calc-child.R")
source("code/chart-density-child.R")
source("code/hmrc-data-impute.R")
source("code/chart-timeseries-ch.R")
# analysis for working age people
source("code/index-calc-wkg.R")
source("code/chart-density-wkg.R")
source("code/chart-timeseries-wkg.R")
# analysis for older people
source("code/index-calc-older.R")
source("code/chart-density-older.R")
source("code/chart-timeseries-op.R")
# visualise the distribution of the independent variables
source("code/chart-ind-vars.R")
# prepare variables for multiple regression
source("code/var-prep-PRS.R")
source("code/var-prep-RSL.R")
source("code/var-prep-dilution-displacement.R")
# collate data and run models
source("code/model-ch-data.R")
source("code/model-ch-spec.R")
source("code/model-ch-table.R")
source("code/model-ch-alt.R") # alternative models using other indices
# working age 
source("code/model-wkg-data.R")
source("code/model-wkg-spec.R")
source("code/model-wkg-table.R")
# older people
source("code/model-op-data.R")
source("code/model-op-spec.R")
source("code/model-op-table.R")