library(haven)
library(expss)
source('functions.R')
## original dataset
org_data <- read_sav('H:/LUST/data/EX2004_long_v10EF.sav')
## input instructions
input <- read.csv('input_scale.csv',na.strings = '')

## variables to identify important variables in the dataset
prefix <- 'x4_'
times <- as.character(4:8)
id = 'EX2004_id'
gender = 'Kon'
version <- 'x4_scale'
single_item <- 'x4_singel'
cut_mod <- 'x4_cut_mod'
missing_vals <- c(0,77,88,99)

## items that wrongly have 9 coded as missing instead of 99
recode_9_to_NA <- c('x4_5jobins1','x4_5copsoq28','x4_5qps17','x4_5qps15','x4_5demrel1','x4_5qps40',
                    'x4_5qps89','x4_5qps45','x4_5qps49','x4_5qps66','x4_5nwork12','x4_5nwork14')

## prepares data, the function returns a dataset and other objects needed for later steps
## se functions.R for further specifications
returnobj <- prepare_data(org_data,input,prefix,times,missing_vals,recode_9_to_NA)
rdata_rev <- returnobj[[1]]
select_vars <- returnobj[[2]]
indep_scales <- returnobj[[3]]
dep_scales <- returnobj[[4]]
indep_items <- returnobj[[5]]

## saves an image of the workspace to be loaded in later steps
save.image(file = sprintf('H:/LUST/new_data/%spre_impute.Rdata',prefix))