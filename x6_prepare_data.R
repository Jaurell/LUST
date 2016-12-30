## value labels for items c('x6_1burpload','x6_1burprew','x6_1burpcomm','x6_1burpcont','x6_1burpfair','x6_1burpval') 
# needs to be shortend in order to be able to save the file to an spss format

library(haven)
library(expss)
#source('functions.R')
## original dataset
org_data <- read_sav('H:/LUST/data/EX2006_long_v3EF.sav')
## input instructions
input <- read.csv('input_scale.csv',na.strings = '')

## variables to identify important variables in the dataset
prefix <- 'x6_' #prefix for the cohort
times <- as.character(2:4) #which 
id = 'lopnr' 
gender = 'x6_1kon' 
version <- 'x6_scale'
version2 <- 'x6_scale2'
cut_mod <- 'x6_cut_mod'

missing_vals <- c(0,77,88,99)


## prepares data, the function returns a dataset and other objects needed for later steps
## se functions.R for further specifications
prepare_data(org_data,input,prefix,times,missing_vals)


