prefix <- 'x6_'
#rm(list = ls())
library(haven)
library(mice)
#wrkspc <- sprintf('H:/LUST/new_data/%sprime.Rdata',prefix)
#load(wrkspc)
## reads workspace from reprime
keep <- c('prefix','fulldata','org_data','id')
rm(list = ls()[!ls() %in% keep])
midata2 <- as.mids(fulldata)
rm(fulldata)
incl <- names(midata2$data) # variable names from midata
incl <- names(org_data)[!names(org_data) %in% incl] # all variable names in org_data that is not in midata
add_data <- org_data[c(incl,id)] # new dataframe with incl variables and the id variable
add_data <- add_data[match(midata2$data[[id]],add_data[[id]]),] # sort add_data to match midata on id variable
gc() # empty RAM
midata_all <<- cbind.mids(midata2, data.frame(add_data[!colnames(add_data) == id])) # binds add_data to midata and excludes id variable to not make a duplicate
rm(midata2,add_data)
gc()
start <- Sys.time()
fulldata_all <<- complete(midata_all,include = T,'long') # fulldata with all variables
Sys.time() - start
#utils::View(fulldata_all[c('EX2004_id','LOPNRx4_1','lopnr',gender)]) # to check if it seems right
#fulldata_all[c('EX2004_id','LOPNRx4_1','lopnr')][which(fulldata_all['EX2004_id'] != fulldata_all['lopnr']),]

imput_pre_image <- sprintf('H:/LUST/new_data/%spre_impute.Rdata',prefix)
load(imput_pre_image)
fulldata_all <- format_spss_impute(fulldata_all)
first_vars <- c('Imputation_','ID',id)
last_vars <- colnames(fulldata_all)
last_vars <- last_vars[!last_vars %in% first_vars]
fulldata_all <- fulldata_all[c(first_vars,last_vars)]

## restores the labels if they have gotten lost during the process
for (v in colnames(fulldata_all)){
  atr <- attributes(fulldata_all[[v]])
  if (is.null(atr[['label']])){
    attributes(fulldata_all[[v]])['label'] <- attributes(org_data[[v]])['label']
  }
}
gc()
start <- Sys.time()
write_sav(fulldata, sprintf('C:/Users/jonaur/Desktop/Jon/%sprime_all_3454.sav',prefix))
Sys.time() - start

