rm(list = ls())
library(haven)
## reads workspace from reprime
load('H:/LUST/new_data/x4_prime.Rdata')
#load('H:/LUST/new_data/x6_prime.Rdata')

fulldata <- tempdf
new <- colnames(fulldata)
org <- colnames(org_data)

## makes list of variables in the original dataset that are not included
new_vars <- subset(org, !(org %in% new[new != id]))
add_data <- org_data[new_vars]

## if a variable is named ID this renames it to ID.2 ID is the variable name spss uses for multiple imputation datasets
if ('ID' %in% colnames(add_data)){
  names(add_data)[names(add_data) == 'ID'] <- 'ID.2'
}

## makes a list of merged data for the unimputed variables
L <- lapply(0:imputs, function(x) merge(fulldata[which(fulldata[1] == x),],add_data, by = id))
## binds all the items in the list to one long dataset
df <- do.call("rbind", L)
## order of columns in dataset
first_vars <- c('Imputation_','ID',id)
last_vars <- colnames(df)
last_vars <- last_vars[!last_vars %in% first_vars]
df <- df[c(first_vars,last_vars)]

## saves an image of the workspace for later use.
save.image(file = sprintf('H:/LUST/new_data/%sprime_all.Rdata',prefix))

## saves a spss file on the harddrive (it takes a long time if you want to save it on the network drive)
## This file will be very big, to reduce the size open it in spss and save it.
write_sav(df, sprintf('C:/Users/jonaur/Desktop/Jon/%sprime_all.sav',prefix))

