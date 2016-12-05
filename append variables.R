append_variables <- function(prefix){
#rm(list = ls())
library(haven)
  wrkspc <- sprintf('H:/LUST/new_data/%sprime.Rdata',prefix)
  load(wrkspc)
## reads workspace from reprime


fulldata <- tempdf
new <- colnames(fulldata)
org <- colnames(org_data)

## makes list of variables in the original dataset that are not included
new_vars <- subset(org, !(org %in% new[new != id]))
add_data <- org_data[new_vars]

imputs <-length(unique(fulldata$Imputation_))-1

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

## restores the labels if they have gotten lost during the process
for (v in colnames(df)){
  atr <- attributes(df[[v]])
  if (is.null(atr[['label']])){
    attributes(df[[v]])['label'] <- attributes(org_data[[v]])['label']
  }
}



return(df)
}