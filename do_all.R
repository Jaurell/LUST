rm(list = ls())
prefix <- 'x4_' # sets the cohort
input <- read.csv('input_scale.csv',na.strings = '')
source('functions.R')
source('impute.R') # loads the functions to impute the data
source('analyse.R') # loads the function to do all the logistic analysises, this step can be skipped without consequenses
source('reprime.R') # codes new longitudinal variables
#source('append variables.R') # adds all variables from the original data to the imputed dataset
#source(sprintf('%sprepare_data.R',prefix)) # prepares the datafile
##
imput_pre_image <- sprintf('H:/LUST/new_data/%spre_impute.Rdata',prefix)
imput_image <- sprintf('H:/LUST/new_data/%simputed.Rdata',prefix)
reprime_image <- sprintf('H:/LUST/new_data/%sprime.Rdata',prefix)
reprime_all_image <- sprintf('H:/LUST/new_data/%sprime_all.Rdata',prefix)
source(sprintf('%sprepare_data.R',prefix))
save.image(file = imput_pre_image)

impute(prefix) # run impute and assign the mids object to the global enviorment so its saved with the workspace
save.image(file = imput_image) # save workspace
#load(imput_image)
load(imput_image)
source('analyse.R')
analyse(prefix) # analyse

load(imput_image)
source('reprime.R') # codes new longitudinal variables
reprime(prefix, 0) # run reprime and assign the dataframe to the global enviorment so its saved with the worksapce
save.image(file = reprime_image)# save workspace
write_sav(fulldata, sprintf('C:/Users/jonaur/Desktop/Jon/%sprime_all.sav',prefix))


#load(reprime_all_image)
#not a function any more ###append_variables(prefix) # run append_variables and assign the dataframe to the global enviorment so its saved with the worksapce
save.image(file = reprime_all_image) # save workspace

# the spss file will be huge before resaving it in spss, therefore it is recomended to save it to your local hard drive first and then move it to the network drive
write_sav(fulldf, sprintf('C:/Users/jonaur/Desktop/Jon/%sprime_all.sav',prefix))

