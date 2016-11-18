library(haven)
source('functions.R')
org_data <- read_sav('H:/LUST/data/EX2004_long_v10EF.sav')
input <- read.csv('input_scale.csv',na.strings = '')

prefix <- 'x4_'
times <- as.character(4:8)
urvalsinfo = c('svar_1', '1')
id = 'EX2004_id'
gender = 'Kon'
version <- 'suggestion_03'
single_item <- 'single_item_02'
cut_mod <- 'cut_mod_05'
missing_vals <- c(0,77,88,99)

select_vars <- as.vector(input[['items']][which(!is.na(input[version]) | !is.na(input[single_item]))])
rev_vars <- select_vars[select_vars %in% as.vector(input[['items']][which(!is.na(input['reverse']))])]

rev_vars <- sapply(rev_vars, function(x) sprintf('%s%s%s',prefix,times,x))
select_vars <- sapply(select_vars, function(x) sprintf('%s%s%s',prefix,times,x))
select_vars[which(!select_vars %in% colnames(org_data))] <- NA
rev_vars[which(!rev_vars %in% colnames(org_data))] <- NA

rev_list <- as.vector(rev_vars)
rev_list <- rev_vars[which(!is.na(rev_vars))]
select_list <- c(id,gender,urvalsinfo[1],as.vector(select_vars))
select_list <- select_list[which(!is.na(select_list))]

rdata <- org_data[select_list]
rdata[rdata == 99 | rdata == 88 | rdata == 77 | rdata == 0 | is.na(rdata)]<- NA
recode_9_to_NA <- c('x4_5jobins1','x4_5copsoq28','x4_5qps17','x4_5qps15','x4_5demrel1')
for (var in recode_9_to_NA){
  rdata[var][which(rdata[var] == 9),] <- NA
}

rdata_rev <- item_rev(rdata,rev_list,missing_vals = missing_vals)

