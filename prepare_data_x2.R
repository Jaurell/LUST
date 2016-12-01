library(haven)
library(expss)
source('functions.R')
org_data <- read_sav('H:/LUST/data/EX2002_long_v1.sav')

input <- read.csv('input_scale.csv',na.strings = '')

prefix <- 'x2_' #'x6_' #'x4_'
times <- as.character(1:3)#as.character(4:8)
#urvalsinfo = c('svar_1', '1')
#urvals_var = 'x6_4svar_4' #'x4_8svar_8'
id = 'lopnr'  #'x6_1UENR_ID' #'EX2004_id'
gender = 'x2_1sex' #'x6_1kon' #'Kon'
version <- 'suggestion_03'
single_item <- 'single_item_02'
cut_mod <- 'cut_mod_05'
missing_vals <- c(0,77,88,99)


indep_scales <- unique(input[version][!is.na(input[version]) & is.na(input['dependent'])])
dep_scales <- unique(input[version][!is.na(input[version]) & !is.na(input['dependent'])])

select_vars <- as.vector(input[['items']][which(!is.na(input[version]) | !is.na(input[single_item]))])
rev_vars <- select_vars[select_vars %in% as.vector(input[['items']][which(!is.na(input['reverse']))])]


rev_vars <- sapply(rev_vars, function(x) sprintf('%s%s%s',prefix,times,x))
select_vars <- sapply(select_vars, function(x) sprintf('%s%s%s',prefix,times,x))
select_vars[which(!select_vars %in% colnames(org_data))] <- NA
rev_vars[which(!rev_vars %in% colnames(org_data))] <- NA
vars <- as.vector(select_vars);vars <- vars[!is.na(vars)]

rev_list <- as.vector(rev_vars)
rev_list <- rev_vars[which(!is.na(rev_vars))]
select_list <- c(id,gender,as.vector(select_vars))
select_list <- select_list[which(!is.na(select_list))]

rdata <- org_data[select_list]
#rdata <- rdata[which(rdata[urvals_var] == 1),]

rdata[rdata == 99 | rdata == 88 | rdata == 77 | rdata == 0 | is.na(rdata)]<- NA
recode_9_to_NA <- c('x4_5jobins1','x4_5copsoq28','x4_5qps17','x4_5qps15','x4_5demrel1','x4_5qps40',
                    'x4_5qps89','x4_5qps45','x4_5qps49','x4_5qps66','x4_5nwork12','x4_5nwork14')
for (var in recode_9_to_NA){
  rdata[var][which(rdata[var] == 9),] <- NA
}

rdata_rev <- check_reverse(rdata,vars,rev_list,missing_vals = missing_vals)
cat(paste(readLines('check_log.txt'), collapse = '\n'))

#for (var in colnames(rdata_rev)){
#  attributes(rdata_rev[[var]])$label <- gsub('Õ|Ô','',attributes(rdata_rev[[var]])$label)
#}

check_variables(rdata_rev,c(indep_scales,dep_scales),input = input)
select_vars
