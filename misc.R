prefix <- 'x4_'
l <- 'H:/LUST/new_data/x4_prime_all.Rdata'
l <- sprintf('H:/LUST/new_data/%sfor_long.Rdata',prefix)
load(l)
library(mice)


version <- 'x4_scale'
single_item <- 'x4_singel'
cut_mod <- 'x4_cut_mod'
input <- read.csv('input_scale.csv',na.strings = '')
indep_items <- unique(input[single_item][!is.na(input[single_item]) & is.na(input['dependent'])]) # independent variables 
indep_scales <- unique(input[version][!is.na(input[version]) & is.na(input['dependent'])]) # independet scales 
indeps <- c(indep_items,indep_scales)
#indeps <- indeps[indeps %in% colnames(fulldata)]
as.mids(df[c('Imputation_','ID','x4_8genhealt','support_chef_a_first_3')])



deps <- c('x4_8genhealt','x4_8sleepq1')
types <- c(sapply(c('first_','last_'), function(x) sprintf('%s%s',x,1:4)), 'sum')

dep_cut <- '(x4_8genhealt > 2)'
dep <- deps[1]
#tempdat <- dat
dep
outfile <- sprintf('out/%slong_odds.csv',prefix)
write.table(t(c('dep','indep','OR','OR_lo95','OR_hi95','indep_proc')),outfile,row.names = F, col.names = F, sep = ',')
for (dep in deps){
  for (indep in indeps){
    for (t in types){
      fx1 <- sprintf('%s_%s',indep,t)
      if (fx1 %in% colnames(df)){
        tempdat <- as.mids(df[c('Imputation_','ID',dep,fx1)])
        if (t == 'sum'){
          for (c in 0:3){
            fx <- sprintf('as.numeric(%s > %s)',fx1,c)
            tab <- table(df[fx1] > c)
            proc <- round((tab[2]/sum(tab))*100,2)
            if (length(tab) > 1) {
              log_reg(dep_cut = dep_cut,fx = fx,tempdat = tempdat,df=df,fx1=fx1,proc=proc)
            }
          }
        } else {
          fx <- fx1
          tab <- table(df[fx1])
          proc <- round((tab[2]/sum(tab))*100,2)
          log_reg(dep_cut = dep_cut,fx = fx,tempdat = tempdat,df=df,fx1=fx1,proc = proc)
        }
      } else {
        print(sprintf('%s not in dataset',fx1))
      }
    }
  }
}

log_reg <- function(dep_cut,fx,tempdat,df,fx1,proc){
  f <- formula(paste(dep_cut,fx, sep = ' ~ '))
  log_reg <- glm.mids(f, data=tempdat, family = 'binomial')
  s_reg <- summary(pool(log_reg))
  OR <- round(exp(s_reg[2,c(1,6,7)]),3)
  write.table(t(c(dep,fx,OR[1],OR[2],OR[3],proc)), row.names = F, col.names = F, append = T, file = outfile, sep = ',')
}



















