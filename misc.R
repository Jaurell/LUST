log_reg <- function(dep_cut,fx,tempdat,df,indep_proc,dep_proc){
  f <- formula(paste(dep_cut,fx, sep = ' ~ '))
  log_reg <- glm.mids(f, data=tempdat, family = 'binomial')
  s_reg <- summary(pool(log_reg))
  OR <- round(exp(s_reg[2,c(1,6,7)]),3)
  fout <- as.character(f)
  write.table(t(c(fout[2],fout[3],OR[1],OR[2],OR[3],indep_proc,dep_proc)), row.names = F, col.names = F, append = T, file = outfile, sep = ',')
}

tempdat <- as.mids(df[c('Imputation_','ID','x4_8moodev4','reward_uppskattning_chef_sum')])
glm.mids((x4_8moodev4 > 1) ~ as.numeric(reward_uppskattning_chef_sum > 0) , data=tempdat, family = 'binomial')

prefix <- 'x4_'
l <- 'H:/LUST/new_data/x4_prime_all.Rdata'
l <- sprintf('H:/LUST/new_data/%sfor_long.Rdata',prefix)
load(l)
library(mice)


version <- 'x4_scale'
single_item <- 'x4_single'
cut_mod <- 'x4_cut_mod'
input <- read.csv('input_scale.csv',na.strings = '')
indep_items <- unique(input[single_item][!is.na(input[single_item]) & is.na(input['dependent'])]) # independent variables 
indep_scales <- unique(input[version][!is.na(input[version]) & is.na(input['dependent'])]) # independet scales 
indeps <- c(indep_items,indep_scales)


types <- c(sapply(c('first_','last_'), function(x) sprintf('%s%s',x,1:4)), 'sum')

outfile <- sprintf('out/%slong_odds.csv',prefix)
write.table(t(c('dep','indep','OR','OR_lo95','OR_hi95','indep_proc','dep_proc')),outfile,row.names = F, col.names = F, sep = ',')
for (n in 15:nrow(longexp)){
  dep <- as.character(longexp[n,1])
  dep_cut <- sprintf('(%s)',paste(longexp[n,1],longexp[n,2], collapse = ' '))
  dep_tab <- eval(parse(text = sprintf("table(df[dep][which(!df['Imputation_'] == 0),]%s)",longexp[n,2])))
  dep_proc <- round(dep_tab[2]/sum(dep_tab)*100,2)
  for (indep in indeps[2]){
    for (t in types){
      fx1 <- sprintf('%s_%s',indep,t)
      if (fx1 %in% colnames(df)){
        tempdat <- as.mids(df[c('Imputation_','ID',dep,fx1)])
        if (t == 'sum'){
          for (c in 0:3){
            fx <- sprintf('as.numeric(%s > %s)',fx1,c)
            indep_tab <- table(df[fx1][which(!df['Imputation_'] == 0),] > c)
            indep_proc <- round((indep_tab[2]/sum(indep_tab))*100,2)
            if (length(indep_tab) > 1) {
              log_reg(dep_cut = dep_cut, fx = fx, tempdat = tempdat, df = df, indep_proc = indep_proc, dep_proc = dep_proc)
            }
          }
        } else {
          fx <- fx1
          indep_tab <- table(df[fx])
          indep_proc <- round((indep_tab[2]/sum(indep_tab))*100,2)
          log_reg(dep_cut = dep_cut, fx = fx, tempdat = tempdat, df = df, indep_proc = indep_proc, dep_proc = dep_proc)
        }
      } else {
        print(sprintf('%s not in dataset',fx1))
      }
    }
  }
}

df[fx1][which(!df['Imputation_'] == 0),]

unique(df$Imputation_)
f

dep <- 'x4_8mdi'
dep_cut <- '(x4_8mdi < 28)'
fx <- 'blivit_bully2_first_4'
tempdat <- as.mids(df[c('Imputation_','ID',dep,fx)])

table(df[dep] > 1)
s <- glm.mids((x4_8mdi < 28) ~ blivit_bully2_first_4, data=as.mids(df[c('Imputation_','ID','blivit_bully2_first_4','x4_8mdi')]), family = 'binomial')
ss <- summary(pool(s))
exp(ss)
ss

eval(parse(text = sprintf('table(df[dep]%s)',longexp[n,2])))


eval(parse(text = txt))

?eval
eval(envir = table(df[dep]), expr = parse(text = longexp[n,2]))



table(df['x4_8moodno7'] < 3)



