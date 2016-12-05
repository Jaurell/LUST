prefix <- 'x4_'
l <- sprintf('H:/LUST/new_data/%sprime_all.Rdata',prefix)
load(l)
library(mice)
dat <- as.mids(df)
saveRDS(dat, 'x4_mids.RDS')
dat <- readRDS('x4_mids.RDS')

dep <- 'x4_8sleepq1'
input <- read.csv('input_scale.csv',na.strings = '')
indep_items <- unique(input[single_item][!is.na(input[single_item]) & is.na(input['dependent'])]) # independent variables 
indep_scales <- unique(input[version][!is.na(input[version]) & is.na(input['dependent'])]) # independet scales 
indeps <- c(indep_items,indep_scales)
indeps
dep <- '(x4_8genhealt > 2)'
indep <- 'krav_kvantitativa'
table()
frequency('re')
for (indep in indeps){
  fx <- sprintf('%s_last_2',indep)
  f <- formula(paste(dep,fx, sep = ' ~ '))
  log_reg <- glm.mids(f, data=dat)
  print(summary(pool(log_reg)))
}
f




attributes(df[['x4_4qps15']])









a1 <- glm(((x4_6satworka + x4_6satworkb + x4_6satworkc) > 6) ~ (((x4_6occdev3 > 2) + (x4_6occdev4 > 2)) > 1),data = x4, family = 'binomial')
or1 <- exp(coef(a1)[2])
t1 <- table(rowSums(x4[c('x4_6occdev3','x4_6occdev4')] > 2)>1)
q1 <- t1[2]/sum(t1)
#median(rowSums(rdata_rev[c('x4_8satworka','x4_8satworkb','x4_8satworkc')]),na.rm =T)


a2 <- glm(((x6_4satworka + x6_4satworkb + x6_4satworkc) > 6) ~ (((x6_4occdev3 > 2) + (x6_4occdev4 > 2)) > 1),data = x6, family = 'binomial')
or2 <- exp(coef(a2)[2])
t2 <- table(rowSums(x6[c('x6_4occdev3','x6_4occdev4')] > 2)>1)
q2 <- t2[2]/sum(t2)
#median(rowSums(rdata_rev[c('x4_8satworka','x4_8satworkb','x4_8satworkc')]),na.rm =T)

c(or1,or2)
c(q1,q2)
(x6_4occdevkno > 3)
a3 <- glm((x6_4occdevkno > 3) ~ (((x6_4occdev3 > 3) + (x6_4occdev4 > 3)) > 0),data = x6, family = 'binomial')
exp(coef(a3)[2])
summary(a3)

table(is.na(rdata_rev)) - table(is.na(fulldata))) / table(is.na(rdata_rev))

fulldata <- complete(midata)
table(is.na(fulldata))
table(is.na(rdata_rev))



l <- list(ps[1:2],ps[3:4],ps[5:6])



f <- do.call(expand.grid, l)
n <- sapply(f,names)
plist <- list()
for (h in 1:nrow(f)){
  val <- print(prod(f[h,]))
  nam <- paste(n[h,],collapse = '_')
  plist[nam] <- val
}
unlist(plist)










