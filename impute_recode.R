source('prepare_data.R')
library(mice)

vars <- as.data.frame(select_vars)
vars <- vars[c('qps38','qps40')]
df <- list()

rdata_rev[['Kon']]

for (var in colnames(vars)){
  mi <- vars[var]
  mi <- mi[!is.na(mi)]
  print(mi)
  if (length(mi) > 1){
    m <- mice(rdata_rev[mi], m = 2, maxit = 1,method = 'norm.predict')
    df[[var]] <- m
  }
}

for (x in 1:(length(df)-1)){
  print(x)
  if (x == 1){
    out <- cbind.mids(df[[x]],df[[x+1]])
  }else{
    out <- cbind.mids(out,df[[x+1]])
  }
}

s <- pool(glm.mids((x4_4qps40 < 2) ~ as.numeric(x4_4qps38 > 3) -1, data = out, family = 'binomial'))
ss <-          glm((x4_4qps40 < 2) ~ as.numeric(x4_4qps38 > 3) -1, data = rdata_rev, family = 'binomial')
f <- glm.mids((x4_4qps40 < 2) ~ as.numeric(x4_4qps38 > 3) -1, data = out, family = 'binomial')


scales <- input[version][!is.na(input[version])]
dep<-'x4_4qps40'

for (t in times){
  for (scale in scales){
    items <- input['items'][input[version] ==  scale]
    items <- sapply(items[!is.na(items)], function(x) sprintf('%s%s%s',prefix,t,x))
    items <- items[items %in% colnames(rdata_rev)]
    for (var in items){
      in_var <- substring(var,5)
      cut_val <- ceiling(max(out$imp[[var]], na.rm = T)/2)
      mod <- input[cut_mod][input['items'] == in_var]
      mod[is.na(mod)] <- 0
      cut_val <- cut_val - mod
      for (dep in deps){
        median <- 
        res <- summary(pool(glm.mids((get(dep) > median) ~ as.numeric(get(pred) > 3) -1,data = out,family = 'binomial')))
        summary(pool(out))
        summary(out)
        out$imp
        exp(res[c(1,6,7)])
      }
    }
  }
}

out$imp$new <- out$imp$x4_4qps38 + out$imp$x4_4qps39
out$data[c('x4_4qps38','x4_4qps39')]

max(out$imp$new)
max(out$imp[[var]], na.rm = T)
for var in vars{
  
}

whoot<-complete(out)
whoot[whoot <= 3] <- 0
whoot[whoot > 3] <- 1
whoot$x4_4qps38
summary(glm(x4_4qps40 ~ x4_4qps38 -1, data = whoot,family = 'binomial'))
summary(s)
?mice
?I
cor(rdata[])
rdata$dep <- rdata$x4_4qps40
rdata$indep <- rdata$x4_4qps38
rdata['dep'][which(rdata['dep'] <= 3),] <- 0
rdata['dep'][which(rdata['dep'] > 3),] <- 1
rdata['indep'][which(rdata['indep'] <= 3),] <- 0
rdata['indep'][which(rdata['indep'] > 3),] <- 1
lm()
f
out$imp$x4_4qps40
summary(glm(dep~indep-1,data = rdata,family = 'binomial'))
s <- pool(glm.mids(as.numeric(I(x4_4qps40 > 3)) ~ as.numeric(I(x4_4qps38 > 3)) -1, data = out, family = 'binomial'))
ss <-          glm(as.numeric(I(x4_4qps40 > 3)) ~ as.numeric(I(x4_4qps38 > 3)) -1, data = rdata_rev, family = 'binomial')
summary(ss)
summary(s)
s <- summary(glm(dep~indep-1,data = rdata,family = 'binomial'))
ss <- summary(s)
as.numeric((5<3))
?mice
summary(s)
coef(ss)
OR <- exp(cbind(OR = coef(s), confint(s)))
OR
ss
exp(coef(s))
exp(ss[[1]])
ss
svars
out$imp$x4_4qps38[out$imp$x4_4qps38 <= 3] <- 0 
out$imp$x4_4qps38[out$imp$x4_4qps38 > 3] <- 1
out$imp$x4_4qps38
rownames()
out$imp[c('x4_4qps38','x4_4qps39','x4_4qps40')]
items
fest <- sapply(items, function(x) row.names(out$imp[[x]]))
fest
fest[[1]][!fest[[1]] %in% fest[[2]]]
fest[[1]]
mest
fest
mest <- row.names(out$imp$x4_4qps38)

glm.mids(get(var) ~ I(emot_anx > cut) - 1, data, family = 'binomial')

t <- input[cut_mod][input['items'] == 'qps38']
is.na(t)
t
input[input['items'] == 'qps38']
i <- 'x4_4qps38'
substring(i,5)
items <- as.vector(select_vars)[!is.na(as.vector(select_vars))]

for (i in items){
  cut <- input[cut_mod][input['items'] == ]
}

out
I(var > median(var))
test <- out
test$data['x4_4qps38'][is.na(test$data['x4_4qps38'])] <- 99

summary(rdata_rev[c('x4_6satworka','x4_7satworka','x4_8satworka')])
summary(rdata_rev[c('x4_6satworkc','x4_7satworkc','x4_8satworkc')])

?mice
median()
out
summary(test$data['x4_4qps38'])
summary(test$imp[['x4_4qps38']])
test$imp[['x4_4qps38']][test$imp[['x4_4qps38']] == 1] <- 99

?mids
length(rdata[[id]])
pool(median()out)
md.pattern(rdata_rev[c('x4_4qps38','x4_4qps38','x4_6qps38','x4_7qps38','x4_8qps38')])
