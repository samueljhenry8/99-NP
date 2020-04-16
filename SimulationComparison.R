
### Simulation Comparison

foo2 = function(x, folds=2) {
  s = sample(folds, nrow(x), replace=T)
  r = x
  for(h in 1:folds){
    ss = (1:nrow(x))[s == h]
    for(i in 1:ncol(x))
      r[ss,i]  = cv.glmnet(as.matrix(x[-ss,-i]),  x[-ss,i],  alpha = .05) %>% predict(as.matrix(x[ss,-i]), s="lambda.min")
  }
  r
}

## select only those vars with at least 500 observations
## this will be the ground truth (gt)
vars = names(comb)[comb %>% as.list %>% map_dbl(~sum(!is.na(.x))) > 500]
gt = comb %>% select(vars) %>% na.omit %>% apply(2, scale)

## add noise to the data, control the amount by varying sd
## this is real data: ground truth measured twice with error
noisy1 = gt + matrix(nrow=nrow(gt), ncol=ncol(gt), rnorm(ncol(gt) * nrow(gt), sd=.5))
noisy2 = gt + matrix(nrow=nrow(gt), ncol=ncol(gt), rnorm(ncol(gt) * nrow(gt), sd=.5))

## "retest reliability" in noisy data
cor(noisy1, noisy2) %>% diag %>% summary

## predicted data
predicted1 = foo2(noisy1, folds = 10)
predicted2 = foo2(noisy2, folds = 10)

## smoothed data, scale first easy the weighting
smooth1 = .75 * apply(noisy1, 2, scale) + .25 * apply(predicted1, 2, scale)
smooth2 = .75 * apply(noisy2, 2, scale) + .25 * apply(predicted2, 2, scale)

## "retest reliability" in smoothed data
cor(smooth1, smooth2) %>% diag %>% summary

## correlation of smoothed and noisy data
cor(smooth1, noisy1) %>% diag %>% summary
cor(smooth2, noisy2) %>% diag %>% summary

## correlations with ground truth
## smoothed data is not expected to correlate perfectly
## not because it does not correct for random error
## but because it changes the meaning of the data
cor(smooth1, gt) %>% diag %>% summary
cor(smooth2, gt) %>% diag %>% summary
cor(noisy1, gt) %>% diag %>% summary
cor(noisy2, gt) %>% diag %>% summary

## compare some distributions -- smoothing improves them
par(mfrow=c(3,2))
for(i in 1:3){
  hist(gt[,i], breaks=25)
  hist(smooth1[,i], breaks=25)
}

## compare skewnesses
par(mfrow=c(1,1))
plot(psych::skew(gt), psych::skew(smooth1), xlim=c(-1.5,1.5), ylim=c(-1.5,1.5))
lines(c(-1,1),c(-1,1))


## correlations among items of the same domain (all N items)
lt = function(x) x[lower.tri(x)]
cor(gt[,1:15]) %>% lt %>% abs %>% summary
cor(noisy1[,1:15]) %>% lt %>% abs %>% summary
cor(smooth1[,1:15]) %>% lt %>% abs %>% summary



## correlations among items of different domains
cor(gt[,1:15], gt[,28:42]) %>% lt %>% abs %>% summary
cor(noisy1[,1:15], noisy1[,28:42]) %>% lt %>% abs %>% summary
cor(smooth1[,1:15], smooth1[,28:42]) %>% lt %>% abs %>% summary