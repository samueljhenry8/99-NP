require("psychTools")
require("psych")
require(tidyverse)
require(corrr)
require(glmnet)

data("spi")
data("spi.dictionary")
data("spi.keys")

### Data smoothing
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

foo3 = function(x, folds=2) {
  s = sample(folds, nrow(x), replace=T)
  r = x
  for(h in 1:folds){
    ss = (1:nrow(x))[s == h]
    for(i in 1:ncol(x))
      r[ss,i]  = cv.glmnet(as.matrix(x[-ss,-i]),  x[-ss,i],  alpha = .05) %>% predict(as.matrix(x[ss,-i]), s="lambda.1se")
  }
  r
}

## Create smoothed scores
spi.i135 = apply(spi[, 11:145], 2, scale)
spi.i135smooth <- foo2(spi.i135, folds = 10) # fully smoothed items (all unique variance gone)

## Create scores somewhere btwn predicted and true values

spi.i135.25 <- .75 * apply(spi.i135, 2, scale) + .25 * apply(spi.i135smooth, 2, scale) # 25% smoothed
spi.i135.50 <- .5 * apply(spi.i135, 2, scale) + .5 * apply(spi.i135smooth, 2, scale) # 50% smoothed
spi.i135.75 <- .25 * apply(spi.i135, 2, scale) + .75 * apply(spi.i135smooth, 2, scale) # 75% smoothed
spi.i135.90 <- .1 * apply(spi.i135, 2, scale) + .9 * apply(spi.i135smooth, 2, scale) # 90% smoothed

cor(spi.i135smooth, spi.i135, use = "pairwise") %>% diag %>% summary
cor(spi.i135smooth, spi.i135.50, use = "pairwise") %>% diag %>% summary
cor(spi.i135, spi.i135.50, use = "pairwise") %>% diag %>% summary

#find the scale scores for the Big 5 and little 27: Using real, predicted, and semi-smoothed estimates

#combine demographics and scores
#The demographic information is in the first 10 columns of spi

# Unsmoothed scale scores and 
spi.scales <- scoreItems(spi.keys,spi) #find scores as well as scale statistics
summary(spi.scales)
spi.scores <- data.frame(spi[1:10],spi.scales$scores) # all outcomes & items (raw)
R5<- lowerCor(spi.scores[11:15])


# Make scales using all levels of smoothed items
spi.scales.smooth <- scoreItems(spi.keys,spi.i135smooth) # scale scores full smooth
spi.scores.smooth <- data.frame(spi[1:10],spi.scales.smooth$scores) # 


spi.scales.25 <- scoreItems(spi.keys,spi.i135.25)
spi.scores.25 <- data.frame(spi[1:10],spi.scales.25$scores)
spi.scales.50 <- scoreItems(spi.keys,spi.i135.50)
spi.scores.50 <- data.frame(spi[1:10],spi.scales.50$scores)
spi.scales.75 <- scoreItems(spi.keys,spi.i135.75)
spi.scores.75 <- data.frame(spi[1:10],spi.scales.75$scores)
spi.scales.90 <- scoreItems(spi.keys,spi.i135.90)
spi.scores.90 <- data.frame(spi[1:10],spi.scales.90$scores)


# create dfs of only scale/item scores (raw, predicted, 50) (to then re-combine w/ median-imputed outcomes)
spi.b5 = spi.scores[,11:15]
spi.l27 = spi.scores[, 16:42]
spi.i135 = spi[, 11:145]

spi.b5smooth = spi.scores.smooth[,11:15]
spi.l27smooth = spi.scores.smooth[, 16:42]
# spi.i135smooth already made above

spi.b5.25 = spi.scores.25[,11:15]
spi.l27.25 = spi.scores.25[, 16:42]
# spi.i35.25 already made

spi.b5.50 = spi.scores.50[,11:15]
spi.l27.50 = spi.scores.50[, 16:42]
# spi.i35.50 already made

spi.b5.75 = spi.scores.75[,11:15]
spi.l27.75 = spi.scores.75[, 16:42]
# spi.i35.75 already made

spi.b5.90 = spi.scores.90[,11:15]
spi.l27.90 = spi.scores.90[, 16:42]
# spi.i35.90 already made

spi.criteria = spi.scores[,1:10] # contains NAs

# Median imputation for NA values of outcomes
spi.criteria.med <- as.tbl(spi[,1:10])
spi.criteria.med <- mutate_if(spi[,1:10], is.numeric, list(~ replace(., is.na(.), median(., na.rm = TRUE))))


# merge updated criteria with scores for B5, L27, and 135: normal, smoothed, and 25/50/75/90
sc.b5 <- data.frame(spi.criteria.med, spi.b5)
sc.l27 <- data.frame(spi.criteria.med, spi.l27) 
sc.i135 <- data.frame(spi.criteria.med, spi.i135)

sc.b5smooth <- data.frame(spi.criteria.med, spi.b5smooth)
sc.l27smooth <- data.frame(spi.criteria.med, spi.l27smooth) 
sc.i135smooth <- data.frame(spi.criteria.med, spi.i135smooth)

sc.b5.25 <- data.frame(spi.criteria.med, spi.b5.25)
sc.l27.25 <- data.frame(spi.criteria.med, spi.l27.25) 
sc.i135.25 <- data.frame(spi.criteria.med, spi.i135.25)

sc.b5.50 <- data.frame(spi.criteria.med, spi.b5.50)
sc.l27.50 <- data.frame(spi.criteria.med, spi.l27.50) 
sc.i135.50 <- data.frame(spi.criteria.med, spi.i135.50)

sc.b5.75 <- data.frame(spi.criteria.med, spi.b5.75)
sc.l27.75 <- data.frame(spi.criteria.med, spi.l27.75) 
sc.i135.75 <- data.frame(spi.criteria.med, spi.i135.75)

sc.b5.90 <- data.frame(spi.criteria.med, spi.b5.90)
sc.l27.90 <- data.frame(spi.criteria.med, spi.l27.90) 
sc.i135.90 <- data.frame(spi.criteria.med, spi.i135.90)


### Fxn to Predict Outcomes using elastic net (default for spi - 10 outcomes)
cv.spi= function(x, folds=2, criteria = 10) {
  s = sample(folds, nrow(x), replace=T)
  r = x
  for(h in 1:folds){
    ss = (1:nrow(x))[s == h]
    for(i in 1:criteria)
      r[ss,i]  = cv.glmnet(as.matrix(x[-ss, (criteria+1):ncol(x)]),  x[-ss,i],  alpha = .05) %>% predict(as.matrix(x[ss, (criteria+1):ncol(x)]), s="lambda.min")
  }
  r[, 1:criteria]
}

# Predictions 
cv5 <- cv.spi(sc.b5, folds = 2, criteria = 10)
cv27 <- cv.spi(sc.l27, folds = 2, criteria = 10)
cv135 <- cv.spi(sc.i135, folds = 2, criteria = 10)

cv5smooth <-cv.spi(sc.b5smooth, folds = 2, criteria = 10)
cv27smooth <-cv.spi(sc.l27smooth, folds = 2, criteria = 10)
cv135smooth <- cv.spi(sc.i135smooth, folds = 2, criteria = 10)

cv5.25 <-cv.spi(sc.b5.25, folds = 2, criteria = 10)
cv27.25 <-cv.spi(sc.l27.25, folds = 2, criteria = 10)
cv135.25 <- cv.spi(sc.i135.25, folds = 2, criteria = 10)

cv5.50 <-cv.spi(sc.b5.50, folds = 2, criteria = 10)
cv27.50 <-cv.spi(sc.l27.50, folds = 2, criteria = 10)
cv135.50 <- cv.spi(sc.i135.50, folds = 2, criteria = 10)

cv5.75 <-cv.spi(sc.b5.75, folds = 2, criteria = 10)
cv27.75 <-cv.spi(sc.l27.75, folds = 2, criteria = 10)
cv135.75 <- cv.spi(sc.i135.75, folds = 2, criteria = 10)

cv5.90 <-cv.spi(sc.b5.90, folds = 2, criteria = 10)
cv27.90 <-cv.spi(sc.l27.90, folds = 2, criteria = 10)
cv135.90 <- cv.spi(sc.i135.90, folds = 2, criteria = 10)


# Correlate predicted values with actual values
cor(spi.criteria, cv5, use = "pairwise") %>% diag
cor(spi.criteria, cv27, use = "pairwise") %>% diag
cor(spi.criteria, cv135, use = "pairwise") %>% diag

cor(spi.criteria, cv5smooth, use = "pairwise") %>% diag
cor(spi.criteria, cv27smooth, use = "pairwise") %>% diag
cor(spi.criteria, cv135smooth, use = "pairwise") %>% diag

cor(spi.criteria, cv5.50, use = "pairwise") %>% diag
cor(spi.criteria, cv27.50, use = "pairwise") %>% diag
cor(spi.criteria, cv135.50, use = "pairwise") %>% diag



# cross.valid.df <- data.frame(cv5, cv27, cv135, cv135smooth)

### Combine all correlations in one df:
cross.valid.df.b5 <- round(data.frame(cor(spi.criteria.med, cv5, use = "pairwise") %>% diag,
                             cor(spi.criteria.med, cv5smooth, use = "pairwise") %>% diag,
                             cor(spi.criteria.med, cv5.25, use = "pairwise") %>% diag,
                             cor(spi.criteria.med, cv5.50, use = "pairwise") %>% diag,
                             cor(spi.criteria.med, cv5.75, use = "pairwise") %>% diag,
                             cor(spi.criteria.med, cv5.90, use = "pairwise") %>% diag
),3)
                            
cross.valid.df.l27 <- round(data.frame(cor(spi.criteria.med, cv27, use = "pairwise") %>% diag,
                             cor(spi.criteria.med, cv27smooth, use = "pairwise") %>% diag,
                             cor(spi.criteria.med, cv27.25, use = "pairwise") %>% diag,
                             cor(spi.criteria.med, cv27.50, use = "pairwise") %>% diag,
                             cor(spi.criteria.med, cv27.75, use = "pairwise") %>% diag,
                             cor(spi.criteria.med, cv27.90, use = "pairwise") %>% diag
), 3)
                             
                             
cross.valid.df.i135 <- round(data.frame(cor(spi.criteria.med, cv135, use = "pairwise") %>% diag,
                              cor(spi.criteria.med, cv135smooth, use = "pairwise") %>% diag,
                             cor(spi.criteria.med, cv135.25, use = "pairwise") %>% diag,
                             cor(spi.criteria.med, cv135.50, use = "pairwise") %>% diag,
                             cor(spi.criteria.med, cv135.75, use = "pairwise") %>% diag,
                             cor(spi.criteria.med, cv135.90, use = "pairwise") %>% diag
), 3)

names(cross.valid.df.b5) <- c("Big5", "Big5.Smoothed", "Big5.25", "Big5.50", "Big5.75", "Big5.90") names(cross.valid.df.l27) <- c("Little27", "Little27.Smoothed", "Little27.25", "Little27.50", "Little27.75", "Little27.90")
names(cross.valid.df.i135) <- c("i135", "i135.Smoothed","i135.25","i135.50","i135.75","i135.90")

cv.df.sorted.b5 <- dfOrder(cross.valid.df.b5,1)
cv.df.sorted.l27 <- dfOrder(cross.valid.df.l27,1)
cv.df.sorted.i135 <- dfOrder(cross.valid.df.i135,1)

# write.csv(cv.df.sorted, "spi-smoothing.csv")

mean(cor(spi.criteria.med, spi.i135, use = "pairwise") - cor(spi.criteria.med, spi.i135smooth, use = "pairwise"))

bs <- bestScales(spi,criteria=colnames(spi)[1:10], folds=10, n.item=20,
                 dictionary=spi.dictionary,cut=.05)
bs.smooth <- bestScales(sc.i135smooth,criteria=colnames(sc.i135smooth)[1:10], folds=10, n.item=20,
                 dictionary=spi.dictionary,cut=.05)
bs.25 <- bestScales(sc.i135.25,criteria=colnames(sc.i135.25)[1:10], folds=10, n.item=20,
                 dictionary=spi.dictionary,cut=.05)
bs.50 <- bestScales(sc.i135.50,criteria=colnames(sc.i135.50)[1:10], folds=10, n.item=20,
                 dictionary=spi.dictionary,cut=.05)
bs.75 <- bestScales(sc.i135.75,criteria=colnames(sc.i135.75)[1:10], folds=10, n.item=20,
                 dictionary=spi.dictionary,cut=.05)
bs.90 <- bestScales(sc.i135.90,criteria=colnames(sc.i135.90)[1:10], folds=10, n.item=20,
                 dictionary=spi.dictionary,cut=.05)

# bs$items$age[, 1:5]
# bs.smooth$items$age[, 1:5]
# bs.50$items$age[, 1:5]


### Correlations between items and outcomes with varied levels of smoothing

cor(spi.criteria.med, spi.i135, use = "pairwise") %>% abs %>% t %>% apply(1, mean) %>% summary
cor(spi.criteria.med, spi.i135smooth, use = "pairwise") %>% abs %>% t %>% apply(1, mean) %>% summary
cor(spi.criteria.med, spi.i135.25, use = "pairwise") %>% abs %>% t %>% apply(1, mean) %>% summary
cor(spi.criteria.med, spi.i135.50, use = "pairwise") %>% abs %>% t %>% apply(1, mean) %>% summary
cor(spi.criteria.med, spi.i135.75, use = "pairwise") %>% abs %>% t %>% apply(1, mean) %>% summary
cor(spi.criteria.med, spi.i135.90, use = "pairwise") %>% abs %>% t %>% apply(1, mean) %>% summary

outcome.cors.i135smooth <- cor(spi.criteria.med, spi.i135smooth, use = "pairwise")
outcome.cors.i135.25 <- cor(spi.criteria.med, spi.i135.25, use = "pairwise")
outcome.cors.i135.50 <- cor(spi.criteria.med, spi.i135.50, use = "pairwise")
outcome.cors.i135.75 <- cor(spi.criteria.med, spi.i135.75, use = "pairwise")
outcome.cors.i135.90 <- cor(spi.criteria.med, spi.i135.90, use = "pairwise")



## inter-correlations with different levels of smoothing
lt <- function(x) x[lower.tri(x)]

cor(spi.i135) %>% lt %>% abs %>% summary # none
cor(spi.i135smooth) %>% lt %>% abs %>% summary # predicted values
cor(spi.i135.25) %>% lt %>% abs %>% summary # 25% smoothed
cor(spi.i135.50) %>% lt %>% abs %>% summary # 50% smoothed
cor(spi.i135.75) %>% lt %>% abs %>% summary # 75% smoothed
cor(spi.i135.90) %>% lt %>% abs %>% summary # 90% smoothed


##
# ss <- sample(1:nrow(sc.i135),nrow(sc.i135)/2)
# ?bestScales
# ### Best Scales
# bs <- bestScales(spi,criteria=colnames(spi)[1:10], folds=10, n.item=20,
#                  dictionary=spi.dictionary,cut=.05)
# 
# 
# spi.keys %>% .[6:32] %>% map(~filter(spi.dictionary, spi.dictionary$item_id %in% .))
# # 
# # bs$items[,1]
# # 
# # bs.cv <- cv.glmnet(x = spi[-ss,], y = bs)
# # ??crossValidation
# 
# 
# summary(bs)
# ####### Exclude for now
# ### Now multiple regression
# 
# set.seed(42)  #for reproducible results
# ss <- sample(1:nrow(sc.demos),nrow(sc.demos)/2)
# 
# #combine them into one data frame
# cross.valid.df <- data.frame(cv5=cv.5$crossV, cv.27=cv.27$crossV,
#                              cv135=cv.135$crossV)
# bs <- bestScales(spi[ss,],criteria=colnames(spi)[1:10], folds=10, n.item=20,
#                  dictionary=spi.dictionary,cut=.05)
# bs.cv <- crossValidation(bs,spi[-ss,])
# #sort them in ascending order of the Big 5 multiple R
# cross.valid.df.bs <- cbind(cross.valid.df,bs=bs.cv$crossV)
# cv.df.bs.sorted <- dfOrder(cross.valid.df.bs,1)
# matPlot(cv.df.bs.sorted[c(2,4,6,8)],minlength=8,
#         main="Cross validation of multiple regression on spi data",
#         xlas=3, ylab="Cross Validated R", pch=15:19)
# legend(1,.6,cs(135,27,bestS,b5),lty=c(3,2,4,1),col=c(3,2,4,1))
# bs.spi.smoke <- bs$items$smoke
# df2latex(bs.spi.smoke[c(2,3,5)])
# 
# 
# 
# ### Predict 19 criteria using 696 SAPA items
# 
# #get the files from Dataverse and then combine them
# 
# load("~/OneDrive - University of Edinburgh/PhD/100_nuances/Analysis/DataPlusScript/sapaTempData696items08dec2013thru26jul2014.RData")
# load("~/OneDrive - University of Edinburgh/PhD/100_nuances/Analysis/DataPlusScript/sapaTempData696items26jul2014thru22dec2015.RData")
# load("~/OneDrive - University of Edinburgh/PhD/100_nuances/Analysis/DataPlusScript/sapaTempData696items22dec2015thru07feb2017.RData")
# 
# full.sapa <- rbind(sapaTempData696items08dec2013thru26jul2014, sapaTempData696items26jul2014thru22dec2015, sapaTempData696items22dec2015thru07feb2017)
# 
# sapa <-char2numeric(full.sapa) #convert string  character data to numeric data
# spi.items <- selectFromKeys(spi.keys)
# #set the number of multiple cores to take advantage of them
# #does not work on PCs
# options("mc.cores" = 4)
# #find the scale scores and reliabililties
# sapa.spi <- scoreItems(spi.keys,sapa,impute="none")
# spi.scores <- sapa.spi$scores
# demos <- sapa[c(2:10,14:23)]  #choose the 19 demographic data
# demos.spi <- cbind(demos,spi.scores)
# 
# # psych::bestScales(spi, criteria="health",dictionary=spi.dictionary)
# # 
# # sc <- psych::scoreVeryFast(spi.keys,spi) #much faster scoring for just scores
# # sc <- psych::scoreItems(spi.keys,spi)  #gives the alpha reliabilities and various stats 
# # 
# # psych::corPlot(sc$corrected,numbers=TRUE,cex=.4,xlas=2,min.length=6,
# #                main="Structure of SPI (disattenuated r above the diagonal)")
# 
# 
# 
