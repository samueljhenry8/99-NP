### N.B. need 'aggregate.RData' and 'Data.RData' 

load("aggregate.RData")
load("Data.RData")

require(tidyverse)
require(psych)
require(glmnet)
require(corrr)
require(magrittr)

covid <- readxl::read_excel("covid.xlsx")
covidNames <- readxl::read_excel("covid.names.xlsx")

covDict <- readxl::read_excel("covDict.xlsx") %>% as.data.frame
rownames(covDict) = rbind(covDict[,1])
covNuanceDict <- readxl::read_excel("cov99npDict.xlsx") %>% as.data.frame
rownames(covNuanceDict) = rbind(covNuanceDict[,1])

# re-name variables to something more concise
names(covid) <- c("PID", "Worried", "TimeWorried", "SocDisBelief", "LockBelief", "Stockpile", "leftHome1Week", "numPeople1Week", "pubSpace2Weeks", "SocDisPrac", "NatGovTrust", "LocGovTrust", "Traveled", "FollowNews")
# 
names(covidNames) <- c("PID", "Worried", "TimeWorried", "SocDisBelief", "LockBelief", "Stockpile", "leftHome1Week", "numPeople1Week", "pubSpace2Weeks", "SocDisPrac", "NatGovTrust", "LocGovTrust", "Traveled", "FollowNews")

covidNames$leftHome1Week <-  ifelse(covidNames$leftHome1Week == 43955, "4-5", covidNames$leftHome1Week)

covidNames$numPeople1Week <-  ifelse(covidNames$numPeople1Week == 43955, "4-5", covidNames$numPeople1Week)
covidNames$numPeople1Week <-  ifelse(covidNames$numPeople1Week == 44049, "6-8", covidNames$numPeople1Week)
covidNames$numPeople1Week <-  ifelse(covidNames$numPeople1Week == 44113, "9-10", covidNames$numPeople1Week)

covidNames$pubSpace2Weeks <-  ifelse(covidNames$pubSpace2Weeks == 43862, "1-2", covidNames$pubSpace2Weeks)
covidNames$pubSpace2Weeks <-  ifelse(covidNames$pubSpace2Weeks == 43924, "3-4", covidNames$pubSpace2Weeks)
covidNames$pubSpace2Weeks <-  ifelse(covidNames$pubSpace2Weeks == 44017, "5-7", covidNames$pubSpace2Weeks)
covidNames$pubSpace2Weeks <-  ifelse(covidNames$pubSpace2Weeks == 44112, "8-10", covidNames$pubSpace2Weeks)

head(covid)


# Look for outliers
table(covidNames$leftHome1Week) #find those >2 times (legal limit) per day leaving the house (N=47)
table(covidNames$numPeople1Week) # find >0 people; spending time with NON-family (N = 191) 
table(covidNames$pubSpace2Weeks) # find >0 times; being in public spaces (N=49)

# Now use numeric values
table(covidNames$leftHome1Week) #find those >2 (N=47)
table(covidNames$numPeople1Week) # find >1 (N = 191)
table(covidNames$pubSpace2Weeks) # find >1 (N=49)
table(covidNames$SocDisPrac) # find <6 (N=148)

# Outliers by PID
leftHome <- covidNames %>% filter(leftHome1Week >3)
leftHome$leftHome1Week <=3 # sanity check
seePeople <- covidNames %>% filter(numPeople1Week >1)
publicSpace <- covidNames %>% filter(pubSpace2Weeks >1)
noDistance <- covidNames %>% filter(SocDisPrac <4) # didn't say '

essentialCheck <- rbind(leftHome, seePeople, publicSpace, noDistance)[,1] %>% filter(!duplicated(`PID`)) # N = 236
# write.csv(essential.check, "essential.check.csv")

# Message outliers to see if essential employees

covid = covid %>% merge(outcomesR, by = "PID") %>% select(c("PID", "Age", "Sex", "Education", "Student Status", "Current Country of Residence", "Worried", "TimeWorried", "SocDisBelief", "LockBelief", "Stockpile", "leftHome1Week", "numPeople1Week", "pubSpace2Weeks", "SocDisPrac", "NatGovTrust", "LocGovTrust", "Traveled", "FollowNews"))

covid$UKRes = covid %$% ifelse(`Current Country of Residence` == c("United Kingdom"), 1, 0)
table(covid$`UKRes`)
covidCountry <- covid$`Current Country of Residence` %>% table
covid$`Current Country of Residence` = NULL

descriptives = describe(covid[,-1]) %>% round(3)
lowerCor(covid[,-1])

### Personality Data


combItem <- comb
combItem[, -258] <- combItem[, -258] %>% scale()

combNuance <- new[, 76:ncol(new)] %>% 
  cbind(new[, c("Am good at saving money", "Cry easily", "Like to read", "Consider myself good-looking", "Support liberal political candidates", "Make friends easily", "Get deeply immersed in music", "Have strong sexual urges")])
combNuance$PID = comb$PID
combNuance[, 92:99] <- combNuance[, 92:99] %>% scale

bigFive <- data.frame(matrix(nrow = nrow(combNuance), ncol = 6))
colnames(bigFive) = c("PID", "N", "E", "O","A","C")
bigFive$PID = combNuance$PID
bigFive$N = rowMeans(combNuance[, 1:17], na.rm = T)
bigFive$E = rowMeans(combNuance[, 18:29], na.rm = T)
bigFive$O = rowMeans(combNuance[, 30:39], na.rm = T)
bigFive$A = rowMeans(combNuance[, 40:56], na.rm = T)
bigFive$C = rowMeans(combNuance[, 57:91], na.rm = T)

### Combined nuances/items with covid data
covCombNuance = merge(covid, combNuance, by="PID", all.x=T) # nuances
covCombItem = merge(covid, combItem, by="PID", all.x=T) # all items
covCombBigFive = merge(covid, bigFive, by="PID", all.x=T) 

# Remove PIDs
covCombItem <- covCombItem[,-1]
covCombNuance <- covCombNuance[,-1] 
covCombBigFive <- covCombBigFive[,-1] 

# Create 
interCor = cor(covCombItem[, 19:275], covCombItem[, 1:18], use = "pairwise") # all item-outcome correlations
persCor = cor(interCor, use = "pairwise") %>% round(3) # "persome-wide" correlations: 
# Phenotypic Corr
phenoCor = cor(covid[, 2:19], use = "pairwise") %>% round(3)


ut = function(x) x %>% t %>%  .[lower.tri(.)]
lt = function(x) x[lower.tri(x)]

(phenoPersCor = cor(phenoCor, persCor, use = "pairwise") %>% diag %>% round(3))
phenoPersCor %>% lt %>% abs %>% summary

interCorNuance = cor(covCombNuance[, 19:117], covCombNuance[, 1:18], use = "pairwise") # all item-outcome correlations
persCorNuance = cor(interCorNuance, use = "pairwise") # "persome-wide" correlations: 
(phenoPersCorNuance = cor(phenoCor, persCorNuance, use = "pairwise") %>% diag)


### Variables created ==> Now make predictions

# scree(cor(covid[, 6:18]))
# pca(cor(covid[, 6:18]), nfactors = 3)
 

colnames(covCombItem) = rbind(covDict[,1])
colnames(covCombNuance) = rbind(covNuanceDict[,1])

# Median Imputation of missing item data
medCovCombNuance <- as.tbl(covCombNuance)
medCovCombNuance <- mutate_if(medCovCombNuance, is.numeric, list(~ replace(., is.na(.), median(., na.rm = TRUE)))) %>% as.data.frame
medCovCombItem <- as.tbl(covCombItem)
medCovCombItem <- mutate_if(medCovCombItem, is.numeric, list(~ replace(., is.na(.), median(., na.rm = TRUE)))) %>% as.data.frame


### Fxn to Predict Outcomes using elastic net (default for spi - 10 outcomes)
cv = function(x, folds=2, criteria = 10) {
  s = sample(folds, nrow(x), replace=T)
  r = x
  for(h in 1:folds){
    ss = (1:nrow(x))[s == h]
    for(i in 1:criteria)
      r[ss,i]  = cv.glmnet(as.matrix(x[-ss, (criteria+1):ncol(x)]),  x[-ss,i],  alpha = .05) %>% predict(as.matrix(x[ss, (criteria+1):ncol(x)]), s="lambda.min")
  }
  r[, 1:criteria]
}

cvBigFive = cv(na.omit(covCombBigFive), folds = 5, criteria = 18) %>% cor(na.omit(covCombBigFive[,1:18]), use = "pairwise") %>% diag %>% as.data.frame
cvNuance = cv(medCovCombNuance, folds = 5, criteria = 18) %>% cor(covCombNuance[,1:18], use = "pairwise") %>% diag %>% as.data.frame
cvItem = cv(medCovCombItem, folds = 5, criteria = 18) %>% cor(covCombItem[,1:18], use = "pairwise") %>% diag %>% as.data.frame

itemBS <- bestScales(covCombItem,criteria=colnames(covCombItem)[1:18], folds=10, n.item=20,
                 dictionary=covDict, cut=.05, overlap = F)
# nuanceBS <- bestScales(covCombNuance,criteria=colnames(covCombNuance)[1:18], folds=10, n.item=20, dictionary=covNuanceDict ,cut=.05, overlap = F)

preds = cbind(cvItem, cvNuance, cvBigFive, itemBS$summary$final.valid) %>% round(3) %>% as.data.frame
colnames(preds) = c("257 Items", "99 NP", "Big 5", "Item BS")
preds %>% summary


write.csv(cbind(
  itemBS$items$Age$item,
  itemBS$items$Sex$item,
  itemBS$items$Education$item,
  itemBS$items$`Student Status`$item,
  itemBS$items$Worried$item,
  itemBS$items$TimeWorried$item,
  itemBS$items$SocDisBelief$item,
  itemBS$items$LockBelief$item,
  itemBS$items$Stockpile$item,
  itemBS$items$leftHome1Week$item,
  itemBS$items$numPeople1Week$item,
  itemBS$items$pubSpace2Weeks$item,
  itemBS$items$SocDisPrac$item,
  itemBS$items$NatGovTrust$item,
  itemBS$items$LocGovTrust$item,
  itemBS$items$Traveled$item,
  itemBS$items$FollowNews$item, 
  itemBS$items$UKRes$item), 'itemsBS.csv')


bsItems = read.csv("itemsBsClean.csv") %>% t %>% table %>% as.data.frame()
# 114 items used in total


### Data to report: 
descriptives %>% write.table(file = "descriptives.txt", sep = ",", eol = "\r\n", quote = F)
preds %>% write.table(file = "preds.txt", sep = ",", eol = "\r\n", quote = F)

preds %>% summary

phenoCor %>% lt %>% abs %>% summary
persCor %>% lt %>% abs %>% summary
persCorNuance %>% lt %>% abs %>% summary


lowerUpper(phenoCor, persCor) %>% corPlot(xlas = 3, numbers = T)
y = lowerUpper(phenoCor, persCor)
cor(lt(y), ut(y))



# outcomePCA = covCombItem[, 5:17] %>% scale %>% pca(4) %$% scores
# cvBigFivePCA = cv(c(outcomePCA, na.omit(covCombBigFive[])), folds = 5, criteria = 4) %>% cor(na.omit(covCombBigFive[,1:18]), use = "pairwise") %>% diag %>% as.data.frame
# cvNuance = cv(medCovCombNuance, folds = 5, criteria = 18) %>% cor(covCombNuance[,1:18], use = "pairwise") %>% diag %>% as.data.frame
# cvItem = cv(medCovCombItem, folds = 5, criteria = 18) %>% cor(covCombItem[,1:18], use = "pairwise") %>% diag %>% as.data.frame
# 
# # phenoCor %>% as_cordf %>% shave %>% stretch %>% arrange(desc(abs(r)))


### Supplement: How many predictors 'survive' regularization in 257i, 99NP? 
# X = as.matrix(medCovCombNuance[, c(19:117)])
# Y = na.omit(medCovCombNuance$Traveled)
# Z = cv.glmnet(x = X, y = Y)
# 
# Z$lambda
# 
# # CVs
# 
# # adjust the plot layout to make a two-panel plot
# layout(rbind(1,2))
# par(mar=c(3,3,2,1)+.1)
# 
# # coefficient path plot
# plot(cv.glmnet(x = X, y = Y), xvar="lambda", xlim=c(-4,4), mgp=2:0)
# 
# # CV error plot
# plot(CV, xlim=c(-4,4), mgp=2:0)
# 


# preds %>% gather %>% ggplot + geom_point(mapping = aes(x = key, y = value))
# matplot(preds, xlab = 3, ylab = "Cross validation of elastic net modeling on 99NP data", pch = 15:19)
# ggplot(data = preds) + 
#   geom_point(mapping = aes(x = , y = hwy, color = ))
# 
# par(mfrow=c(1,1))

## later: 
# predictions, but using only the # of items in BS
