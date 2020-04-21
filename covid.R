### N.B. need 'aggregate.RData' and 'Data.RData' 

load("aggregate.RData")
load("Data.RData")

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


### Personality Data


combItem <- (comb)
combNuance <- new[, 76:ncol(new)] %>% 
  cbind(new[, c("Am good at saving money", "Cry easily", "Like to read", "Consider myself good-looking", "Support liberal political candidates", "Make friends easily", "Get deeply immersed in music", "Have strong sexual urges")])
combNuance$PID = comb$PID
combNuance[, 92:99] <- combNuance[, 92:99] %>% scale

### Combined nuances/items with covid data
covCombNuance = merge(covid, combNuance, by="PID", all.x=T) # nuances
covCombItem = merge(covid, combItem, by="PID", all.x=T) # all items

# Remove PIDs
covCombItem <- covCombItem[,-1]
covCombNuance <- covCombNuance[,-1]

colnames(covCombItem) = rbind(covDict[,1])
colnames(covCombNuance) = rbind(covNuanceDict[,1])

# Median Imputation of missing item data
medCovCombNuance <- as.tbl(covCombNuance)
medCovCombNuance <- mutate_if(medCovCombNuance, is.numeric, list(~ replace(., is.na(.), median(., na.rm = TRUE)))) %>% as.data.frame
medCovCombItem <- as.tbl(covCombItem)
medCovCombItem <- mutate_if(medCovCombItem, is.numeric, list(~ replace(., is.na(.), median(., na.rm = TRUE)))) %>% as.data.frame

## Change names to get descriptives
# names(covid) <- names(covidNames)
describe(covid[,-1])

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


cvNuance = cv(medCovCombNuance, folds = 5, criteria = 13) %>% cor(covCombNuance[,1:13], use = "pairwise") %>% diag %>% as.data.frame
cvItem = cv(medCovCombItem, folds = 5, criteria = 13) %>% cor(covCombItem[,1:13], use = "pairwise") %>% diag %>% as.data.frame

preds = cbind(cvItem, cvNuance) %>% as.data.frame
## some predictive validity for covid outcomes. Moreso in items.


itemBS <- bestScales(covCombItem,criteria=colnames(covCombItem)[1:13], folds=10, n.item=20,
                 dictionary=covDict, cut=.05, overlap = F)
itemBS$summary

nuanceBS <- bestScales(covCombNuance,criteria=colnames(covCombNuance)[1:13], folds=10, n.item=10, dictionary=covNuanceDict ,cut=.05, overlap = F)
nuanceBS$summary
