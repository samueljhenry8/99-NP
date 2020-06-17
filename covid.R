### N.B. need 'aggregate.RData' and 'Data.RData' 

load("aggregate.RData")
load("Data.RData")

require(tidyverse)
require(dplyr)
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

covid %$% ifelse(`Current Country of Residence` == c("United Kingdom"), 1, 0) %>% table
covidCountry <- covid$`Current Country of Residence` %>% table

AUS = covid %$% ifelse(`Current Country of Residence` == "Australia", 70.91, NA)
CAN = covid %$% ifelse(`Current Country of Residence` == "Canada", 76.73, NA)
ISR = covid %$% ifelse(`Current Country of Residence` == "Israel", 89.41, NA)
USA = covid %$% ifelse(`Current Country of Residence` == "United States", 68.41, NA)
AST = covid %$% ifelse(`Current Country of Residence` == "Austria", 84.79, NA)
BLG = covid %$% ifelse(`Current Country of Residence` == "Belgium", 83.6, NA)
CZC = covid %$% ifelse(`Current Country of Residence` == "Czech Republic", 75.13, NA)
EST = covid %$% ifelse(`Current Country of Residence` == "Estonia", 80.29, NA)
FIN = covid %$% ifelse(`Current Country of Residence` == "Finland", 75.13, NA)
FRA = covid %$% ifelse(`Current Country of Residence` == "France", 89.41, NA)
GER = covid %$% ifelse(`Current Country of Residence` == "Germany", 74.61, NA)
GRE = covid %$% ifelse(`Current Country of Residence` == "Greece", 87.96, NA)
HUN = covid %$% ifelse(`Current Country of Residence` == "Hungary", 78.44, NA)
ITA = covid %$% ifelse(`Current Country of Residence` == "Italy", 93.25, NA)
MEX = covid %$% ifelse(`Current Country of Residence` == "Mexico", 85.45, NA)
NED = covid %$% ifelse(`Current Country of Residence` == "Netherlands", 84.65, NA)
NOR = covid %$% ifelse(`Current Country of Residence` == "Norway", 79.63, NA)
POL = covid %$% ifelse(`Current Country of Residence` == "Poland", 83.6, NA)
POR = covid %$% ifelse(`Current Country of Residence` == "Portugal", 86.64, NA)
SLV = covid %$% ifelse(`Current Country of Residence` == "Slovenia", 91.93, NA)
ESP = covid %$% ifelse(`Current Country of Residence` == "Spain", 89.41, NA)
SWE = covid %$% ifelse(`Current Country of Residence` == "Sweden", 38.22, NA)
SUI = covid %$% ifelse(`Current Country of Residence` == "Switzerland", 79.49, NA)
UK = covid %$% ifelse(`Current Country of Residence` == "United Kingdom", 82.27, NA)

covid$CountrySeverity = coalesce(AUS, AST, BLG, CAN, CZC, ESP, EST, FIN, FRA, GER, GRE, HUN, ISR, ITA, MEX, NED, NOR, POL, POR, SLV, SUI, SWE, UK, USA)

# diff = TukeyHSD(aov(CountrySeverity ~ 1 + `Current Country of Residence`, data = covid)) %>% unlist %>% as.matrix(ncol = 2)
# (covid$CountrySeverity < 68) %>% table
# geom_boxplot(data = covid, mapping = aes(CountrySeverity))

covid$CountrySeverity %>% summary()
# covid$`Current Country of Residence` = NULL

covid = covid %>% select(1:6, 20, 7:10, 15, 19, 16:18, everything())

descriptives = describe(covid[,c(-1, -6)]) %>% round(3)
lowerCor(covid[,c(-1,-6)])
covid[, c(4,7:20)] = covid[, c(4,7:20)] %>% scale

comb %>% apply(1, function(x) sum(is.na(x))) %>% table 
# comb2 = comb %>% apply(1, function(x) count(is.na(x)) <5 )
comb2 <- comb[rowSums(is.na(comb)) < 50, ]
comb3 <- comb2 %>% merge(covid, by = "PID")
comb3$PID %>% write.csv("PIDs.csv")



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
covCombItem <- covCombItem[,-c(1, 6)]
covCombNuance <- covCombNuance[,-c(1, 6)]
covCombBigFive <- covCombBigFive[,-c(1, 6)] 

# Item-Outcome Correlations
inter = corr.test(covCombItem[, 19:275], covCombItem[, 1:18], use = "pairwise", adjust = "fdr") # all item-outcome correlations
interCor = inter$r
interP = inter$p

# profile correlations
pers = corr.test(interCor, use = "pairwise", adjust = "fdr") # "persome-wide" correlations
persCor = pers$r %>% round(3)
persP = pers$p 
persPOrdered = persP %>% t %>% as_cordf() %>% shave %>% stretch %>% arrange(desc(abs(r)))

# Phenotypic Cor
pheno = corr.test(covid[, c(2:5, 7:20)], use = "pairwise", adjust = "fdr")
phenoCor = pheno$r %>% round(3)
phenoP = pheno$p #%>% round(4) #%>% as_cordf() %>% shave
phenoPOrdered = phenoP %>% t %>% as_cordf() %>% shave %>% stretch %>% arrange(desc(abs(r)))


ut = function(x) x %>% t %>%  .[lower.tri(.)]
lt = function(x) x[lower.tri(x)]

(phenoPersCor = cor(phenoCor, persCor, use = "pairwise") %>% diag %>% round(3))
phenoPersCor %>% lt %>% abs %>% summary

interCorNuance = cor(covCombNuance[, 19:117], covCombNuance[, 1:18], use = "pairwise") # all item-outcome correlations
persCorNuance = cor(interCorNuance, use = "pairwise") # "persome-wide" correlations: 
(phenoPersCorNuance = cor(phenoCor, persCorNuance, use = "pairwise") %>% diag)

phenoCor %>% lt %>% abs %>% summary
persCor %>% lt %>% abs %>% summary
persCorNuance %>% lt %>% abs %>% summary


# correlations
corMat = (lowerUpper(phenoCor, persCor)) %>% corPlot(xlas = 3, numbers = T)
y = lowerUpper(phenoCor, persCor)
cor(lt(y), ut(y))
# p-values
(pVal = lowerUpper(phenoP, persP) %>% round(5)) 
pVal %>% corPlot(xlas = 3, numbers = T)



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

itemBS <- bestScales(covCombItem,criteria=colnames(covCombItem)[1:18], folds=20, n.item=20, dictionary=covDict, cut=.05, overlap = F)
nuanceBS <- bestScales(covCombNuance,criteria=colnames(covCombNuance)[1:18], folds=10, n.item=20, dictionary=covNuanceDict ,cut=.05, overlap = F)

# itemBSTest = bestScales(covCombItem,criteria=colnames(covCombItem)[1:18], folds=10, n.item=257, dictionary=covDict, cut=.01, overlap = F)
# cbind(preds, itemBSTest$summary$final.valid)

preds = cbind(cvItem, cvNuance, cvBigFive, itemBS$summary$final.valid, nuanceBS$summary$final.valid) %>% round(3) %>% as.data.frame
colnames(preds) = c("257 Items", "99 NP", "Big 5", "Item BS", "Nuance BS")
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
  itemBS$items$CountrySeverity$item), 'itemsBS.csv')


bsItems = read.csv("itemsBsClean.csv") %>% t %>% table %>% as.data.frame() %>% desc("Freq")
# 114 items used in total
write.csv(bsItems, "bsItemsFreq.csv")




# dfi = for (i in 1:length(dfi)) lapply(dfi, function(x) names(paste(colnames(covCombItem[1:x]))))
# for(i in 1:length(itemBS$items)) dfi = lapply(names(itemBS$items),function(x) itemBS$items[[x]][,c(7, 4, 5)]) 



### Data to report: 
descriptives %>% write.table(file = "descriptives.txt", sep = ",", eol = "\r\n", quote = F)
preds %>% write.table(file = "preds.txt", sep = ",", eol = "\r\n", quote = F)

preds %>% summary

# creates .txt files of bestScales tables including 1) item name, 2) mean r, and 3) SD
dfi = lapply(itemBS$items, '[', c(7, 4, 5))
names(dfi[1:18]) = colnames(covCombItem[1:18])

for (x in 1:length(dfi)) {
  dfi[x] %>% unlist(recursive = F) %>% as.data.frame %>% as.tbl %>% mutate_if(is.numeric, round, 3) %>% write.table(file = paste(names(dfi[x]),".txt",sep=""), sep = ",", eol = "\r\n", quote = F, row.names = F)
}
