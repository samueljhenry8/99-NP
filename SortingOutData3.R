require(tidyverse)
require("psychTools")
require("psych")
require(tidyverse)
require(corrr)
require(glmnet)

# setwd("~/ownCloud/EdiSync/Nuances/")
part11 = readxl::read_excel("RTestBatch1Part1.xlsx")   %>% filter(!is.na(`PID`))
part21 = readxl::read_excel("RTestBatch1Part2.xlsx")   %>% filter(!is.na(`PID`))  %>% filter(!duplicated(`PID`)) ## four people did it twice
part12 = readxl::read_excel("RTestBatch2Part1.xlsx")   %>% filter(!is.na(`PID`))
part22 = readxl::read_excel("RTestBatch2Part2.xlsx")   %>% filter(!is.na(`PID`))
part13 = read_csv("RTestBatch3Part1.csv")
part23 = read_csv("RTestBatch3Part2.csv")
outcomes = read_csv("outcomes_all1.csv") %>% filter(!is.na(`PID`))

## Sort Batches 1 to 3, calculate profile correlations

part11 = part11 %>% filter(`PID` %in% part21$`PID`) %>% arrange(`PID`)
part21 = part21 %>% arrange(`PID`)
profcors1 = cor(part11[,c(2:3,5:300,302:303)] %>% t, part21[,c(2:3,5:300,302:303)] %>% t) %>% diag

part12 = part12 %>% filter(`PID` %in% part22$`PID`) %>% arrange(`PID`)
part22 = part22 %>% arrange(`PID`)
profcors2 = cor(part12[,-c(1,83,84)] %>% t, part22[,-c(1,83,84)] %>% t) %>% diag

part13 = part13 %>% filter(`PID` %in% part23$`PID`) %>% arrange(`PID`)
part23 = part23 %>% arrange(`PID`)
profcors3 = cor(part13[,-c(1,54)] %>% t, part23[,-c(1,54)] %>% t) %>% diag

## QA: 
#     individuals correlations with normative response profiles
#     cross-time correlations of response profiles
#     failures in "catch" questions

# Batch 1

l11 = colMeans(part11[,c(2:3,5:300,302:303)]) %>% cor(t(part11[,c(2:3,5:300,302:303)]),.)
l21 = colMeans(part21[,c(2:3,5:300,302:303)]) %>% cor(t(part21[,c(2:3,5:300,302:303)]),.)
l1 = tibble(l11, l21, profcors1) %>% mutate(l = l11 + l21) %>% cor 

# Profile consistency over time is quite highly correlated with normativeness

# Part 1 validity checks did not work (about the Moon)
# Did part 2 validity check questions work? Partly
# Some participants who failed had high cross-time correlations and normative correlations
# Consistencies in response patterns probably better quality indicators

profcors1[part21[,4] == 4]
profcors1[part21[,301] == 2]
l11[part21[,4] == 4]
l11[part21[,301] == 2]

# Drop those with consistencies over time 2.5 SDs below median (SDs are inflated by some very low values)
qual1 = profcors1 > (median(profcors1) - 2.5 * sd(profcors1)) #& (l11 > median(l11) - 2.5 * sd(l11)) & (l11 > median(l21) - 2.5 * sd(l21))
part11R = part11 %>% filter(qual1) %>% select(-4,-301) %>% select(-contains("_")) %>% select(1:301)
part21R = part21 %>% filter(qual1) %>% select(-4,-301) %>% select(-contains("_")) %>% select(1:301)

(rs1 = cor(part11R[-1], part21R[-1]) %>% diag %>% round(2)) %>% summary
(profrs1 = cor(part11R[-1] %>% t, part21R[-1] %>% t) %>% diag) %>% summary

# Batch 2 (subset of participants in Batch 1, but with different items)

l12 = colMeans(part12[,-c(1,83,84)]) %>% cor(t(part12[,-c(1,83,84)]),.)
l22 = colMeans(part22[,-c(1,83,84)]) %>% cor(t(part22[,-c(1,83,84)]),.)
tibble(l12, l22, profcors2) %>% mutate(l = l12 + l22) %>% cor 

# Check profile correlatiosn of those failing validity checks
# Little evidence for validity checks working
profcors2[part12[,83] != 3]
profcors2[part12[,84] != 6]
profcors2[part22[,83] != 3]
profcors2[part22[,84] != 6]

# Drop those with consistencies over time 3 SDs below median (SDs are inflated by some very low values)
# Less stringent here because participants were preselected
qual2 = profcors2 > (median(profcors2) - 3 * sd(profcors2)) #& (l12 > median(l12) - 3 * sd(l12)) & (l22 > median(l22) - 3 * sd(l22))
part12R = part12 %>% filter(qual2) %>% select(-83,-84) %>% select(-contains("_")) 
part22R = part22 %>% filter(qual2) %>% select(-83,-84) %>% select(-contains("_")) 

(rs2 = cor(part12R[-1], part22R[-1]) %>% diag %>% round(2)) %>% summary
(profrs2 = cor(part12R[-1] %>% t, part22R[-1] %>% t) %>% diag) %>% summary

# Batch 3 (subset of participants in Batch 1 and 2, but with different items)

l13 = colMeans(part13[,-c(1,54)]) %>% cor(t(part13[,-c(1,54)]),.)
l23 = colMeans(part23[,-c(1,54)]) %>% cor(t(part23[,-c(1,54)]),.)
tibble(l13, l23, profcors3) %>% mutate(l = l13 + l23) %>% cor 

# Check profile correlatiosn of those failing validity checks
# Little evidence for validity checks working
profcors3[part13[,54] != 2]
profcors3[part23[,54] != 2]

# Drop those with consistencies over time 3 SDs below median (SDs are inflated by some very low values)
# Even less stringent here than in batch1 because participants were preselected 2 x 
qual3 = (profcors3 > median(profcors3) - 3 * sd(profcors3)) #& (l13 > median(l13) - 3 * sd(l13)) & (l23 > median(l23) - 3 * sd(l23))
part13R = part13 %>% filter(qual3) %>% select(-54) 
part23R = part23 %>% filter(qual3) %>% select(-54) 

(rs3 = cor(part13R[-1], part23R[-1]) %>% diag %>% round(2)) %>% summary
(profrs3 = cor(part13R[-1] %>% t, part23R[-1] %>% t) %>% diag) %>% summary

## Start merging data

part1 = merge(part11R, part12R, by="PID", all.x=T)
part2 = merge(part21R, part22R, by="PID", all.x=T)

dupitems = c("Am easily intimidated....37","Choose my words with care....92","Try not to be rude to people....93",
             "Often take on more activities than I have time for....155","Love to think up new ways of doing things..x",
             "Let others take advantage of me....91")

part1 = part1 %>% select(-dupitems)
part2 = part2 %>% select(-dupitems)

part1 = merge(part1, part13R, by="PID", all.x=T) %>% `names<-`(names(.) %>% gsub("'","", ., fixed=T)) %>% `names<-`(names(.) %>% gsub(".","", ., fixed=T))
part2 = merge(part2, part23R, by="PID", all.x=T) %>% `names<-`(names(.) %>% gsub("'","", ., fixed=T)) %>% `names<-`(names(.) %>% gsub(".","", ., fixed=T))

rscomb = cor(part1[,-1], part2[,-1], use="pairwise") %>% diag 

## Data from the second sample

part14 = read_csv("RTestNewSamplePart1.csv") 
part24 = read_csv("RTestNewSamplePart2.csv")  %>% filter(!duplicated(`PID`))

part14 = part14 %>% filter(`PID` %in% part24$`PID`) %>% arrange(`PID`)
part24 = part24 %>% arrange(`PID`)
profcors4 = cor(part14[,c(2:254)] %>% t, part24[,c(2:254)] %>% t, use="pairwise") %>% diag

l14 = colMeans(part14[,2:254]) %>% cor(t(part14[,2:254]),., use="pairwise")
l24 = colMeans(part24[,2:254]) %>% cor(t(part24[,2:254]),., use="pairwise")
tibble(l14, l24, profcors4) %>% mutate(l = l14 + l24) %>% cor 

# Check profile correlatiosn of those failing validity checks
# Little evidence for validity checks working
profcors4[part14[,255] != 3]
profcors4[part14[,256] != 6]
profcors4[part24[,255] != 3]
profcors4[part24[,256] != 6]

# Drop those with consistencies over time 2.5 SDs below median (SDs are inflated by some very low values)
# More stringent again because of new participants
qual4 = profcors4 > (median(profcors4) - 2.5 * sd(profcors4))# & (l14 > median(l14) - 2.5 * sd(l14)) & (l24 > median(l24) - 2.5 * sd(l24))
part14R = part14 %>% filter(qual4) %>% select(1:254) 
part24R = part24 %>% filter(qual4) %>% select(1:254) 

(rs4 = cor(part14R[-1], part24R[-1]) %>% diag %>% round(2)) %>% summary
(profrs4 = cor(part14R[-1] %>% t, part24R[-1] %>% t) %>% diag) %>% summary

## Fix some variable names

changeTo = c("Often do things that I later regret", "Avoid changes", "Take offence easily", 
             "Often feel that others misunderstand me", "Talk a lot", "Let others take advantage of me", 
             "Support liberal political candidates", "Love to think up new ways of doing things", 
             "Am interested in many things", "Try to out do others", "Am quick to correct others", 
             "Believe that there is never an excuse for lying", "My interests change quickly")

changeFrom = c("Do things I later regret","Dislike changes","Take offense easily","Feel that others misunderstand me",
               "Dont talk a lot", "Let others take advantage of me53","Tend to vote for liberal political candidates",
               "Love to think up new ways of doing thingsy","Interested in many things","Try to out-do others",
               "Quick to correct others","Believe there is never an excuse for lying","Feel that my interests change quickly")

names(part1)[match(changeFrom, names(part1))] = changeTo
names(part2)[match(changeFrom, names(part2))] = changeTo

## Because one item got reversed in second sample
part1$`Talk a lot` = 7 - part1$`Talk a lot`
part2$`Talk a lot` = 7 - part2$`Talk a lot`

part1comb = part1 %>% select(names(part14R)) %>% rbind(part14R) 
part2comb = part2 %>% select(names(part24R)) %>% rbind(part24R)

## Append the 4 items not in the second sample

tmp1 = part1 %>% select("PID", "Easily apologize when I have been wrong","Consider myself healthy for my age", 
                        "Talking with others energizes me","After a stressful day, I need some time alone to relax")
tmp2 = part2 %>% select("PID", "Easily apologize when I have been wrong","Consider myself healthy for my age", 
                        "Talking with others energizes me","After a stressful day, I need some time alone to relax")
part1comb = merge(part1comb, tmp1, by = "PID", all.x = T) 
part2comb = merge(part2comb, tmp2, by = "PID", all.x = T)


(rscomb = cor(part1comb[,2:258], part2comb[,2:258], use="pairwise") %>% diag) %>% summary
# write.csv(rscomb, "rTTs.csv")

reliabMatrix = (cbind(rscomb) %*% rbind(rscomb)) %>% sqrt # use to correct inter-item correlations for reliability

## Combine part1 and part2

comb = (part1comb[,-1] + part2comb[,-1]) / 2

require("corrr")
(pairs <- cor(scale(comb), use = "pairwise") %>% as_cordf %>% shave %>% stretch() %>% arrange(desc(abs(r))))

pairs$item1_rTT <- rscomb[pairs$x]
pairs$item2_rTT <- rscomb[pairs$y]
# write.csv(pairs, "ItemPairCorrelations.csv")

# correlation(comb[,rscomb > .80]) %>% arrange(desc(abs(r))) %>% select(-Method, t, df)

# Pairs corrected for reliability
reliabCorrectCorelations = (((cor(part1comb[,-1], use="pairwise") +  cor(part2comb[,-1], use="pairwise")) / 2)  / reliabMatrix ) %>% as_cordf %>% shave %>% stretch() %>% arrange(desc(abs(r)))

head(reliabCorrectCorelations,20)


## test retest of nuances (2, 3-item combinations)

rscomb[grep("apolo", names(rscomb))]
rscomb[grep("making a", names(rscomb))]

cor(scale(part1comb$`Am quick to admit making a mistake`), part2comb$`Am quick to admit making a mistake`, use = "pairwise") # .67
cor(scale(part1comb$`Easily apologize when I have been wrong`), part2comb$`Easily apologize when I have been wrong`, use = "pairwise") # .66

(scale(part1comb$`Am quick to admit making a mistake`) +  scale(part1comb$`Easily apologize when I have been wrong`)) %>% 
  cor(
    (scale(part2comb$`Am quick to admit making a mistake`) +  scale(part2comb$`Easily apologize when I have been wrong`)),
  use="pairwise"
    ) # .77

rscomb[grep("Rarely wo", names(rscomb))]
rscomb[grep("always wor", names(rscomb))]

(scale(part1comb$`Rarely worry`)*-1 +  scale(part1comb$`Am always worried about something`)) %>% 
  cor(
    (scale(part2comb$`Rarely worry`)*-1 +  scale(part2comb$`Am always worried about something`)),
    use="pairwise"
  )



(scale(part1comb$`Find political discussions interesting`)*-1 +  scale(part1comb$`Avoid philosophical discussions`)) %>% 
  cor(
    (scale(part2comb$`Find political discussions interesting`)*-1 +  scale(part2comb$`Avoid philosophical discussions`)),
    use="pairwise"
  )

### Consolidate environmental and eating items 

ee11 = part11 %>% filter(qual1) %>% select(-4,-301) %>% select(-contains("_")) %>% select(1, 302, 304:308)
ee21 = part21 %>% filter(qual1) %>% select(-4,-301) %>% select(-contains("_")) %>% select(1, 322, 324:328)
names(ee21) <- names(ee11)


ee12 = part12R[, c(1, 94:95)]
ee22 = part22R[, c(1, 94:95)]

part13R = part13 %>% filter(qual3) %>% select(-54) 
part23R = part23 %>% filter(qual3) %>% select(-54) 


part1.ee = merge(ee11, ee12, by="PID", all.x=T) 
part2.ee = merge(ee21, ee22, by="PID", all.x=T)

part1.ee = part1.ee %>% `names<-`(names(.) %>% gsub("'","", ., fixed=T)) %>% `names<-`(names(.) %>% gsub(".","", ., fixed=T)) %>% `names<-`(names(.) %>% gsub(",","", ., fixed=T))
part2.ee = part2.ee %>% `names<-`(names(.) %>% gsub("'","", ., fixed=T)) %>% `names<-`(names(.) %>% gsub(".","", ., fixed=T)) %>% `names<-`(names(.) %>% gsub(",","", ., fixed=T))

ee14 = part14 %>% filter(qual4) %>% select(1, 257:264) 
ee24 = part24 %>% filter(qual4) %>% select(1, 257:264)

# Merge all environment & eating questions, t1 & t2
part1comb.ee = part1.ee %>% select(names(ee14)) %>% rbind(ee14) 
part2comb.ee = part2.ee %>% select(names(ee24)) %>% rbind(ee24)

# rTTs for env/eat
cor(part1comb.ee[,-1], part2comb.ee[,-1], use = "pairwise") %>% diag

# take averages
comb.ee = (part1comb.ee[,-1] + part2comb.ee[,-1]) / 2

# add PID to avg responses (for external use)
comb.ee$PID = part1comb.ee$PID

comb1 = cbind(part1comb$PID, ((part1comb[,-1] + part2comb[,-1]) / 2))

### Filter all other outcomes 
outcomesR = outcomes %>% filter(`PID` %in% comb.ee$PID) %>% arrange(`PID`) # %>% filter((!duplicated(`PID`)))
head(outcomesR)
# %>% filter(`PID` %in% part2comb$`PID`) 


### Add Extra Prolific Information
demo <- readxl::read_xlsx("ProlificDemographics.xlsx")
head(demo)

sex.d <- ifelse(demo$Sex=="Female", 1, 2)
demo$Sex <- sex.d
student.d <-ifelse(demo$`Student Status`=="Yes", 1, 0)
demo$`Student Status` <- student.d
# emp.d <- ifelse(demo$`Employment Status`== "Full-Time", 1, )
