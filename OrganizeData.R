require(tidyverse)
require("psychTools")
require("psych")
require(tidyverse)
require(corrr)
require(glmnet)

part11 = readxl::read_excel("RTestBatch1Part1.xlsx")   %>% filter(!is.na(`PID`))
part21 = readxl::read_excel("RTestBatch1Part2.xlsx")   %>% filter(!is.na(`PID`))  %>% filter(!duplicated(`PID`)) ## four people did it twice
part12 = readxl::read_excel("RTestBatch2Part1.xlsx")   %>% filter(!is.na(`PID`))
part22 = readxl::read_excel("RTestBatch2Part2.xlsx")   %>% filter(!is.na(`PID`))
part13 = read_csv("RTestBatch3Part1.csv")
part23 = read_csv("RTestBatch3Part2.csv")
outcomes = read_csv("outcomes_all1.csv") %>% filter(!is.na(`PID`))

## Sort Batches 1 to 3

part11 = part11 %>% filter(`PID` %in% part21$`PID`) %>% arrange(`PID`)
part21 = part21 %>% arrange(`PID`)

part12 = part12 %>% filter(`PID` %in% part22$`PID`) %>% arrange(`PID`)
part22 = part22 %>% arrange(`PID`)

part13 = part13 %>% filter(`PID` %in% part23$`PID`) %>% arrange(`PID`)
part23 = part23 %>% arrange(`PID`)

part11R = part11 %>% select(-4,-301) %>% select(-contains("_")) %>% select(1:301)
part21R = part21 %>% select(-4,-301) %>% select(-contains("_")) %>% select(1:301)

part12R = part12 %>% select(-83,-84) %>% select(-contains("_")) 
part22R = part22 %>% select(-83,-84) %>% select(-contains("_")) 

part13R = part13 %>% select(-54) 
part23R = part23 %>% select(-54) 


part1 = merge(part11R, part12R, by="PID", all.x=T)
part2 = merge(part21R, part22R, by="PID", all.x=T)

dupitems = c("Am easily intimidated....37","Choose my words with care....92","Try not to be rude to people....93",
             "Often take on more activities than I have time for....155","Love to think up new ways of doing things..x",
             "Let others take advantage of me....91")

part1 = part1 %>% select(-dupitems)
part2 = part2 %>% select(-dupitems)

part1 = merge(part1, part13R, by="PID", all.x=T) %>% `names<-`(names(.) %>% gsub("'","", ., fixed=T)) %>% `names<-`(names(.) %>% gsub(".","", ., fixed=T))
part2 = merge(part2, part23R, by="PID", all.x=T) %>% `names<-`(names(.) %>% gsub("'","", ., fixed=T)) %>% `names<-`(names(.) %>% gsub(".","", ., fixed=T))

## Data from the second sample

part14 = read_csv("RTestNewSamplePart1.csv") 
part24 = read_csv("RTestNewSamplePart2.csv")  %>% filter(!duplicated(`PID`))

part14 = part14 %>% filter(`PID` %in% part24$`PID`) %>% arrange(`PID`)
part24 = part24 %>% arrange(`PID`)

part14R = part14 %>% select(1:254) 
part24R = part24 %>% select(1:254) 

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



## Combine all personality items into t1 and t2 dataframes

part1all = part1 %>% select(names(part1)[!names(part1) %in% names(part1comb)]) %>% 
  mutate(PID = part1$PID) %>% merge(part1comb, ., by = "PID", all = T) %>%
  select(-"I enjoy eating", -"I consciously hold back on how much I eat at meals to keep from gaining weight")
part2all = part2 %>% select(names(part2)[!names(part2) %in% names(part2comb)]) %>% 
  mutate(PID = part2$PID) %>% merge(part2comb, ., by = "PID", all = T)%>%
  select(-"I enjoy eating", -"I consciously hold back on how much I eat at meals to keep from gaining weight")

## Q & A: low correlations of item profiles between t1 and t2 go with non-normative response profiles
## These are very likely inconistent responders
## There is a long thin tail showing inconsistent responders
profrs = cor(part1all[-1] %>% t, part2all[-1] %>% t, use="pairwise") %>% diag
l1 = colMeans(part1all[-1], na.rm=T)  %>% cor(t(part1all[-1]),., use="pairwise")
l2 = colMeans(part2all[-1], na.rm=T)  %>% cor(t(part2all[-1]),., use="pairwise")

## Omit people with profile consistencies 3 SDs below median (but omit the few very off participants outright, because they skew medians and SDs)
cutoff = median(profrs[profrs > 0]) - sd(profrs[profrs > 0]) * 3
sum(profrs < cutoff)

part1all = part1all %>% filter(profrs > cutoff)
part2all = part2all %>% filter(profrs > cutoff)

### Consolidate environmental and eating items 

ee11 = part11 %>% select(-4,-301) %>% select(-contains("_")) %>% select(1, 302, 304:308)
ee21 = part21 %>% select(-4,-301) %>% select(-contains("_")) %>% select(1, 322, 324:328)
names(ee21) <- names(ee11)

ee12 = part12R[, c(1, 94:95)]
ee22 = part22R[, c(1, 94:95)]

part1.ee = merge(ee11, ee12, by="PID", all.x=T) 
part2.ee = merge(ee21, ee22, by="PID", all.x=T)

part1.ee = part1.ee %>% `names<-`(names(.) %>% gsub("'","", ., fixed=T)) %>% `names<-`(names(.) %>% gsub(".","", ., fixed=T)) %>% `names<-`(names(.) %>% gsub(",","", ., fixed=T))
part2.ee = part2.ee %>% `names<-`(names(.) %>% gsub("'","", ., fixed=T)) %>% `names<-`(names(.) %>% gsub(".","", ., fixed=T)) %>% `names<-`(names(.) %>% gsub(",","", ., fixed=T))

ee14 = part14 %>% select(1, 257:264) 
ee24 = part24 %>% select(1, 257:264)

# Merge all environment & eating questions, t1 & t2
part1comb.ee = part1.ee %>% select(names(ee14)) %>% rbind(ee14) 
part2comb.ee = part2.ee %>% select(names(ee24)) %>% rbind(ee24)

part1all = part1all %>% merge(part1comb.ee, by = "PID", all.x = T)
part2all = part2all %>% merge(part2comb.ee, by = "PID", all.x = T)

### Filter all other outcomes 
outcomesR = outcomes %>% filter(`PID` %in% part1all$PID) %>% filter(!duplicated(PID))

### Add Extra Prolific Information
demo <- readxl::read_xlsx("ProlificDemographics.xlsx") %>% filter(`PID` %in% part1all$PID) %>% filter(!duplicated(PID))

sex.d <- ifelse(demo$Sex=="Female", 1, 2)
demo$Sex <- sex.d
student.d <-ifelse(demo$`Student Status`=="Yes", 1, 0)
demo$`Student Status` <- student.d
# emp.d <- ifelse(demo$`Employment Status`== "Full-Time", 1, )

## Delete all but necessary data
rm(list = ls()[!ls() %in% c("part1all","part2all", "outcomesR", "demo")])

save.image("Data.RData")
