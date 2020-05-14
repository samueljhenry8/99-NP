### Aggregate items pairwise - uses 'comb' df formed in SortingOutData3.R

require(tidyverse)
require("psychTools")
require("psych")
require(tidyverse)
require(corrr)
require(glmnet)
require(magrittr)

load("Data.RData") 
# loads cleaned retest data, parts 1 and 2 (all items and current list). 
# part1comb & part2comb compile responses for the most up-to-date item-list we use. 
# get item retest reliability by correlating them (use = "pairwise") and taking the diagonal

load("crossRater99NP.RData") 
# loads cleaned self and informant data, plus rCA values and rCA-corrected inter-item correlations
# get a paired item list with:
crossRaterCorrected <- tmp7 %>% as_cordf %>% stretch %>% arrange(desc(abs(r))) %>% filter(!duplicated(r)) # %>% slice(1:100)"



## ag function:
# 1) Creates (returns) new variable, a combination of the two items
# 2) Prints: correlation between the two items
# df: combined item scores to use to create nuance
# item1 & item2: item names in "" (must be exact)
# rev1, rev2: logical, reverses the value of item 1 or 2, respectively

ag = function(df,item1,item2, rev1=FALSE, rev2=FALSE){
  i1 = df[,item1] %>% scale
  i2 = df[,item2] %>% scale
  if(rev1) i1 = -1 * i1
  if(rev2) i2 = -1 * i2
  df = df %>% select(-item1, -item2) %>% mutate(i3 = rowMeans(cbind(i1,i2),na.rm=T))
  names(df)[grep("i3", names(df))] = paste(item1, item2, sep="; ")
  cat("\nr = ", cor(i1,i2, use="pairwise"), "\n")
  return(df)
}

## ag_rtt: returns the retest reliability of any 2-item nuance

# df1 & df2: data frames of item scores at t1 and t2
# item1 & item2: item names in "" (must be exact)
# rev1, rev2: logical, reverses the value of item 1 or 2, respectively
 
ag_rtt = function(df1,df2,item1,item2, rev1=FALSE, rev2=FALSE){
  i11 = df1[,item1] %>% scale
  i21 = df1[,item2] %>% scale
  i12 = df2[,item1] %>% scale
  i22 = df2[,item2] %>% scale
  if(rev1) i11 = -1 * i11 
  if(rev1) i12 = -1 * i12
  if(rev2) i21 = -1 * i21
  if(rev2) i22 = -1 * i22
  rtt = (i11 + i21) %>% cor(i12 + i22, use = "pairwise")
}

comb = (part1comb[,-1] + part2comb[,-1]) / 2
comb$PID = part1comb$PID # need PID to match up across datasets

rscomb = cor(part1comb[,2:258], part2comb[,2:258], use="pairwise") %>% diag 

pairs  <- cor(scale(comb[,-258]), use = "pairwise") %>% as_cordf %>% shave %>% stretch() %>% arrange(desc(abs(r))) # all paired correlations (uncorrected), arranged by highest absolute value

# add item rTTs
pairs$item1_rTT <- rscomb[pairs$x]
pairs$item2_rTT <- rscomb[pairs$y]

pairs$reliabCorrectCorrelations <- with(pairs, (r / ((sqrt(item1_rTT*item2_rTT))))) # correct pairs for rTT
pairs <- as.tbl(pairs) %>% arrange(desc(abs(pairs$reliabCorrectCorrelations))) # all paired correlations (corrected), arranged by highest absolute value

new <- comb[, -258] # remove PIDs




### Form nuances
# N
new = ag(new, item1 = "Am always worried about something", item2 = "Rarely worry", rev2 = T)
new = ag(new, "Am nervous or tense most of the time", "Am relaxed most of the time", rev2=T)
new = ag(new,"Am afraid of many things", "Am easily frightened")
new = ag(new,"Get stressed out easily", "Remain calm under pressure", rev2 = T)
new = ag(new, "Get irritated easily", "Get angry easily")
new = ag(new, "Am often down in the dumps", "Often feel blue")
new = ag(new, "Have a dark outlook on the future", "Tend to feel very hopeless")
new = ag(new, "Blame myself when something goes wrong", "Am often troubled by feelings of guilt")
new = ag(new, "Feel that my life lacks direction", "Am happy with my life", rev2 = T)
new = ag(new, "Have a low opinion of myself", "Think highly of myself", rev2 = T)
new = ag(new, "Avoid changes", "Adapt easily to new situations", rev2 = T)
new = ag(new, "Take offence easily", "My feelings are easily hurt")
new = ag(new, "Need the approval of others", "Need reassurance")
new = ag(new, "Feel isolated from other people", "Often feel lonely")
new = ag(new, "Hate to hear about the successes of others", "Am happy to see others doing well for themselves", rev2 = T)
new = ag(new, "Often feel that others misunderstand me", "Feel that people are against me")
new = ag(new, "Worry about what people think of me", "Am not embarrassed easily", rev2 = T)


# E
new = ag(new,"Enjoy social gatherings", "Prefer to be alone", rev2 = T)
new = ag(new,"Having good friends is important for me", "Have no need for close friendships", rev2 = T)
new = ag(new, "Act as a leader", "See myself as a good leader")
new = ag(new, "Radiate joy", "Smile a lot")
new = ag(new, "Find that nothing excites me", "Am often bored", rev1 = T, rev2 = T)
new = ag(new, "Am usually active and full of energy", "Often feel tired", rev2 = T)
new = ag(new, "Love dangerous situations", "Take risks")
new = ag(new, "Am always joking", "Have a great sense of humor")
new = ag(new, "Talk a lot", "Talking with others energizes me")
new = ag(new, "Love excitement", "Have a lot of fun")
new = ag(new, "Am skilled in handling social situations", "Find it difficult to approach others", rev2 = T)
new = ag(new, "Dont mind being the center of attention", "Like to stand out in a crowd")


# O
new = ag(new, "Experience my emotions intensely", "Am not easily affected by my emotions", rev2 = T)
new = ag(new, "Am full of ideas", "Love to think up new ways of doing things")
new = ag(new, "Am open about my feelings", "Have difficulty showing affection", rev2 = T)
new = ag(new, "Love to learn new things", "Am interested in many things")
new = ag(new, "Dislike routine", "Prefer variety to routine")
#new = ag(new, "Like to visit new places", "Am willing to try anything once") ## problematic
new = ag(new, "Believe in the importance of art", "Do not like poetry", rev2 = T)
new = ag(new, "Have a vivid imagination", "Love to daydream")
new = ag(new, "Find political discussions interesting", "Avoid philosophical discussions", rev2 = T)
new = ag(new, "Am interested in science", "Like to solve complex problems") ## problematic
new = ag(new, "Spend time reflecting on things", "Often think about my experiences and emotions")


# (D) A
new = ag(new, "Want to be in charge", "Would like to have more power than other people")
new = ag(new, "Enjoy cooperating with others", "Like being part of a team", rev1 = T, rev2 = T)
new = ag(new, "Am friendly with most people", "Like most people", rev1 = T, rev2 = T)
new = ag(new, "Believe that others have good intentions", "Trust others", rev1 = T, rev2 = T)
new = ag(new, "Cheat to get ahead", "Often tell lies")
new = ag(new, "Am quick to judge others", "Accept people as they are", rev2 = T)
new = ag(new, "Find it easy to manipulate others", "Have a natural talent for influencing people")
new = ag(new, "Take advantage of others", "Use others to get what I want")
new = ag(new, "Insist on getting my way", "Feel frustrated when I dont get my own way")
new = ag(new, "Care about others", "Love to help others", rev1 = T, rev2 = T)
new = ag(new, "Believe that I am better than others", "See myself as an average person", rev2 = T)
new = ag(new, "Find it hard to forgive others", "Get back at people who insult me")
new = ag(new, "Snap at people", "Yell at people")
new = ag(new, "Would like for other people to be afraid of me", "Enjoy hurting others")
new = ag(new, "Try to out do others", "Like to compete and do everything I can to win")
new = ag(new, "Demand a lot from others", "Am hard to satisfy")
new = ag(new, "Am quick to correct others", "Believe that I am always right")


# C
new = ag(new, "Love order and regularity", "Do things in a logical order")
new = ag(new, "Cant make up my mind", "Postpone decisions", rev1 = T, rev2 = T)
new = ag(new, "Keep things tidy", "Leave a mess in my room", rev2 = T)
new = ag(new, "Start tasks right away", "Find it difficult to get down to work", rev2 = T)
new = ag(new, "Work hard", "Push myself very hard to succeed")
new = ag(new, "Keep my promises", "Break my promises", rev2 = T)
new = ag(new, "Dont quit a task before it is finished", "Often stop working if it becomes too hard", rev2 = T)
new = ag(new, "Act without thinking", "Make rash decisions", rev1 = T, rev2 = T)
new = ag(new, "Continue until everything is perfect", "Want every detail taken care of")
new = ag(new, "Buy only the things I need", "Spend more money than I should", rev2 = T)
new = ag(new, "Am able to stand up for myself", "Let others take advantage of me", rev2 = T)
new = ag(new, "Am able to control my cravings", "Easily resist temptations")
new = ag(new, "Am willing to accept responsibilities", "Avoid responsibilities", rev2 = T)
new = ag(new, "Can maintain interest in something for a long time", "My interests change quickly", rev2 = T)
new = ag(new, "Am an extremely loyal person", "Switch my loyalties when I feel like it", rev2 = T)
new = ag(new, "Am always on time", "Hardly ever finish things on time", rev2 = T)
new = ag(new, "Am easy to fool", "Let myself be influenced by others", rev1 = T, rev2 = T)
new = ag(new, "Am easily distracted", "Often forget things", rev1 = T, rev2 = T)
new = ag(new, "Learn quickly", "Am good at many things")


# Other
new = ag(new, "Enjoy flirting", "Dont think much about sex", rev2 = T)
new = ag(new, "Am a spiritual person", "Dont consider myself religious", rev2 = T)
new = ag(new, "Wear stylish clothing", "Love to look my best")
new = ag(new, "Love luxury", "Am mainly interested in money")
# new = ag(new, "Do things that men traditionally do", "Do things that women traditionally do", rev2=T)
new = ag(new, "Believe in the power of fate", "Believe that all events can be explained scientifically", rev2 = T)
# new = ag(new, "Believe that by working hard a person can achieve anything", "Believe that some people are born lucky", rev2 = T)
# new = ag(new, "Worry about my health", "Consider myself healthy for my age", rev2 = T)
new = ag(new, "Am always busy", "Have too many things to do")
new = ag(new, "Rebel against authority", "Like to be viewed as proper and conventional", rev2 = T)
new = ag(new, "Believe in the importance of tradition", "Think good manners are very important")
new = ag(new, "Treat all people equally", "Treat all ethnicities and religions equally")
new = ag(new, "Feel thankful for what I have received in life", "Have been richly blessed in my life")
new = ag(new, "Am quick to admit making a mistake", "Easily apologize when I have been wrong")
new = ag(new, "Dislike talking about myself", "Dont like to talk about my achievements")
new = ag(new, "Am good at understanding others feelings", "Know how to comfort others")
new = ag(new, "Get suspicious when someone treats me nicely", "Often feel that others laugh or talk about me")
new = ag(new, "Dont hesitate to express an unpopular opinion", "Tell others what I really think")#new = ag(new, "Believe that the poor deserve our sympathy", "Cant stand weak people", rev2=T)
new = ag(new, "Am usually a patient person", "Hate waiting for anything", rev2 = T)



### Create df of Nuance Retest Reliabilities

nuances <- as.data.frame(rbind(
ag_rtt(part1comb, part2comb, "Am always worried about something", item2 = "Rarely worry", rev2 = T),
ag_rtt(part1comb, part2comb, "Am nervous or tense most of the time", "Am relaxed most of the time", rev2=T),
ag_rtt(part1comb, part2comb, "Am afraid of many things", "Am easily frightened"),
ag_rtt(part1comb, part2comb, "Get stressed out easily", "Remain calm under pressure", rev2 = T),
ag_rtt(part1comb, part2comb, "Get irritated easily", "Get angry easily"),
ag_rtt(part1comb, part2comb, "Am often down in the dumps", "Often feel blue"),
ag_rtt(part1comb, part2comb, "Have a dark outlook on the future", "Tend to feel very hopeless"),
ag_rtt(part1comb, part2comb, "Blame myself when something goes wrong", "Am often troubled by feelings of guilt"),
ag_rtt(part1comb, part2comb, "Feel that my life lacks direction", "Am happy with my life", rev2 = T),
ag_rtt(part1comb, part2comb, "Have a low opinion of myself", "Think highly of myself", rev2 = T),
ag_rtt(part1comb, part2comb, "Avoid changes", "Adapt easily to new situations", rev2 = T),
ag_rtt(part1comb, part2comb, "Take offence easily", "My feelings are easily hurt"),
ag_rtt(part1comb, part2comb, "Need the approval of others", "Need reassurance"),
ag_rtt(part1comb, part2comb, "Feel isolated from other people", "Often feel lonely"),
ag_rtt(part1comb, part2comb, "Hate to hear about the successes of others", "Am happy to see others doing well for themselves", rev2 = T),
ag_rtt(part1comb, part2comb, "Often feel that others misunderstand me", "Feel that people are against me"),
ag_rtt(part1comb, part2comb, "Worry about what people think of me", "Am not embarrassed easily", rev2 = T),


# E
ag_rtt(part1comb, part2comb,"Enjoy social gatherings", "Prefer to be alone", rev2 = T),
ag_rtt(part1comb, part2comb, "Having good friends is important for me", "Have no need for close friendships", rev2 = T),
ag_rtt(part1comb, part2comb,"Act as a leader", "See myself as a good leader"),
ag_rtt(part1comb, part2comb,"Radiate joy", "Smile a lot"),
ag_rtt(part1comb, part2comb,"Find that nothing excites me", "Am often bored", rev1 = T, rev2 = T),
ag_rtt(part1comb, part2comb,"Am usually active and full of energy", "Often feel tired", rev2 = T),
ag_rtt(part1comb, part2comb,"Love dangerous situations", "Take risks"),
ag_rtt(part1comb, part2comb,"Am always joking", "Have a great sense of humor"),
ag_rtt(part1comb, part2comb,"Talk a lot", "Talking with others energizes me"),
ag_rtt(part1comb, part2comb,"Love excitement", "Have a lot of fun"),
ag_rtt(part1comb, part2comb, "Am skilled in handling social situations", "Find it difficult to approach others", rev2 = T),
ag_rtt(part1comb, part2comb, "Dont mind being the center of attention", "Like to stand out in a crowd"),


# O
ag_rtt(part1comb, part2comb, "Experience my emotions intensely", "Am not easily affected by my emotions", rev2 = T),
ag_rtt(part1comb, part2comb, "Am full of ideas", "Love to think up new ways of doing things"),
ag_rtt(part1comb, part2comb, "Am open about my feelings", "Have difficulty showing affection", rev2 = T),
ag_rtt(part1comb, part2comb, "Love to learn new things", "Am interested in many things"),
ag_rtt(part1comb, part2comb, "Dislike routine", "Prefer variety to routine"),
# ag_rtt(part1comb, part2comb,"Like to visit new places", "Am willing to try anything once") ## problematic
ag_rtt(part1comb, part2comb, "Believe in the importance of art", "Do not like poetry", rev2 = T),
ag_rtt(part1comb, part2comb,"Have a vivid imagination", "Love to daydream"),
ag_rtt(part1comb, part2comb,"Find political discussions interesting", "Avoid philosophical discussions", rev2 = T),
ag_rtt(part1comb, part2comb,"Am interested in science", "Like to solve complex problems"), ##, problematic
ag_rtt(part1comb, part2comb,"Spend time reflecting on things", "Often think about my experiences and emotions"),


# (D) A
ag_rtt(part1comb, part2comb,"Want to be in charge", "Would like to have more power than other people"),
ag_rtt(part1comb, part2comb,"Enjoy cooperating with others", "Like being part of a team", rev1 = T, rev2 = T),
ag_rtt(part1comb, part2comb,"Am friendly with most people", "Like most people", rev1 = T, rev2 = T),
ag_rtt(part1comb, part2comb,"Believe that others have good intentions", "Trust others", rev1 = T, rev2 = T),
ag_rtt(part1comb, part2comb,"Cheat to get ahead", "Often tell lies"),
ag_rtt(part1comb, part2comb, "Am quick to judge others", "Accept people as they are", rev2 = T),
ag_rtt(part1comb, part2comb, "Find it easy to manipulate others", "Have a natural talent for influencing people"),
ag_rtt(part1comb, part2comb, "Take advantage of others", "Use others to get what I want"),
ag_rtt(part1comb, part2comb, "Insist on getting my way", "Feel frustrated when I dont get my own way"),
ag_rtt(part1comb, part2comb, "Care about others", "Love to help others", rev1 = T, rev2 = T),
ag_rtt(part1comb, part2comb, "Believe that I am better than others", "See myself as an average person", rev2 = T),
ag_rtt(part1comb, part2comb, "Find it hard to forgive others", "Get back at people who insult me"),
ag_rtt(part1comb, part2comb, "Snap at people", "Yell at people"),
ag_rtt(part1comb, part2comb, "Would like for other people to be afraid of me", "Enjoy hurting others"),
ag_rtt(part1comb, part2comb, "Try to out do others", "Like to compete and do everything I can to win"),
ag_rtt(part1comb, part2comb, "Demand a lot from others", "Am hard to satisfy"),
ag_rtt(part1comb, part2comb, "Am quick to correct others", "Believe that I am always right"),


# C
ag_rtt(part1comb, part2comb, "Love order and regularity", "Do things in a logical order"),
ag_rtt(part1comb, part2comb, "Cant make up my mind", "Postpone decisions", rev1 = T, rev2 = T),
ag_rtt(part1comb, part2comb, "Keep things tidy", "Leave a mess in my room", rev2 = T),
ag_rtt(part1comb, part2comb, "Start tasks right away", "Find it difficult to get down to work", rev2 = T),
ag_rtt(part1comb, part2comb, "Work hard", "Push myself very hard to succeed"),
ag_rtt(part1comb, part2comb, "Keep my promises", "Break my promises", rev2 = T),
ag_rtt(part1comb, part2comb, "Dont quit a task before it is finished", "Often stop working if it becomes too hard", rev2 = T),
ag_rtt(part1comb, part2comb, "Act without thinking", "Make rash decisions", rev1 = T, rev2 = T),
ag_rtt(part1comb, part2comb, "Continue until everything is perfect", "Want every detail taken care of"),
ag_rtt(part1comb, part2comb, "Buy only the things I need", "Spend more money than I should", rev2 = T),
ag_rtt(part1comb, part2comb, "Am able to stand up for myself", "Let others take advantage of me", rev2 = T),
ag_rtt(part1comb, part2comb, "Am able to control my cravings", "Easily resist temptations"),
ag_rtt(part1comb, part2comb, "Am willing to accept responsibilities", "Avoid responsibilities", rev2 = T),
ag_rtt(part1comb, part2comb, "Can maintain interest in something for a long time", "My interests change quickly", rev2 = T),
ag_rtt(part1comb, part2comb, "Am an extremely loyal person", "Switch my loyalties when I feel like it", rev2 = T),
ag_rtt(part1comb, part2comb, "Am always on time", "Hardly ever finish things on time", rev2 = T),
ag_rtt(part1comb, part2comb, "Am easy to fool", "Let myself be influenced by others", rev1 = T, rev2 = T),
ag_rtt(part1comb, part2comb, "Am easily distracted", "Often forget things", rev1 = T, rev2 = T),
ag_rtt(part1comb, part2comb, "Learn quickly", "Am good at many things"),


# Other
ag_rtt(part1comb, part2comb, "Enjoy flirting", "Dont think much about sex", rev2 = T),
ag_rtt(part1comb, part2comb, "Am a spiritual person", "Dont consider myself religious", rev2 = T),
ag_rtt(part1comb, part2comb, "Wear stylish clothing", "Love to look my best"),
ag_rtt(part1comb, part2comb, "Love luxury", "Am mainly interested in money"),
# ag_rtt(part1comb, part2comb, "Do things that men traditionally do", "Do things that women traditionally do", rev2=T),
ag_rtt(part1comb, part2comb, "Believe in the power of fate", "Believe that all events can be explained scientifically", rev2 = T),
# ag_rtt(part1comb, part2comb, "Believe that by working hard a person can achieve anything", "Believe that some people are born lucky", rev2 = T),
# ag_rtt(part1comb, part2comb, "Worry about my health", "Consider myself healthy for my age", rev2 = T),
ag_rtt(part1comb, part2comb, "Am always busy", "Have too many things to do"),
ag_rtt(part1comb, part2comb, "Rebel against authority", "Like to be viewed as proper and conventional", rev2 = T),
ag_rtt(part1comb, part2comb,"Believe in the importance of tradition", "Think good manners are very important"),
ag_rtt(part1comb, part2comb, "Treat all people equally", "Treat all ethnicities and religions equally"),
ag_rtt(part1comb, part2comb,"Feel thankful for what I have received in life", "Have been richly blessed in my life"),
ag_rtt(part1comb, part2comb,"Am quick to admit making a mistake", "Easily apologize when I have been wrong"),
ag_rtt(part1comb, part2comb, "Dislike talking about myself", "Dont like to talk about my achievements"),
ag_rtt(part1comb, part2comb, "Am good at understanding others feelings", "Know how to comfort others"),
ag_rtt(part1comb, part2comb, "Get suspicious when someone treats me nicely", "Often feel that others laugh or talk about me"),
ag_rtt(part1comb, part2comb, "Dont hesitate to express an unpopular opinion", "Tell others what I really think"), #new = ag(new, "Believe that the poor deserve our sympathy", "Cant stand weak people", rev2=T)
ag_rtt(part1comb, part2comb, "Am usually a patient person", "Hate waiting for anything", rev2 = T)
)) %>% cbind(names(new[, 76:ncol(new)]))

names(nuances) = c("rTT", "names")

### END NUANCE GENERATION


## merge some additional descriptives with the nuance list

# Some items reversed in intercorrelations: create dfs with ';'-separated pairs in both orders
pairs1 <- pairs %>% unite(col = "names", x:y, sep = "; ")
pairs2 <- pairs %>% unite(col = "names", y:x, sep = "; ")

nuances <- merge(nuances, pairs1, by="names", all.x=T)
nuances <- merge(nuances, pairs2, by="names", all.x=T)

nuances <- nuances %>% mutate("Intercorrelations" = coalesce(r.x, r.y)) %>% select("names", "rTT", "Intercorrelations")

# Add reliability-corrected intra-nuance correlations
pairsReliabilityCorrected1 <- pairs %>% unite(col = "names", x:y, sep = "; ") %>% select("names", "reliabCorrectCorrelations")
pairsReliabilityCorrected2 <- pairs %>% unite(col = "names", y:x, sep = "; ") %>% select("names", "reliabCorrectCorrelations")

nuances <- merge(nuances, pairsReliabilityCorrected1, by="names", all.x=T)
nuances <- merge(nuances, pairsReliabilityCorrected2, by="names", all.x=T)

nuances <- nuances %>% mutate("reliabCorrectCorrelations" = coalesce(reliabCorrectCorrelations.x, reliabCorrectCorrelations.y)) %>% select("names", "rTT", "Intercorrelations", "reliabCorrectCorrelations")


nuances = nuances %>% separate(names, into = c("item1", "item2"), sep = "; ")
nuances$item1_rTT <- rscomb[nuances$item1]
nuances$item2_rTT <- rscomb[nuances$item2]

# can use this to reset order to whichever variable you want
nuances = nuances %>% as.tbl %>% arrange(desc(abs(reliabCorrectCorrelations)))

leftovers <- rscomb[names(new[, 1:75])] %>% sort(decreasing = T)  # col index = 1:last item before nuances

head(nuances)
# write.csv(nuances, "nuances.csv")

# table(nuances$rTT <.80)
# summary(nuances)

# can mostly ignore this, just looks at how different variables relate to one another

with(nuances, cor(abs(Intercorrelations), rTT))
with(nuances, cor(abs(Intercorrelations), ((item2_rTT + item1_rTT)/2)))
with(nuances, cor(rTT, item2_rTT))
with(nuances, cor(rTT, item1_rTT))
with(nuances, cor(item1_rTT, item2_rTT))
nuances %$% cor(abs(Intercorrelations), abs(reliabCorrectCorrelations))
nuances %$% cor(abs(reliabCorrectCorrelations), rTT)

lm(rTT ~ abs(Intercorrelations), data = nuances) %>% summary
lm(rTT ~ abs(reliabCorrectCorrelations), data = nuances) %>% summary
# Corrected correlations do not predict nuance rTT (but non-corrected do) 


rm(list = ls()[!ls() %in% c("nuances","comb", "new", "rscomb", "leftovers", "pairs")])
save.image("aggregateNuances.RData")

nuances %>% arrange(desc(abs(reliabCorrectCorrelations))) %>% 
  mutate_if(is.numeric, ~round(.x, 2)) %>% slice(1:100) %>% View

# cor(part1comb[,-1], part2comb[, -1], use = "pairwise") 
