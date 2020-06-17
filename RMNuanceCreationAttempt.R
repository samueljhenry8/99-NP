require(tidyverse); require(corrr); require(EGAnet)

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


Data = read_csv("CombinedT1SelfRatings.csv")
new = Data %>% select(7:263) %>% replace_na(map(., median, na.rm=T)) 

new = ag(new, item1 = "Am always worried about something", item2 = "Rarely worry", rev2 = T)
new = ag(new,"Am afraid of many things", "Am easily frightened")
new = ag(new, "Get irritated easily", "Get angry easily")
new = ag(new, "Am often down in the dumps", "Often feel blue")
new = ag(new, "Have a low opinion of myself", "Think highly of myself", rev2 = T)
new = ag(new, "Take offence easily", "My feelings are easily hurt")
new = ag(new, "Need the approval of others", "Need reassurance")
new = ag(new, "Feel isolated from other people", "Often feel lonely")
new = ag(new, "Hate to hear about the successes of others", "Am happy to see others doing well for themselves", rev2 = T)
new = ag(new, "Feel that people are against me", "Often feel that others laugh or talk about me")

# E
new = ag(new,"Having good friends is important for me", "Have no need for close friendships", rev2 = T)
new = ag(new, "Act as a leader", "See myself as a good leader")
new = ag(new, "Radiate joy", "Smile a lot")
new = ag(new, "Love dangerous situations", "Take risks")
new = ag(new, "Am always joking", "Have a great sense of humor")
new = ag(new, "Dont mind being the center of attention", "Like to stand out in a crowd")
new = ag(new, "Find it difficult to approach others", "Am skilled in handling social situations")

# O

new = ag(new, "Am full of ideas", "Love to think up new ways of doing things")
new = ag(new, "Am open about my feelings", "Have difficulty showing affection", rev2 = T)
new = ag(new, "Love to learn new things", "Am interested in many things")
new = ag(new, "Dislike routine", "Prefer variety to routine")
new = ag(new, "Have a vivid imagination", "Love to daydream")
new = ag(new, "Spend time reflecting on things", "Often think about my experiences and emotions")
new = ag(new, "Believe in the importance of art", "Need a creative outlet")


# (D) A
new = ag(new, "Believe that others have good intentions", "Trust others", rev1 = T, rev2 = T)
new = ag(new, "Find it easy to manipulate others", "Have a natural talent for influencing people")
new = ag(new, "Take advantage of others", "Use others to get what I want")
new = ag(new, "Insist on getting my way", "Feel frustrated when I dont get my own way")
new = ag(new, "Snap at people", "Yell at people")
new = ag(new, "Try to out do others", "Like to compete and do everything I can to win")
new = ag(new, "Rebel against authority", "Respect authority", rev2 = T)

# C
new = ag(new, "Cant make up my mind", "Postpone decisions", rev1 = T, rev2 = T)
new = ag(new, "Keep things tidy", "Leave a mess in my room", rev2 = T)
new = ag(new, "Work hard", "Push myself very hard to succeed")
new = ag(new, "Keep my promises", "Break my promises", rev2 = T)
new = ag(new, "Act without thinking", "Make rash decisions", rev1 = T, rev2 = T)
new = ag(new, "Continue until everything is perfect", "Want every detail taken care of")
new = ag(new, "Am able to control my cravings", "Easily resist temptations")
new = ag(new, "Am willing to accept responsibilities", "Avoid responsibilities", rev2 = T)
new = ag(new, "Am always on time", "Hardly ever finish things on time", rev2 = T)
new = ag(new, "Learn quickly", "Am good at many things")
new = ag(new, "Complete my duties as soon as possible", "Start tasks right away")


# Other

new = ag(new, "Am a spiritual person", "Dont consider myself religious", rev2 = T)
new = ag(new, "Wear stylish clothing", "Love to look my best")
new = ag(new, "Am always busy", "Have too many things to do")
new = ag(new, "Treat all people equally", "Treat all ethnicities and religions equally")
new = ag(new, "Feel thankful for what I have received in life", "Have been richly blessed in my life")
new = ag(new, "Am quick to admit making a mistake", "Easily apologize when I have been wrong")
new = ag(new, "Dislike talking about myself", "Dont like to talk about my achievements")
new = ag(new, "Am good at understanding others feelings", "Know how to comfort others")
new = ag(new, "Dont hesitate to express an unpopular opinion", "Tell others what I really think")
new = ag(new, "Have strong sexual urges", "Dont think much about sex", rev2 = T)

## From the non-overlapping list

new = ag(new,"Get stressed out easily", "Remain calm under pressure", rev2 = T)
new = ag(new, "Have a dark outlook on the future", "Tend to feel very hopeless")
new = ag(new, "Avoid changes", "Adapt easily to new situations", rev2 = T)
#
new = ag(new, "Am usually active and full of energy", "Often feel tired", rev2 = T)
#
new = ag(new, "Experience my emotions intensely", "Am not easily affected by my emotions", rev2 = T)
new = ag(new, "Find political discussions interesting", "Avoid philosophical discussions", rev2 = T)
new = ag(new, "Am interested in science", "Like to solve complex problems") ## problematic
#
new = ag(new, "Cheat to get ahead", "Often tell lies")
new = ag(new, "Think of others first", "Love to help others", rev1 = T, rev2 = T)
new = ag(new, "Believe that I am better than others", "See myself as an average person", rev2 = T)
new = ag(new, "Would like for other people to be afraid of me", "Enjoy hurting others")
new = ag(new, "Am quick to correct others", "Believe that I am always right")
new = ag(new, "Demand a lot from others", "Am hard to satisfy")
#
new = ag(new, "Buy only the things I need", "Spend more money than I should", rev2 = T)
new = ag(new, "Am an extremely loyal person", "Switch my loyalties when I feel like it", rev2 = T)
new = ag(new, "Can maintain interest in something for a long time", "My interests change quickly", rev2 = T)
#
new = ag(new, "Believe in the power of fate", "Believe that all events can be explained scientifically", rev2 = T)
new = ag(new, "Am usually a patient person", "Hate waiting for anything", rev2 = T)

## Rene would add

new = ag(new, "Worry about what people think of me", "Dont care what others think", rev2 = T)
new = ag(new, "Am nervous or tense most of the time", "Am relaxed most of the time", rev2=T)
new = ag(new, "Feel that my life lacks direction", "Am happy with my life", rev2 = T)
new = ag(new, "Blame myself when something goes wrong", "Am often troubled by feelings of guilt")
new = ag(new,"Enjoy social gatherings", "Prefer to be alone", rev2 = T)
new = ag(new, "Talk a lot", "Talking with others energizes me")
new = ag(new, "Have a lot of fun", "Find that nothing excites me", rev2 = T)
new = ag(new, "Want to be in charge", "Would like to have more power than other people")
new = ag(new, "Enjoy cooperating with others", "Like being part of a team", rev1 = T, rev2 = T)
new = ag(new, "Am friendly with most people", "Like most people", rev1 = T, rev2 = T)
new = ag(new, "Dont quit a task before it is finished", "Often stop working if it becomes too hard", rev2 = T)
new = ag(new, "Love order and regularity", "Get upset if objects are not arranged properly")
new = ag(new, "Am able to stand up for myself", "Let others take advantage of me", rev2 = T)
new = ag(new, "Am easy to fool", "Let myself be influenced by others", rev1 = T, rev2 = T)
new = ag(new, "Am easily distracted", "Often forget things", rev1 = T, rev2 = T)
new = ag(new, "Love luxury", "Am mainly interested in money")
new = ag(new, "Believe in the importance of tradition", "Like to be viewed as proper and conventional")
new = ag(new, "Do things in a logical order", "Am always prepared")
new = ag(new, "Am likely to show off if I get the chance", "Love to be complimented")


new %>% select(-contains(";")) %>% cor %>% as_cordf %>% stretch %>% arrange(desc(abs(r))) %>% filter(!duplicated(r)) %>% slice(1:20)

tmp = new %>% select(-contains(";"))

red = node.redundant(tmp, sig = .01)

red$redundant
