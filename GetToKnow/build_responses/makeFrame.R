## build a pre-history of reponses for the Georgetown server
## (makes the Summary tab Panle more interesting for the first few classes)

## run this script from the root directory

library(plyr) # to map values
library(tigerstats) # source of the survey df's

# read in from my original df.  Omit this if it's already built.
resp <- read.csv(file = "responses.csv", header = T, stringsAsFactors = TRUE)

# > names(resp)
# [1] "name"       "class"      "semester"   "year"       "age"        "address"    "major"     
# [8] "height"     "ideal_ht"   "fastest"    "sleep"      "seat"       "love_first" "extra_life"
# [15] "sex"        "random"     "surprise"   "link"       "latitude"   "longitude"  "time" 

# > names(m111survey)
# [1] "height"          "ideal_ht"        "sleep"           "fastest"         "weight_feel"    
# [6] "love_first"      "extra_life"      "seat"            "GPA"             "enough_Sleep"   
# [11] "sex"             "diff.ideal.act."
# 
# > names(m111surveyfa12)
# [1] "height"      "ideal_ht"    "sleep"       "fastest"     "wt_feel"     "love_first" 
# [7] "et_life"     "seat"        "GPA"         "engh_slp"    "sex"         "anchor"     
# [13] "canada"      "diff.ih.ah."
# 
# > names(m111surveyfa13)
# [1] "height"       "ideal_ht"     "sleep"        "fastest"      "weight_feel"  "love_first"  
# [7] "extra_life"   "seat"         "GPA"          "enough_Sleep" "sex"          "diff"        
# [13] "symbol"       "pop_Canada"  

# grab myself from responses:
temp0 <- resp[1,]
temp0[,"sleep"] <- as.numeric(temp0[,"sleep"])

# make new frame from m111survey
t1 <- m111survey
n1 <- nrow(t1)
name <- rep(NA, n1)
class <- rep("m111", n1)
semester <- rep("Fall",n1)
year <- rep("2013",n1)
age <- rep(NA,n1)
address <- rep(NA,n1)
major <- rep(NA,n1)
height <- t1$height
ideal_ht <- t1$ideal_ht
fastest <- t1$fastest
sleep <- t1$sleep
seat <- mapvalues(x = t1$seat, from = c("1_front", "2_middle", "3_back"),
                  to = c("front","middle","back"))
love_first <- t1$love_first
extra_life <- t1$extra_life
sex <- t1$sex
random <- rep(NA,n1)
surprise <- rep(NA,n1)
link <- rep(NA,n1)
latitude <- rep(NA,n1)
longitude <- rep(NA,n1)
time <- rep(NA,n1)

temp1 <- data.frame(name, class, semester, year, age, address, major, height, ideal_ht,
                    fastest, sleep, seat, love_first, extra_life, sex, random,
                    surprise, link, latitude, longitude, time)

# make new frame from m111surveyfa12
t1 <- m111surveyfa12
t1[28,"seat"] <- NA
n1 <- nrow(t1)
name <- rep(NA, n1)
class <- rep("m111", n1)
semester <- rep("Fall",n1)
year <- rep("2012",n1)
age <- rep(NA,n1)
address <- rep(NA,n1)
major <- rep(NA,n1)
height <- t1$height
ideal_ht <- t1$ideal_ht
fastest <- t1$fastest
sleep <- t1$sleep
seat <- mapvalues(x = t1$seat, from = c("1_front", "2_middle", "3_back"),
                  to = c("front","middle","back"))
love_first <- t1$love_first
extra_life <- t1$et_life
sex <- t1$sex
random <- rep(NA,n1)
surprise <- rep(NA,n1)
link <- rep(NA,n1)
latitude <- rep(NA,n1)
longitude <- rep(NA,n1)
time <- rep(NA,n1)

temp2 <- data.frame(name, class, semester, year, age, address, major, height, ideal_ht,
                    fastest, sleep, seat, love_first, extra_life, sex, random,
                    surprise, link, latitude, longitude, time)

# make new frame from m111surveyfa13
t1 <- m111surveyfa13
n1 <- nrow(t1)
name <- rep(NA, n1)
class <- rep("m111", n1)
semester <- rep("Fall",n1)
year <- rep("2013",n1)
age <- rep(NA,n1)
address <- rep(NA,n1)
major <- rep(NA,n1)
height <- t1$height
ideal_ht <- t1$ideal_ht
fastest <- t1$fastest
sleep <- t1$sleep
seat <- mapvalues(x = t1$seat, from = c("1_front", "2_middle", "3_back"),
                  to = c("front","middle","back"))
love_first <- t1$love_first
extra_life <- t1$extra_life
sex <- t1$sex
random <- rep(NA,n1)
surprise <- rep(NA,n1)
link <- rep(NA,n1)
latitude <- rep(NA,n1)
longitude <- rep(NA,n1)
time <- rep(NA,n1)

temp3 <- data.frame(name, class, semester, year, age, address, major, height, ideal_ht,
                    fastest, sleep, seat, love_first, extra_life, sex, random,
                    surprise, link, latitude, longitude, time)

responses <- rbind(temp0,temp1,temp2,temp3)

write.csv(responses, file = "responses.csv", row.names = FALSE)

c(temp1$sleep,temp2$sleep,temp3$sleep)
