library(tidyverse)
library(psych)
library(lm.beta)
library(gridExtra)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(car)
library(dplyr)
library(lmtest)
library(lme4)
library(lmerTest) 
library(cAIC4) 	
library(r2glmm) 
library(MuMIn)
library(gsheet)
library(boot)
library(lmboot)
library(boot)
library(jtools)
library(huxtable)
library(caTools)
library(corrplot)
source("http://www.sthda.com/upload/rquery_cormat.r")
library(Hmisc)


daddy_issues <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1jG_Kxbxgpil1OvVI1tU6KPdQt48d_bHH26ILacyOSVg/edit#gid=0')
View(daddy_issues)

#Rename variables 
daddy_issues <- daddy_issues %>% 
  rename(
    woman = Q2,
    country = Q3,
    other.country = Q4, 
    ethn. = Q5, 
    other.ethn = Q6,
    age.group = Q7,
    rel = Q8,
    other.rel = Q9,
    sex.or = Q10,
    other.sex.or = Q11,
    RCSE1 = Q12_1,
    RCSE2 = Q12_2,
    RCSE3 = Q12_3,
    RCSE4 = Q12_4,
    RCSE5 = Q12_5,
    RCSE6 = Q12_6,
    RCSE7 = Q12_7,
    RCSE8 = Q12_8,
    RCSE9 = Q12_9,
    RCSE10 = Q12_10,
    FIS_e1 =  Q13_1,
    FIS_e2 = Q13_2,
    FIS_e3 = Q13_3,
    FIS_e4 = Q13_4,
    FIS_e5 = Q13_5,
    FIS_e6 = Q13_6,
    FIS_e7 = Q13_7,
    FIS_e8 = Q13_8,
    FIS_i1 = Q13_9,
    FIS_i2 = Q13_10,
    FIS_i3 = Q13_11,
    FIS_i4 = Q13_12,
    FIS_i5 = Q13_13,
    FIS_i6 = Q13_14,
    FIS_i7 = Q13_15,
    FIS_i8 = Q13_16,
    FIS_m1 = Q13_17,
    FIS_m2 = Q13_18,
    FIS_m3 = Q13_19,
    FIS_m4 = Q13_20,
    MIS_e1 = Q14_1,
    MIS_e2 = Q14_2,
    MIS_e3 = Q14_3,
    MIS_e4 = Q14_4,
    MIS_e5 = Q14_5,
    MIS_e6 = Q14_6,
    MIS_e7 = Q14_7,
    MIS_e8 = Q14_8,
    MIS_i1 = Q14_9,
    MIS_i2 = Q14_10,
    MIS_i3 = Q14_11,
    MIS_i4 = Q14_12,
    MIS_i5 = Q14_13,
    MIS_i6 = Q14_14,
    MIS_i7 = Q14_15,
    MIS_m1 = Q14_16,
    MIS_m2 = Q14_17,
    MIS_m3 = Q14_18,
    AMORE_e1 = Q15_1,
    AMORE_e2 = Q15_2,
    AMORE_e3 = Q15_3,
    AMORE_e4 = Q15_4,
    AMORE_e5 = Q15_5,
    AMORE_e6 = Q15_6,
    AMORE_e7 = Q15_7,
    AMORE_p1 = Q16_1,
    AMORE_p2 = Q16_2,
    AMORE_p3 = Q16_3,
    AMORE_p4 = Q16_4,
    AMORE_p5 = Q16_5,
    AMORE_p6 = Q16_6,
    AMORE_p7 = Q16_7,
    AMORE_p8 = Q16_8,
    AMORE_p9 = Q16_9,
    AMORE_p10 = Q16_10,
  )

View(daddy_issues)

#Remove top line with question prompt
daddy_issues <- daddy_issues[-c(1),]
View(daddy_issues)

#Re-coding values 
  #RCSE
RCSE_scores <- c("Not at all like me" = 1, "Slightly like me" = 2, "Somewhat like me" = 3, "Moderately like me" = 4, "Very much like me" = 5)
RCSE_scores.reversed <- c("Not at all like me" = 5, "Slightly like me" = 4, "Somewhat like me" = 3, "Moderately like me" = 2, "Very much like me" = 1)
  
  
daddy_issues$RCSE1 <- RCSE_scores[daddy_issues$RCSE1]
daddy_issues$RCSE2 <- RCSE_scores[daddy_issues$RCSE2]
daddy_issues$RCSE3 <- RCSE_scores[daddy_issues$RCSE3]
daddy_issues$RCSE4 <- RCSE_scores[daddy_issues$RCSE4]
daddy_issues$RCSE5 <- RCSE_scores[daddy_issues$RCSE5]
daddy_issues$RCSE8 <- RCSE_scores[daddy_issues$RCSE8]
daddy_issues$RCSE9 <- RCSE_scores[daddy_issues$RCSE9]
daddy_issues$RCSE10 <- RCSE_scores[daddy_issues$RCSE10]

#Reverse scoring values
  #RCSE
daddy_issues$RCSE6 <- RCSE_scores.reversed[daddy_issues$RCSE6]
daddy_issues$RCSE7 <- RCSE_scores.reversed[daddy_issues$RCSE7]

is.factor(daddy_issues$RCSE6)
is.list(daddy_issues$RCSE6)
is.vector(daddy_issues$RCSE6)

#Re-coding values 
  #FIS
FIS_scores <- c("Never involved" = 1, "Hardly involved" = 2, "Can't say" = 3, "Frequently involved" = 4, "Always involved" = 5)

daddy_issues$FIS_e1 <- FIS_scores[daddy_issues$FIS_e1]
daddy_issues$FIS_e2 <- FIS_scores[daddy_issues$FIS_e2]
daddy_issues$FIS_e3 <- FIS_scores[daddy_issues$FIS_e3]
daddy_issues$FIS_e4 <- FIS_scores[daddy_issues$FIS_e4]
daddy_issues$FIS_e5 <- FIS_scores[daddy_issues$FIS_e5]
daddy_issues$FIS_e6 <- FIS_scores[daddy_issues$FIS_e6]
daddy_issues$FIS_e8 <- FIS_scores[daddy_issues$FIS_e8]
daddy_issues$FIS_i1 <- FIS_scores[daddy_issues$FIS_i1]
daddy_issues$FIS_i2 <- FIS_scores[daddy_issues$FIS_i2]
daddy_issues$FIS_i3 <- FIS_scores[daddy_issues$FIS_i3]
daddy_issues$FIS_i5 <- FIS_scores[daddy_issues$FIS_i5]
daddy_issues$FIS_i6 <- FIS_scores[daddy_issues$FIS_i6]
daddy_issues$FIS_i7 <- FIS_scores[daddy_issues$FIS_i7]
daddy_issues$FIS_m2 <- FIS_scores[daddy_issues$FIS_m2]
daddy_issues$FIS_m3 <- FIS_scores[daddy_issues$FIS_m3]
daddy_issues$FIS_m4 <- FIS_scores[daddy_issues$FIS_m4]

is.vector(daddy_issues$FIS_e1)

  #MIS
MIS_scores <- c("Never involved" = 1, "Hardly involved" = 2, "Can't say" = 3, "Frequently involved" = 4, "Always involved" = 5)

daddy_issues$MIS_e1 <- MIS_scores[daddy_issues$MIS_e1]
daddy_issues$MIS_e2 <- MIS_scores[daddy_issues$MIS_e2]
daddy_issues$MIS_e3 <- MIS_scores[daddy_issues$MIS_e3]
daddy_issues$MIS_e4 <- MIS_scores[daddy_issues$MIS_e4]
daddy_issues$MIS_e5 <- MIS_scores[daddy_issues$MIS_e5]
daddy_issues$MIS_e6 <- MIS_scores[daddy_issues$MIS_e6]
daddy_issues$MIS_e8 <- MIS_scores[daddy_issues$MIS_e8]
daddy_issues$MIS_i1 <- MIS_scores[daddy_issues$MIS_i1]
daddy_issues$MIS_i2 <- MIS_scores[daddy_issues$MIS_i2]
daddy_issues$MIS_i3 <- MIS_scores[daddy_issues$MIS_i3]
daddy_issues$MIS_i5 <- MIS_scores[daddy_issues$MIS_i5]
daddy_issues$MIS_i6 <- MIS_scores[daddy_issues$MIS_i6]
daddy_issues$MIS_i7 <- MIS_scores[daddy_issues$MIS_i7]
daddy_issues$MIS_m1 <- MIS_scores[daddy_issues$MIS_m1]
daddy_issues$MIS_m2 <- MIS_scores[daddy_issues$MIS_m2]
daddy_issues$MIS_m3 <- MIS_scores[daddy_issues$MIS_m3]

is.vector(daddy_issues$MIS_e1)

  #AMORE - E
AMORE_scores <- c("Not at all true" = 1, "Somewhat untrue" = 2, "Moderately true" = 3, "Somewhat true" = 4, "Completely true" = 5)

daddy_issues$AMORE_e1 <- AMORE_scores[daddy_issues$AMORE_e1]
daddy_issues$AMORE_e2 <- AMORE_scores[daddy_issues$AMORE_e2]
daddy_issues$AMORE_e3 <- AMORE_scores[daddy_issues$AMORE_e3]
daddy_issues$AMORE_e4 <- AMORE_scores[daddy_issues$AMORE_e4]
daddy_issues$AMORE_e5 <- AMORE_scores[daddy_issues$AMORE_e5]
daddy_issues$AMORE_e6 <- AMORE_scores[daddy_issues$AMORE_e6]
daddy_issues$AMORE_e7 <- AMORE_scores[daddy_issues$AMORE_e7]

  #AMORE - P
daddy_issues$AMORE_p1 <- AMORE_scores[daddy_issues$AMORE_p1]
daddy_issues$AMORE_p2 <- AMORE_scores[daddy_issues$AMORE_p2]
daddy_issues$AMORE_p3 <- AMORE_scores[daddy_issues$AMORE_p3]
daddy_issues$AMORE_p4 <- AMORE_scores[daddy_issues$AMORE_p4]
daddy_issues$AMORE_p5 <- AMORE_scores[daddy_issues$AMORE_p5]
daddy_issues$AMORE_p6 <- AMORE_scores[daddy_issues$AMORE_p6]
daddy_issues$AMORE_p7 <- AMORE_scores[daddy_issues$AMORE_p7]
daddy_issues$AMORE_p8 <- AMORE_scores[daddy_issues$AMORE_p8]
daddy_issues$AMORE_p9 <- AMORE_scores[daddy_issues$AMORE_p9]
daddy_issues$AMORE_p10 <- AMORE_scores[daddy_issues$AMORE_p10]

is.vector(daddy_issues$AMORE_e1)
is.vector(daddy_issues$AMORE_p1)

#Examine data set for any outliers 
#RCSE has normal range, skew, and kurtosis. no outliers
#FIS has normal range, skew, and kurtosis. no outliers
#MIS has normal range, skew, and kurtosis. no outliers
#AMORE_e has normal range, skew, and kurtosis. no outliers
#AMORE_p has normal range, skew, and kurtosis. no outliers

#Dealing with missing data 
#How many cases are missing per variable
#How many observations each participant is missing 
#Added missing count ot each participant with new variable nmiss (6 people are missing 5 data, 25 are missing 6, etc)
fdaddy_issues <- daddy_issues
View(fdaddy_issues)
sapply(fdaddy_issues, function(X) sum(is.na(X)))

apply(fdaddy_issues, 1, function(X) sum(is.na(X)))
fdaddy_issues$nmiss <- apply(fdaddy_issues, 1, function(X) sum(is.na(X)))
table(fdaddy_issues$nmiss)

#What will sample look like if we remove the rest of the NA's?
#Would mean removing anybody who has 17 or more observations missing
#This would mean dropping 50 and keeping 586 
sapply(fdaddy_issues, function(X) sum(is.na(X)))
fdaddy_issues$nmiss <- apply(fdaddy_issues, 1, function(X) sum(is.na(X)))

fdaddy_issues$retain <- fdaddy_issues$nmiss < 13
table(fdaddy_issues$retain) 

#Create new data set with this number 
#Instead of having 643 rows we have 586
#Can see how many observations per variable are still missing. 0 in RCSE, FIS, & MIS,
#but still 2 in AMORE_p10, p9, p8, and 1 in p7, 6 and 5
#Will removed these NA's by case 
#This leaves us with 584 rows
fdaddy_issues <- fdaddy_issues[ fdaddy_issues$retain, ]
View(fdaddy_issues)
sapply(fdaddy_issues, function(X) sum(is.na(X)))
dim(fdaddy_issues)

#Does this still leave us with a full-powered U.S. and international sample? Yes (:
table(fdaddy_issues$country)
View(fdaddy_issues)

#Visualize data 
fdaddy_issues %>%
  ggplot() +
  aes(x = sex.or, color = sex.or) +
  geom_bar(fill = "light blue")
fdaddy_issues %>%
  ggplot() +
  aes(x = other.sex.or, color = other.sex.or) +
  geom_bar(fill = "light blue")

fdaddy_issues %>%
  ggplot() +
  aes(x = age.group, color = age.group) +
  geom_bar(fill = "light blue")

fdaddy_issues %>%
  ggplot() +
  aes(x = country, color = country) +
  geom_bar(fill = "light blue")
fdaddy_issues %>%
  ggplot() +
  aes(x = other.country, color = other.country) +
  geom_bar(fill = "light blue")

fdaddy_issues %>%
  ggplot() +
  aes(x = rel, color = rel) +
  geom_bar(fill = "light blue")
fdaddy_issues %>%
  ggplot() +
  aes(x = other.rel, color = other.rel) +
  geom_bar(fill = "light blue")

#Recategorize categorical data 
  #sexual orientation: 153 bisexual, 32, self-describe, 14 gay/lesbian, 23 queer, 2 no say, 419 straight
  #All "prefer to describe" sexual orientations will be labeled "Other" for ease of analysis 
table(fdaddy_issues$sex.or)

fdaddy_issues$sex.or[fdaddy_issues$sex.or == "Prefer to self-describe"] <- "Other"
fdaddy_issues$sex.or[fdaddy_issues$sex.or == "Gay / Lesbian"] <- "Gay"
fdaddy_issues$sex.or[fdaddy_issues$sex.or == "Straight / Heterosexual"] <- "Straight"

  #Age group: 328 18-25, 272 26-39, 39 40-59, 4 60 and older
  #Do we want to relabel them to "Young adult" etc?
table(fdaddy_issues$age.group)

  #Country: 81 India, 338 U.S., 223 Other
  #Other: Southern Europe: 7, South America: 10, Australia/New Zealand: 9, Western Europe: 111, Middle East: 4, Eastern Europe: 20, Canada: 38, Carribean: 2, Africa: 5, Mexico: 2, Asia: 90   
  #Recategorized specific countries into the above regions 
  #Will include India in the Middle East

table(fdaddy_issues$country)
table(fdaddy_issues$other.country)

fdaddy_issues$other.country[fdaddy_issues$other.country == "Croatia"] <- "Southern Europe"
fdaddy_issues$other.country[fdaddy_issues$other.country == "France"] <- "Southern Europe"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Greece"] <- "Southern Europe"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Greecw"] <- "Southern Europe"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Spain"] <- "Southern Europe"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Portugal"] <- "Southern Europe"

fdaddy_issues$other.country[fdaddy_issues$other.country == "Argentina"] <- "South America"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Brasil"] <- "South America"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Brazil"] <- "South America"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Chile"] <- "South America"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Venezuela"] <- "South America"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Uruguay"] <- "South America"

fdaddy_issues$other.country[fdaddy_issues$other.country == "Australia"] <- "Australia / New Zealand"
fdaddy_issues$other.country[fdaddy_issues$other.country == "New Zealand"] <- "Australia / New Zealand"

fdaddy_issues$other.country[fdaddy_issues$other.country == "Austria"] <- "Western Europe"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Belgium"] <- "Western Europe"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Denmark"] <- "Western Europe"
fdaddy_issues$other.country[fdaddy_issues$other.country == "England"] <- "Western Europe"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Germany"] <- "Western Europe"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Iceland"] <- "Western Europe"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Ireland"] <- "Western Europe"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Netherlands"] <- "Western Europe"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Norway"] <- "Western Europe"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Scotland"] <- "Western Europe"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Sweden"] <- "Western Europe"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Switzerland"] <- "Western Europe"
fdaddy_issues$other.country[fdaddy_issues$other.country == "the Netherlands"] <- "Western Europe"
fdaddy_issues$other.country[fdaddy_issues$other.country == "The netherlands"] <- "Western Europe"
fdaddy_issues$other.country[fdaddy_issues$other.country == "The Netherlands"] <- "Western Europe"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Uk"] <- "Western Europe"
fdaddy_issues$other.country[fdaddy_issues$other.country == "UK"] <- "Western Europe"
fdaddy_issues$other.country[fdaddy_issues$other.country == "United kingdom"] <- "Western Europe"
fdaddy_issues$other.country[fdaddy_issues$other.country == "United Kingdom"] <- "Western Europe"

fdaddy_issues$other.country[fdaddy_issues$other.country == "Bangladesh"] <- "Middle East"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Iran"] <- "Middle East"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Jordan"] <- "Middle East"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Kuwait"] <- "Middle East"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Bangladesh"] <- "Middle East"

fdaddy_issues$other.country[fdaddy_issues$other.country == "Albania"] <- "Eastern Europe"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Bosnia and Herzegovina"] <- "Eastern Europe"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Bulgaria"] <- "Eastern Europe"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Estonia"] <- "Eastern Europe"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Finland"] <- "Eastern Europe"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Hungary"] <- "Eastern Europe"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Lithuania"] <- "Eastern Europe"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Romania"] <- "Eastern Europe"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Slovenia"] <- "Eastern Europe"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Turkey"] <- "Eastern Europe"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Albania"] <- "Eastern Europe"

fdaddy_issues$other.country[fdaddy_issues$other.country == "canada"] <- "Canada"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Canada"] <- "Canada"

fdaddy_issues$other.country[fdaddy_issues$other.country == "China"] <- "Asia"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Indonesia/South Korea/Sweden"] <- "Asia"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Myanmar"] <- "Asia"
fdaddy_issues$other.country[fdaddy_issues$other.country == "China"] <- "Asia"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Philippines"] <- "Asia"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Singapore"] <- "Asia"

fdaddy_issues$other.country[fdaddy_issues$other.country == "Dominican republic"] <- "Carribean"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Dominican Republic"] <- "Carribean"

fdaddy_issues$other.country[fdaddy_issues$other.country == "Eswatini"] <- "Africa"
fdaddy_issues$other.country[fdaddy_issues$other.country == "Kenya"] <- "Africa"
fdaddy_issues$other.country[fdaddy_issues$other.country == "South Africa"] <- "Africa"
fdaddy_issues$other.country[fdaddy_issues$other.country == "South African"] <- "Africa"

fdaddy_issues$other.country[fdaddy_issues$other.country == "Mexico"] <- "Mexico"

fdaddy_issues$country[fdaddy_issues$country == "India"] <- "Asia"

  #Relationship
table(fdaddy_issues$rel)
table(fdaddy_issues$other.rel)

#Remove any cases where people did not identify as women
#Removed NA cases - rows 1, 2, & 445
which(is.na(fdaddy_issues$woman))
fdaddy_issues <- fdaddy_issues[-c(1, 2, 445),]
View(fdaddy_issues)
which(is.na(fdaddy_issues$woman))
 
#Remove any cases where women said they are not currently in a relationship
fdaddy_issues <- fdaddy_issues[-c(5),]
View(fdaddy_issues)
fdaddy_issues <- fdaddy_issues[-c(233),]
View(fdaddy_issues)
fdaddy_issues <- fdaddy_issues[-c(424),]
View(fdaddy_issues)
fdaddy_issues <- fdaddy_issues[-c(557),]
View(fdaddy_issues)

#Create vectors for each variable 
fdaddy_issues$RCSE <- fdaddy_issues$RCSE1 + fdaddy_issues$RCSE2 + fdaddy_issues$RCSE3 + fdaddy_issues$RCSE4 + fdaddy_issues$RCSE5 + fdaddy_issues$RCSE6 + fdaddy_issues$RCSE7 + fdaddy_issues$RCSE8 + fdaddy_issues$RCSE9 + fdaddy_issues$RCSE10
fdaddy_issues$FIS_E <- fdaddy_issues$FIS_e1 + fdaddy_issues$FIS_e2 + fdaddy_issues$FIS_e3 + fdaddy_issues$FIS_e4 + fdaddy_issues$FIS_e5 + fdaddy_issues$FIS_e6 + fdaddy_issues$FIS_e8 
fdaddy_issues$FIS_I <- fdaddy_issues$FIS_i1 + fdaddy_issues$FIS_i2 + fdaddy_issues$FIS_i3 + fdaddy_issues$FIS_i5 + fdaddy_issues$FIS_i6 + fdaddy_issues$FIS_i7 
fdaddy_issues$FIS_M <- fdaddy_issues$FIS_m2 + fdaddy_issues$FIS_m3 + fdaddy_issues$FIS_m4
fdaddy_issues$MIS_E <- fdaddy_issues$MIS_e1 + fdaddy_issues$MIS_e2 + fdaddy_issues$MIS_e3 + fdaddy_issues$MIS_e4 + fdaddy_issues$MIS_e5 + fdaddy_issues$MIS_e6 + fdaddy_issues$MIS_e8
fdaddy_issues$MIS_I <- fdaddy_issues$MIS_i1 + fdaddy_issues$MIS_i2 + fdaddy_issues$MIS_i3 + fdaddy_issues$MIS_i5 + fdaddy_issues$MIS_i6 + fdaddy_issues$MIS_i7
fdaddy_issues$MIS_M <- fdaddy_issues$MIS_m1 + fdaddy_issues$MIS_m2 + fdaddy_issues$MIS_m3
fdaddy_issues$AMORE_E <- fdaddy_issues$AMORE_e1 + fdaddy_issues$AMORE_e2 + fdaddy_issues$AMORE_e3 + fdaddy_issues$AMORE_e4 + fdaddy_issues$AMORE_e5 + fdaddy_issues$AMORE_e6 + fdaddy_issues$AMORE_e7 
fdaddy_issues$ AMORE_P <- fdaddy_issues$AMORE_p1 + fdaddy_issues$AMORE_p2 + fdaddy_issues$AMORE_p3 + fdaddy_issues$AMORE_p4 + fdaddy_issues$AMORE_p5 + fdaddy_issues$AMORE_p6 + fdaddy_issues$AMORE_p7 + fdaddy_issues$AMORE_p8 + fdaddy_issues$AMORE_p9 + fdaddy_issues$AMORE_p10

fdaddy_issues$FIS.tot <- fdaddy_issues$FIS_E + fdaddy_issues$FIS_I + fdaddy_issues$FIS_M
fdaddy_issues$MIS.tot <- fdaddy_issues$MIS_E + fdaddy_issues$MIS_I + fdaddy_issues$MIS_M

View(fdaddy_issues)
is.list(FIS)
is.list(RCSE)
is.list(AMORE_E)
is.list(FIS.tot)




# Created the first model for the multiple regression containing the DV of RCSE and predictor variables FIS & MIS. 
# Checked RCSE model for influential outliers using Cook's distance and the slice function to view the flagged values.
# Decided to keep all flagged values, as the data was realistic and want model to be reflective of the real world.
# Checked RCSE model to see if the assumptions of linear regression held true.
# Decided that all assumptions of  linear regression held true.

RCSE_model = lm(RCSE ~ FIS, data = fdaddy_issues)

#Cook's distance
RCSE_model %>%
  plot(which = 4)

#Normality 
#Not violated
RCSE_model %>%
  plot(which = 2)

residuals_RCSE = enframe(residuals(RCSE_model))

residuals_RCSE %>%
  ggplot() +
  aes(x = value) +
  geom_histogram()

describe(residuals(RCSE_model))

#Linearity
#Not violating assumptions 
RCSE_model %>%
  residualPlots()

#Homoscedasticity
#Not violated
RCSE_model %>%
  plot(which = 3)

RCSE_model %>%
  ncvTest()
RCSE_model %>%
  bptest()

summary(RCSE_model)
sum(abs(fdaddy_issues$RCSE - predict(RCSE_model))^2)

FIS_df <- as.tibble(RCSE)

predictions = predict(RCSE_model, newdata = FIS_df)

fdaddy_issues %>%
  ggplot() +
  aes(x = FIS, y = RCSE) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

#AMORE E model
AMORE.e_model = lm(AMORE_E ~ FIS, data = fdaddy_issues)

#Cook's distance
AMORE.e_model %>%
  plot(which = 4)

#Normality 
#Not violated
AMORE.e_model %>%
  plot(which = 2)

residuals_AMORE.e = enframe(residuals(RCSE_model))

residuals_AMORE.e %>%
  ggplot() +
  aes(x = value) +
  geom_histogram()

describe(residuals(AMORE.e_model))

#Linearity
#Not violating assumptions 
AMORE.e_model %>%
  residualPlots()

#Homoscedasticity
#Not violated
AMORE.e_model %>%
  plot(which = 3)

AMORE.e_model %>%
  ncvTest()
AMORE.e_model %>%
  bptest()

fdaddy_issues %>%
  ggplot() +
  aes(x = FIS, y = AMORE_E) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

#AMORE P model
AMORE.p_model = lm(AMORE_P ~ FIS, data = fdaddy_issues)

#Cook's distance
AMORE.p_model %>%
  plot(which = 4)
fdaddy_issues %>%
  slice(c(52, 437, 509))

#Normality 
#Not violated
AMORE.p_model %>%
  plot(which = 2)

residuals_AMORE.p = enframe(residuals(RCSE_model))

residuals_AMORE.p %>%
  ggplot() +
  aes(x = value) +
  geom_histogram()

describe(residuals(AMORE.p_model))

boxplot(residuals(AMORE.p_model))

shapiro.test(residuals(AMORE.p_model))

#Linearity
#Not violating assumptions 
AMORE.e_model %>%
  residualPlots()

#Homoscedasticity
#Not violated
AMORE.e_model %>%
  plot(which = 3)

AMORE.e_model %>%
  ncvTest()
AMORE.e_model %>%
  bptest()

fdaddy_issues %>%
  ggplot() +
  aes(x = FIS, y = AMORE_P) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

####### multivariate regression
RCSE_mult.model <- lm(RCSE ~ FIS, data = fdaddy_issues)
export_summs(RCSE_mult.model)

RCSE_mult.model <- lm(FIS ~ RCSE + AMORE_E + AMORE_P, data = fdaddy_issues)
export_summs(RCSE_mult.model)

###linear hypothesis test?
linearHypothesis(RCSE_mult.model, c())

####linear regression 
#Check for errors

fdaddy_issues %>%
  ggplot() +
  aes(x = RCSE) +
  geom_histogram()

fdaddy_issues %>%
  ggplot() +
  aes(x = AMORE_E) +
  geom_histogram()

fdaddy_issues %>%
  ggplot() +
  aes(x = AMORE_P) +
  geom_histogram()

#Visualize relationships
fdaddy_issues %>%
  ggplot() +
  aes(y = RCSE, x = FIS.tot) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

fdaddy_issues %>%
  ggplot() +
  aes(y = AMORE_E, x = FIS.tot) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

fdaddy_issues %>%
  ggplot() +
  aes(y = AMORE_P, x = FIS.tot) +
  geom_point() +
  geom_smooth(method = "lm", se = F)  

RCSE.mod <- lm(RCSE ~ FIS.tot, data = fdaddy_issues)
RCSE.mod

#Create vector of RCSE values 
#Create df of these values
RCSE.vec <- c(26, 49, 20, 35, 19)
RCSE.df = as.tibble(RCSE.vec)

#Create predictions
predictions = predict(RCSE.mod, newdata = RCSE.df)

is.list(fdaddy_issues$RCSE)
is.list(fdaddy_issues$AMORE_E)
is.list(fdaddy_issues$AMORE_P)
is.list(fdaddy_issues$FIS.tot)
is.list(fdaddy_issues$MIS.tot)

variables.df <- data.frame(fdaddy_issues$RCSE, fdaddy_issues$AMORE_E, fdaddy_issues$AMORE_P, fdaddy_issues$FIS.tot, fdaddy_issues$MIS.tot)
variables.df

variables.cor = cor(variables.df)
variables.cor

variables.rcorr = rcorr(as.matrix(variables.df))
variables.rcorr

corrplot(variables.cor)
