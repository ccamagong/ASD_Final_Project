# Analysis_ASD_Final
# Chi-Squared for Children, Adolescent, & Adult data

# Load Libraries
library("dplyr")
library("gmodels")
library(tidyverse)
library(DAAG)
install.packages("psych")
library("psych")

# Import Data


#Data Wrangling

## Check Data Type
str(Autism_Child)
str(Autism_Adolescent)
str(Autism_Adult)

## Dummy Code Ethnicity
Autism_Child1 <- dummy.code(Autism_Child$Ethnicity)
Autism_Child <- data.frame(Autism_Child, Autism_Child1)

Autism_Adolescent1 <- dummy.code(Autism_Adolescent$Ethnicity)
Autism_Adolescent <- data.frame(Autism_Adolescent, Autism_Adolescent1)

Autism_Adult1 <- dummy.code(Autism_Adult$Ethnicity)
Autism_Adult <- data.frame(Autism_Adult, Autism_Adult1)

Autism_ALL1 <- dummy.code(Autism_ALL$Ethnicity)
Autism_ALL <- data.frame(Autism_ALL, Autism_ALL1)

## Recode gender
Autism_Child$SexR <- NA
Autism_Adolescent$SexR <- NA
Autism_Adult$SexR <- NA
Autism_ALL$SexR <- NA

Autism_Child$SexR[Autism_Child$Sex=='m'] <- 0
Autism_Child$SexR[Autism_Child$Sex=='f'] <- 1

Autism_Adolescent$SexR[Autism_Adolescent$Sex=='m'] <- 0
Autism_Adolescent$SexR[Autism_Adolescent$Sex=='f'] <- 1

Autism_Adult$JaundiceR[Autism_Adult$Sex=='m'] <- 0
Autism_Adult$SexR[Autism_Adult$Sex=='f'] <- 1

Autism_ALL$JaundiceR[Autism_ALL$Sex=='m'] <- 0
Autism_ALL$SexR[Autism_ALL$Sex=='f'] <- 1

## Recode Jaundice
Autism_Child$JaundiceR <- NA
Autism_Adolescent$JaundiceR <- NA
Autism_Adult$JaundiceR <- NA
Autism_ALL$JaundiceR <- NA

Autism_Child$JaundiceR[Autism_Child$Jaundice=='no'] <- 0
Autism_Child$JaundiceR[Autism_Child$Jaundice=='yes'] <- 1

Autism_Adolescent$JaundiceR[Autism_Adolescent$Jaundice=='no'] <- 0
Autism_Adolescent$JaundiceR[Autism_Adolescent$Jaundice=='yes'] <- 1

Autism_Adult$JaundiceR[Autism_Adult$Jaundice=='no'] <- 0
Autism_Adult$JaundiceR[Autism_Adult$Jaundice=='yes'] <- 1

Autism_ALL$JaundiceR[Autism_ALL$Jaundice=='no'] <- 0
Autism_ALL$JaundiceR[Autism_ALL$Jaundice=='yes'] <- 1

## Recode Family_ASD
Autism_Child$Family_ASDR <- NA
Autism_Adolescent$Family_ASDR <- NA
Autism_Adult$Family_ASDR <- NA
Autism_ALL$Family_ASDR <- NA

Autism_Child$Family_ASDR[Autism_Child$Family_ASD=='no'] <- 0
Autism_Child$Family_ASDR[Autism_Child$Family_ASD=='yes'] <- 1

Autism_Adolescent$Family_ASDR[Autism_Adolescent$Family_ASD=='no'] <- 0
Autism_Adolescent$Family_ASDR[Autism_Adolescent$Family_ASD=='yes'] <- 1

Autism_Adult$Family_ASDR[Autism_Adult$Family_ASD=='no'] <- 0
Autism_Adult$Family_ASDR[Autism_Adult$Family_ASD=='yes'] <- 1

Autism_ALL$Family_ASDR[Autism_ALL$Family_ASD=='no'] <- 0
Autism_ALL$Family_ASDR[Autism_ALL$Family_ASD=='yes'] <- 1

## Drop orginal columns that were recoded
Autism_Child <- Autism_Child[-c(12:15)]
Autism_Adolescent <- Autism_Adolescent[-c(12:15)]
Autism_Adult <- Autism_Adult[-c(12:15)]

## Create df's to filter 'yes' columns
asd_child_yes <- filter(Autism_Child, Class == "YES")
asd_adolescent_yes <- filter(Autism_Adolescent, Class == "YES")
asd_adult_yes <- filter(Autism_Adult, Class == "YES")
asd_all_yes <- filter(Autism_Adult, Class == "YES")

# Chi-Squared for Children, Adolescent, & Adult data
## Stack data one on top of the other - requires the same columns in each
### Order names and order of the columns
names(asd_child_yes)
names(asd_adolescent_yes)
names(asd_adult_yes)

#### Column names need to be the same
child_raceSubsetRenames <- asd_child_yes %>% rename( others = others.)
adolescent_raceSubsetRenames <- asd_adolescent_yes %>% rename( others = others.)
adult_raceSubsetRenames <- asd_adult_yes %>% rename( others = others.)

### Reorder and drop columns to look like the child. 'Class' & 'aboriginal' dropped
child_reordered <- child_raceSubsetRenames[,c(1:11, 13, 15, 14, 17, 16, 19, 18, 20, 22:24)]
adolescent_reordered <- adolescent_raceSubsetRenames[, c(1:11, 13, 15, 14, 21, 16, 18, 17, 19, 22:24)]
adult_reordered <- adult_raceSubsetRenames[, c(1:11, 13, 15, 14, 18, 16, 20, 17, 19, 22:24)]

### Check to make sure they are now all the same order and number of columns:
names(child_reordered)
names(adolescent_reordered)
names(adult_reordered)

### Add a column to each data frame that adds the time frame on there

childTime <- mutate(child_reordered, Time = "Child")
adolescentTime <- mutate(adolescent_reordered, Time = "Adolescent")
adultTime <- mutate(adult_reordered, Time = "Adult")

## Combine data one on top of each other!

all <- rbind(childTime, adolescentTime, adultTime)

#Independent Chi-Square

CrossTable(all$Time, all$A1, chisq=TRUE, expected = TRUE, sresid=TRUE, format="SPSS")

