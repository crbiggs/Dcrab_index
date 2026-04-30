library(tidyverse)


## QA/QC of logbooks - filtering down to used 


## Creating a column for Crab-year 
Log2.0 <- Log2.0 |> mutate(Cyear = year(`Landing Date`), Month = month(`Landing Date`))

## using set date for year column when Landing Date is absent
Log2.0 <- Log2.0 |> mutate(Cyear = ifelse(is.na(Cyear), year(`Set Date`), Cyear))

#Checking for missing Cyear 
sum(is.na(Log2.0$Cyear))


Log2.0 <- Log2.0 |> mutate(Month = ifelse(is.na(Month), month(`Set Date`), Month))

## Adjusting Cyear to reflect crab year; Nov-Sep -classified as the ending year-(Nov. 2011 is Cyear 2012)
Log2.0 <- Log2.0 |> mutate(Cyear = ifelse(Month %in% c(11,12), Cyear +1, Cyear))



Log2.0 <- Log2.0 |> mutate(CperPot = `Crab Retained (count)`/`Pots Fished`)

### Logbook data from 2010-2023 was used for this analysis. Logbook data was checked for missing data and records were removed when either location (latitude or longitude) of the string were missing, or the number of pots or number of crab retained were missing or the crab retained was greater than 100 per pot. 
#Removing a few records that were way off the shelf
LogT <- Log3.13Ck |> filter(!(Lonbeg< -124.9 & Latbeg < 46.5))

LogT <- LogT |> filter(CperPot <=100)



