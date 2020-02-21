#################################################################
## Replication Code for Fraga 2015, "Candidates or Districts?" ##
#################################################################

## Clear existing memory

rm(list=ls())

## Source in 'gee' Package

library(gee) # Tested with gee_4.13-18, R version 3.1.1

###################
## MAIN ANALYSIS ##
###################

## Load in Group-Specific Datasets

load('Fraga2015_MainData.RData')

## General Turnout ##
# General Election Covariates

# create a response variable that will sum the values of the jurisdiction covariates: citizen age voting population, co-ethnic candidate, the product of both,
# statewide coethnic (indicator for jurisdictionsin states where a co-ethnic ran for U.S. Senate or governor), south (indicator for whether the 
# jurisdiction is in the south), year 2006, year 2008, and record reliability 

covariates1a_Any <- "CVAP + Candidate + I(CVAP*Candidate) + CoEthSW + South + Yr06 + Yr08 + G_CatQuality"

# create a response variable that will include the variables in covariates1a_Any as well as education, income, presence of open seat (incumbent 
# did not seek reelection), Cook Political Report Partisan Voting Index, competitiveness, senate election (indicator for jursidictions 
# where a Senate election was held in the same state that year), Gov. election (indicator for jurisdiction where
# gubernational election was held in the same state that year)

covariates1b_Any <- "CVAP + Candidate + I(CVAP*Candidate) + CoEthSW + South + Yr06 + Yr08 + Age + Education + Income + Open + CookPVI + CookPVICompet + 
SenElect + GovElect + G_CatQuality"

# create generalized estimation equation that uses voter turnout data for white people during general elections and explains white voter 
# turnout as a function of the different covariates in covariates1a_Any

geeW1a <- gee(as.formula(paste("RawTurn_White_G ~ ",covariates1a_Any,sep="")), data=WhitedataG, id=Dist_Cluster, corstr="unstructured")

# create generalized estimation equation that uses voter turnout data for white people during general elections and explains white voter 
# turnout as a function of the different covariates in covariates1b_Any

geeW1b <- gee(as.formula(paste("RawTurn_White_G ~ ",covariates1b_Any,sep="")), data=WhitedataG, id=Dist_Cluster, corstr="unstructured")

# create generalized estimation equation that uses voter turnout data for black people during general elections and explains black voter turnout 
# as a function of the different covariates in covariates1a_Any

geeB1a <- gee(as.formula(paste("RawTurn_Black_G ~ ",covariates1a_Any,sep="")), data=BlackdataG, id=Dist_Cluster, corstr="unstructured")

# create generalized estimation equation that uses voter turnout data for black people during general elections and explains black voter turnout 
# as a function of the different covariates in covariates1b_Any

geeB1b <- gee(as.formula(paste("RawTurn_Black_G ~ ",covariates1b_Any,sep="")), data=BlackdataG, id=Dist_Cluster, corstr="unstructured")

# create generalized estimation equation that uses voter turnout data for Latino people during general elections and explains Latino voter turnout 
# as a function of the different covariates in covariates1a_Any

geeL1a <- gee(as.formula(paste("RawTurn_Latino_G ~ ",covariates1a_Any,sep="")), data=LatinodataG, id=Dist_Cluster, corstr="unstructured")

# create generalized estimation equation that uses voter turnout data for Latino people during general elections and explains Latino voter turnout 
# as a function of the different covariates in covariates1b_Any

geeL1b <- gee(as.formula(paste("RawTurn_Latino_G ~ ",covariates1b_Any,sep="")), data=LatinodataG, id=Dist_Cluster, corstr="unstructured")

# create generalized estimation equation that uses voter turnout data for Asian Americans during general elections and explains Asian American
# voter turnout as a function of the different covariates in covariates1a_Any

geeA1a <- gee(as.formula(paste("RawTurn_Asian_G ~ ",covariates1a_Any,sep="")), data=AsiandataG, id=Dist_Cluster, corstr="unstructured")

# create generalized estimation equation that uses voter turnout data for Asian Americans during general elections and explains Asian American
# voter turnout as a function of the different covariates in covariates1b_Any

geeA1b <- gee(as.formula(paste("RawTurn_Asian_G ~ ",covariates1b_Any,sep="")), data=AsiandataG, id=Dist_Cluster, corstr="unstructured")


## Primary Turnout ##
# Primary Election Covariates

# create a response variable that will sum the values of the jurisdiction covariates: citizen age voting population, co-ethnic candidate, the product of both,
# statewide coethnic (indicator for jurisdictionsin states where a co-ethnic ran for U.S. Senate or governor), south (indicator for whether the 
# jurisdiction is in the south), year 2006, year 2008, presidential primary (indicator for jurisdictions where the congressional primary was held at 
# the same time as at least one party's presidential primary), and record reliability 

covariates2a_Any <- "CVAP + Candidate + I(CVAP*Candidate) + CoEthSW + South + Yr06 + Yr08 + PresPrim + P_CatQuality"

# create a response variable that will include the variables in covariates1a_Any as well as education, income, presence of open seat (incumbent 
# did not seek reelection), Cook Political Report Partisan Voting Index, competitiveness, senate election (indicator for jursidictions 
# where a Senate election was held in the same state that year), Gov. election (indicator for jurisdiction where
# gubernational election was held in the same state that year), primary type (openness of primary eligibility rules), runoff state (indicator for 
# jurisdictions where runoff elections could be held)

covariates2b_Any <- "CVAP + Candidate + I(CVAP*Candidate) + CoEthSW + South + Yr06 + Yr08 + PresPrim + P_CatQuality + Age + Education + Income + Open + CookPVI + CookPVICompet + SenElect + GovElect + PrimType + Runoff"

# create generalized estimation equation that uses voter turnout data for white people during primary elections and explains white voter 
# turnout as a function of the different covariates in covariates1a_Any

geeW2a <- gee(as.formula(paste("RawTurn_White_P ~ ",covariates2a_Any,sep="")), data=WhitedataP, id=Dist_Cluster, corstr="unstructured")

# create generalized estimation equation that uses voter turnout data for white people during primary elections and explains white voter 
# turnout as a function of the different covariates in covariates2b_Any

geeW2b <- gee(as.formula(paste("RawTurn_White_P ~ ",covariates2b_Any,sep="")), data=WhitedataP, id=Dist_Cluster, corstr="unstructured")

# create generalized estimation equation that uses voter turnout data for black people during primary elections and explains black voter turnout 
# as a function of the different covariates in covariates2a_Any

geeB2a <- gee(as.formula(paste("RawTurn_Black_P ~ ",covariates2a_Any,sep="")), data=BlackdataP, id=Dist_Cluster, corstr="unstructured")

# create generalized estimation equation that uses voter turnout data for black people during primary elections and explains black voter turnout 
# as a function of the different covariates in covariates2b_Any

geeB2b <- gee(as.formula(paste("RawTurn_Black_P ~ ",covariates2b_Any,sep="")), data=BlackdataP, id=Dist_Cluster, corstr="unstructured")

# create generalized estimation equation that uses voter turnout data for Latino people during primary elections and explains Latino voter turnout 
# as a function of the different covariates in covariates2a_Any

geeL2a <- gee(as.formula(paste("RawTurn_Latino_P ~ ",covariates2a_Any,sep="")), data=LatinodataP, id=Dist_Cluster, corstr="unstructured")

# create generalized estimation equation that uses voter turnout data for Latino people during primary elections and explains Latino voter turnout 
# as a function of the different covariates in covariates2b_Any

geeL2b <- gee(as.formula(paste("RawTurn_Latino_P ~ ",covariates2b_Any,sep="")), data=LatinodataP, id=Dist_Cluster, corstr="unstructured")

# create generalized estimation equation that uses voter turnout data for Asian Americans during primary elections and explains Asian American voter turnout 
# as a function of the different covariates in covariates2a_Any

geeA2a <- gee(as.formula(paste("RawTurn_Asian_P ~ ",covariates2a_Any,sep="")), data=AsiandataP, id=Dist_Cluster, corstr="unstructured")

# create generalized estimation equation that uses voter turnout data for Asian Americans during primary elections and explains Asian American voter turnout 
# as a function of the different covariates in covariates2b_Any

geeA2b <- gee(as.formula(paste("RawTurn_Asian_P ~ ",covariates2b_Any,sep="")), data=AsiandataP, id=Dist_Cluster, corstr="unstructured")



## Primary by Party Turnout, DEMOCRATS ##
# Primary Election Covariates, Democrats

# create a response variable that will sum the values of the jurisdiction covariates: citizen age voting population, co-ethnic candidate, the product of both,
# statewide coethnic (indicator for jurisdictions in states where a co-ethnic ran for U.S. Senate or governor), south (indicator for whether the 
# jurisdiction is in the south), year 2006, year 2008, and record reliability 

covariates3a_Any <- "CVAP + DCandidate + I(CVAP*DCandidate) + CoEthSW + South + Yr06 + Yr08 + P_CatQuality"

# create a response variable that will include the variables in covariates1a_Any as well as education, income, presence of open seat (incumbent 
# did not seek reelection), Cook Political Report Partisan Voting Index, competitiveness, senate election (indicator for jursidictions 
# where a Senate election was held in the same state that year), gov. election (indicator for jurisdiction where gubernational 
# election was held in the same state that year), democratic primary type (openness of primary eligibility rules), runoff state (indicator for 
# jurisdictions where runoff elections could be held)

covariates3b_Any <- "CVAP + DCandidate + I(CVAP*DCandidate) + CoEthSW + South + Yr06 + Yr08 + P_CatQuality + Age + Education + Income + Open + CookPVI + 
CookPVICompet + SenElect + GovElect + D_PrimType + Runoff"

# create generalized estimation equation that uses voter turnout data for white people during democratic primary elections and explains white voter 
# turnout as a function of the different covariates in covariates3a_Any

geeW3a <- gee(as.formula(paste("RawTurn_White_DP ~ ",covariates3a_Any,sep="")), data=WhitedataDP, id=Dist_Cluster, corstr="unstructured")

# create generalized estimation equation that uses voter turnout data for white people during democratic primary elections and explains white voter 
# turnout as a function of the different covariates in covariates3b_Any

geeW3b <- gee(as.formula(paste("RawTurn_White_DP ~ ",covariates3b_Any,sep="")), data=WhitedataDP, id=Dist_Cluster, corstr="unstructured")

# create generalized estimation equation that uses voter turnout data for black people during democratic primary elections and explains 
# black voter turnout as a function of the different covariates in covariates3a_Any

geeB3a <- gee(as.formula(paste("RawTurn_Black_DP ~ ",covariates3a_Any,sep="")), data=BlackdataDP, id=Dist_Cluster, corstr="unstructured")

# create generalized estimation equation that uses voter turnout data for black people during democratic primary elections and explains 
# black voter turnout as a function of the different covariates in covariates3b_Any

geeB3b <- gee(as.formula(paste("RawTurn_Black_DP ~ ",covariates3b_Any,sep="")), data=BlackdataDP, id=Dist_Cluster, corstr="unstructured")

# create generalized estimation equation that uses voter turnout data for Latino people during democratic primary elections and explains 
# Latino voter turnout as a function of the different covariates in covariates3a_Any

geeL3a <- gee(as.formula(paste("RawTurn_Latino_DP ~ ",covariates3a_Any,sep="")), data=LatinodataDP, id=Dist_Cluster, corstr="unstructured")

# create generalized estimation equation that uses voter turnout data for Latino people during democratic primary elections and explains 
# Latino voter turnout as a function of the different covariates in covariates3b_Any

geeL3b <- gee(as.formula(paste("RawTurn_Latino_DP ~ ",covariates3b_Any,sep="")), data=LatinodataDP, id=Dist_Cluster, corstr="unstructured")

# create generalized estimation equation that uses voter turnout data for Asian Americans during democratic primary elections and explains 
# Asian American voter turnout as a function of the different covariates
# For Asians do not use Dem Statewide Candidate covariates, as there are none in the data

geeA3a <- gee(RawTurn_Asian_DP ~ CVAP + DCandidate + I(CVAP*DCandidate) + South + Yr06 + Yr08 + P_CatQuality, data=AsiandataDP, id=Dist_Cluster, corstr="unstructured")

# create generalized estimation equation that uses voter turnout data for Asian Americans during democratic primary elections and explains 
# Asian American voter turnout as a function of the different covariates

geeA3b <- gee(RawTurn_Asian_DP ~ CVAP + DCandidate + I(CVAP*DCandidate) + South + Yr06 + Yr08 + P_CatQuality + Age + Education + Income + Open + CookPVI + 
                CookPVICompet + SenElect + GovElect + D_PrimType + Runoff, data=AsiandataDP, id=Dist_Cluster, corstr="unstructured")


## Primary by Party Turnout, REPUBLICANS ##
# Primary Election Covariates, Republicans

# create a response variable that will sum the values of the jurisdiction covariates: citizen age voting population, co-ethnic candidate, the product of both,
# statewide coethnic (indicator for jurisdictions in states where a co-ethnic ran for U.S. Senate or governor), south (indicator for whether the 
# jurisdiction is in the south), year 2006, year 2008, and record reliability 

covariates4a_Any <- "CVAP + RCandidate + I(CVAP*RCandidate) + CoEthSW + South + Yr06 + Yr08 + P_CatQuality"

# create a response variable that will include the variables in covariates1a_Any as well as education, income, presence of open seat (incumbent 
# did not seek reelection), Cook Political Report Partisan Voting Index, competitiveness, senate election (indicator for jursidictions 
# where a Senate election was held in the same state that year), gov. election (indicator for jurisdiction where gubernational 
# election was held in the same state that year), republican primary type (openness of primary eligibility rules), runoff state (indicator for 
# jurisdictions where runoff elections could be held)

covariates4b_Any <- "CVAP + RCandidate + I(CVAP*RCandidate) + CoEthSW + South + Yr06 + Yr08 + P_CatQuality + Age + Education + Income + 
Open + CookPVI + CookPVICompet + SenElect + GovElect + R_PrimType + Runoff"

# create generalized estimation equation that uses voter turnout data for white people during republican primary elections and explains white voter 
# turnout as a function of the different covariates in covariates4a_Any

geeW4a <- gee(as.formula(paste("RawTurn_White_RP ~ ",covariates4a_Any,sep="")), data=WhitedataRP, id=Dist_Cluster, corstr="unstructured")

# create generalized estimation equation that uses voter turnout data for white people during republican primary elections and explains white voter 
# turnout as a function of the different covariates in covariates4b_Any

geeW4b <- gee(as.formula(paste("RawTurn_White_RP ~ ",covariates4b_Any,sep="")), data=WhitedataRP, id=Dist_Cluster, corstr="unstructured")

# create generalized estimation equation that uses voter turnout data for black people during republican primary elections and explains 
# black voter turnout as a function of the different covariates in covariates4a_Any

geeB4a <- gee(as.formula(paste("RawTurn_Black_RP ~ ",covariates4a_Any,sep="")), data=BlackdataRP, id=Dist_Cluster, corstr="unstructured")

# create generalized estimation equation that uses voter turnout data for black people during republican primary elections and explains 
# black voter turnout as a function of the different covariates in covariates4b_Any

geeB4b <- gee(as.formula(paste("RawTurn_Black_RP ~ ",covariates4b_Any,sep="")), data=BlackdataRP, id=Dist_Cluster, corstr="unstructured")

# create generalized estimation equation that uses voter turnout data for Latino people during republican primary elections and explains 
# Latino voter turnout as a function of the different covariates in covariates4a_Any

geeL4a <- gee(as.formula(paste("RawTurn_Latino_RP ~ ",covariates4a_Any,sep="")), data=LatinodataRP, id=Dist_Cluster, corstr="unstructured")

# create generalized estimation equation that uses voter turnout data for Latino people during republican primary elections and explains 
# Latino voter turnout as a function of the different covariates in covariates4b_Any

geeL4b <- gee(as.formula(paste("RawTurn_Latino_RP ~ ",covariates4b_Any,sep="")), data=LatinodataRP, id=Dist_Cluster, corstr="unstructured")

# create generalized estimation equation that uses voter turnout data for Asian Americans during republican primary elections and explains 
# Asian American voter turnout as a function of the different covariates in covariates4a_Any

geeA4a <- gee(as.formula(paste("RawTurn_Asian_RP ~ ",covariates4a_Any,sep="")), data=AsiandataRP, id=Dist_Cluster, corstr="unstructured")

# create generalized estimation equation that uses voter turnout data for Asian Americans during republican primary elections and explains 
# Asian American voter turnout as a function of the different covariates in covariates4b_Any

geeA4b <- gee(as.formula(paste("RawTurn_Asian_RP ~ ",covariates4b_Any,sep="")), data=AsiandataRP, id=Dist_Cluster, corstr="unstructured")

#############################################
## Misclassification Sensitivity Analaysis ##
#############################################

## Clear existing memory

rm(list=ls())

## Load in Group-Specific Datasets, NOTE: Each object is a list of 100 dataframes with dependent variable randomized to reflect possibility of misclassification of individual race.

load('Fraga2015_RobustData.RData')

## Run code from previous section of this document on each dataframe to replicate regression results used in main text. To select a single randomized dataframe for regression analysis, type 'WhitedataG_RobustList[[1]]'
### NAMING CONVENTION: WhitedataG_RobustList corresponds to WhitedataG in code from previous section