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
covariates1a_Any <- "CVAP + Candidate + I(CVAP*Candidate) + CoEthSW + South + Yr06 + Yr08 + G_CatQuality"
covariates1b_Any <- "CVAP + Candidate + I(CVAP*Candidate) + CoEthSW + South + Yr06 + Yr08 + Age + Education + Income + Open + CookPVI + CookPVICompet + SenElect + GovElect + G_CatQuality"

geeW1a <- gee(as.formula(paste("RawTurn_White_G ~ ",covariates1a_Any,sep="")), data=WhitedataG, id=Dist_Cluster, corstr="unstructured")
geeW1b <- gee(as.formula(paste("RawTurn_White_G ~ ",covariates1b_Any,sep="")), data=WhitedataG, id=Dist_Cluster, corstr="unstructured")

geeB1a <- gee(as.formula(paste("RawTurn_Black_G ~ ",covariates1a_Any,sep="")), data=BlackdataG, id=Dist_Cluster, corstr="unstructured")
geeB1b <- gee(as.formula(paste("RawTurn_Black_G ~ ",covariates1b_Any,sep="")), data=BlackdataG, id=Dist_Cluster, corstr="unstructured")

geeL1a <- gee(as.formula(paste("RawTurn_Latino_G ~ ",covariates1a_Any,sep="")), data=LatinodataG, id=Dist_Cluster, corstr="unstructured")
geeL1b <- gee(as.formula(paste("RawTurn_Latino_G ~ ",covariates1b_Any,sep="")), data=LatinodataG, id=Dist_Cluster, corstr="unstructured")

geeA1a <- gee(as.formula(paste("RawTurn_Asian_G ~ ",covariates1a_Any,sep="")), data=AsiandataG, id=Dist_Cluster, corstr="unstructured")
geeA1b <- gee(as.formula(paste("RawTurn_Asian_G ~ ",covariates1b_Any,sep="")), data=AsiandataG, id=Dist_Cluster, corstr="unstructured")

## Primary Turnout ##
# Primary Election Covariates
covariates2a_Any <- "CVAP + Candidate + I(CVAP*Candidate) + CoEthSW + South + Yr06 + Yr08 + PresPrim + P_CatQuality"
covariates2b_Any <- "CVAP + Candidate + I(CVAP*Candidate) + CoEthSW + South + Yr06 + Yr08 + PresPrim + P_CatQuality + Age + Education + Income + Open + CookPVI + CookPVICompet + SenElect + GovElect + PrimType + Runoff"

geeW2a <- gee(as.formula(paste("RawTurn_White_P ~ ",covariates2a_Any,sep="")), data=WhitedataP, id=Dist_Cluster, corstr="unstructured")
geeW2b <- gee(as.formula(paste("RawTurn_White_P ~ ",covariates2b_Any,sep="")), data=WhitedataP, id=Dist_Cluster, corstr="unstructured")

geeB2a <- gee(as.formula(paste("RawTurn_Black_P ~ ",covariates2a_Any,sep="")), data=BlackdataP, id=Dist_Cluster, corstr="unstructured")
geeB2b <- gee(as.formula(paste("RawTurn_Black_P ~ ",covariates2b_Any,sep="")), data=BlackdataP, id=Dist_Cluster, corstr="unstructured")

geeL2a <- gee(as.formula(paste("RawTurn_Latino_P ~ ",covariates2a_Any,sep="")), data=LatinodataP, id=Dist_Cluster, corstr="unstructured")
geeL2b <- gee(as.formula(paste("RawTurn_Latino_P ~ ",covariates2b_Any,sep="")), data=LatinodataP, id=Dist_Cluster, corstr="unstructured")

geeA2a <- gee(as.formula(paste("RawTurn_Asian_P ~ ",covariates2a_Any,sep="")), data=AsiandataP, id=Dist_Cluster, corstr="unstructured")
geeA2b <- gee(as.formula(paste("RawTurn_Asian_P ~ ",covariates2b_Any,sep="")), data=AsiandataP, id=Dist_Cluster, corstr="unstructured")

## Primary by Party Turnout, DEMOCRATS ##
# Primary Election Covariates, Democrats
covariates3a_Any <- "CVAP + DCandidate + I(CVAP*DCandidate) + CoEthSW + South + Yr06 + Yr08 + P_CatQuality"
covariates3b_Any <- "CVAP + DCandidate + I(CVAP*DCandidate) + CoEthSW + South + Yr06 + Yr08 + P_CatQuality + Age + Education + Income + Open + CookPVI + CookPVICompet + SenElect + GovElect + D_PrimType + Runoff"

geeW3a <- gee(as.formula(paste("RawTurn_White_DP ~ ",covariates3a_Any,sep="")), data=WhitedataDP, id=Dist_Cluster, corstr="unstructured")
geeW3b <- gee(as.formula(paste("RawTurn_White_DP ~ ",covariates3b_Any,sep="")), data=WhitedataDP, id=Dist_Cluster, corstr="unstructured")

geeB3a <- gee(as.formula(paste("RawTurn_Black_DP ~ ",covariates3a_Any,sep="")), data=BlackdataDP, id=Dist_Cluster, corstr="unstructured")
geeB3b <- gee(as.formula(paste("RawTurn_Black_DP ~ ",covariates3b_Any,sep="")), data=BlackdataDP, id=Dist_Cluster, corstr="unstructured")

geeL3a <- gee(as.formula(paste("RawTurn_Latino_DP ~ ",covariates3a_Any,sep="")), data=LatinodataDP, id=Dist_Cluster, corstr="unstructured")
geeL3b <- gee(as.formula(paste("RawTurn_Latino_DP ~ ",covariates3b_Any,sep="")), data=LatinodataDP, id=Dist_Cluster, corstr="unstructured")

# For Asians do not use Dem Statewide Candidate covariates, as there are none in the data
geeA3a <- gee(RawTurn_Asian_DP ~ CVAP + DCandidate + I(CVAP*DCandidate) + South + Yr06 + Yr08 + P_CatQuality, data=AsiandataDP, id=Dist_Cluster, corstr="unstructured")
geeA3b <- gee(RawTurn_Asian_DP ~ CVAP + DCandidate + I(CVAP*DCandidate) + South + Yr06 + Yr08 + P_CatQuality + Age + Education + Income + Open + CookPVI + CookPVICompet + SenElect + GovElect + D_PrimType + Runoff, data=AsiandataDP, id=Dist_Cluster, corstr="unstructured")

## Primary by Party Turnout, REPUBLICANS ##
# Primary Election Covariates, Republicans
covariates4a_Any <- "CVAP + RCandidate + I(CVAP*RCandidate) + CoEthSW + South + Yr06 + Yr08 + P_CatQuality"
covariates4b_Any <- "CVAP + RCandidate + I(CVAP*RCandidate) + CoEthSW + South + Yr06 + Yr08 + P_CatQuality + Age + Education + Income + Open + CookPVI + CookPVICompet + SenElect + GovElect + R_PrimType + Runoff"

geeW4a <- gee(as.formula(paste("RawTurn_White_RP ~ ",covariates4a_Any,sep="")), data=WhitedataRP, id=Dist_Cluster, corstr="unstructured")
geeW4b <- gee(as.formula(paste("RawTurn_White_RP ~ ",covariates4b_Any,sep="")), data=WhitedataRP, id=Dist_Cluster, corstr="unstructured")

geeB4a <- gee(as.formula(paste("RawTurn_Black_RP ~ ",covariates4a_Any,sep="")), data=BlackdataRP, id=Dist_Cluster, corstr="unstructured")
geeB4b <- gee(as.formula(paste("RawTurn_Black_RP ~ ",covariates4b_Any,sep="")), data=BlackdataRP, id=Dist_Cluster, corstr="unstructured")

geeL4a <- gee(as.formula(paste("RawTurn_Latino_RP ~ ",covariates4a_Any,sep="")), data=LatinodataRP, id=Dist_Cluster, corstr="unstructured")
geeL4b <- gee(as.formula(paste("RawTurn_Latino_RP ~ ",covariates4b_Any,sep="")), data=LatinodataRP, id=Dist_Cluster, corstr="unstructured")

geeA4a <- gee(as.formula(paste("RawTurn_Asian_RP ~ ",covariates4a_Any,sep="")), data=AsiandataRP, id=Dist_Cluster, corstr="unstructured")
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