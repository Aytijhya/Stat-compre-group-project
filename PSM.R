
#######################################################################
##### Propensity Score Analysis (PSA) with MatchIt in R
#######################################################################

##### Outline

# What is Propensity Score Analysis (PSA) or Propensity
# Score Matching

# Will review matching methods: Nearest Neighbor 
# Matching, Optimal Matching, and Full Matching 
# with MatchIt in R

# How to estimate ATT/ATE

# Discuss some things to consider

##### PSA

# Randomized Control Trials (RCT) and when to use
# PSA

# What PSA provides.

# Why not to use regression/ANCOVA without
# PSA.


#######################################################################
##### Data set to use
#######################################################################

# load dataset
library(readxl)
data <- read_excel("Desktop/stat compre group project/saline-master-sheet-modified.xlsx")
unique(data[,3])
data=as.data.frame(data)

colnames(data)=rownames(data)=NULL
colnames(data)=data[2,]
data=data[-c(1,2),]
#######################################################################
##### Design Phase: Selecting covariates
#######################################################################

# Design Phase General Steps:

# 1 -- Decide covariates to balance

# 2 -- Estimate PS i.e. estimate probability
#      of being in a treatment group using
#      covariates.

# 3 -- Apply matching method using those PS

# 4 -- Evaluate Balance of covariates.
#      If balanced, move to analysis phase. 
#      If not balanced, start back at step 2 
#      and repeat.

# Not all covariates need balancing!!!

# Generally, the covariates you include should be related 
# to the treatment variable and the outcome variable, 
# (Harris & Horst, 2014). If you have an in-depth 
# understanding of the nature and relationships among 
# your data, Zhao et al 2020 showed some gave some 
# specfic examples of covariate relationships between 
# treatment and outcome that either should or do not 
# need to be balanced.

# Note. Goal is to attempt to balance covariates related
# to selectivity and median earnings.

### No PSA, just regression (in this case ANCOVA)
mod_test1<- glm(Hdtype~Hb+PVC+DC4+DF4+BPR4+UFR4+AP4+VP4, 
                data,
                family="binomial")

mod_test2<- lm(X4Hr ~  Hb+PVC+DC4+DF4+BPR4+UFR4+AP4+VP4, data )

# check covariate signifcance in predicting
# treatment and outcome
summary(mod_test1)
summary(mod_test2)

#######################################################################
##### Design Phase: PSA for 2-level gouping variable 
#######################################################################

# install/load package
#install.packages("MatchIt")
library(MatchIt)

# Generally PSA involves: 

### Nearest Neighbor Matching (NNM) (method="nearest")
# Specfic arguments in matchit(): exact, replace,  
# m.order, ratio, & caliper.

# Note. Arguent defaults may change when adjusting other
# arguments.
ISF=data[which(data[,4]=="ISF"),]
CSIF=data[which(data[,4]=="CSIF"),]
ISF_Hep=data[which(data[,4]=="ISF+hep"),]
CSIF_Hep=data[which(data[,4]=="CSIF+hep"),]
data_isf=rbind(ISF,ISF_Hep)
data_csif=rbind(CSIF,CSIF_Hep)
data_isf[,4]=as.factor(data_isf[,4])
data_isf4<-na.omit(data_isf[,c("Hdtype","Hb","PVC","DC4","DF4","BPR4","UFR4","AP4","VP4","X4Hr")])
for(i in 2:9)
 data_isf4[,i]=as.numeric(data_isf4[,i])

psa_n<-matchit(Hdtype~Hb+PVC+DC4+DF4+BPR4+UFR4+AP4+VP4, 
               data=data_isf4,
               distance="glm",
               method="nearest",
               m.order = "largest",
               replace=FALSE)

# description
psa_n

# summary
summary(psa_n)

# NNM is sensitive to the order. Therefore, other
# oders may produce different results.

# Indicating calipers sets a limit on the distance
# tolerated between treated and control PS for 
# matches to be made. 
# Suggested a caliper of .20 or .25 standard deviations
# (e.g., Rosembaum & Rubin, 1985; Austin, 2011)

# Though setting a caliper can reduce estimation bias,
# you may lose participants/observations.
# Therefore, consider adjusting the ratio to 
# pull in more participants/observations.

### Optimal Pair Matching (OPM; method="optimal")
# Specfic arguments in matchit(): ratio 

psa_o<-matchit(Hdtype~Hb+PVC+DC4+DF4+BPR4+UFR4+AP4+VP4, 
               data = data_isf4,
               distance="glm",
               method="optimal",
               ratio=2)

# description
psa_o

### Full Matching (FM; method="full")
# Specfic arguments in matchit(): estimand

# Note. Certain matching method allow 
# for different types of treatment effect to be
# estimated. There are 3:
# ATT -- Average Treatment of Treated
# ATC -- Average Treatment of Control
# ATE -- Average Treatment Effect

# This determines whether you can interpret the  
# treatment effect for the entire sample (ATE) 
# or just those that recieved treatment only (ATT) 
# or were in the control only (ATC). 

# Some matching methods (e.g., NNM and OM) do not 
# allow for ATE to be estimated but FM does. The 
# estimand argument controls how the weights will 
# be computed for FM. 

# Between ATT and ATC estimad will just adjust 
# the focal group used to be matched on.

psa_f<-matchit(Hdtype~Hb+PVC+DC4+DF4+BPR4+UFR4+AP4+VP4,
               data = data_isf4,
               distance="glm",
               method="full",
               estimand = "ATE")

# description
psa_f

#######################################################################
##### Design Phase: Evaluating Balance
#######################################################################

# Balance Criteria:

# Standardized mean difference values between -.1 and +.1
# Variance ratios balance between .8 and 1.25 

# SMD of .1 and VR approaching 1 but less than 2 
# could be considered balanced (Zhang et al., 2019)

# load package
library(cobalt)

# vizualize covariate balance of NNM
love.plot(bal.tab(psa_n), 
          stat = c("m"),
          grid=TRUE,
          threshold=c(m=.25,v=1.25))

# vizualize covariate balance of OPM
love.plot(bal.tab(psa_o), 
          stat = c("m"),
          grid=TRUE,
          threshold=c(m=.25,v=1.25))

# vizualize covariate balance of FM
love.plot(bal.tab(psa_f), 
          stat = c("m"),
          grid=TRUE,
          threshold=c(m=.25,v=1.25))

psa_f_dat<-match.data(psa_f)
library(tableone)
tab=CreateTableOne(data=psa_f_dat,strata="Hdtype",test=TRUE,smd=TRUE)
#######################################################################
##### Design Phase: Readjusting and Evaluating Balance
#######################################################################

# Account for nonlinear relationships by introducing
# quadratic terms, rerun PS and FM and check to see
# if covariate balance. 

# Also not that making adjustments could improve
# balancing of other matching methods.

### Full Matching (FM; method="full")
# Note. Also known as Optimal Full Matching.
psa_f2<-matchit(Hdtype~Hb+PVC+DC4+DF4+BPR4+UFR4+AP4+VP4+I(AP4^2)+I(VP4^2),
                data = data_isf4,
                distance="glm",
                method="full",
                estimand = "ATE")

# vizualize covariate balance of OFM
love.plot(bal.tab(psa_f2), 
          stat = c("m"),
          grid=TRUE,
          threshold=c(m=.25,v=1.25))

# Picking Matching Method:

# Though there has been many investigations comparing 
# different matching method, additional modifications
# could be introduced to any one matching method
# that could yield beter results balnancing covariates
# for your data.

#######################################################################
##### Analysis Phase: Estimating Treatment
#######################################################################

# inspect dataset to be used for analysis

library("nnet")
# PS FM weighted Regression
mod1<-multinom(X4Hr~Hdtype+DC4+DF4+BPR4+UFR4+AP4+VP4,
           data = psa_f_dat,
           weights=weights)

# ATE for selective
summary(mod1)


#######################################################################
##### Compare to not using PSA
#######################################################################

### No PSA, just regression (in this case ANCOVA)
mod0<-multinom(X4Hr~Hdtype+DC4+DF4+BPR4+UFR4+AP4+VP4,
               data = data_isf4)
summary(mod0)
#######################################################################
##### Other Matching Methods
#######################################################################

### Subclassification (method="subclass")
# Specfic arguments in matchit(): subclass & estimand.
# PS are divided into quantiles and these quantiles
# form the subclasses which determine the weights 
# to used.

# Note. FM compared to subclass is more optimal as it
# reduces within subclass differences.

### Exact Matching (method="exact")
# Specfic arguments in matchit(): estimand.
# Participants/observations with exact values across
# covariates are placed in subclasses. 
# These subclasses must contain at least one treatment 
# and control.
# Note. Exact matching becomes increasing more difficult with
# more covariates and levels within covariates.

### Coarsened Exact Matching (method="cem")
# Specfic arguments in matchit(): estimand, cutpoints, & grouping.
# First covariates are coarsened (i.e., subdivided into bins)
# then exact matching is done on the coarsened covariates.
# Note. Though less restrictive than EM, CEM is still impacted
# by too many covariates.

### Genetic Matching (method="genetic")
# Specfic arguments in matchit(): distance, mahvars, pop.val, & ratio.
# Matches are made with generalized Mahalanobis Distances
# containing weights. These weights are estimated by a 
# genetic search algrorithim which determines importance
# of covraiate in achieving balance.
# May or may not include PS. 

# Note. Requires installation of packages "Matching"
# and "rgenoud".

#######################################################################
##### Considerations
#######################################################################
# Data should be theoretically possible

# Missing data in the beginning

# Potential confounders not included

# PS matching or other matching methods may not always be 
# the right analysis for nonrandomized treatment 
# (e.g., regression discontinuity, etc.) 