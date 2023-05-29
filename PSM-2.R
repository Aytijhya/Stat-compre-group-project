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

mod_test1<- glm(Hdtype~Hb+PVC+DC2+DF2+BPR2+UFR2+AP2+VP2, 
                data,
                family="binomial")

mod_test2<- lm(X4Hr ~  Hb+PVC+DC2+DF2+BPR2+UFR2+AP2+VP2, data )

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
data_isf2<-na.omit(data_isf[,c("Hdtype","Hb","PVC","DC2","DF2","BPR2","UFR2","AP2","VP2","X2Hr")])
for(i in 2:9)
  data_isf2[,i]=as.numeric(data_isf2[,i])

psa_n<-matchit(Hdtype~Hb+PVC+DC2+DF2+BPR2+UFR2+AP2+VP2, 
               data=data_isf2,
               distance="glm",
               method="nearest",
               m.order = "largest",
               replace=FALSE)

# description
psa_n

# summary
summary(psa_n)



psa_o<-matchit(Hdtype~Hb+PVC+DC2+DF2+BPR2+UFR2+AP2+VP2, 
               data = data_isf2,
               distance="glm",
               method="optimal",
               ratio=2)

# description
psa_o



psa_f<-matchit(Hdtype~Hb+PVC+DC2+DF2+BPR2+UFR2+AP2+VP2,
               data = data_isf2,
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
          threshold=c(m=.25))

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
######################################################################
##### Analysis Phase: Estimating Treatment
#######################################################################

# inspect dataset to be used for analysis

library("nnet")
# PS FM weighted Regression
mod1<-multinom(X2Hr~Hdtype+DC2+DF2+BPR2+UFR2+AP2+VP2,
               data = psa_f_dat,
               weights=weights)

# ATE for selective
summary(mod1)


#######################################################################
##### Compare to not using PSA
#######################################################################

### No PSA, just regression (in this case ANCOVA)
#mod0<-multinom(X2Hr~Hdtype+DC2+DF2+BPR2+UFR2+AP2+VP2+I(AP2^2)+I(VP2^2),data = data_isf2)
#summary(mod0)
