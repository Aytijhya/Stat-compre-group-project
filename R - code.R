data<-read.csv(file.choose(),header=TRUE)
library("caTools")
library("ROCR")
library("lmtest")
library("nnet")
data$X1Hr
data1<-data[-c(7,40),]
data1$X1Hr<-as.factor(data1$X1Hr)
logmodel<-multinom(X1Hr~HD.type+DC1+DF1+BPR1+UFR1+AP1+VP1,data=data1)
logmodel1<-multinom(X1Hr~DC1+DF1+BPR1+UFR1+AP1+VP1,data=data1)
lrtest(logmodel1,logmodel)    
data$X2Hr
data2<-data[-c(7,40),]
data2$X2Hr<-as.factor(data2$X2Hr)
lmod<-multinom(X2Hr~HD.type+DC2+DF2+BPR2+UFR2+AP2+VP2,data=data2)
lmod1<-multinom(X2Hr~DC2+DF2+BPR2+UFR2+AP2+VP2,data=data2)
lrtest(lmod1,lmod)
data$X3Hr
data3<-data[-c(7,37),]
data3$X3Hr<-as.factor(data3$X3Hr)
llmod<-multinom(X3Hr~HD.type+DC3+DF3+BPR3+UFR3+AP3+VP3,data=data3)
llmod1<-multinom(X3Hr~DC3+DF3+BPR3+UFR3+AP3+VP3,data=data3)
lrtest(llmod1,llmod)
data$X4Hr
data4<-data[-c(7,8,22,36,37,43),]
data4$X4Hr<-as.factor(data4$X4Hr)
lmd<-multinom(X4Hr~HD.type+DC4+DF4+BPR4+UFR4+AP4+VP4,data=data4)
lmd1<-multinom(X4Hr~DC4+DF4+BPR4+UFR4+AP4+VP4,data=data4)
lrtest(lmd1,lmd)
a<-c()
data$X2Hr
data2<-data[-c(7,40),]
data2$X2Hr<-as.factor(data2$X2Hr)
for(i in 1:nrow(data2)){
  if(data2[i,3]=="ISF" || data2[i,3]=="ISF+hep" || data2[i,3]=="ISF+low hep")
    a<-c(a,i)
}
data3<-data2[-a,]
lmod<-multinom(X2Hr~HD.type+DC2+DF2+BPR2+UFR2+AP2+VP2,data=data3)
lmod1<-multinom(X2Hr~DC2+DF2+BPR2+UFR2+AP2+VP2,data=data3)
lrtest(lmod1,lmod)
a<-c()
data$X3Hr
data3<-data[-c(7,37),]
data3$X3Hr<-as.factor(data3$X3Hr)
for(i in 1:nrow(data3)){
  if(data3[i,3]=="ISF" || data3[i,3]=="ISF+hep" || data3[i,3]=="ISF+low hep")
    a<-c(a,i)
}
data4<-data3[-a,]
llmod<-multinom(X3Hr~HD.type+DC3+DF3+BPR3+UFR3+AP3+VP3,data=data4)
llmod1<-multinom(X3Hr~DC3+DF3+BPR3+UFR3+AP3+VP3,data=data4)
lrtest(llmod1,llmod)
a<-c()
data$X4Hr
data4<-data[-c(7,8,22,36,37,43),]
data4$X4Hr<-as.factor(data4$X4Hr)
for(i in 1:nrow(data4)){
  if(data4[i,3]=="ISF" || data4[i,3]=="ISF+hep" || data4[i,3]=="ISF+low hep")
    a<-c(a,i)
}
data5<-data4[-a,]
lmd<-multinom(X4Hr~HD.type+DC4+DF4+BPR4+UFR4+AP4+VP4,data=data5)
lmd1<-multinom(X4Hr~DC4+DF4+BPR4+UFR4+AP4+VP4,data=data5)
lrtest(lmd1,lmd)
a<-c()
data$X1Hr
data1<-data[-c(7,40),]
data1$X1Hr<-as.factor(data1$X1Hr)
for(i in 1:nrow(data1)){
  if(data1[i,3]=="ISF" || data1[i,3]=="ISF+hep" || data1[i,3]=="ISF+low hep")
    a<-c(a,i)
}
data2<-data1[a,]
logmodel<-multinom(X1Hr~HD.type+DC1+DF1+BPR1+UFR1+AP1+VP1,data=data2)
logmodel1<-multinom(X1Hr~DC1+DF1+BPR1+UFR1+AP1+VP1,data=data2)
lrtest(logmodel1,logmodel)
a<-c()
data$X2Hr
data2<-data[-c(7,40),]
data2$X2Hr<-as.factor(data2$X2Hr)
for(i in 1:nrow(data2)){
  if(data2[i,3]=="ISF" || data2[i,3]=="ISF+hep" || data2[i,3]=="ISF+low hep")
    a<-c(a,i)
}
data3<-data2[a,]
lmod<-multinom(X2Hr~HD.type+DC2+DF2+BPR2+UFR2+AP2+VP2,data=data3)
lmod1<-multinom(X2Hr~DC2+DF2+BPR2+UFR2+AP2+VP2,data=data3)
lrtest(lmod1,lmod)
a<-c()
data$X4Hr
data4<-data[-c(7,8,22,36,37,43),]
data4$X4Hr<-as.factor(data4$X4Hr)
for(i in 1:nrow(data4)){
  if(data4[i,3]=="ISF" || data4[i,3]=="ISF+hep" || data4[i,3]=="ISF+low hep")
    a<-c(a,i)
}
data5<-data4[a,]
lmd<-multinom(X4Hr~HD.type+DC4+DF4+BPR4+UFR4+AP4+VP4,data=data5)
lmd1<-multinom(X4Hr~DC4+DF4+BPR4+UFR4+AP4+VP4,data=data5)
lrtest(lmd1,lmd)
data1<-data[-c(7,8,22,36,37,38,40,43),]
event<-c()
times<-c()
for(i in 1:length(data1$X4Hr)){
  if(data1$X4Hr[i]==1){
    event[i]<-0
  }
  else
    event[i]<-1
}
for(i in 1:length(event)){
  if(data1$X1Hr[i]>1){
    times[i]<-1
  }
  else if(data1$X2Hr[i]>1){
    times[i]<-2
  }
  else if(data1$X3Hr[i]>1){
    times[i]<-3
  }
  else
    times[i]<-4
}
DC<-c()
DF<-c()
BPR<-c()
UFR<-c()
AP<-c()
VP<-c()
for(i in 1:length(event)){
  if(times[i]==1){
    DC[i]<-data1$DC1[i]
    DF[i]<-data1$DF1[i]
    BPR[i]<-data1$BPR1[i]
    UFR[i]<-data1$UFR1[i]
    AP[i]<-data1$AP1[i]
    VP[i]<-data1$VP1[i]
  }
  else if(times[i]==2){
    DC[i]<-data1$DC2[i]
    DF[i]<-data1$DF2[i]
    BPR[i]<-data1$BPR2[i]
    UFR[i]<-data1$UFR2[i]
    AP[i]<-data1$AP2[i]
    VP[i]<-data1$VP2[i]
  }
  else if(times[i]==3){
    DC[i]<-data1$DC3[i]
    DF[i]<-data1$DF3[i]
    BPR[i]<-data1$BPR3[i]
    UFR[i]<-data1$UFR3[i]
    AP[i]<-data1$AP3[i]
    VP[i]<-data1$VP3[i]
  }
  else
    DC[i]<-data1$DC4[i]
  DF[i]<-data1$DF4[i]
  BPR[i]<-data1$BPR4[i]
  UFR[i]<-data1$UFR4[i]
  AP[i]<-data1$AP4[i]
  VP[i]<-data1$VP4[i]
}
hemdat<-data.frame(times,event,data1$HD.type,DC,DF)
model <- coxph(surv_obj ~ data1.HD.type+ 
                 tt(DC)+tt(DF)+tt(BPR)+tt(UFR)+tt(AP)+tt(VP), data = hemdat)
summary(model)
data2<-data1
a<-c()
for(i in 1:nrow(data2)){
  if(data1[i,3]=="ISF" || data1[i,3]=="ISF+hep" || data1[i,3]=="ISF+low hep")
    a<-c(a,i)
}
data1<-data1[-a,]
event<-c()
times<-c()
for(i in 1:length(data1$X4Hr)){
  if(data1$X4Hr[i]==1){
    event[i]<-0
  }
  else
    event[i]<-1
}
for(i in 1:length(event)){
  if(data1$X1Hr[i]>1){
    times[i]<-1
  }
  else if(data1$X2Hr[i]>1){
    times[i]<-2
  }
  else if(data1$X3Hr[i]>1){
    times[i]<-3
  }
  else
    times[i]<-4
}
DC<-c()
DF<-c()
BPR<-c()
UFR<-c()
AP<-c()
VP<-c()
for(i in 1:length(event)){
  if(times[i]==1){
    DC[i]<-data1$DC1[i]
    DF[i]<-data1$DF1[i]
    BPR[i]<-data1$BPR1[i]
    UFR[i]<-data1$UFR1[i]
    AP[i]<-data1$AP1[i]
    VP[i]<-data1$VP1[i]
  }
  else if(times[i]==2){
    DC[i]<-data1$DC2[i]
    DF[i]<-data1$DF2[i]
    BPR[i]<-data1$BPR2[i]
    UFR[i]<-data1$UFR2[i]
    AP[i]<-data1$AP2[i]
    VP[i]<-data1$VP2[i]
  }
  else if(times[i]==3){
    DC[i]<-data1$DC3[i]
    DF[i]<-data1$DF3[i]
    BPR[i]<-data1$BPR3[i]
    UFR[i]<-data1$UFR3[i]
    AP[i]<-data1$AP3[i]
    VP[i]<-data1$VP3[i]
  }
  else
    DC[i]<-data1$DC4[i]
  DF[i]<-data1$DF4[i]
  BPR[i]<-data1$BPR4[i]
  UFR[i]<-data1$UFR4[i]
  AP[i]<-data1$AP4[i]
  VP[i]<-data1$VP4[i]
}
hemdat<-data.frame(times,event,data1$HD.type,DC,DF)
model <- coxph(Surv(times,event) ~ data1.HD.type+ tt(DC)+tt(DF)+tt(BPR)
               +tt(UFR)+tt(AP)+tt(VP), data = hemdat)
summary(model)
hazard_ratio <- exp(coef(model))
conf_interval <- exp(confint(model))
print(hazard_ratio)
print(conf_interval)
data1<-data[-c(7,8,22,36,37,38,40,43),]
a<-c()
for(i in 1:nrow(data2)){
  if(data1[i,3]=="ISF" || data1[i,3]=="ISF+hep" || data1[i,3]=="ISF+low hep")
    a<-c(a,i)
}
data1<-data1[a,]
event<-c()
times<-c()
for(i in 1:length(data1$X4Hr)){
  if(data1$X4Hr[i]==1){
    event[i]<-0
  }
  else
    event[i]<-1
}
for(i in 1:length(event)){
  if(data1$X1Hr[i]>1){
    times[i]<-1
  }
  else if(data1$X2Hr[i]>1){
    times[i]<-2
  }
  else if(data1$X3Hr[i]>1){
    times[i]<-3
  }
  else
    times[i]<-4
}
DC<-c()
DF<-c()
BPR<-c()
UFR<-c()
AP<-c()
VP<-c()
for(i in 1:length(event)){
  if(times[i]==1){
    DC[i]<-data1$DC1[i]
    DF[i]<-data1$DF1[i]
    BPR[i]<-data1$BPR1[i]
    UFR[i]<-data1$UFR1[i]
    AP[i]<-data1$AP1[i]
    VP[i]<-data1$VP1[i]
  }
  else if(times[i]==2){
    DC[i]<-data1$DC2[i]
    DF[i]<-data1$DF2[i]
    BPR[i]<-data1$BPR2[i]
    UFR[i]<-data1$UFR2[i]
    AP[i]<-data1$AP2[i]
    VP[i]<-data1$VP2[i]
  }
  else if(times[i]==3){
    DC[i]<-data1$DC3[i]
    DF[i]<-data1$DF3[i]
    BPR[i]<-data1$BPR3[i]
    UFR[i]<-data1$UFR3[i]
    AP[i]<-data1$AP3[i]
    VP[i]<-data1$VP3[i]
  }
  else
    DC[i]<-data1$DC4[i]
  DF[i]<-data1$DF4[i]
  BPR[i]<-data1$BPR4[i]
  UFR[i]<-data1$UFR4[i]
  AP[i]<-data1$AP4[i]
  VP[i]<-data1$VP4[i]
}
for(i in 1:length(data1$HD.type)){
  if(data1$HD.type[i]=="ISF+low hep"){
    data1$HD.type[i]="ISF"
  }
}
hemdat<-data.frame(times,event,data1$HD.type,DC,DF)
model <- coxph(Surv(times,event) ~ data1.HD.type+ tt(DC)+tt(DF)+tt(BPR)
               +tt(UFR)+tt(AP)+tt(VP), data = hemdat)
summary(model)
hazard_ratio <- exp(coef(model))
conf_interval <- exp(confint(model))
print(hazard_ratio)
print(conf_interval)

data=as.data.frame(data)
colnames(data)=rownames(data)=NULL
ISF=data[which(data[,3]=="ISF"),]
CSIF=data[which(data[,3]=="CSIF"),]
ISF_Hep=data[which(data[,3]=="ISF+hep"),]
CSIF_Hep=data[which(data[,3]=="CSIF+hep"),]
pal <- brewer.pal(12,"Paired")

#treatment -wise
plot(as.numeric(ISF[1,29:32]), type = "o", col = "red",
     xlab = "Hour", ylab = "Blood clotting level",
     main = "ISF", ylim = c(0,4))
for(i in 1:nrow(ISF))
  lines(as.numeric(ISF[i,29:32]), type = "o",lwd=2, col = pal[i%%12])
plot(as.numeric(CSIF[1,29:32]), type = "o", col = "red",
     xlab = "Hour", ylab = "Blood clotting level",
     main = "CSIF", ylim = c(0,4))
for(i in 1:nrow(CSIF))
  lines(as.numeric(CSIF[i,29:32]), type = "o",lwd=2, col = pal[i%%12])
plot(as.numeric(ISF_Hep[1,29:32]), type = "o", col = "red",
     xlab = "Hour", ylab = "Blood clotting level",
     main = "ISF+Hep", ylim = c(0,4))
for(i in 1:nrow(ISF_Hep))
  lines(as.numeric(ISF_Hep[i,29:32]), type = "o",lwd=2, col = pal[i%%12])
plot(as.numeric(CSIF_Hep[1,29:32]), type = "o", col = "red",
     xlab = "Hour", ylab = "Blood clotting level",
     main = "CSIF+Hep", ylim = c(0,4))
for(i in 1:nrow(CSIF_Hep))
  lines(as.numeric(CSIF_Hep[i,29:32]), type = "o",lwd=2, col = pal[i%%12])

#person-wise
plot(as.numeric(data[1,29:32]), type = "o", col = "red",
     xlab = "Hour", ylab = "Blood clotting level", 
     main = "Patient 1", ylim = c(0,4))
for(i in 1:7)
  lines(as.numeric(data[i,29:32]), type = "o",lwd=2, col = pal[i%%12])
plot(as.numeric(data[8,29:32]), type = "o", col = "red",
     xlab = "Hour", ylab = "Blood clotting level", 
     main = "Patient 2", ylim = c(0,4))
for(i in 8:19)
  lines(as.numeric(data[i,29:32]), type = "o",lwd=2, col = pal[i%%12])
plot(as.numeric(data[20,29:32]), type = "o", col = "red",
     xlab = "Hour", ylab = "Blood clotting level", 
     main = "Patient 3", ylim = c(0,4))
for(i in 20:34)
  lines(as.numeric(data[i,29:32]), type = "o",lwd=2, col = pal[i%%12])
plot(as.numeric(data[35,29:32]), type = "o", col = "red",
     xlab = "Hour", ylab = "Blood clotting level",
     main = "Patient 4", ylim = c(0,4))
for(i in 35:37)
  lines(as.numeric(data[i,29:32]), type = "o",lwd=2, col = pal[i%%12])
plot(as.numeric(data[38,29:32]), type = "o", col = "red",
     xlab = "Hour", ylab = "Blood clotting level",
     main = "Patient 5", ylim = c(0,4))
for(i in 38:54)
  lines(as.numeric(data[i,29:32]), type = "o",lwd=2, col = pal[i%%12])

Treatment<-as.factor(data$HD.type)
DC=data$DC
DF=data$DF
BPR=data$BPR
UFR=data$UFR
VP=data$VP
Clotting_Score=data$Clotting.Score
data<-data.frame(Treatment,DC,DF,BPR,UFR,VP,Clotting_Score) 
model <- aov(Clotting_Score ~ Treatment +DC + DF + BPR + UFR + VP)
Anova(model, type="III")

boxplot(patient1DC,patient2DC,patient3DC,patient4DC,patient5DC,
        beside=T,col=c("yellow","green","red","orange","blue"),
        main="Dialysate Conductivity of Patients",names=c("Patient 1","Patient 2",
                                                          "Patient 3","Patient 4","Patient 5"),ylab="Dialysate Conductivity (in mS)")

boxplot(patient1UF,patient2UF,patient3UF,patient4UF,patient5UF,
        beside=T,col=c("yellow","green","red","orange","blue"),
        main="Total Ultrafiltration done on Patients",names=c("Patient 1",
                                                              "Patient 2","Patient 3","Patient 4","Patient 5"),
        ylab="Total Ultrafiltration (in ltr)")

boxplot(ClotScoreISF_hep,ClotScoreCSIF_hep,ClotScoreISF,ClotScoreCSIF,
        names=c("ISF+Heparin","CSIF+Heparin","ISF","CSIF"),ylab="Clotting Score",
        main="Clotting Score vs Treatment",col=c("yellow","green"
                                                 ,"red","orange","blue"))

boxplot(patient1CS,patient2CS,patient3CS,patient4CS,patient5CS,
        names=c("Patient 1","Patient 2","Patient 3","Patient 4","Patient 5"),
        ylab="Clotting Score",main="Clotting Score vs Patient",col=c("yellow",
                                                                     "green","red","orange","blue"))

CSIF_HepDClot=c(3,2,1)
ISF_HepDClot=c(2,2,2)
ISFDClot=c(3,3,3)
CSIFDClot=c(10,1,3)
DClot=cbind(ISF_HepDClot,CSIF_HepDClot,ISFDClot,CSIFDClot)
barplot(DClot,names=c("ISF+Heparin","CSIF+Heparin","ISF","CSIF"),
        col=pal[1:3],main="Dialyzer Clotting Score vs Treatment",
        ylab="Frequency",xlab="Treatment")
legend(x="topleft",legend=c("1","2","3"),fill=pal[1:3])