data <- read_excel("Desktop/stat compre group project/Saline master sheet.xlsx")
unique(data[,3])
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

plot(as.numeric(data[3,29:32]), type = "o", col = "red",
     xlab = "Hour", ylab = "Blood clotting level",
     main = "Patient 1", ylim = c(0,4))
for(i in 3:9)
  lines(as.numeric(data[i,29:32]), type = "o",lwd=2, col = pal[i%%12])

plot(as.numeric(data[10,29:32]), type = "o", col = "red",
     xlab = "Hour", ylab = "Blood clotting level",
     main = "Patient 2", ylim = c(0,4))
for(i in 10:21)
  lines(as.numeric(data[i,29:32]), type = "o",lwd=2, col = pal[i%%12])

plot(as.numeric(data[22,29:32]), type = "o", col = "red",
     xlab = "Hour", ylab = "Blood clotting level",
     main = "Patient 3", ylim = c(0,4))
for(i in 22:36)
  lines(as.numeric(data[i,29:32]), type = "o",lwd=2, col = pal[i%%12])

plot(as.numeric(data[37,29:32]), type = "o", col = "red",
     xlab = "Hour", ylab = "Blood clotting level",
     main = "Patient 4", ylim = c(0,4))
for(i in 37:39)
  lines(as.numeric(data[i,29:32]), type = "o",lwd=2, col = pal[i%%12])

plot(as.numeric(data[40,29:32]), type = "o", col = "red",
     xlab = "Hour", ylab = "Blood clotting level",
     main = "Patient 5", ylim = c(0,4))
for(i in 40:56)
  lines(as.numeric(data[i,29:32]), type = "o",lwd=2, col = pal[i%%12])

