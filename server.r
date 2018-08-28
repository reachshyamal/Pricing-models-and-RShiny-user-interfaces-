server <- function(input,output)
{
#rm(list = ls())
library('glmnet')
library(readxl)
library("MASS")
library(quantreg)
library('dmm')
library('ggplot2')
setwd("C:/Users/raja.ambrish/Desktop/Projects/Arab osp/shiny")
df1<-read_excel("AGdatabase.xlsx")
colnames(df1)
ah.osp<-df1$`Arab Hvy Crude Saudi Asia`
df2<-df1[,2:18]
df2<-cbind(df2,ah.osp)
df2<-data.frame(diff(as.matrix(df2)))
tail(df2)
df2$index=c(1:nrow(df2))
dfcheck <- df2
###Arab Heavy###
##Scatterplots##
#plot(df2$Platts.Oman...Dubai,df2$ah.osp,xlab="Oman Dubai differential",ylab="Arab Heavy"); grid(col = 'black')
#plot(df2$Sing.GO...Dubai,df2$ah.osp,xlab="S'pore Gasoil Dubai differential",ylab="Arab Heavy"); grid(col = 'black')
#plot(df2$Dubai.M1..M3,df2$ah.osp,xlab="Dubai M1-M3",ylab="Arab Heavy"); grid(col = 'black')
#plot(df2$FO.M2..M3,df2$ah.osp,xlab="FO M2-M3",ylab="Arab Heavy"); grid(col = 'black')

##Applying model boundaries##
dim(df2)
df2<-subset(df2,df2$Dubai.M1..M3> -1.5)
dim(df2)
df2<-subset(df2,df2$Brent.M1..M3> -1.2 & df2$Brent.M1..M3< 1)
dim(df2)
df2<-subset(df2,df2$CL.M1...M3> -2 & df2$CL.M1...M3 < 2)
dim(df2)
df2<-subset(df2,df2$FO.M2..M3 > -5 & df2$FO.M2..M3 < 5)
dim(df2)
df2<-subset(df2,df2$HILO > -30 & df2$HILO < 20)
dim(df2)
df2<-subset(df2,df2$HSFO.180...Dubai > -4)
dim(df2)
df2<-subset(df2,df2$X92.Dubai > -10)
dim(df2)
df2<-subset(df2,df2$Kero..Dubai > -4 & df2$Kero..Dubai < 4)
dim(df2)
df2<-subset(df2,df2$Platts.Oman...Dubai< 0.75)
dim(df2)
df2<-subset(df2,df2$Brent...Dubai> -3)
dim(df2)
df2<-subset(df2,df2$Regrade> -2 & df2$Regrade < 3)
dim(df2)
df2<-subset(df2,df2$Visco> -5 & df2$Visco < 6)
dim(df2)

if (df2$index[nrow(df2)] == dfcheck$index[nrow(dfcheck)])
{
  msg <- "Model boundaries not violated" 
} else {
  msg <- "Model boundaries violated. No output for this month."
}

showNotification(msg, action = NULL, duration = NULL, closeButton = TRUE,
                 type = "message",
                 session = getDefaultReactiveDomain())

c33 <- format(df1$Month[df2$index[nrow(df2)] + 1],"%b-%y")
c34 <- paste("MoM change", c33, sep = " ")
output$cmonth <- renderText(c34)

##Rolling window M1 regression##
###prediction
df4Train<-df2[1:49,]
df4Test<-df2[50:nrow(df2),]

#############################################
df4 <- rbind(df4Train,df4Test)
list1 <- matrix(nrow=(nrow(df2)-50),ncol = 3)
list2<-matrix(nrow=(nrow(df2)-50),ncol=1)
list3<-matrix(nrow=(nrow(df2)-50),ncol=1)
list4<-matrix(nrow=(nrow(df2)-50),ncol=1)
for (i in 1:(nrow(df2)-50))
{
  df5Train <- df4[(1+i):(49+i),]
  df5Test <-  df4[(50+i),]
  library(car)
  bestmodel <- lm(ah.osp~(Dubai.M1..M3+FO.M2..M3+Platts.Oman...Dubai+Sing.GO...Dubai)-1, data = df5Train,weights = seq(1/1225,49/1225,by=1/1225))
  w <- abs(rstudent(bestmodel)) < 3 & abs(cooks.distance(bestmodel)) < 4/nrow(bestmodel$model)
  bestmodel <- update(bestmodel, weights=as.numeric(w)*seq(1/1225,49/1225,by=1/1225))
  list1[i,]<-predict(bestmodel,newdata=df5Test,interval='predict')
  list2[i,]<-t(df5Test$ah.osp)
  list3[i,]<-t(df5Test$index)
  list4[i,]<-abs(list1[i,1]-list2[i,])
}

c1 <- as.list(bestmodel$coefficients)
c2 <- as.list(c(df5Test$Dubai.M1..M3, df5Test$FO.M2..M3, df5Test$Platts.Oman...Dubai
              ,df5Test$Sing.GO...Dubai))
c3 <- c("Dubai M1-M3", "FO M2-M3", "Oman - Dubai", "Sing GO - Dubai")
df6 <- as.data.frame(cbind(c3,c1,c2))
df6$c1 <- round(as.numeric(df6$c1),2)
df6$c2 <- round(as.numeric(df6$c2),2)
df6$Contribution <- df6$c1 * df6$c2
df6$Contribution <- round(df6$Contribution,2)
colnames(df6) <- c("Varibales","Coefficient", "MoM change", "Contribution")
c4 <- c("Prediction", "","","")
df6 <- rbind(df6,c4)
rownames(df6)[5] <- "Prediction"
df6[5,4] <- as.numeric(df6$Contribution[1]) + as.numeric(df6$Contribution[2]) + 
            as.numeric(df6$Contribution[3]) + as.numeric(df6$Contribution[4])

output$Predtable1 <- renderTable(df6)

c5 <- c("Announcement Month", "Predicted", "Realised")
df1$Month <- format(df1$Month, "%b-%y")
c6 <- df1$Month[(nrow(df1)-6):(nrow(df1)-1)]
c7 <- list1[(nrow(list1)-6):(nrow(list1)-1),1]
c8 <- list2[(nrow(list2)-6):(nrow(list2)-1),1]
df7 <- as.data.frame(cbind(c6,c7,c8))
df7$c7 <- unfactor(df7$c7)
df7$c7 <- round(df7$c7,2)
df7$c8 <- unfactor(df7$c8)
df7$c8 <- round(df7$c8,2)
colnames(df7) <- c5
df7$Difference <- df7$Predicted - df7$Realised
df7$`Abs difference` <- abs(df7$Difference) 

output$Prevpred1 <- renderTable(df7)

####Plots
p1 <- ggplot(df5Train, aes(x= Platts.Oman...Dubai, y= Arab.Hvy.Crude.Saudi.Asia)) + 
  geom_smooth(method="lm", fullrange=TRUE, se = FALSE) + labs(x = "Oman - Dubai MoM"  
  , y = "Arab Heavy OSP MoM", title = "Scatter plot w Oman-Dubai" )+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "bottom")+ geom_point()

p2 <- ggplot(df5Train, aes(x= Sing.GO...Dubai, y= Arab.Hvy.Crude.Saudi.Asia)) + 
  geom_smooth(method="lm", fullrange=TRUE, se = FALSE) + labs(x = "GO - Dubai MoM"  
          , y = "Arab Heavy OSP MoM", title = "Scatter plot w GO-Dubai" )+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "bottom")+ geom_point()

p3 <- ggplot(df5Train, aes(x= Dubai.M1..M3, y= Arab.Hvy.Crude.Saudi.Asia)) + 
  geom_smooth(method="lm", fullrange=TRUE, se = FALSE) + labs(x = "Dubai M1-M3 MoM"  
          , y = "Arab Heavy OSP MoM", title = "Scatter plot w Dubai M1-M3" )+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "bottom")+ geom_point()

p4 <- ggplot(df5Train, aes(x= FO.M2..M3, y= Arab.Hvy.Crude.Saudi.Asia)) + 
  geom_smooth(method="lm", fullrange=TRUE, se = FALSE) + labs(x = "FO M2-M3 MoM"  
  , y = "Arab Heavy OSP MoM", title = "Scatter plot w FO M2-M3" )+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "bottom")+ geom_point()
output$Plot1 <- renderPlot(p1)
output$Plot2 <- renderPlot(p2)
output$Plot3 <- renderPlot(p3)
output$Plot4 <- renderPlot(p4)


###Arab Medium###
am.osp<-df1$`Arab Medium Crude Saudi Asia`
df2<-df1[,2:18]
df2<-cbind(df2,am.osp)
df2<-data.frame(diff(as.matrix(df2)))
tail(df2)
df2$index=c(1:nrow(df2))

##Scatterplots##
#plot(df2$Platts.Oman...Dubai,df2$am.osp,xlab="Oman Dubai differential",ylab="Arab Medium");grid(col = 'black')
#plot(df2$Sing.GO...Dubai,df2$am.osp,xlab="S'pore Gasoil Dubai differential",ylab="Arab Medium");grid(col = 'black')
#plot(df2$Dubai.M1..M3,df2$am.osp,xlab="Dubai M1-M3",ylab="Arab Medium");grid(col = 'black')
#plot(df2$Brent.M1..M3,df2$am.osp,xlab="Brent M1-M3",ylab="Arab Medium");grid(col = 'black')
#plot(df2$FO.M2..M3,df2$am.osp,xlab="FO M2-M3",ylab="Arab Medium"); grid(col = 'black')

##Applying model boundaries##
dim(df2)
df2<-subset(df2,df2$Dubai.M1..M3> -1.5)
dim(df2)
df2<-subset(df2,df2$Brent.M1..M3> -1.2 & df2$Brent.M1..M3< 1)
dim(df2)
df2<-subset(df2,df2$CL.M1...M3> -2 & df2$CL.M1...M3 < 2)
dim(df2)
df2<-subset(df2,df2$FO.M2..M3 > -5 & df2$FO.M2..M3 < 5)
dim(df2)
df2<-subset(df2,df2$HILO > -30 & df2$HILO < 20)
dim(df2)
df2<-subset(df2,df2$HSFO.180...Dubai > -4)
dim(df2)
df2<-subset(df2,df2$X92.Dubai > -10)
dim(df2)
df2<-subset(df2,df2$Kero..Dubai > -4 & df2$Kero..Dubai < 4)
dim(df2)
df2<-subset(df2,df2$Platts.Oman...Dubai< 0.75)
dim(df2)
df2<-subset(df2,df2$Brent...Dubai> -3)
dim(df2)
df2<-subset(df2,df2$Regrade> -2 & df2$Regrade < 3)
dim(df2)
df2<-subset(df2,df2$Visco> -5 & df2$Visco < 6)
dim(df2)

##Rolling window M1 regression##
###prediction
df4Train<-df2[1:49,]
df4Test<-df2[50:nrow(df2),]
df4 <- rbind(df4Train,df4Test)
list1 <- matrix(nrow=(nrow(df2)-50),ncol = 3)
list2<-matrix(nrow=(nrow(df2)-50),ncol=1)
list3<-matrix(nrow=(nrow(df2)-50),ncol=1)
list4<-matrix(nrow=(nrow(df2)-50),ncol=1)

for (i in 1:(nrow(df2)-50))
{
  df5Train <- df4[(1+i):(49+i),]
  df5Test <-  df4[(50+i),]
  bestmodel <- lm(am.osp~(Dubai.M1..M3+FO.M2..M3+Sing.GO...Dubai+Brent.M1..M3+Platts.Oman...Dubai)-1, data = df5Train,weights=seq(1/1225,49/1225,by=1/1225))
  w <- abs(rstudent(bestmodel)) < 3 & abs(cooks.distance(bestmodel)) < 4/nrow(bestmodel$model)
  bestmodel <- update(bestmodel, weights=as.numeric(w)*seq(1/1225,49/1225,by=1/1225))
  print(summary(bestmodel))
  list1[i,] <- predict(bestmodel,newdata=df5Test,interval='predict')
  list2[i,]<-t(df5Test$am.osp)
  list3[i,]<-t(df5Test$index)
  list4[i,]<-abs(list1[i,1]-list2[i,])
}

c9 <- as.list(bestmodel$coefficients)
c10 <- as.list(c(df5Test$Dubai.M1..M3, df5Test$FO.M2..M3, df5Test$Sing.GO...Dubai, 
                 df5Test$Brent.M1..M3, df5Test$Platts.Oman...Dubai))
c11 <- c("Dubai M1-M3", "FO M2-M3", "Sing GO - Dubai", "Brent M1-M3", "Platts Oman - Dubai")
df8 <- as.data.frame(cbind(c11,c9,c10))
df8$c9 <- round(as.numeric(df8$c9),2)
df8$c10 <- round(as.numeric(df8$c10),2)
df8$Contribution <- df8$c9 * df8$c10
df8$Contribution <- round(df8$Contribution,2)
colnames(df8) <- c("Varibales","Coefficient", "MoM change", "Contribution")
c12 <- c("Prediction", "","","")
df8 <- rbind(df8,c12)
rownames(df8)[6] <- "Prediction"
df8[6,4] <- as.numeric(df8$Contribution[1]) + as.numeric(df8$Contribution[2]) + 
  as.numeric(df8$Contribution[3]) + as.numeric(df8$Contribution[4]) + 
                                                 as.numeric(df8$Contribution[5])

output$Predtable2 <- renderTable(df8)

c13 <- c("Announcement Month", "Predicted", "Realised")
c14 <- df1$Month[(nrow(df1)-6):(nrow(df1)-1)]
c15 <- list1[(nrow(list1)-6):(nrow(list1)-1),1]
c16 <- list2[(nrow(list2)-6):(nrow(list2)-1),1]
df9 <- as.data.frame(cbind(c14,c15,c16))
df9$c15 <- unfactor(df9$c15)
df9$c15 <- round(df9$c15,2)
df9$c16 <- unfactor(df9$c16)
df9$c16 <- round(df9$c16,2)
colnames(df9) <- c13
df9$Difference <- df9$Predicted - df9$Realised
df9$`Abs difference` <- abs(df9$Difference) 

output$Prevpred2 <- renderTable(df9)

####Plots
p5 <- ggplot(df5Train, aes(x= Platts.Oman...Dubai, y= Arab.Medium.Crude.Saudi.Asia)) + 
  geom_smooth(method="lm", fullrange=TRUE, se = FALSE) + labs(x = "Oman - Dubai MoM"  
  , y = "Arab Medium OSP MoM", title = "Scatter plot w Oman-Dubai" )+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "bottom")+ geom_point()

p6 <- ggplot(df5Train, aes(x= Sing.GO...Dubai, y= Arab.Medium.Crude.Saudi.Asia)) + 
  geom_smooth(method="lm", fullrange=TRUE, se = FALSE) + labs(x = "GO - Dubai MoM"  
  , y = "Arab Medium OSP MoM", title = "Scatter plot w GO-Dubai" )+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "bottom")+ geom_point()

p7 <- ggplot(df5Train, aes(x= Dubai.M1..M3, y= Arab.Medium.Crude.Saudi.Asia)) + 
  geom_smooth(method="lm", fullrange=TRUE, se = FALSE) + labs(x = "Dubai M1-M3 MoM"  
  , y = "Arab Medium OSP MoM", title = "Scatter plot w Dubai M1-M3" )+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "bottom")+ geom_point()

p8 <- ggplot(df5Train, aes(x= FO.M2..M3, y= Arab.Medium.Crude.Saudi.Asia)) + 
  geom_smooth(method="lm", fullrange=TRUE, se = FALSE) + labs(x = "FO M2-M3 MoM"  
 , y = "Arab Medium OSP MoM", title = "Scatter plot w FO M2-M3" )+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "bottom")+ geom_point()

p9 <- ggplot(df5Train, aes(x= Brent.M1..M3, y= Arab.Medium.Crude.Saudi.Asia)) + 
  geom_smooth(method="lm", fullrange=TRUE, se = FALSE) + labs(x = "Brent M1-M3 MoM"  
      , y = "Arab Medium OSP MoM", title = "Scatter plot w Brent M1-M3" )+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "bottom")+ geom_point()

output$Plot5 <- renderPlot(p5)
output$Plot6 <- renderPlot(p6)
output$Plot7 <- renderPlot(p7)
output$Plot8 <- renderPlot(p8)
output$Plot9 <- renderPlot(p9)

##Arab Light##
al.osp<-df1$`Arab Light Crude Saudi Asia`
df2<-df1[,2:18]
df2<-cbind(df2,al.osp)
df2<-data.frame(diff(as.matrix(df2)))
tail(df2)
df2$index=c(1:nrow(df2))

##Scatterplots##
#plot(df2$Platts.Oman...Dubai,df2$al.osp,xlab="Oman Dubai differential",ylab="Arab Light");grid(col = 'black')
#plot(df2$Dubai.M1..M3,df2$al.osp,xlab="Dubai M1-M3",ylab="Arab Light");grid(col = 'black')

##Applying model boundaries##
dim(df2)
df2<-subset(df2,df2$Dubai.M1..M3> -1.5)
dim(df2)
df2<-subset(df2,df2$Brent.M1..M3> -1.2 & df2$Brent.M1..M3< 1)
dim(df2)
df2<-subset(df2,df2$CL.M1...M3> -2 & df2$CL.M1...M3 < 2)
dim(df2)
df2<-subset(df2,df2$FO.M2..M3 > -5 & df2$FO.M2..M3 < 5)
dim(df2)
df2<-subset(df2,df2$HILO > -30 & df2$HILO < 20)
dim(df2)
df2<-subset(df2,df2$HSFO.180...Dubai > -4)
dim(df2)
df2<-subset(df2,df2$X92.Dubai > -10)
dim(df2)
df2<-subset(df2,df2$Kero..Dubai > -4 & df2$Kero..Dubai < 4)
dim(df2)
df2<-subset(df2,df2$Platts.Oman...Dubai< 0.75)
dim(df2)
df2<-subset(df2,df2$Brent...Dubai> -3)
dim(df2)
df2<-subset(df2,df2$Regrade> -2 & df2$Regrade < 3)
dim(df2)
df2<-subset(df2,df2$Visco> -5 & df2$Visco < 6)
dim(df2)

##Rolling window M1 regression##
###prediction
df4Train<-df2[1:49,]
df4Test<-df2[50:nrow(df2),]
df4 <- rbind(df4Train,df4Test)
list1 <- matrix(nrow=(nrow(df2)-50),ncol = 3)
list2<-matrix(nrow=(nrow(df2)-50),ncol=1)
list3<-matrix(nrow=(nrow(df2)-50),ncol=1)
list4<-matrix(nrow=(nrow(df2)-50),ncol=1)
list5<-matrix(nrow=(nrow(df2)-50),ncol=1)
list6<-matrix(nrow=(nrow(df2)-50),ncol=1)
for (i in 1:(nrow(df2)-50))
{
  df5Train <- df4[(1+i):(49+i),]
  df5Test <-  df4[(50+i),]
  bestmodel <- lm(al.osp~(Dubai.M1..M3+Platts.Oman...Dubai)-1, data = df5Train,weights=seq(1/1225,49/1225,by=1/1225))
  w <- abs(rstudent(bestmodel)) < 3 & abs(cooks.distance(bestmodel)) < 4/nrow(bestmodel$model)
  bestmodel <- update(bestmodel, weights=as.numeric(w)*seq(1/1225,49/1225,by=1/1225))
  print(summary(bestmodel))
  list1[i,] <- predict(bestmodel,newdata=df5Test,interval='predict')
  list2[i,]<-t(df5Test$al.osp)
  list3[i,]<-t(df5Test$index)
  list4[i,]<-abs(list1[i,1]-list2[i,])
  list5[i,]<-t(df5Test$Dubai.M1..M3)
  list6[i,]<-t(df5Test$Platts.Oman...Dubai)
}

c17 <- as.list(bestmodel$coefficients)
c18 <- as.list(c(df5Test$Dubai.M1..M3, df5Test$Platts.Oman...Dubai))
c19 <- c("Dubai M1-M3", "Platts Oman - Dubai")
df10 <- as.data.frame(cbind(c19,c17,c18))
df10$c17 <- round(as.numeric(df10$c17),2)
df10$c18 <- round(as.numeric(df10$c18),2)
df10$Contribution <- df10$c17 * df10$c18
df10$Contribution <- round(df10$Contribution,2)
colnames(df10) <- c("Varibales","Coefficient", "MoM change", "Contribution")
c20 <- c("Prediction", "","","")
df10 <- rbind(df10,c20)
rownames(df10)[3] <- "Prediction"
df10[3,4] <- as.numeric(df10$Contribution[1]) + as.numeric(df10$Contribution[2])

output$Predtable3 <- renderTable(df10)

c21 <- c("Announcement Month", "Predicted", "Realised")
c22 <- df1$Month[(nrow(df1)-6):(nrow(df1)-1)]
c23 <- list1[(nrow(list1)-6):(nrow(list1)-1),1]
c24 <- list2[(nrow(list2)-6):(nrow(list2)-1),1]
df11 <- as.data.frame(cbind(c22,c23,c24))
df11$c23 <- unfactor(df11$c23)
df11$c23 <- round(df11$c23,2)
df11$c24 <- unfactor(df11$c24)
df11$c24 <- round(df11$c24,2)
colnames(df11) <- c21
df11$Difference <- df11$Predicted - df11$Realised
df11$`Abs difference` <- abs(df11$Difference) 

output$Prevpred3 <- renderTable(df11)

####Plots
p10 <- ggplot(df5Train, aes(x= Platts.Oman...Dubai, y= Arab.Light.Crude.Saudi.Asia)) + 
  geom_smooth(method="lm", fullrange=TRUE, se = FALSE) + labs(x = "Oman - Dubai MoM"  
  , y = "Arab Light OSP MoM", title = "Scatter plot w Oman-Dubai" )+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "bottom")+ geom_point()

p11 <- ggplot(df5Train, aes(x= Dubai.M1..M3, y= Arab.Light.Crude.Saudi.Asia)) + 
  geom_smooth(method="lm", fullrange=TRUE, se = FALSE) + labs(x = "Dubai M1-M3 MoM"  
  , y = "Arab Light OSP MoM", title = "Scatter plot w Dubai M1-M3" )+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "bottom")+ geom_point()

output$Plot10 <- renderPlot(p10)
output$Plot11 <- renderPlot(p11)

##Arab Extra Light##
axl.osp<-df1$`Extra Light Crude Saudi Asia`
df2<-df1[,2:18]
df2<-cbind(df2,axl.osp)
df2<-data.frame(diff(as.matrix(df2)))
tail(df2)
df2$index=c(1:nrow(df2))

##Scatterplots##
#plot(df2$X92.Dubai,df2$axl.osp,xlab="92 Dubai differential",ylab="Arab Extra Light");grid(col = 'black')
#plot(df2$Dubai.M1..M3,df2$axl.osp,xlab="Dubai M1-M3",ylab="Arab Extra Light");grid(col = 'black')
#plot(df2$Brent.M1..M3,df2$axl.osp,xlab="Brent M1-M3",ylab="Arab Extra Light");grid(col = 'black')
#plot(df2$FO.M2..M3,df2$axl.osp,xlab="FO M2-M3",ylab="Arab Extra Light");grid(col = 'black')

##Applying model boundaries##
dim(df2)
df2<-subset(df2,df2$Dubai.M1..M3> -1.5)
dim(df2)
df2<-subset(df2,df2$Brent.M1..M3> -1.2 & df2$Brent.M1..M3< 1)
dim(df2)
df2<-subset(df2,df2$CL.M1...M3> -2 & df2$CL.M1...M3 < 2)
dim(df2)
df2<-subset(df2,df2$FO.M2..M3 > -5 & df2$FO.M2..M3 < 5)
dim(df2)
df2<-subset(df2,df2$HILO > -30 & df2$HILO < 20)
dim(df2)
df2<-subset(df2,df2$HSFO.180...Dubai > -4)
dim(df2)
df2<-subset(df2,df2$X92.Dubai > -10)
dim(df2)
df2<-subset(df2,df2$Kero..Dubai > -4 & df2$Kero..Dubai < 4)
dim(df2)
df2<-subset(df2,df2$Platts.Oman...Dubai< 0.75)
dim(df2)
df2<-subset(df2,df2$Brent...Dubai> -3)
dim(df2)
df2<-subset(df2,df2$Regrade> -2 & df2$Regrade < 3)
dim(df2)
df2<-subset(df2,df2$Visco> -5 & df2$Visco < 6)
dim(df2)

##Rolling window M1 regression##
###prediction
df4Train<-df2[1:49,]
df4Test<-df2[50:nrow(df2),]
df4 <- rbind(df4Train,df4Test)
list1 <- matrix(nrow=(nrow(df2)-50),ncol = 3)
list2<-matrix(nrow=(nrow(df2)-50),ncol=1)
list3<-matrix(nrow=(nrow(df2)-50),ncol=1)
list4<-matrix(nrow=(nrow(df2)-50),ncol=1)
list5<-matrix(nrow=(nrow(df2)-50),ncol=1)
list6<-matrix(nrow=(nrow(df2)-50),ncol=1)
for (i in 1:(nrow(df2)-50))
{
  df5Train <- df4[(1+i):(49+i),]
  df5Test <-  df4[(50+i),]
  bestmodel <- lm(axl.osp~(Dubai.M1..M3+X92.Dubai+FO.M2..M3+Brent.M1..M3)-1, data = df5Train,weights=seq(1/1225,49/1225,by=1/1225))
  w <- abs(rstudent(bestmodel)) < 3 & abs(cooks.distance(bestmodel)) < 4/nrow(bestmodel$model)
  bestmodel <- update(bestmodel, weights=as.numeric(w)*seq(1/1225,49/1225,by=1/1225))
  list1[i,] <- predict(bestmodel,newdata=df5Test,interval='predict')
  list2[i,]<-t(df5Test$axl.osp)
  list3[i,]<-t(df5Test$index)
  list4[i,]<-abs(list1[i,1]-list2[i,])
  list5[i,]<-t(df5Test$Dubai.M1..M3)
  list6[i,]<-t(df5Test$X92.Dubai)
}

c25 <- as.list(bestmodel$coefficients)
c26 <- as.list(c(df5Test$Dubai.M1..M3, df5Test$X92.Dubai, df5Test$FO.M2..M3, df5Test$Brent.M1..M3))
c27 <- c("Dubai M1-M3", "92 - Dubai", "FO M2-M3", "Brent M1-M3")
df12 <- as.data.frame(cbind(c27,c25,c26))
df12$c25 <- round(as.numeric(df12$c25),2)
df12$c26 <- round(as.numeric(df12$c26),2)
df12$Contribution <- df12$c25 * df12$c26
df12$Contribution <- round(df12$Contribution,2)
colnames(df12) <- c("Varibales","Coefficient", "MoM change", "Contribution")
c28 <- c("Prediction", "","","")
df12 <- rbind(df12,c28)
rownames(df12)[5] <- "Prediction"
df12[5,4] <- as.numeric(df12$Contribution[1]) + as.numeric(df12$Contribution[2]) + 
             as.numeric(df12$Contribution[3]) + as.numeric(df12$Contribution[4])  

output$Predtable4 <- renderTable(df12)

c29 <- c("Announcement Month", "Predicted", "Realised")
c30 <- df1$Month[(nrow(df1)-6):(nrow(df1)-1)]
c31 <- list1[(nrow(list1)-6):(nrow(list1)-1),1]
c32 <- list2[(nrow(list2)-6):(nrow(list2)-1),1]
df13 <- as.data.frame(cbind(c30,c31,c32))
df13$c31 <- unfactor(df13$c31)
df13$c31 <- round(df13$c31,2)
df13$c32 <- unfactor(df13$c32)
df13$c32 <- round(df13$c32,2)
colnames(df13) <- c29
df13$Difference <- df13$Predicted - df13$Realised
df13$`Abs difference` <- abs(df13$Difference) 

output$Prevpred4 <- renderTable(df13)

####Plots
p12 <- ggplot(df5Train, aes(x= X92.Dubai, y= Extra.Light.Crude.Saudi.Asia)) + 
  geom_smooth(method="lm", fullrange=TRUE, se = FALSE) + labs(x = "X92 - Dubai MoM"  
      , y = "Arab Extra Light OSP MoM", title = "Scatter plot w X92-Dubai" )+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "bottom")+ geom_point()

p13 <- ggplot(df5Train, aes(x= Dubai.M1..M3, y= Extra.Light.Crude.Saudi.Asia)) + 
  geom_smooth(method="lm", fullrange=TRUE, se = FALSE) + labs(x = "Dubai M1-M3 MoM"  
    , y = "Arab Extra Light OSP MoM", title = "Scatter plot w Dubai M1-M3" )+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "bottom")+ geom_point()

p14 <- ggplot(df5Train, aes(x= Brent.M1..M3, y= Extra.Light.Crude.Saudi.Asia)) + 
  geom_smooth(method="lm", fullrange=TRUE, se = FALSE) + labs(x = "Brent M1-M3 MoM"  
  , y = "Arab Extra Light OSP MoM", title = "Scatter plot w Brent M1-M3" )+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "bottom")+ geom_point()

p15 <- ggplot(df5Train, aes(x= FO.M2..M3, y= Extra.Light.Crude.Saudi.Asia)) + 
  geom_smooth(method="lm", fullrange=TRUE, se = FALSE) + labs(x = "FO M2-M3 MoM"  
  , y = "Arab Extra Light OSP MoM", title = "Scatter plot w FO M2-M3" )+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "bottom")+ geom_point()

output$Plot12 <- renderPlot(p12)
output$Plot13 <- renderPlot(p13)
output$Plot14 <- renderPlot(p14)
output$Plot15 <- renderPlot(p15)

}
