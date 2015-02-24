# Plot week 1 data Fortran output

Week1data_OUT<-read.table("C:/Users/Sdenholm/Desktop/week1data_OUT.txt",header=F)
attach(Week1data_OUT)
names(Week1data_OUT)
Week1data_OUT[Week1data_OUT == -999] <- NA

par(mfrow=c(2,2))
hist(na.omit(Week1data_OUT$V1), xlab="number", main='Apples', breaks = 30)
hist(na.omit(Week1data_OUT$V2), xlab="number", main='Oranges', breaks = 30)
hist(na.omit(Week1data_OUT$V3), xlab="number",main='Control', breaks = 30)
