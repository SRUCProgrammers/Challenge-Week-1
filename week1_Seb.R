working_dir<-"C:/Users/Sebastian/Dropbox/Programming_club/"
setwd(working_dir)
file1<-"Week1Data.txt"
week1<-read.table(file1, head=TRUE, sep=",")
week1<-readLines(file1)
week1a<-gsub(",,", ",", week1)
cat(week1a, sep="\n", file="week1_clean.txt")
week1_cl<-read.table("week1_clean.txt", head=TRUE, sep=",", na.strings=c("-999", "NaN"))

#### basic stats
apply(week1_cl, 2, mean, na.rm=TRUE)
apply(week1_cl, 2, sd, na.rm=TRUE)

#### histograms
Ntraits<-ncol(week1_cl)
for(i in 1:Ntraits){
    if(i==1){
	   rel_graph<-data.frame(rel=week1_cl[,i], trait=colnames(week1_cl)[i])
	}else{
       rel_graph<-rbind(rel_graph, data.frame(rel=week1_cl[,i], trait=colnames(week1_cl)[i]))
	}
}
library(ggplot2)
pic_width=15
pic_height=10
x_lable=""
y_lable="Density"
fileout<-"week1.png"
png(fileout, width=pic_width, height=pic_height, units="cm", res=300)#, pointsize=pt_size)
print({
ggplot(rel_graph, aes(rel, fill = trait)) + 
geom_density(alpha = 0.2)+
#geom_histogram(alpha = 0.4)
xlab(x_lable)+ 
  ylab(y_lable)+
  theme(axis.title.x = element_text(face="bold", colour="black", size=12, vjust=-0.05), # x axis title
        axis.title.y = element_text(face="bold", colour="black", size=12),  # y axis title
        axis.text.x  = element_text(angle=0, vjust=0.5, size=12, colour="black"),             # x axis lables
	    axis.text.y  = element_text(angle=0, vjust=0.5, size=12, colour="black"),
		axis.line = element_line(colour = "black"))
})
dev.off()


####
