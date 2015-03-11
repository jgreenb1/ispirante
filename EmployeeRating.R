###### Settings
options(scipen=10)
library(corrplot)
library(plyr)
setwd("C:/Users/Jon.INCUBET/Desktop/")
today<-as.Date(substr(Sys.time(),0,10))

###### Loading data
data<-read.csv("s1.csv")

## Min Tix Variable
min_tix<-10

###### Formatting data
## Removing Tickets with no assignee
data1<-data[data$assignee_name!="",]

data1$full_resolution_time_in_minutes_within_business_hours<-as.numeric(as.character(data1$full_resolution_time_in_minutes_within_business_hours))
data1$resolution_time<-as.numeric(as.character(data1$resolution_time))
data1$first_reply_time_in_minutes<-as.numeric(as.character(data1$first_reply_time_in_minutes))

###### Aggregate by Assignee
aggdata<-ddply(data1,.(assignee_name),summarise,
	TICKETS=length(assignee_name),
	FULL_RESOLVE=median(full_resolution_time_in_minutes_within_business_hours,na.rm=TRUE),
	RESOLVE=median(resolution_time,na.rm=TRUE),
	REPLY_TIME=median(first_reply_time_in_minutes,na.rm=TRUE),
	CLOSED=sum(status=="Closed"),
	SUCCESS_RATIO=sum(status=="Closed")/length(assignee_name))

## Minimum number of tickets required
aggdata<-aggdata[aggdata$TICKETS>=min_tix,]

###### Weighting the KPIs from -1 to 1
for (i in 2:ncol(aggdata))
	{
	aggdata<-cbind(aggdata,(ecdf(aggdata[,i])(aggdata[,i])-.5)/.5)
	colnames(aggdata)[ncol(aggdata)]<-paste0(colnames(aggdata)[i],"_WEIGHT")
	}

###### Adding Blank rows for corrplot()
aggdata1<-aggdata[,grep("WEIGHT",colnames(aggdata))]
rownames(aggdata1)<-aggdata$assignee_name
aggdata1$OVERALL_WEIGHT<-(ecdf(apply(aggdata1,1,mean))(apply(aggdata1,1,mean))-.5)/.5
aggdata1<-aggdata1[,c(ncol(aggdata1),1:(ncol(aggdata1)-1))]
colnames(aggdata1)<-c("Overall","# of Tickets","Resolution Time","Resolution Time2","1st Reply Time","Tickets Closed","Success Ratio")

aggdata2<-cbind(aggdata1,matrix(NA,nrow=nrow(aggdata1),ncol=nrow(aggdata1)-ncol(aggdata1)))
aggdata2<-as.matrix(aggdata2)

## Replacing NA with neutral or corrplot() won't run (DISCUSS WITH STEFANO)
aggdata3<-aggdata2
aggdata3[is.na(aggdata3)]<-0

###### Plotting
fileName<-paste("ispirante_",gsub("-","",today),".jpeg",sep="")

jpeg(file=fileName,quality=100,width=1200,height=800)
corrplot(aggdata3[,1:ncol(aggdata1)],method="circle",tl.col="black",
	cl.ratio=0.3,cl.align="r",title="ispirante Client Employee KPIs")
dev.off()

## Without bar on side
fileName1<-paste("ispirante1_",gsub("-","",today),".jpeg",sep="")

jpeg(file=fileName1,quality=100,width=1200,height=800)
corrplot(aggdata3[,1:ncol(aggdata1)],method="circle",tl.col="black",
	cl.pos="n",title="ispirante Client Employee KPIs")
dev.off()

############### NEXT STEPS
1) Find which variables are in ALL reports
2) Make a list of other variables that MIGHT be in reports
3) Fix things for the variables in all reports
4) Do examples for two samples with different variables (make sure flexible)






























aggdata[,grep("WEIGHT",colnames(aggdata))][1:10,]


###### Overall averages
overallmed<-apply(aggdata[,2:4],2,function(x)median(x[!is.na(x)]))
overall<-apply(aggdata[,2:4],2,function(x)quantile(x[!is.na(x)],seq(.25,.75,by=.25)))






####### QUESTIONS
1) Minimum # of tickets to display?
2) First resolution time in minutes? Full resolution time in minutes?  Reply time in minutes?
3) Within business hours vs not noted?
4) Closed vs Solved?







library(corrplot)

data(mtcars)
M <- cor(mtcars)
corrplot(M, method = "circle")

m1<-M
colnames(m1)<-rep("",length(colnames(m1)))
corrplot(m1, method = "circle",is.corr=FALSE)

m2<-m1
m2[1,1]<-.5
m2[2,2]<-.75
m2[3,3]<-.1

corrplot(m2, method = "circle",is.corr=FALSE)







##the input matrix is not square
M

corrplot(M[1:8,])
corrplot(M[,1:8])
cor.mtest <- function(mat, conf.level = 0.95){
mat <- as.matrix(mat)
n <- ncol(mat)
p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
diag(p.mat) <- 0
diag(lowCI.mat) <- diag(uppCI.mat) <- 1
for(i in 1:(n-1)){
for(j in (i+1):n){
tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
}
}
return(list(p.mat, lowCI.mat, uppCI.mat))
}
res1 <- cor.mtest(mtcars,0.95)