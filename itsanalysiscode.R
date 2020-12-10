library(ggplot2)
library(dplyr)
library(nlme)
library(car)
library(seasonal)
df<-read.csv("C:/Users/tonynickonchuk/Desktop/uccsv.csv",stringsAsFactors = F)

seq<-c(1,seq(3,54,by=3))
ggplot(df,aes(x=time,y=cultures,col=site))+geom_line(size=1)+ggtitle("Urine Cultures per 1000 ED Visits")+labs(x="Date",y="# of Cultures")+
  scale_x_continuous(breaks=seq,labels=df$date_lab[seq])+geom_vline(xintercept=25,linetype="dashed")+
  scale_color_discrete(name="Site",breaks=c("fmc","control"),labels=c("Foothills","Control Sites"))+annotate("text",size=5,x=25,y=10,hjust=0,vjust=-0.2,angle=90,label="Start of Intervention")

df$wp<-c(rep(0,24),1,rep(0,49))

p0<-lm(cultures~time+fmc+fmctime+level+trend+fmclevel+fmctrend+wp,data=df)
summary(p0)

df$wp<-0
df$fitted<-predict(p0,df)
counter<-df
counter$fmclevel<-0
counter$fmctrend<-0
df$counter<-predict(p0,counter)

ggplot(df,aes(x=time,y=cultures,col=site))+geom_point()+geom_vline(xintercept=25,linetype="dashed")+labs(x="Month",y="# of Cultures")+
  ggtitle("Time Series Visualization of Urine Cultures per 1000 ED Visits",subtitle="Dashed teal line=Counterfactual scenario")+scale_x_continuous(breaks=seq,labels=df$date_lab[seq])+
  scale_color_discrete(name="Site",breaks=c("fmc","control"),labels=c("Foothills","Control Sites"))+annotate("text",x=25,y=10,angle=90,hjust=0,vjust=-0.2,label="Start of Intervention",size=5)+
  annotate("segment",x=1,xend=24,y=df$fitted[1],yend=df$fitted[24],col="#00BFC4",size=1)+
  annotate("segment",x=25,xend=37,y=df$fitted[25],yend=df$fitted[37],col="#00BFC4",size=1)+
  annotate("segment",x=1,xend=24,y=df$fitted[38],yend=df$fitted[61],col="#F8766D",size=1)+
  annotate("segment",x=25,xend=37,y=df$fitted[62],yend=df$fitted[74],col="#F8766D",size=1)+
  annotate("segment",x=25,xend=37,y=df$counter[25],yend=df$counter[37],col="#00BFC4",linetype="dashed",size=1)


diff<-df$counter[df$site=="fmc"&df$level==1]-df$fitted[df$site=="fmc"&df$level==1]
sum(diff)
diff

fitted<-predict(p0,df,se.fit=T)
fitted<-as.data.frame(fitted)
fitted<-fitted[26:37,]
fitted$ed_pred<-c(7143,6968,6835,6993,6720,7297,7122,6630,6018,5011,6115,6459)/1000
ctr<-predict(p0,counter,se.fit=T)
ctr<-as.data.frame(ctr)
ctr<-ctr[26:37,]
ctr$ed_pred<-fitted$ed_pred

fitted$upr<-fitted$fit+1.96*fitted$se.fit
fitted$lwr<-fitted$fit-1.96*fitted$se.fit
ctr$upr<-ctr$fit+1.96*ctr$se.fit
ctr$lwr<-ctr$fit-1.96*ctr$se.fit
fitted$upr<-fitted$upr*fitted$ed_pred
fitted$lwr<-fitted$lwr*fitted$ed_pred
ctr$upr<-ctr$upr*ctr$ed_pred
ctr$lwr<-ctr$lwr*ctr$ed_pred

maxdiff<-ctr$upr-fitted$lwr
maxdiff<-sum(maxdiff)
mindiff<-ctr$lwr-fitted$upr
mindiff<-sum(mindiff)
diff<-ctr$fit-fitted$fit
sum(diff)
diff<-(ctr$fit*ctr$ed_pred)-(fitted$fit*fitted$ed_pred)
sum(diff)
