library(readxl)
library(tidyverse)
library(AWQMSdata)
source("//deqhq1/abrits/GitHub/ShinyNPDES_AWQMS/NPDES_AWQMSQuery.R")

#download all cyanide and free cyanide from last 20 years
cyn<-NPDES_AWQMS_Qry(startdate="2000-01-01",char=c("Cyanide","Cyanides amenable to chlorination (HCN & CN)"))

#convert all to ug/L
cyn<-unit_conv(cyn,c("Cyanide","Cyanides amenable to chlorination (HCN & CN)"),"mg/l","ug/l")

#cannibalize dissolved vs total recoverable function from EDD data checks to compare total recoverable arsenic with inorganic arsenic

#remove data without MDL or MRL, unsure if they are ND or not - 

cyn<-subset(cyn, !(is.na(cyn$MRLValue))& !(is.na(cyn$MDLValue)))

#remove data where result numeric equals MDL, it is likely ND...
cyn<-subset(cyn,!(cyn$Result_Numeric==cyn$MDLValue))

#remove any estimated data...either QA/QC issue or J flag (which is low enough that the value is questionable)
cyn<-subset(cyn,cyn$Result_Type=="Actual")

#create new identifier out of date and characteristic name
#(used to use activity ID, but a lot of the labs have been using multiple activity IDs per batch 
#(e.g. unfilterd sample is xx-001A and filtered sample is xx-001B))
#use date and location since it is extremely rare for a permittee to be taking more than one sample a day at each location for metals data
cyn$comb<-paste0(cyn$SampleStartDate,",",cyn$MLocID)

#get unique identifiers, put into new dataset
newcyn<-unique(subset(cyn,select=c("act_id","SampleStartDate","MLocID","MonLocType","Org_Name","Char_Name","comb")))

#select all cyanide
totcyn<-subset(cyn,cyn$Char_Name %in% "Cyanide")

#select all free cyanide
freecyn<-subset(cyn,cyn$Char_Name %in% "Cyanides amenable to chlorination (HCN & CN)")

#add arsenic and inorganic arsenic to new dataset
newcyn$totcyn<-totcyn$Result_Numeric[match(newcyn$comb,totcyn$comb,nomatch=NA)]
newcyn$freecyn<-freecyn$Result_Numeric[match(newcyn$comb,freecyn$comb,nomatch=NA)]

#add MDL value
newcyn$totcynMDL<-totcyn$MDLValue[match(newcyn$comb,totcyn$comb,nomatch=NA)]
newcyn$freecynMDL<-freecyn$MDLValue[match(newcyn$comb,freecyn$comb,nomatch=NA)]


#calculate the difference, round to 3 figures
newcyn$diff<-round(newcyn$totcyn-newcyn$freecyn,3)

#calculate the percent of arsenic that is inorganic arsenic
newcyn$percfree<-(newcyn$freecyn/newcyn$totcyn)*100

#round the percentage
newcyn$percfree<-round(newcyn$percfree,2)

#remove all data points where there is no comparison, just keep arsenic so we don't have duplicate rows
newcyn<-subset(newcyn, !(is.na(newcyn$percfree))& newcyn$Char_Name %in% "Cyanide")


#run stats 
cyntot<-newcyn%>% group_by(Char_Name,MonLocType) %>%
  summarise(sample=n(), mean=mean(percfree),median=median(percfree),stdev=sd(percfree),ninetyth=quantile(percfree, probs=.9),tenth=quantile(percfree, probs=.1))

ggplot(newcyn,aes(x=Char_Name, y=percfree,fill=MonLocType,group=MonLocType))+
  geom_boxplot()+
  facet_wrap(~Char_Name, scale="free")+
  scale_y_continuous(limits=c(0,100))+
  #geom_text(aes(y=0,label=nrow(metgraph)))+
  labs(x="",y="Percent Free")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
