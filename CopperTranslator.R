#examine differences between total recoverable and dissolved fractions of metals, see if a translator is feasible

library(readxl)
library(tidyverse)
library(AWQMSdata)
library(dplyr)
library(ggplot2)
library(quantreg)
source("//deqhq1/abrits/GitHub/ShinyNPDES_AWQMS/NPDES_AWQMSQuery.R")
source("DissVsTotFunction.R")

#pull all metals data from last 20 years
metals1<-NPDES_AWQMS_Qry(startdate="2000-01-01",
                         char=c("Copper","Lead","Zinc","Cadmium","Chromium","Selenium","Silver","Nickel"))

#create copy that we can play with more while leaving an original pull to compare to
metals<-metals1

#convert all to ug/L
metals<-unit_conv(metals,c("Copper","Lead","Zinc","Cadmium","Chromium","Selenium","Silver","Nickel"),"mg/l","ug/l")

#remove any ND data, can't really compare percents when results are ND 
metals<-subset(metals, metals$Result_Operator=="=")

#remove data without MDL or MRL, unsure if they are ND or not - 
#unfortunately this removes all of portland harbor's data, but they didn't include MDL or MRL, so I just don't know if it's ND or not
metals<-subset(metals, !(is.na(metals$MRLValue))& !(is.na(metals$MDLValue)))

#remove data where result numeric equals MDL, it is likely ND...
metals<-subset(metals,!(metals$Result_Numeric==metals$MDLValue))

#remove any estimated data...either QA/QC issue or J flag (which is low enough that the value is questionable)
metals<-subset(metals,metals$Result_Type=="Actual")


#cannibalize the Dissolved vs Total Recoverable code from EDD quality control check script to compare total and dissolved copper
#get data with fraction outlined
cop<-subset(metals,metals$Sample_Fraction %in% c("Total Recoverable","Dissolved","Total")& Char_Name %in% "Copper")
lead<-subset(metals,metals$Sample_Fraction %in% c("Total Recoverable","Dissolved","Total")& Char_Name %in% "Lead")
zinc<-subset(metals,metals$Sample_Fraction %in% c("Total Recoverable","Dissolved","Total")& Char_Name %in% "Zinc")
cad<-subset(metals,metals$Sample_Fraction %in% c("Total Recoverable","Dissolved","Total")& Char_Name %in% "Cadmium")
chrom<-subset(metals,metals$Sample_Fraction %in% c("Total Recoverable","Dissolved","Total")& Char_Name %in% "Chromium")
sel<-subset(metals,metals$Sample_Fraction %in% c("Total Recoverable","Dissolved","Total")& Char_Name %in% "Selenium")
silver<-subset(metals,metals$Sample_Fraction %in% c("Total Recoverable","Dissolved","Total")& Char_Name %in% "Silver")
nick<-subset(metals,metals$Sample_Fraction %in% c("Total Recoverable","Dissolved","Total")& Char_Name %in% "Nickel")

#run through percent function
copper<-PercDiss(cop)
leadper<-PercDiss(lead)
zincper<-PercDiss(zinc)
cadper<-PercDiss(cad)
chromper<-PercDiss(chrom)
selper<-PercDiss(sel)
silverper<-PercDiss(silver)
nickper<-PercDiss(nick)

#put them all back together for easier analysis
metals2<-rbind(copper,leadper,zincper,cadper,chromper,selper,silverper,nickper)

#for copper, lead, Chromium, and zinc it looks like we have enough data points to try to split the results by Huc8
#note that we are currently missing some HUC8 information for data submitted by permittees, 
#working with Lesley and Sarah to try to get that solved since the information should be in AWQMS 
metres<-metals2%>% group_by(HUC8_Name,Char_Name,MonLocType) %>%
  summarise(sample=n(), mean=mean(perc),median=median(perc),stdev=sd(perc),ninetyth=quantile(perc, probs=.9),tenth=quantile(perc, probs=.1))

metres<-subset(metres,MonLocType %in% c("River/Stream","Estuary"))

#run stats without HUC8 split to get a general overview
mettot<-metals2%>% group_by(Char_Name,MonLocType) %>%
  summarise(sample=n(), mean=mean(perc),median=median(perc),stdev=sd(perc),ninetyth=quantile(perc, probs=.9),tenth=quantile(perc, probs=.1))

#just get river/estuary
metsub<-subset(mettot,mettot$MonLocType %in% c("River/Stream","Estuary"))

#graphs
#remove canal transport, facility, other and lake- almost no data
metgraph<-subset(metals2,!(metals2$MonLocType %in% c("Lake","Canal Transport","Facility Other")))

ggplot(metgraph,aes(x=Char_Name, y=perc,fill=MonLocType,group=MonLocType))+
  geom_boxplot()+
  facet_wrap(~Char_Name, scale="free")+
  scale_y_continuous(limits=c(0,100))+
  #geom_text(aes(y=0,label=nrow(metgraph)))+
  labs(x="",y="Percent Dissolved")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#just do river/stream
metriv<-subset(metals2,metals2$MonLocType %in% c("River/Stream","Estuary"))

ggplot(metriv,aes(x=Char_Name, y=perc,fill=MonLocType))+
  geom_boxplot()+
  scale_y_continuous(limits=c(0,100))+
  labs(x="",y="Percent Dissolved")



