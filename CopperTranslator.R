#determine if copper translator is feasible

library(readxl)
library(tidyverse)


#upload data
data<-read_excel("C:/COVID-19 WORK/Copper-DATA-AWQMS-20201020.xlsx", sheet=3)

#cannibalize the Dissolved vs Total Recoverable code from EDD quality control check script to compare total and dissolved copper
#get data with fraction outlined (should be everything anyway)
data<-subset(data,data$Sample_Fraction %in% c("Total Recoverable","Dissolved","Total"))
  
#transform any non-detects to 0
data$Result<-ifelse(data$Result=="ND",0,data$Result)
data$Result<-ifelse(str_detect(data$Result,"<"),0,data$Result)

#transform Result column to number
data$Result<-as.numeric(data$Result)
    
    #get unique identifiers, put into new dataset
    new<-unique(subset(data,select=c("act_id","SampleStartDate","MLocID","Char_Name")))
    
    #select all dissolved
    dis<-subset(data,data$Sample_Fraction %in% "Dissolved")
    
    #select all total recoverable/total
    tot<-subset(data,data$Sample_Fraction %in% c("Total Recoverable","Total"))
    
    #add dissolved and total columns to new dataset
    new$dissolved<-dis$Result[match(new$act_id,dis$act_id,nomatch=NA)]
    new$total<-tot$Result[match(new$act_id,tot$act_id,nomatch=NA)]
    
    #calculate the difference, round to 3 figures
    new$diff<-round(new$total-new$dissolved,3)
    
    #calculate the RPD
    new$RPD<-abs(new$diff)/((new$total+new$dissolved)/2)*100
    
    #round the RPD value
    new$RPD<-round(new$RPD,2)
    
    

