#--------part1----------
df[df$M=='N-',]
head(DF)
DF <- merge(df,df1,by='HH')


rm(list=ls())
setwd("/Users/Apple/Desktop/simon/2018Winter/Marketing research using r/homework7/wegmans_data")
getwd()

df<-read_excel("Survey Data FINAL.xlsx")
df1<-read.delim("txdata_insiders.txt", header = TRUE,sep = '|')
df2<-read.delim("txdata_othertop30.txt", header = TRUE,sep = '|')
df3<-read.delim("custdata.txt", header = TRUE,sep = '|')

#------------dataset----------
dfaa<-merge(df,df3,by="HH")
TwoChoice<-dfaa[duplicated(dfaa$HH)!=TRUE,'HH']
e<-intersect(TwoChoice,dfaa$HH)
dfaa<-dfaa[match(e,dfaa$HH),]
dfaa#在df1里有数据的HH
Agg<-aggregate(cbind(UNITS,WEEKS,UNITS_POST)~HH+ALTERNATIVE,sum,data=df1)
Aggnew<-Agg[Agg$ALTERNATIVE=="T0",]
Aggnew#Aggregate了df1里units和weeks和post 根据不同HH和ALTERNATIVE
Finaldata<-merge(dfaa,Aggnew,by="HH",all=TRUE)
Finaldata[,20]<-NA


for(i in 1:nrow(Finaldata)){
  if(Finaldata[i,'HH_SIZE'] %in% 1:3){
    Finaldata[i,'Size']=1
  }else{
    if(Finaldata[i,'HH_SIZE'] %in% 4:6){
      Finaldata[i,'Size']=2
    }else{
      Finaldata[i,'Size']=3
    }
  }
}

Fdata<-Finaldata[,c(1,2,5,8,9,16,17,19,21)]
Fdata1<-Fdata[!is.na(Fdata$UNITS),]
#3472/8850=39.2%
Fdata1<-split(Fdata1,f=Fdata1$DECILE)
for (i in 1:length(Fdata1)) {
  assign(paste0("FdataD", i), as.data.frame(Fdata1[[i]]))
}
#Decile1
FdataD1<-split(FdataD1,f=FdataD1$Size)
for (i in 1:length(FdataD1)) {
  assign(paste0("FdataD1S", i), as.data.frame(FdataD1[[i]]))
}
#Decile2
FdataD2<-split(FdataD2,f=FdataD2$Size)
for (i in 1:length(FdataD2)) {
  assign(paste0("FdataD2S", i), as.data.frame(FdataD2[[i]]))
}
#Decile3
FdataD3<-split(FdataD3,f=FdataD3$Size)
for (i in 1:length(FdataD3)) {
  assign(paste0("FdataD3S", i), as.data.frame(FdataD3[[i]]))
}




#-----------------FOR LOOPS------------------#
E3results <- rep(1, 9)
E5results <- rep(1, 9)
# # for loop E1
FdataDS<-list(FdataD1S1,FdataD1S2,FdataD1S3,FdataD2S1,FdataD2S2,FdataD2S3,
              FdataD3S1,FdataD3S2,FdataD3S3)
E1<-for (i in 1:length(FdataDS)) {
  E1results[i]=(sum(FdataDS[[i]][FdataDS[[i]]$EndPoint=='E1','UNITS'])+ sum(FdataDS[[i]][FdataDS[[i]]$EndPoint=='E1','UNITS_POST']))/
    (length(unique(FdataDS[[i]][FdataDS[[i]]$EndPoint=='E1','HH']))*55)*
    (length(unique(FdataDS[[i]][FdataDS[[i]]$EndPoint=='E1','HH']))/length(unique(FdataDS[[i]]$HH)))
}

# for loop E3
E3 <- for (i in 1:length(FdataDS)) {
  E3results[i]=sum(FdataDS[[i]][FdataDS[[i]]$EndPoint=='E3','UNITS_POST'])/
    (length(unique(FdataDS[[i]][FdataDS[[i]]$EndPoint=='E3','HH']))*10)*
    (length(unique(FdataDS[[i]][FdataDS[[i]]$EndPoint=='E3','HH']))/length(unique(FdataDS[[i]]$HH)))
}

# for loop E2, E4, E6
FdataDS<-list(FdataD1S1,FdataD1S2,FdataD1S3,FdataD2S1,FdataD2S2,FdataD2S3,
              FdataD3S1,FdataD3S2,FdataD3S3)
E1results <- rep(NA,9)
E246results <- rep(NA,9)
FdataDS.E246.More <- list(c(rep(NA,9)))
FdataDS.E246.More <- list(NA,NA,NA,NA,NA,NA,NA,NA,NA)
FdataDS.E246.NA <- list(NA,NA,NA,NA,NA,NA,NA,NA,NA)
FdataDS.E246 <- list(NA,NA,NA,NA,NA,NA,NA,NA,NA)

for (i in 1:length(FdataDS)){
  FdataDS.E246.More[[i]] <- FdataDS[[i]][(FdataDS[[i]]$EndPoint == "E2"|FdataDS[[i]]$EndPoint =="E4"|FdataDS[[i]]$EndPoint =="E6") & FdataDS[[i]]$M == "N-" & FdataDS[[i]]$FOLLOWUP == "More",]
  FdataDS.E246.NA[[i]] <- FdataDS[[i]][(FdataDS[[i]]$EndPoint == "E2"|FdataDS[[i]]$EndPoint =="E4"|FdataDS[[i]]$EndPoint =="E6") & FdataDS[[i]]$M == "N-" & FdataDS[[i]]$FOLLOWUP == "",]
  FdataDS.E246[[i]] <- FdataDS[[i]][(FdataDS[[i]]$EndPoint == "E2"|FdataDS[[i]]$EndPoint =="E4"|FdataDS[[i]]$EndPoint =="E6") & FdataDS[[i]]$M == "N-",]
  E246results[i] <- sum(FdataDS.E246.More[[i]]$UNITS_POST)/length(unique(FdataDS.E246.More[[i]]$HH))/10*(length(unique(FdataDS.E246.More[[i]]$HH))+length(unique(FdataDS.E246.NA[[i]]$HH))*(length(unique(FdataDS.E246.More[[i]]$HH))/(length(unique(FdataDS.E246[[i]]$HH))-length(unique(FdataDS.E246.NA[[i]]$HH))))/length(unique(FdataDS[[i]]$HH)))
}

# for loop E5
E5 <- for (i in 1:length(FdataDS)) {
  E5results[i]=sum(FdataDS[[i]][FdataDS[[i]]$EndPoint=='E5','UNITS_POST'])/
    (length(unique(FdataDS[[i]][FdataDS[[i]]$EndPoint=='E5','HH']))*10)*
    (length(unique(FdataDS[[i]][FdataDS[[i]]$EndPoint=='E5','HH']))/length(unique(FdataDS[[i]]$HH)))
}



#-------part 2 


