rm(list=ls())
setwd("D:/Simon Business School/Marketing Research/Wegmans case")
getwd()
df <- read.csv("Survey Data FINAL.csv")
df1<-read.delim("txdata_insiders.txt", header = TRUE,sep = '|')
df2<-read.delim("txdata_othertop30.txt", header = TRUE,sep = '|')
df3<-read.delim("custdata.txt", header = TRUE,sep = '|')

#1.a)
DF<-merge(df,df1, by='HH')
B <- DF[DF$ALTERNATIVE=='T0',]

df[df$HH=='1641488',]$EndPoint
df[df$HH=='2395652',]$EndPoint
df[df$HH=='6810667',]$EndPoint

B[B$M=='N-',]
print(B[B$M=='N-','HH'])
df1[df1$HH=='1641488'&df1$ALTERNATIVE!='N',]
sum(df1[df1$HH=='1641488'&df1$ALTERNATIVE=='T0','UNITS'])
sum(df1[df1$HH=='1641488'&df1$ALTERNATIVE=='T1','UNITS'])
sum(df1[df1$HH=='1641488'&df1$ALTERNATIVE=='T2','UNITS'])
df1[df1$HH=='2395652'&df1$ALTERNATIVE!='N',]
sum(df1[df1$HH=='2395652'&df1$ALTERNATIVE=='T0','UNITS'])
sum(df1[df1$HH=='2395652'&df1$ALTERNATIVE=='T1','UNITS'])
sum(df1[df1$HH=='2395652'&df1$ALTERNATIVE=='T2','UNITS'])
df1[df1$HH=='6810667'&df1$ALTERNATIVE!='N',]
sum(df1[df1$HH=='6810667'&df1$ALTERNATIVE=='T0','UNITS'])
sum(df1[df1$HH=='6810667'&df1$ALTERNATIVE=='T1','UNITS'])
sum(df1[df1$HH=='6810667'&df1$ALTERNATIVE=='T2','UNITS'])
B1 <- DF[DF$ALTERNATIVE=='T1',]
B1[B1$E=='N-',]
print(B1[B1$E=='N-','HH'])
df1[df1$HH=='6643819'&df1$ALTERNATIVE!='N',]
sum(df1[df1$HH=='6643819'&df1$ALTERNATIVE=='T0','UNITS'])
sum(df1[df1$HH=='6643819'&df1$ALTERNATIVE=='T1','UNITS'])
sum(df1[df1$HH=='6643819'&df1$ALTERNATIVE=='T2','UNITS'])

#1.b
E3E5<-df[df$EndPoint %in% c('E3','E5'),]
