df.E1 <- subset(df, EndPoint=='E1')
DF.E1 <- merge(df.E1, df1, by='HH')
DF.E1.MORE <- subset(DF.E1, ALTERNATIVE=='T0')
DF.E1.MORE.POST <- aggregate(UNITS+UNITS_POST~HH, data = DF.E1.MORE, FUN=sum)
length(unique(DF.E1$HH))

# E2 M:N- FOLLOWUP=MORE UNIT POST!=0 AND =0

df.E2 <- subset(df, EndPoint=='E2')
length(unique(df.E2$HH))
DF.E2 <- merge(df.E2, df1,by='HH')
DF.E2.MORE <- subset(DF.E2, ALTERNATIVE=='T0')
DF.E2.MORE.POST <- aggregate(UNITS_POST~HH, data = DF.E2.MORE, FUN=sum)
length(unique(DF.E2.MORE.POST$HH))#6

#UNITSPOST PER WEEK PER CUSTOMER
sum(DF.E2.MORE$UNITS_POST)/(1*10)#38.29

intersect(unique(df.E2$HH), unique(DF.E2.MORE$HH))
length(unique(DF.E2$HH))

#

DF1 <- merge(df, df1,by='HH')
DF1.E2 <- subset(DF1, M=='N-'&FOLLOWUP=='No change'&EndPoint=='E2'&ALTERNATIVE=='T0')
length(unique(DF1.E2$HH))


DF1.E3 <- subset(DF1, M=='N-'&FOLLOWUP=='More'&EndPoint=='E2'&ALTERNATIVE=='T0')
length(unique(DF1.E2$HH))
DF1.E2 <- subset(DF1, M=='N-')


# E4 M:N- FOLLOWUP=MORE UNIT POST!=0 AND =0

df.E4 <- subset(df, M=='N-'&EndPoint=='E4')
length(unique(df.E4$HH))
DF.E4 <- merge(df.E4, df1,by='HH')
length(unique(DF.E4$HH))
DF.E4.MORE <- subset(DF.E4, ALTERNATIVE=='T0')
DF.E4.MORE.POST <- aggregate(UNITS_POST~HH, data = DF.E4.MORE, FUN=sum)
length(unique(DF.E4.MORE.POST$HH))#6

#UNITSPOST PER WEEK PER CUSTOMER
sum(DF.E4.MORE$UNITS_POST)/(9*10)#38.29

length(intersect(unique(df1$HH), unique(df$HH)))
length(unique(DF.E4$HH))


# E6 M:N- FOLLOWUP=MORE UNIT POST!=0 AND =0

df.E6 <- subset(df, M=='N-'&FOLLOWUP=='More'&EndPoint=='E6')
length(unique(df.E6$HH))
DF.E6 <- merge(df.E6, df1,by='HH')
length(unique(DF.E6$HH))
DF.E6.MORE <- subset(DF.E6, ALTERNATIVE=='T0')
DF.E6.MORE.POST <- aggregate(UNITS_POST~HH, data = DF.E6.MORE, FUN=sum)
length(unique(DF.E6.MORE.POST$HH))#6

#UNITSPOST PER WEEK PER CUSTOMER
sum(DF.E6.MORE$UNITS_POST)/(9*10)#38.29

intersect(unique(df.E4$HH), unique(DF.E4.MORE$HH))
length(unique(DF.E4$HH))







#
DF1 <- merge(df, df1,by='HH', all.y = TRUE)
DF1.E1 <- subset(DF1,EndPoint=='E1'& ALTERNATIVE=='T0')
DF1.E2 <- subset(DF1,EndPoint=='E2')
DF1.E3 <- subset(DF1,EndPoint=='E3'& ALTERNATIVE=='T0')
DF1.E4 <- subset(DF1,EndPoint=='E4')
DF1.E5 <- subset(DF1,EndPoint=='E5'& ALTERNATIVE=='T0')
DF1.E6 <- subset(DF1,EndPoint=='E6')

length(unique(DF1.E1$HH))
length(unique(DF1.E2$HH))
length(unique(DF1.E3$HH))
length(unique(DF1.E4$HH))
length(unique(DF1.E5$HH))
length(unique(DF1.E6$HH))

length(DF1[DF1$EndPoint=="E1",])+length(DF1[DF1$EndPoint=="E2",])+length(DF1[DF1$EndPoint=="E3",])+length(DF1[DF1$EndPoint=="E4",])+length(DF1[DF1$EndPoint=="E5",])+length(DF1[DF1$EndPoint=="E6",])


#
DF1 <- merge(df, df1,by='HH')
DF1.E2 <- subset(DF1, M!='N-'&EndPoint=='E2'&ALTERNATIVE=='T0')
length(unique(DF1.E2$HH))
DF1.E6.AGG <- aggregate(UNITS_POST~HH, DF1.E6, FUN=sum)

#
summary(DF)




