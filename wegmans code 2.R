length(intersect(unique(df$HH), unique(df3$HH)))

length(unique(df$HH))
length(unique(df1$HH))
length(unique(df2$HH))
length(unique(df3$HH))
length(unique(df$HH))


Aggnew$ATTR_E[Aggnew$ALTERNATIVE == 'T0' | Aggnew$ALTERNATIVE == 'T1'] = 1
Aggnew$ATTR_E[Aggnew$ALTERNATIVE =='T2'] = 0
Aggnew$ATTR_M[Aggnew$ALTERNATIVE == 'T0'] = 1
Aggnew$ATTR_M[Aggnew$ALTERNATIVE != 'T0'] = 0