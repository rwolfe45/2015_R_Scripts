#repeated measures anova to measure statistical significance of top 30 OTUs in rank abundance curve

#load data
Rank_ranova<-read.table(file.choose(), header=TRUE, sep="\t", check.names=TRUE)
#This table needs to have sample names on the right and corresponding OTUs and abundances in the following columns
#Additionally, in the same table, need to have factors to group repeated measures by.
rownames(Rank_ranova)<-Rank_ranova[,1]
Rank_ranova<-Rank_ranova[-1]
#establish factors
Diet<-Rank_ranova$Treatment
Samples<-Rank_ranova$Sample
Timepoint<-Rank_ranova$Timepoint
#The way we are going to do this is to establish each OTU as a vector and then measure the significance by repeated measures.
#We are grouping the samples by diet (spring, summer, winter) and correcting for timepoint and diet.
#Command: aov.out1<-aov(OTU1~Diet +Error(Timepoint/Diet), data=Rank_ranova)
#Summary will tell us results. summary(aov.out)
#Following is establishing all vectors and summaries.
#If the OTU was significant, the summary output is saved here.

OTU1<-Rank_ranova[,3]
OTU2<-Rank_ranova[,4]
OTU3<-Rank_ranova[,5]
OTU4<-Rank_ranova[,6]
OTU5<-Rank_ranova[,7]
OTU6<-Rank_ranova[,8]
OTU7<-Rank_ranova[,9]
OTU8<-Rank_ranova[,10]
OTU9<-Rank_ranova[,11]
OTU10<-Rank_ranova[,12]
OTU11<-Rank_ranova[,13]
OTU12<-Rank_ranova[,14]
OTU13<-Rank_ranova[,15]
OTU14<-Rank_ranova[,16]
OTU15<-Rank_ranova[,17]
OTU16<-Rank_ranova[,18]
OTU17<-Rank_ranova[,19]
OTU18<-Rank_ranova[,20]
OTU19<-Rank_ranova[,21]
OTU20<-Rank_ranova[,22]
OTU21<-Rank_ranova[,23]
OTU22<-Rank_ranova[,24]
OTU23<-Rank_ranova[,25]
OTU24<-Rank_ranova[,26]
OTU25<-Rank_ranova[,27]
OTU26<-Rank_ranova[,28]
aov.out1<-aov(OTU1~Diet +Error(Timepoint/Diet), data=Rank_ranova)
aov.out2<-aov(OTU2~Diet +Error(Timepoint/Diet), data=Rank_ranova)
aov.out3<-aov(OTU3~Diet +Error(Timepoint/Diet), data=Rank_ranova)
aov.out4<-aov(OTU4~Diet +Error(Timepoint/Diet), data=Rank_ranova)
aov.out5<-aov(OTU5~Diet +Error(Timepoint/Diet), data=Rank_ranova)
aov.out6<-aov(OTU6~Diet +Error(Timepoint/Diet), data=Rank_ranova)
aov.out7<-aov(OTU7~Diet +Error(Timepoint/Diet), data=Rank_ranova)
aov.out8<-aov(OTU8~Diet +Error(Timepoint/Diet), data=Rank_ranova)
aov.out9<-aov(OTU9~Diet +Error(Timepoint/Diet), data=Rank_ranova)
aov.out10<-aov(OTU10~Diet +Error(Timepoint/Diet), data=Rank_ranova)
aov.out11<-aov(OTU11~Diet +Error(Timepoint/Diet), data=Rank_ranova)
aov.out12<-aov(OTU12~Diet +Error(Timepoint/Diet), data=Rank_ranova)
aov.out13<-aov(OTU13~Diet +Error(Timepoint/Diet), data=Rank_ranova)
aov.out14<-aov(OTU14~Diet +Error(Timepoint/Diet), data=Rank_ranova)
aov.out15<-aov(OTU15~Diet +Error(Timepoint/Diet), data=Rank_ranova)
aov.out16<-aov(OTU16~Diet +Error(Timepoint/Diet), data=Rank_ranova)
aov.out17<-aov(OTU17~Diet +Error(Timepoint/Diet), data=Rank_ranova)
aov.out18<-aov(OTU18~Diet +Error(Timepoint/Diet), data=Rank_ranova)
aov.out19<-aov(OTU19~Diet +Error(Timepoint/Diet), data=Rank_ranova)
aov.out20<-aov(OTU20~Diet +Error(Timepoint/Diet), data=Rank_ranova)
aov.out21<-aov(OTU21~Diet +Error(Timepoint/Diet), data=Rank_ranova)
aov.out22<-aov(OTU22~Diet +Error(Timepoint/Diet), data=Rank_ranova)
aov.out23<-aov(OTU23~Diet +Error(Timepoint/Diet), data=Rank_ranova)
aov.out24<-aov(OTU24~Diet +Error(Timepoint/Diet), data=Rank_ranova)
aov.out25<-aov(OTU25~Diet +Error(Timepoint/Diet), data=Rank_ranova)
aov.out26<-aov(OTU26~Diet +Error(Timepoint/Diet), data=Rank_ranova)
summary(aov.out1)
summary(aov.out2)

summary(aov.out3)
#significant#Error: Timepoint
#Df    Sum Sq   Mean Sq F value Pr(>F)
#Diet       1 0.0002768 0.0002768   0.178  0.746
#Residuals  1 0.0015510 0.0015510               

#Error: Timepoint:Diet
#Df   Sum Sq  Mean Sq F value Pr(>F)  
#Diet       1 0.007345 0.007345   35.41 0.0271 *
  #Residuals  2 0.000415 0.000207                 
---
 # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Error: Within
#Df   Sum Sq  Mean Sq F value Pr(>F)
#Residuals  5 0.008056 0.001611           

summary(aov.out4)
summary(aov.out5)

#significantError: Timepoint
#Df   Sum Sq  Mean Sq F value Pr(>F)
#Diet       1 0.003300 0.003300   9.474    0.2
#Residuals  1 0.000348 0.000348               

#Error: Timepoint:Diet
#Df   Sum Sq  Mean Sq F value Pr(>F)  
#Diet       1 0.014688 0.014688   29.73  0.032 *
 # Residuals  2 0.000988 0.000494                 
---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Error: Within
#Df   Sum Sq  Mean Sq F value Pr(>F)
#Residuals  5 0.002905 0.000581    

summary(aov.out6)
summary(aov.out7)
summary(aov.out8)
summary(aov.out9)

#Error: Timepoint
#Df    Sum Sq   Mean Sq F value Pr(>F)  
#Diet       1 7.408e-05 7.408e-05   242.4 0.0408 *
  #Residuals  1 3.100e-07 3.100e-07                 
---
 # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Error: Timepoint:Diet
#Df   Sum Sq  Mean Sq F value  Pr(>F)   
#Diet       1 1.31e-03 1.31e-03   128.2 0.00771 **
  #Residuals  2 2.04e-05 1.02e-05                   
---
 # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Error: Within
#Df    Sum Sq   Mean Sq F value Pr(>F)
#Residuals  5 2.958e-05 5.917e-06   

summary(aov.out10)

#Error: Timepoint
#Df    Sum Sq   Mean Sq F value Pr(>F)
#Diet       1 3.300e-08 3.300e-08   0.003  0.964
#Residuals  1 1.017e-05 1.017e-05               

#Error: Timepoint:Diet
#Df    Sum Sq   Mean Sq F value Pr(>F)  
#Diet       1 0.0003525 0.0003525   58.67 0.0166 *
  #Residuals  2 0.0000120 0.0000060                 
---
 # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Error: Within
#Df    Sum Sq   Mean Sq F value Pr(>F)
#Residuals  5 6.084e-05 1.217e-05  
summary(aov.out11)
summary(aov.out12)
summary(aov.out13)
summary(aov.out14)

#Error: Timepoint
#Df   Sum Sq  Mean Sq F value Pr(>F)  
#Diet       1 7.14e-05 7.14e-05    2565 0.0126 *
  #Residuals  1 3.00e-08 3.00e-08                 
---
 # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Error: Timepoint:Diet
#Df    Sum Sq  Mean Sq F value Pr(>F)
#Diet       1 7.700e-07 7.70e-07   0.017  0.909
#Residuals  2 9.239e-05 4.62e-05               

#Error: Within
#Df    Sum Sq  Mean Sq F value Pr(>F)
#Residuals  5 0.0003185 6.37e-05               

summary(aov.out15)
summary(aov.out16)
summary(aov.out17)
summary(aov.out18)
summary(aov.out19)
summary(aov.out20)
summary(aov.out21)


#Error: Timepoint
#Df    Sum Sq   Mean Sq F value Pr(>F)
#Diet       1 1.975e-04 1.975e-04   32.95   0.11
#Residuals  1 5.990e-06 5.990e-06               

#Error: Timepoint:Diet
#Df    Sum Sq   Mean Sq F value  Pr(>F)   
#Diet       1 0.0031474 0.0031474   138.9 0.00712 **
 # Residuals  2 0.0000453 0.0000227                   
---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Error: Within
#Df    Sum Sq   Mean Sq F value Pr(>F)
#Residuals  5 0.0001537 3.073e-05   
summary(aov.out22)



#Error: Timepoint
#Df    Sum Sq   Mean Sq F value Pr(>F)  
#Diet       1 4.107e-05 4.107e-05   742.7 0.0233 *
 # Residuals  1 6.000e-08 6.000e-08                 
---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Error: Timepoint:Diet
#Df    Sum Sq   Mean Sq F value Pr(>F)
#Diet       1 2.176e-04 2.176e-04   7.974  0.106
#Residuals  2 5.459e-05 2.729e-05               

#Error: Within
#Df    Sum Sq   Mean Sq F value Pr(>F)
#Residuals  5 0.0001064 2.128e-05    



summary(aov.out23)
summary(aov.out24)
summary(aov.out25)
summary(aov.out26)
#Error: Timepoint
#Df    Sum Sq   Mean Sq F value Pr(>F)  
#Diet       1 0.0016673 0.0016673   193.9 0.0456 *
#  Residuals  1 0.0000086 0.0000086                 
---
 # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Error: Timepoint:Diet
#Df   Sum Sq  Mean Sq F value Pr(>F)  
#Diet       1 0.018076 0.018076   16.72 0.0549 .
#Residuals  2 0.002162 0.001081                 
---
 # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Error: Within
#Df   Sum Sq   Mean Sq F value Pr(>F)
#Residuals  5 0.003181 0.0006362   
