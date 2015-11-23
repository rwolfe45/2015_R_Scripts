library(ggplot2)
library(vegan)
library(grid)
library(MASS)
library(RColorBrewer)
#Add data in tab delimited format.
#For both of these data sets, the rows need to be samples
#columns need to be either OTUS for BIO, or variables for CHEM


CHEM<-read.table(file.choose(), header=TRUE, sep="\t", check.names=TRUE)
BIO<-read.table(file.choose(), header=TRUE, sep="\t", check.names=TRUE)
#establish samples as rownames. These need to be in the exact same order, otherwise the scaling of the vectors will be wrong.
rownames(BIO)<-BIO[,1]
BIO<-BIO[,-1]
rownames(CHEM)<-CHEM[,1]
CHEM<-CHEM[,-1]
#Since we are using chemical data on different scales we will want to use the log transformation of the data
#I may not log transform all of the factors later on, but to see what it looks like first...
#This is the log transformation of all the chem variables that have a numerical value.
log.chem<-log(CHEM[,3:7])
#log.chem is what we will use for establishing the vectors.
#to size the dots based on the PC1 of all the chemical data (like a bubble plot in primer), let's apply a PCA scale
chem.pca<-prcomp(log.chem, center=TRUE, scale.=TRUE)
#To check variances by PCA
summary(chem.pca)
#In order to use the PC1 scale, let's establish a variable with all of the PCA scale coordinates by sample
chem.variables<-predict(chem.pca)
chem.variables.PC1<-chem.variables[,1]
#This establishes the coordinates for the samples and all OTUS onto an non-metric dimensional scaling
ord<-metaMDS(BIO)
#envfit here will fit the vectors onto the ordination established above
(fit <- envfit(ord, log.chem, perm = 999))
scores(fit, "vectors")

#Lots plot this first to make sure it works, just in vegan
plot(ord)
plot(fit)
plot(fit, p.max = 0.05, col = "red")

#Now let's try it in ggplot
#for ggplot we need to establish scores from the ord and fit vectors above.
#the scores need to be in a dataframe. This language is what is used below, but you can change it to whatever you need, as long as you maintain the logic.


ord.scrs<-as.data.frame(scores(ord),display="sites")
fit.scrs<-as.data.frame(scores(fit,display="vectors"))
Species<-rownames(fit.scrs)
fit.scrs<-cbind(fit.scrs,Species)


#Build the plot in ggplot:
#First we want to establish a vector that we want to color by
#Here I am using diet, but you could also use seddep (like Adrienne uses for hers)
Treatment<-CHEM$Treatment
#Here we will try to use the PC1 scale of chem.variables.PC1 to scale the points by.
p<-ggplot(ord.scrs)+geom_point(mapping = aes(x = NMDS1, y = NMDS2, colour=Treatment, shape=Treatment))
p

#That worked!  Now lets add on the vectors.
p<-p+coord_fixed()+geom_segment(data=fit.scrs,aes(x=0,xend=NMDS1,y=0,yend=NMDS2),arrow=arrow(length=unit(0.25,"cm")),color="grey")
#Now let's add text to the vectors so we can identify which one is which, and also add the names of the sample points. the label here is an established vector of the sample names that I identified above.
p<-p+geom_text(data=fit.scrs,aes(x=NMDS1,y=NMDS2),label=Species, size=3.5)+geom_text(data=ord.scrs, aes(x=NMDS1, y=NMDS2), label=rownames(CHEM), size=5)
p
