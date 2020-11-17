# Title     : Font K means
# Objective : K means
# Created by: madelineluong
# Created on: 11/5/20

################################ Data Cleaning #################################
library(dplyr) # Package for subseting data

attach(BODONI)
bodoni_clean <- select(BODONI,-c(2,3,6,7,8,9,10,11,12)) # Discard the 9 columns
# View(bodoni_clean)
# 3964x403

attach(REFERENCE)
ref_clean <- select(REFERENCE,-c(2,3,6,7,8,9,10,11,12))
# View(ref_clean)
# 4652x403

attach(ROMAN)
roman_clean <- select(ROMAN,-c(2,3,6,7,8,9,10,11,12))
# View(roman_clean)
# 4776x403

# Discarding row containing missing numerical data
BODONI_clean <- na.omit(bodoni_clean)
REF_clean <- na.omit(ref_clean)
ROMAN_clean <- na.omit(roman_clean)

# Defining three classes of images of normal characters
# cl1 = all rows of BODONI_clean.csv file for which (strength = 0.4 and italic =0)
# cl2 = all rows of REF_clean.csv file for which (strength = 0.4 and italic =0)
# cl3 = all rows of ROMAN_clean.csv file for which (strength = 0.4 and italic =0)

BODONI_clean <- data.frame(BODONI_clean)#creating a data frame to add conditional statements to filter out non needed i features
BODONI_clean$CL = ifelse((BODONI_clean$strength == 0.4 & BODONI_clean$italic == 0),"CL1","NA")
BODONI_CLEAN = BODONI_clean[which(BODONI_clean$CL =="CL1"),] #labeling the new filter data as cl1
# 404 columns
# 991 rows

REF_clean <- data.frame(REF_clean)
REF_clean$CL = ifelse((REF_clean$strength == 0.4 & REF_clean$italic == 0),"CL2","NA")
REF_CLEAN = REF_clean[which(REF_clean$CL =="CL2"),]
# 404 columns
# 1163 rows

ROMAN_clean <- data.frame(ROMAN_clean)
ROMAN_clean$CL = ifelse((ROMAN_clean$strength == 0.4 & ROMAN_clean$italic == 0),"CL3","NA")
ROMAN_CLEAN = ROMAN_clean[which(ROMAN_clean$CL =="CL3"),]
# 404 columns
# 1194 rows

# Combine CL1, CL2, CL3 into DATA
DATA <- rbind(BODONI_CLEAN,REF_CLEAN,ROMAN_CLEAN)
# View(DATA)

# Binded all 3 data sets to a full data set (DATA) which is the union of 3 classes (CL1, CL2, CL3)
# where N = 716
# 404 columns

# Create standardize data set without font, strength, and italics (include CL)
library(standardize)
SFONT <- DATA %>% mutate_if(is.numeric, function (x) as.vector(scale(x))) # scaling by (xj-mj)/sd
SFONT = SFONT[,-c(1,2,3)] # taking out numerical functions of font, strength, and italics
# View(SFONT)
# 401 columns
# 716 rows

############################## K- MEANS LIBARBY ####################################
library(factoextra)
library(plot3D)
library(rgl)
library(scatterplot3d)

############################# QUESTION 1 ####################################
SFONT_p <- SFONT[,-c(401)]
#applying K- MEANS
k_max<-10

for (k in 1:k_max){
  kmfont<- kmeans(SFONT_p,k,nstart = 50,iter.max = 50)
}

#finding the the reduction of variance for each k (perf(k))
Varfont<-kmfont$withinss
Varfont
totalvalue=kmfont$tot.withinss # sum of all withins

#plot the curve Perf(k) vs K and identifying using the elbow rule
Qm <- numeric(10)
Perf.m <- numeric(10)
for (k in seq(1,10)){
  kmfont <- kmeans(SFONT_p, k, nstart=50, iter.max=50) # SDATA
  Q <- kmfont$tot.withinss/kmfont$totss
  Perf <- 1-(kmfont$tot.withinss/kmfont$totss)
  Qm[k] = Q
  Perf.m[k] = Perf
}
par(mfrow=c(2,1))
plot(seq(1,10),sum.withins_by_total,xlab = 'Clusters', ylab = 'Q(m)',
     main = 'Elbow Method for Clustering Quality',col='red', type='b')
plot(seq(1,10),Perf.m,xlab = 'Clusters', ylab = 'Perf(m)',
     main = 'Clustering Performance vs Number of Clusters',col='red', type='b')
par(mfrow=c(1,1))

#best k = is at 4

########################### QUESTION 2 ############################

#running K means on best k = 4
kmbest<- kmeans(SFONT_p,4,nstart = 50)

#disjoint 2D plot
library(plotly)
fviz_cluster(kmbest, SFONT_p, stand = FALSE, frame = FALSE, geom = "point")


#computing the centers
kmcenters<- kmbest$centers #matrix showing 3x400
write.table(kmcenters, file = "K Means Centriod values.csv", sep = ",", row.names = TRUE)
#running a PCA on the centers
pcakm<-prcomp(kmcenters,scale=TRUE)

#finding the vectors of pca centers
pcakm
#computing kx3 matrix listing the k vectors; extracting only first three pcs
xvector<-data.frame(pcakm$x[,1:3])
xvector # k centriod mean vectors value points
#displaying 3D graph of each vector
x <- xvector$PC1
y <- xvector$PC2
z <- xvector$PC3
scatter3D(x,y,z,main= "K vectors on 3D graph", xlab="PC1",ylab="PC2",zlab="PC3",col = c("#1B9E77", "#D95F02", "#7570B3")
,pch=19,cex=2)

#counting clusters
table(kmbest$cluster) # cluster1: 1652, cluster2: 494 cluster3:1202
SFONT$cluster<-as.factor(kmbest$cluster) #binding each cluster to SFONT

#calling cluster with the largest size
bigCLU<-subset(SFONT,SFONT$cluster==1) #size is 1652 x 401 Taking out only cluster 1

#compute 3D vectors using the 3 Principal compoments
bigCLU1<- bigCLU[,c(-401,-402)] #taking out non-numerical values to apply pca on cluster

#applying PCA on cluster 1
bigCLU.pca<-prcomp(bigCLU1,scale=TRUE)
PC<-bigCLU.pca$x # The PCA values that has been tranpose and multiplied
PC3<-data.frame(PC[,c(1,2,3)]) # extracting only the first three Principal compoments
PC3$CL<-bigCLU[,401] # adding the class column back in

PC3$CL <- factor(PC3$CL, levels = c("CL1","CL2","CL3")) # creating levels for colors to append

PCA_Clus1<-data.frame(PC3) #dataframe with the CL levels

colors <- c("#1B9E77", "#D95F02", "#7570B3") # creating color function
colors <- colors[as.numeric(PCA_Clus1$CL)] # adding the colors numbers to each CL

### creating plots for cluster 1 where each class repressents a color###

# top plot
scat1<-scatter3D(PCA_Clus1$PC1,PCA_Clus1$PC2,PCA_Clus1$PC3, pch = 4, col=colors,main="CLuster 1 FONT Classes",
                 xlab= "PC1",ylab ="PC2",zlab="PC3",clab = "Font Classes")

#side plot
scat2<-scatterplot3d(PCA_Clus1[,1:3], pch = 16, color=colors, main = "Cluster 1 Font Classes")
legend(scat2$xyz.convert(-25,1,15),legend = levels(PCA_Clus1$CL),col =c("#1B9E77", "#D95F02", "#7570B3"),pch=16 )

#3D movement plot
plot3d(PCA_Clus1, col=colors, main="FontClass")

# add legend
legend3d("topright", legend = paste('Font Class', c('1', '2', '3')), pch = 16, col= c("#1B9E77", "#D95F02", "#7570B3"),
         cex=1, inset=c(0.02))

################################# QUESTION 3 #################################################
#calculating gini index for each cluster
#cluster 1
clu1<-subset(SFONT,SFONT$cluster == "1")
table(clu1$CL) # cl1 : 112, cl2: 101, cl3:891 # this cluster defines class 3
#frequency
(clu1f1<-112/1104)*100 #10.14493%
(clu1f2<-101/1104)*100 #9.148551%
(clu1f3<-891/1104)*100 #80.70652%
#top j is class 3

#gini
(ginicl1<-clu1f1*(1-clu1f1)+clu1f2*(1-clu1f2)+clu1f3*(1-clu1f3))*100 # 0.3299842


#cluster 2
clu2<-subset(SFONT,SFONT$cluster == "2")
table(clu2$CL)
#frequency
(clu2f1<-476/908)*100 #52.42291%
(clu2f2<-361/908)*100 #39.75771%
(clu2f3<-71/908)*100 #7.819383%
#top j is class 1

#gini
(ginicl2<-clu2f1*(1-clu2f1)+clu2f2*(1-clu2f2)+clu2f3*(1-clu2f3)) # 0.5610021

#cluster 3
clu3<-subset(SFONT,SFONT$cluster == "3")
table(clu3$CL)
#frequency
(clu3f1<-192/269)*100 #71.37546%
(clu3f2<-45/269)*100 #16.72862%
(clu3f3<-32/269)*100 #11.89591%
#top j is class 1

#gini
(ginicl3<-clu3f1*(1-clu3f1)+clu3f2*(1-clu3f2)+clu3f3*(1-clu3f3)) # 0.4484183

#cluster 4
clu4<-subset(SFONT,SFONT$cluster == "4")
table(clu4$CL)
#frequency
(clu4f1<-211/1067)*100 #19.77507%
(clu4f2<-656/1067)*100 #61.48079%
(clu4f3<-200/1067)*100 # 18.74414%
#top j is class 2

#gini
(ginicl4<-clu4f1*(1-clu4f1)+clu4f2*(1-clu4f2)+clu4f3*(1-clu4f3)) # 0.5477717

#compute the IMP(K*)
(imp_k <- ginicl1+ginicl2+ginicl3+ginicl4) # 1.887176

#display frequency in 3x4 of frequencies FREQ= fm(j)

###### WRITE DOWN AMJ#########

#computing maxiumum A(TOP(j),j)= max A(1,j),A(2,j),A(3,j)
#A(TOP(1),1) = class 3 which is 80.70652%
#A(TOP(2),2) = class 1 which is 52.42291%
#A(TOP(3),3) = class 1 which is 71.37546%
#A(TOP(3),4) = class 2 which is 61.48079%

################################## QUESTION 4 ###############################################
SFONT$predclass = NA
SFONT$predclass<-ifelse(SFONT$cluster =="1","CL_3",SFONT$predclass)
SFONT$predclass<-ifelse(SFONT$cluster =="2","CL_1",SFONT$predclass)
SFONT$predclass<-ifelse(SFONT$cluster =="3","CL_1",SFONT$predclass)
SFONT$predclass<-ifelse(SFONT$cluster =="4","CL_2",SFONT$predclass)

table(SFONT$predclass,SFONT$CL) # rows are predicted , columns are true values

mean(SFONT$predclass == SFONT$CL)




