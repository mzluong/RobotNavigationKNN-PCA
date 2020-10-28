# Title     : Navigation KNN
# Objective : TODO
# Created by: madelineluong
# Created on: 10/5/20

attach(Robot)
ROBOT_CLEAN<-na.omit(Robot)#taking out NA Values

library(dplyr)
library(standardize)
SRobot <- ROBOT_CLEAN %>% mutate_if(is.numeric, function (x) as.vector(scale(x))) # scaling by (xj-mj)/sd



SRobot1 <- data.frame(SRobot[,])
Slight_Right <- SRobot1[SRobot1$Class == "Slight-Right-Turn",]
Slight_Left <- SRobot1[SRobot1$Class == "Slight-Left-Turn",]
Sharp_Right <- SRobot1[SRobot1$Class == "Sharp-Right-Turn",]
Move_Forward <- SRobot1[SRobot1$Class == "Move-Forward",]


par(mfrow = c(1,2))
hist(Slight_Right$US1, main = "Slight Right" , col = "lightblue", xlab = "US1")
hist(Slight_Left$US1, main = "Slight Left" , col = "lightblue", xlab = "US1")
hist(Sharp_Right$US1, main = "Sharp Right" , col = "lightblue", xlab = "US1")
hist(Move_Forward$US1, main = "Move Forward" , col = "lightblue", xlab = "US1")


hist(Slight_Right$US5, main = "Slight Right" , col = "lightblue", xlab = "US5")
hist(Slight_Left$US5, main = "Slight Left" , col = "lightblue", xlab = "US5")
hist(Sharp_Right$US5, main = "Sharp Right" , col = "lightblue", xlab = "US5")
hist(Move_Forward$US5, main = "Move Forward" , col = "lightblue", xlab = "US5")



hist(Slight_Right$US15, main = "Slight Right" , col = "lightblue", xlab = "US15")
hist(Slight_Left$US15, main = "Slight Left" , col = "lightblue", xlab = "US15")
hist(Sharp_Right$US15, main = "Sharp Right" , col = "lightblue", xlab = "US15")
hist(Move_Forward$US15, main = "Move Forward" , col = "lightblue", xlab = "US15")


par(mfrow = c(1,4))
hist(Slight_Right$US20, main = "Slight Right" , col = "lightblue", xlab = "US20")
hist(Slight_Left$US20, main = "Slight Left" , col = "lightblue", xlab = "US20")
hist(Sharp_Right$US20, main = "Sharp Right" , col = "lightblue", xlab = "US20")
hist(Move_Forward$US20, main = "Move Forward" , col = "lightblue", xlab = "US20")

#### KS - test discrimiting power
### US1
ks.test(Slight_Right$US1,Slight_Left$US1)
ks.test(Slight_Right$US1,Sharp_Right$US1)
ks.test(Slight_Right$US1,Move_Forward$US1)
ks.test(Slight_Left$US1,Sharp_Right$US1)
ks.test(Slight_Left$US1,Move_Forward$US1)
ks.test(Move_Forward$US1,Sharp_Right$US1)
### US5
ks.test(Slight_Right$US5,Slight_Left$US5)
ks.test(Slight_Right$US5,Sharp_Right$US5)
ks.test(Slight_Right$US5,Move_Forward$US5)
ks.test(Slight_Left$US5,Sharp_Right$US5)
ks.test(Slight_Left$US5,Move_Forward$US5)
ks.test(Move_Forward$US5,Sharp_Right$US5)
### 15
ks.test(Slight_Right$US15,Slight_Left$US15)
ks.test(Slight_Right$US15,Sharp_Right$US15)
ks.test(Slight_Right$US15,Move_Forward$US15)
ks.test(Slight_Left$US15,Sharp_Right$US15)
ks.test(Slight_Left$US15,Move_Forward$US15)
ks.test(Move_Forward$US15,Sharp_Right$US15)
### 20
ks.test(Slight_Right$US20,Slight_Left$US20)
ks.test(Slight_Right$US20,Sharp_Right$US20)
ks.test(Slight_Right$US20,Move_Forward$US20)
ks.test(Slight_Left$US20,Sharp_Right$US20)
ks.test(Slight_Left$US20,Move_Forward$US20)
ks.test(Move_Forward$US20,Sharp_Right$US20)


# Creating the 80% random train set interval by taking ONLY using Slight Rigth
Slight_Right1 = SRobot[which(SRobot$Class =="Slight-Right-Turn"),]
n_SR <- nrow(Slight_Right1[which(Slight_Right1$Class =="Slight-Right-Turn"),])
trainset_SR1 <- sample(1:n_SR, 0.8*n_SR)


trainset_SR <- Slight_Right1[trainset_SR1,]
testset_SR <- Slight_Right1[-trainset_SR1,]


# Creating the 80% random train set interval by taking ONLY using Slight Left
Slight_Left1 = SRobot[which(SRobot$Class =="Slight-Left-Turn"),]
n_SL <- nrow(Slight_Left1[which(Slight_Left1$Class =="Slight-Left-Turn"),])
trainset_SL1 <- sample(1:n_SL, 0.8*n_SL)


trainset_SL <- Slight_Left1[trainset_SL1,]
testset_SL <- Slight_Left1[-trainset_SL1,]


# Creating the 80% random train set interval by taking ONLY using Sharp Right
Sharp_Right1 = SRobot[which(SRobot$Class =="Sharp-Right-Turn"),]
n_ShR <- nrow(Sharp_Right1[which(Sharp_Right1$Class =="Sharp-Right-Turn"),])
trainset_ShR1 <- sample(1:n_ShR, 0.8*n_ShR)


trainset_ShR <- Sharp_Right1[trainset_ShR1,]
testset_ShR <- Sharp_Right1[-trainset_ShR1,]


# Creating the 80% random train set interval by taking ONLY using Move Forward
Move_Forward1 = SRobot[which(SRobot$Class =="Move-Forward"),]
n_MF <- nrow(Move_Forward1[which(Move_Forward1$Class =="Move-Forward"),])
trainset_MF1 <- sample(1:n_MF, 0.8*n_MF)


trainset_MF <- Move_Forward1[trainset_MF1,]
testset_MF <- Move_Forward1[-trainset_MF1,]


# Combining the sets to full trainset and testset
Training_set2 <- rbind(trainset_SR,trainset_SL,trainset_ShR,trainset_MF)
Test_set2 <- rbind(testset_SR,testset_SL,testset_ShR,testset_MF)



# Train and test labels
library(class)
TRAIN_Robot_no <- Training_set2[,-25]
TRAIN_Robot_label <- Training_set2[, "Class"]
TEST_Robot_no <- Test_set2[,-25]
TEST_Robot_label <- Test_set2[,"Class"]



####finding percent accuracy for each value of 5,10...100 for test set

set.seed(1)
i = 1
k.optm = 1
for (i in seq(5, 100, by = 5)){
  knn.mod1 <- knn(train = TRAIN_Robot_no, test = TEST_Robot_no, cl = TRAIN_Robot_label, k = i)
  k.optm[i] <- 100 * sum(TEST_Robot_label == knn.mod1)/ NROW(TEST_Robot_label)
  k=i
  cat(k,"=", k.optm[i],'\n')
}
####finding percent accuracy for each value of 5,10...100 for train set

set.seed(1)
i = 1
k.optm = 1
for (i in seq(5, 100, by = 5)){
  knn.mod1 <- knn(train = TRAIN_Robot_no, test = TRAIN_Robot_no, cl = TRAIN_Robot_label, k = i)
  k.optm[i] <- 100 * sum(TRAIN_Robot_label == knn.mod1)/ NROW(TRAIN_Robot_label)
  k=i
  cat(k,"=", k.optm[i],'\n')
}

# Fit the model on the training set finding the optimized value of k for test set
#running a loop
set.seed(1)
K.set = c(5,10,15,20,25,30,40,50,100)
knn.test.acc1 <- numeric(length(K.set))

for (j in 1:length(K.set)){
  knn.pred1 <- knn(train = TRAIN_Robot_no,
                  test=TEST_Robot_no,
                  cl=TRAIN_Robot_label,
                  k = K.set[j])
  knn.test.acc1[j] <- mean(knn.pred1 == TEST_Robot_label)
}
### finding accuracy for train which will be higher
set.seed(1)
K.set = c(5,10,15,20,25,30,40,50,100)
knn.train.acc1 <- numeric(length(K.set))

for (j in 1:length(K.set)){
  knn.pred1 <- knn(train=TRAIN_Robot_no,
                  test=TRAIN_Robot_no,
                  cl=TRAIN_Robot_label,
                  k=K.set[j])
  knn.train.acc1[j] <- mean(knn.pred1 == TRAIN_Robot_label)
}
# Find best k
set.seed(1)
max(knn.test.acc1)
K.set[which.max(knn.test.acc1)]
## based on the information provided, we use K=5 as k best
### plotting the figure with each other.
## red = test set
## blue = trainset
plot(K.set, knn.train.acc1, type="o", col="blue", pch="o", lty=1 ,xlab= "K VALUE", ylab = "Accuracy", main = "Accuracy of Train and Test")
points(K.set, knn.test.acc1, col="red", pch="*")
lines(K.set, knn.test.acc1, col="red",lty=2)



#### based on the figure we can do 5:20 range to explore more k values
K.set = c(5:20)
knn.test.acc1 <- numeric(length(K.set))
set.seed(1)
for (j in 1:length(K.set)){
  knn.pred1 <- knn(train=TRAIN_Robot_no,
                  test=TEST_Robot_no,
                  cl=TRAIN_Robot_label,
                  k=K.set[j])
  knn.test.acc1[j] <- mean(knn.pred1 == TEST_Robot_label)
}
# Find best k
max(knn.test.acc1)
K.set[which.max(knn.test.acc1)]
## based on the information provided, we use K=5 as k best


## Applying the "best" k value to the both train and test set.
set.seed(1)
knn.predtrainbest2 <- knn(train = TRAIN_Robot_no,
                         test = TRAIN_Robot_no,
                         cl = TRAIN_Robot_label,
                         k = 5)
set.seed(1)
knn.predtestbest2 <- knn(train = TRAIN_Robot_no,
                        test = TEST_Robot_no,
                        cl = TRAIN_Robot_label,
                        k = 5)
## displaying the percent accuracy
mean(knn.predtrainbest2 == TRAIN_Robot_label)
mean(knn.predtestbest2 == TEST_Robot_label)


## displaying confusion matrix
train_cmatrix2 <- table(data.frame(knn.predtrainbest2,TRAIN_Robot_label))
test_cmatrix2 <- table(data.frame(knn.predtestbest2, TEST_Robot_label))
train_cmatrix2
test_cmatrix2

# Variable importance using fisher score
library(rpart)
d1 <- select(SRobot,c(1:24))


CRobot <- SRobot


for (i in 1:9131){
  if(CRobot$Class[i]=="Slight-Right-Turn"){
    CRobot$Turn[i] = "1"
  }else if(CRobot$Class[i]=="Slight-Left-Turn"){
    CRobot$Turn[i] = "2"
  }else if(CRobot$Class[i]=="Sharp-Right-Turn"){
    CRobot$Turn[i] = "3"
  }else{
    CRobot$Turn[i] = "4"
  }
}


fit_m <- rpart( CRobot$Turn~ ., data = d1);


# Visualize the importance of each variable
# BY USING LDA
library(ggplot2)
theme_set(theme_minimal())
options(repr.plot.width=12, repr.plot.height=7)
vi_m <- fit_m$variable.importance;
barplot(vi_m, horiz = F, las = 1, col = rainbow(20),
        main = "Variable importance")

#taking out slight left - turn to improve KNN

Training_set4 <- rbind(trainset_SR,trainset_ShR,trainset_MF)
Test_set4 <- rbind(testset_SR,testset_ShR,testset_MF)

#labels
TRAIN_Robot_no4 <- Training_set4[,-25]
TRAIN_Robot_label4 <- Training_set4[, "Class"]
TEST_Robot_no4 <- Test_set4[,-25]
TEST_Robot_label4 <- Test_set4[,"Class"]

##applying KNN without small class
set.seed(1)
knn.predtrainbest4 <- knn(train = TRAIN_Robot_no4,
                         test = TRAIN_Robot_no4,
                         cl = TRAIN_Robot_label4,
                         k = 5)
set.seed(1)
knn.predtestbest4 <- knn(train = TRAIN_Robot_no4,
                        test = TEST_Robot_no4,
                        cl = TRAIN_Robot_label4,
                        k = 5)
## displaying the percent accuracy
mean(knn.predtrainbest4 == TRAIN_Robot_label4)
mean(knn.predtestbest4 == TEST_Robot_label4)
## about the same didn't influence it

## displaying confusion matrix
train_cmatrix4 <- table(data.frame(knn.predtrainbest4,TRAIN_Robot_label4))
test_cmatrix4 <- table(data.frame(knn.predtestbest4, TEST_Robot_label4))
train_cmatrix4
test_cmatrix4

##another method is taking out features from the LDA and taking out class
training_test_f<- select(Test_set4,-c(8,22,7,6,9,10,1,5,12,11,13))
training_train_f<- select(Training_set4,-c(8,22,7,6,9,10,1,5,12,11,13))

##LABELS
TRAIN_Robot_nof <- training_train_f[,-14]
TRAIN_Robot_labelf <- training_train_f[, "Class"]
TEST_Robot_nof <- training_test_f[,-14]
TEST_Robot_labelf <- training_test_f[,"Class"]

##applying knn
set.seed(1)
knn.predtrainbestf <- knn(train = TRAIN_Robot_nof,
                         test = TRAIN_Robot_nof,
                         cl = TRAIN_Robot_labelf,
                         k = 5)
set.seed(1)
knn.predtestbestf <- knn(train = TRAIN_Robot_nof,
                        test = TEST_Robot_nof,
                        cl = TRAIN_Robot_labelf,
                        k = 5)
## displaying the percent accuracy
mean(knn.predtrainbestf == TRAIN_Robot_labelf)
mean(knn.predtestbestf == TEST_Robot_labelf)
## displaying confusion matrix
train_cmatrixf <- table(data.frame(knn.predtrainbestf,TRAIN_Robot_labelf))
test_cmatrixf <- table(data.frame(knn.predtestbestf, TEST_Robot_labelf))
train_cmatrixf
test_cmatrixf

###PCA ANALYSIS
library(FactoMineR)
head(SRobot)
str(SRobot)
##computing corr matrix
CorRobot<-scale(SRobot[,-c(25)])
Cor_Robot<-cor(CorRobot)
#creating a heat map of correlation matrix
library(reshape2)
library(hrbrthemes)
melted_Robot<-melt(Cor_Robot)
head(melted_Robot)
library(ggplot2)
ggplot(data = melted_Robot,aes(x=Var1,y=Var2, fill= value)) +
  geom_tile()+
  scale_fill_gradient(low = "black", high ="Purple")+
  theme_ipsum()
#based on the correlation matrix we can see not very many features have high correlation

##extracting eigen values from corr matrix
CCOR_ROBOT<-eigen(Cor_Robot)$values
##Computing eigen vectors
WCOR_ROBOT<-eigen(Cor_Robot)$vectors
#tranposeing vectors
TCOR_ROBOT<-t(WCOR_ROBOT)
#tranpose * feature vector
TCOR_ROBOT%*%
###plotting eignvalues on variances
library(factoextra)
####PCA
res.pca<-prcomp(SRobot[,-25],scale = TRUE)

#extracting eigenvalues/ variances
get_eig(res.pca)
#here we can see that variance stops at 20 for 95%

#visualizing egenvalues/varaincve of dim
fviz_eig(res.pca,ncp = 24, addlabels=TRUE, hjust = -0.3,
               barfill=rainbow(24), barcolor ="darkblue",
               linecolor ="black") + ylim(0, 30) +
               theme_minimal()

#contribution biplot
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


#contribution of each feature from 1:20 dim
fviz_contrib(res.pca, choice = "var",axes = 1:20,top = 24,fill = rainbow(24))
#reducing from 24 to 15 features
#biplot with classes
fviz_pca_biplot(res.pca, label = "var", habillage=SRobot$Class,
                addEllipses = TRUE, ellipse.level = 0.90,
                ggtheme = theme_minimal())

### PCA KNN TAKING OUT 9 FEATURES BUT STILL USING 4 CLASSES.
training_test_pca<- select(Test_set2,-c(5,10,11,9,8,6,7,22,4))
training_train_pca<- select(Training_set2,-c(5,10,11,9,8,6,7,22,4))

#LABEL
TRAIN_Robot_no_pca <- training_train_pca[,-16]
TRAIN_Robot_label_pca <- training_train_pca[, "Class"]
TEST_Robot_no_pca <- training_test_pca[,-16]
TEST_Robot_label_pca <- training_test_pca[,"Class"]

## Applying the "best" k value to the both train and test set on PCA features
set.seed(1)
knn.predtrainbest_pca <- knn(train = TRAIN_Robot_no_pca,
                         test = TRAIN_Robot_no_pca,
                         cl = TRAIN_Robot_label_pca,
                         k = 5)
set.seed(1)
knn.predtestbest_pca <- knn(train = TRAIN_Robot_no_pca,
                        test = TEST_Robot_no_pca,
                        cl = TRAIN_Robot_label_pca,
                        k = 5)
## displaying the percent accuracy
mean(knn.predtrainbest_pca == TRAIN_Robot_label_pca)
mean(knn.predtestbest_pca == TEST_Robot_label_pca)


## displaying confusion matrix
train_cmatrix_pca <- table(data.frame(knn.predtrainbest_pca,TRAIN_Robot_label_pca))
test_cmatrix_pca <- table(data.frame(knn.predtestbest_pca, TEST_Robot_label_pca))
train_cmatrix_pca
test_cmatrix_pca


