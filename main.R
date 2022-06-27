#utils::install.packages('') - install new package
#renv::snapshot() - save packages
#this dataset is downloaded from Kaggle: https://www.kaggle.com/datasets/abrambeyer/openintro-possum
#it will be used for gender prediction of a possum

library("ggplot2")
library("corrplot")
library("dplyr")
library("FactoMineR")
library("factoextra")
library("randomForest")

#loading dataset
possum <- read.csv("E:/Projects/My Projects/Possum-Gender-Prediction/possum.csv")

#writing into console number of NA rows
colSums(is.na(possum))

#remove NA rows
possum <- na.omit(possum)

#show all columns as variable selections
str(possum)

#convert selected columns: mm -> cm
possum['hdlngth'] <- round(possum['hdlngth']*0.1, 1)
possum['skullw'] <- round(possum['skullw']*0.1, 1)
possum['earconch'] <- round(possum['earconch']*0.1, 1)
possum['footlgth'] <- round(possum['footlgth']*0.1, 1)
possum['eye'] <- round(possum['eye']*0.1, 1)

#remove less important columns
possum = possum %>% select(- c(case, site, Pop))

#counts how many possums have certain age
plot1 <- ggplot(possum)+geom_histogram(aes(age), binwidth =0.5, color="black", fill="orange")
plot1

#Multivariate Analysis for total length and tail length
plot2 <- ggplot(possum) + geom_point(aes(totlngth, taill), colour = "purple", alpha = 0.8) + theme(axis.title = element_text(size = 8.5))
plot2

#Multivariate Analysis for head length and skull width
plot3 <- ggplot(possum) + geom_point(aes(hdlngth, skullw), colour = "blue", alpha = 0.8) + theme(axis.title = element_text(size = 8.5))
plot3

#counts male and female possums
plot4 <- ggplot(possum %>% group_by(sex) %>% summarise(Count = n())) + geom_bar(aes(sex, Count), stat = "identity", fill = "coral1") + geom_label(aes(sex, Count, label = Count), vjust = 0.5, size =2.5) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot4

#put only numeric values in num set
num=possum %>% select_if(is.numeric)

#PCA (Principal Component Analysis)
res.pca <- PCA(num, graph = FALSE)
get_eigenvalue(res.pca)
fviz_eig(res.pca)
get_pca_ind(res.pca)
fviz_pca_ind(res.pca)
get_pca_var(res.pca)
fviz_pca_var(res.pca)
fviz_pca_biplot(res.pca)

#Correlated matrix
corMatrix=cor(num)
corrplot(corMatrix,order = "FPC",method = "color",type = "lower", tl.cex = 0.6, tl.col = "black")

#make this example reproducible
set.seed(1)
#use 70% of dataset as training set and 30% as test set and same 30% as validate set
d <- sample(c(TRUE, FALSE), nrow(possum), replace=TRUE, prob=c(0.7,0.3))
train <- possum[d, ]
test <- possum[!d, ]

#random forest algorithm
RFM = randomForest(as.factor(sex)~., data = train)
predict_gender = predict(RFM, test)
test$predict_gender = predict_gender
View(test)

#create a matrix of genders
CFM = table(test$sex, test$predict_gender)
CFM

#accuracy of gender prediction
accuracy = round(sum(diag(CFM)/sum(CFM)), 2)
accuracy
