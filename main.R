#this dataset is downloaded from Kaggle: https://www.kaggle.com/datasets/abrambeyer/openintro-possum
#it will be used for gender prediction of a possum

#loading dataset
possum <- read.csv("E:/Projects/My Projects/Possum/possum.csv")
#writing into console number of NA rows
colSums(is.na(possum))
#remove NA rows
possum <- na.omit(possum)