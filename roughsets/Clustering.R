library(Rcpp)
library(RoughSets)
library(ggplot2)
library(lattice)
library(caret)

df <- read.csv("dataset/padi_clean.csv")
summary(df)

df_1 <- df[, c(-1,-2,-3)]

set.seed(240)
kmeans.re <- kmeans(df_1, centers=3, nstart=20)

print(kmeans.re)
summary(df$motif)
cm <- table(df$category, kmeans.re$cluster)

print(cm)
