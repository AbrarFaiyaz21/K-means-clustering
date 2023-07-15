install.packages("factoextra")

library(stats)
library(cluster)
library(ggplot2)
library(factoextra)
library(dplyr)
library(ggrepel)

Mydata<-read.csv("D:/University/10th semester/Ds/Country-data.csv",header = TRUE,sep = ",")
Mydata

#structure of data frame
str(Mydata)

# Check for missing values
sum(is.na(Mydata))

# Detect outliers in numeric variables only
outliers <- sapply(Mydata[, sapply(Mydata, is.numeric)], function(x) {boxplot.stats(x)$out})

# Print the number of outliers detected for each variable
colSums(sapply(outliers, function(x) Mydata %in% x))


#normalization
normali <- function(x) {(x-min(x))/(max(x)-min(x))}
country_norm <- as.data.frame(lapply(Mydata[, c(2,3,4,5,6,7,8,9,10)], normali))
country_norm
summary(country_norm)



#determine and visualize optimum number of clusters
fviz_nbclust(country_norm ,kmeans, method = "wss")
fviz_nbclust(country_norm ,kmeans, method = "silhouette")
fviz_nbclust(country_norm ,kmeans, method = "gap_stat")

kmeans(country_norm ,centers = 4,iter.max = 100, nstart = 100)

#create cluster biplot
fviz_cluster(kmeans(country_norm ,centers = 4,iter.max = 100, nstart = 100),data=country_norm)
