#PCA on a small area, then k-means
#quick test

library(dplyr)
library(readr)
library(corrr)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)

#https://www.datacamp.com/tutorial/pca-analysis-r

dat <- read.csv("C:/Users/asobc/PycharmProjects/Networks/Spectral_Final/diffusion.csv")
names(dat)
mat <- t(as.matrix(dat))
dat <- as.data.frame(mat)
dat <- subset(dat, select = -c(X))
#remove NA values
dat_num <- dat[complete.cases(dat),]
#save lonlat
lonlat <- dat_num[,1:2]
#select relevant columns
dat_num <- dat_num[,3:48]

#normalize data
dat_norm <- scale(dat_num)
#generate covariance matrix
corr_matrix <- cor(dat_norm)
ggcorrplot(corr_matrix)

#apply pca
data.pca <- princomp(corr_matrix)
summary(data.pca)

#cdf type thing of explained variance
fviz_eig(data.pca, addlabels = TRUE)
fviz_pca_var(data.pca, col.var = "black")

#Here are the eigenvectors for the first few principle components
w <- as.matrix(data.pca$loadings[, 1:6])
x <- as.matrix(dat_num)
t <- x %*% w
t_df <- as.data.frame(t)

#now we want to run k-means on t_df

fviz_nbclust(t_df, FUNcluster = kmeans, method = "wss", k.max = 10)
#gonna pick 4 clusters based on this

km <- kmeans(t_df, 4, iter.max = 10, nstart = 30)
print(km)

together <- cbind(lonlat, dat_num, cluster = km$cluster)
tester <- together[,c(1:2,49)]
#now taking this and making a heat map to get a sense of things

library(raster)
# create spatial points data frame
coordinates(tester) <- c("lon", "lat")
#make it a spatial pixels data frame
gridded(tester) <- TRUE
#convert to raster
rasterDF1 <- raster(tester)
rasterDF1
plot(rasterDF1)
