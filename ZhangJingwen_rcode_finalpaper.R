library(readxl)
# data preparation
# here are data for all countries from 2008-2018
fulldata <- read_excel("2019Chapter2xls.xls")
library(dplyr)
# select lastest data 2018
data18 <- filter(fulldata, fulldata$Year=="2018")
rownames(data18) <- data18$`Country name`
# remove blank/sparse columns
data18 <- data18[-c(13,14,17,20:26)]
# only include numeric variables and those we are interested 
data18sub <- data18[c(1,4,5,6,7,8,9,10,11,12,15,16)]
# replacing missing values with the median for each column, do it by hand bc my function does not work
data18sub$`Log GDP per capita`[is.na(data18sub$`Log GDP per capita`)] <- median(data18sub$`Log GDP per capita`, na.rm = TRUE)
data18sub$`Healthy life expectancy at birth`[is.na(data18sub$`Healthy life expectancy at birth`)] <- median(data18sub$`Healthy life expectancy at birth`, na.rm = TRUE)
data18sub$`Perceptions of corruption`[is.na(data18sub$`Perceptions of corruption`)] <- median(data18sub$`Perceptions of corruption`, na.rm = TRUE)
data18sub$`Positive affect`[is.na(data18sub$`Positive affect`)] <- median(data18sub$`Positive affect`, na.rm = TRUE)
data18sub$`Negative affect`[is.na(data18sub$`Negative affect`)] <- median(data18sub$`Negative affect`, na.rm = TRUE)
data18sub$`Confidence in national government`[is.na(data18sub$`Confidence in national government`)] <- median(data18sub$`Confidence in national government`, na.rm = TRUE)
data18sub$`gini of household income reported in Gallup, by wp5-year`[is.na(data18sub$`gini of household income reported in Gallup, by wp5-year`)] <- median(data18sub$`gini of household income reported in Gallup, by wp5-year`, na.rm = TRUE)
data18sub$`GINI index (World Bank estimate), average 2000-16`[is.na(data18sub$`GINI index (World Bank estimate), average 2000-16`)] <- median(data18sub$`GINI index (World Bank estimate), average 2000-16`, na.rm = TRUE)
data18sub$Generosity[is.na(data18sub$Generosity)] <- median(data18sub$Generosity,na.rm = T)


# descriptive statistics
library(stargazer)
myvar <- as.data.frame(data18sub[,2:12])
myvar <- na.omit(myvar)
colnames(myvar) <- c("GDP","Support","HLE","Freedom","Generosity","Corruption","Positive","Negative","Government","GINIaverage", "GINIhousehold")
stargazer(myvar,type = "latex",out="descriptivedata.html",digits = 2,title="Descriptive statistics",df=F,header=F)


# standardize data bc variavles have different scales
library(robustHD)
data18.1 <- standardize(data18sub[,2:12], centerFun = mean, scaleFun = sd)


# PCA
S1 <- cov(data18.1)
pca1 <- eigen(S1)
pca1$values # it is suggested that to obtain components which eigenvalue is higher than 1, so first 3 components 
lambda.star1 <- pca1$values/sum(pca1$values)
lambda.star1 # first 2 components compose of around 60% 


# scree plot for explained variance
library(FactoMineR)
pca_pl1 <- PCA(data18.1, graph=FALSE)
library(factoextra)
fviz_screeplot(pca_pl1, addlabels = TRUE, ylim = c(0, 65), title= "Figure 1: Scree Plot")
# scree plot of parallel analysis with simulated data
library(psych)
R1 <- cor(data18.1)
fa.parallel(R1, n.obs = 136, fa = "pc")


# components loadings
out1 <- princomp(data18.1,cor = F, scores = T)
print(out1$loadings,cutoff = 0.35)
# print(out1$loadings)
loading2 <- as.table(print(out1$loadings,cutoff = 0.35))
loading2 <- loading2[,1:2]
colnames(loading2) <- c("First component", "Second component")
knitr::kable(loading2, caption = "Loadings on the first 2 components")


# plot on 2 dimensions
data18.1_1 <- data18.1[c(1,2,3,4,5,7,8,9)]
library(factoextra)
res.pca <- prcomp(data18.1_1, scale = TRUE)
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
             ,title="Figure 2: First 2 dimensions")


# how many Ks?
# evaluate BIC to decide numbers of K
library(mclust)
BIC <- mclustBIC(data18.1_1, G=2:5)
summary(BIC)# so far k=4 is the best, smallest BIC
em.clus <- Mclust(data18.1,x=BIC)
# info for intepretation, prob,mean
#em.clus$parameters$pro
# em.clus$parameters$mean
# plot it
# plot(em.clus,what="classification") # too messy
# BIC table
bic.res <- as.matrix(summary(BIC))
colnames(bic.res) <- c("BIC values")


# plot clusters on 2-dimension
pca_pl2 <- PCA(data18.1_1, graph=FALSE)
library(cluster)
set.seed(0728)
pam_k4 <- pam(data18.1_1, diss=FALSE, 4, keep.data=TRUE)
# fviz_silhouette(pam_k4)# include size of each cluster
data18.1_1$cluster <- as.factor(pam_k4$clustering)
fviz_pca_ind(pca_pl2,
             label="none",
             habillage = data18.1_1$cluster, 
             palette = c("#FC4E07","#7CAE00","#00AFBB","#C77CFF", "#E7B800"),
             addEllipses=TRUE,ellipse.type = "confidence", title="Figure 3: Clusters on the first 2 dimensions")


# plot clusters on world map
# little data preparation for plotting
data18.2 <- data18.1_1
data18.2$country <- data18$`Country name`
library(maps)
world <- map_data("world")
world$region[world$region=="USA"] <- "United States"
world$region[world$region=="UK"] <- "United Kingdom"

mapjoined <- left_join(world, data18.2, by = c('region' = 'country'))
ggplot() +
  geom_polygon(data = mapjoined, aes(x = long, y = lat, group = group, fill=cluster, color=cluster)) +
  labs(title = "Figure 4: Application of K-means Clusters on World Happiness Index",
       subtitle = "Based on data from World Happiness Report 2019", 
       x=NULL, y=NULL) +
  coord_equal() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_blank())


# comparison: plot happiness scores on world map
fulldata2 <- read_excel("2019Chapter2xls.xls", 2L)
quantile(fulldata2$`Happiness score`, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 6)
fulldata2_copy <- fulldata2
fulldata2_copy$cluster <- NA
fulldata2_copy$cluster[fulldata2$`Happiness score` < 4.53780] <- "1"
fulldata2_copy$cluster[fulldata2$`Happiness score` >= 6.18935] <- "4"
fulldata2_copy$cluster[fulldata2$`Happiness score` >= 5.37955 & fulldata2$`Happiness score` < 6.18935] <- "3"
fulldata2_copy$cluster[fulldata2$`Happiness score` >= 4.53780 & fulldata2$`Happiness score` < 5.37955] <- "2"

world2 <- map_data("world")
world2$region[world2$region=="USA"] <- "United States"
world2$region[world2$region=="UK"] <- "United Kingdom"


mapjoined2 <- left_join(world2, fulldata2_copy, by = c('region' = 'Country'))
ggplot() +
  geom_polygon(data = mapjoined2, aes(x = long, y = lat, group = group, fill=cluster, color=cluster)) +
  labs(title = "Figure 5: Clusters on World Happiness Index",
       subtitle = "Based on data from World Happiness Score 2019", 
       x=NULL, y=NULL) +
  coord_equal() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_blank())
