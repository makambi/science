data_path <- "Desktop/DataAnalysis/Soft/Classification.csv"
cols <- c("Disability program", "Water resourses project", "Adoption project",	"Law on helthcare",	"Salvador project",	"Law on religions",
          "Antisatellite project",	"Nicaragua assistance project",	"Rocket project",	"Law on immigrants",	"Alternative fuel sources",	"Law on education",
          "Project on funds",	"Project on crime",	"Project of customs", 	"Project exports",	"Class")
df <- read.csv(data_path, sep = '\t', header = FALSE, col.names = cols)


#http://www.r-tutor.com/gpu-computing/clustering/distance-matrix
#http://www.r-tutor.com/gpu-computing/clustering/hierarchical-cluster-analysis

#http://www.econ.upf.edu/~michael/stanford/maeb7.pdf

dist(as.matrix(df))

barplot(table(df$Class), col="red")

barplot(table(df$Law.on.religions), col="black")

barplot(table(df$Adoption.project), col="black")

barplot(table(df$Rocket.project))

barplot(table(df$Law.on.immigrants))

barplot(table(df$Law.on.education))


mosaicplot(df$Class ~ df$Law.on.education, shade=FALSE, color=TRUE)

mosaicplot(df$Class ~ df$Law.on.immigrants, shade=FALSE, color=TRUE)

mosaicplot(df$Class ~ df$Law.on.religions, shade=FALSE, color=TRUE)

df.out <- prcomp(df)
hclust(dist(df))

#http://www.inside-r.org/packages/cran/klaR/docs/kmodes
#https://shapeofdata.wordpress.com/2014/03/04/k-modes/
#http://stats.stackexchange.com/questions/15287/hierarchical-clustering-with-mixed-type-data-what-distance-similarity-to-use
library(klaR)
fit <- kmodes(df, modes = 4)
plot(jitter(df), col=fit$cluster)

#http://stackoverflow.com/questions/20438019/how-to-perform-clustering-without-removing-rows-where-na-is-present-in-r
#http://stackoverflow.com/questions/24140339/tree-cut-and-rectangles-around-clusters-for-a-horizontal-dendrogram-in-r
library("gplots")
library(cluster)
dist <- daisy(df, metric = c("gower"))
hca <- hclust(d.arg, method="complete")
k <- 3
clust <- cutree(hca, k)

library(ggdendro)
dendr    <- dendro_data(hca, type="rectangle")
clust.df <- data.frame(label=rownames(iris), cluster=factor(clust))
dendr[["labels"]]   <- merge(dendr[["labels"]],clust.df, by="label")
rect <- aggregate(x~cluster,label(dendr),range)
rect <- data.frame(rect$cluster,rect$x)
ymax <- mean(hca$height[length(hca$height)-((k-2):(k-1))])

ggplot() + 
  geom_segment(data=segment(dendr), aes(x=x, y=y, xend=xend, yend=yend)) + 
  geom_text(data=label(dendr), aes(x, y, label=label, hjust=0, color=cluster), 
            size=3) +
  geom_rect(data=rect, aes(xmin=X1-.3, xmax=X2+.3, ymin=0, ymax=ymax), 
            color="red", fill=NA)+
  geom_hline(yintercept=0.33, color="blue")+
  coord_flip() + scale_y_reverse(expand=c(0.2, 0)) + 
  theme_dendro()


#http://www.r-bloggers.com/multidimensional-scaling-mds-with-r/
fit <- cmdscale(dist, eig = TRUE, k = 2)
x <- fit$points[, 1]
y <- fit$points[, 2]
plot(x, y)

dist.pam <- pam(dist, k=3, diss = TRUE)
clusplot(dist.pam)
?clusplot

#clusters just for weights
votes <- read.csv(data_path, sep = '\t', header = FALSE, col.names = cols)
votes <- subset(votes, select = -c(Class) )

votes.dist <- daisy(votes, metric = "gower", stand = TRUE) #what is stand == TRUE?
votes.pam <- pam(votes.dist, k=3, diss=TRUE)
clusplot(votes.pam)

#weighted Class 
#http://stackoverflow.com/questions/21334677/how-do-i-weight-variables-with-gower-distance-in-r

votes.weighted <- read.csv(data_path, sep = '\t', header = FALSE, col.names = cols)
votes.weights <- rep(1, ncol(votes))
votes.weights[which(colnames(votes.weighted) == "Class")] = 3

votes.weighted.dist <- daisy(votes.weighted, metric = "gower", stand = TRUE)
votes.weighted.pam <- pam(votes.weighted.dist, k=3, diss = TRUE)
clusplot(votes.weighted.pam)
