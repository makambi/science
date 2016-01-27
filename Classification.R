data_path <- "Desktop/DataAnalysis/Soft/Classification.csv"
cols <- c("Disability program", "Water resourses project", "Adoption project",	"Law on helthcare",	"Salvador project",	"Law on religions",
          "Antisatellite project",	"Nicaragua assistance project",	"Rocket project",	"Law on immigrants",	"Alternative fuel sources",	"Law on education",
          "Project on funds",	"Project on crime",	"Project of customs", 	"Project exports",	"Class")
df <- read.csv(data_path, sep = '\t', header = FALSE, col.names = cols)


#http://www.r-tutor.com/gpu-computing/clustering/distance-matrix
#http://www.r-tutor.com/gpu-computing/clustering/hierarchical-cluster-analysis


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
