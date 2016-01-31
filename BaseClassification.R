data_path <- "Desktop/DataAnalysis/Soft/Classification.csv"
cols <- c("Disability program", "Water resourses project", "Adoption project",	"Law on helthcare",	"Salvador project",	"Law on religions",
          "Antisatellite project",	"Nicaragua assistance project",	"Rocket project",	"Law on immigrants",	"Alternative fuel sources",	"Law on education",
          "Project on funds",	"Project on crime",	"Project of customs", 	"Project exports",	"Class")
senators <- read.csv(data_path, sep = '\t', header = FALSE, col.names = cols)

republicans <- senators[senators$Class == "republican",]
democrats <- senators[senators$Class == "democrat",]

barplot(table(senators$Class), col="grey")

barplot(table(senators$Law.on.religions), col="grey")

barplot(table(senators$Adoption.project), col="grey")

require(corrgram)

plotCorrelation <- function (data){
  
  data <- subset(data, select = -c(Class))
  
  data.reval <-  apply(data, 2, function(column){
    column <- as.numeric(factor(column, levels=c("no", "abstain", "yes")))
  })
  
  corrgram(data.reval, order = FALSE, cor.method = "kendall")
  #corrgram(cor.senator.reval, order = FALSE, cor.method = "kendall", lower.panel=panel.ellipse, upper.panel=panel.pie, text.panel=panel.txt)
}

plotCorrelation(senators)

mosaicplot(senators$Class ~ senators$Nicaragua.assistance.project, shade=FALSE, color=TRUE, main="Nicaragua assistance project", xlab = "Senators", ylab ="Votes")
mosaicplot(senators$Class ~ senators$Rocket.project, shade=FALSE, color=TRUE, main="Rocket project", xlab = "Senators", ylab ="Votes")

mosaicplot(senators$Class ~ senators$Salvador.project, shade=FALSE, color=TRUE, main="Salvador project", xlab = "Senators", ylab ="Votes")
mosaicplot(senators$Class ~ senators$Project.on.funds, shade=FALSE, color=TRUE, main="Project on funds", xlab = "Senators", ylab ="Votes")

plotCorrelation(republicans)

plotCorrelation(democrats)
#add heat map for Salvador vs Rocket projects http://www.r-bloggers.com/search/heatmap


clust.hierarchically <- function(data){
  library(cluster)
  library(stats)
  
  data.dist <- daisy(data, metric = "gower", stand = TRUE) 
  data.hc <- hclust(data.dist, method="complete")
  return(data.hc)
}
get.groups <- function(data, groups.num){
  library(stats)
  data.hc <- clust.hierarchically(data)
  groups <- cutree(data.hc, groups.num)
  return(cbind(data, groups))
}

republicans.hc <- clust.hierarchically(republicans)
plot(republicans.hc)

democrats.hc <- clust.hierarchically(democrats)
plot(democrats.hc)

senators.hc <- clust.hierarchically(subset(senators, select = -c(Class)))
senators.hc$labels <- senators$Class
#senators.hc$labels <- paste(as.character(senators.hc$order), as.character(senators$Class))
plot(senators.hc)

#hclust distances http://stackoverflow.com/questions/2218395/how-do-you-compare-the-similarity-between-two-dendrograms-in-r

get.number.of.clusters <- function(data) {
  library(fpc)
  library(cluster)
  
  data.dist <- daisy(data, metric = "gower", stand = TRUE)
  data.best <- pamk(data.dist)
  #cat("Number of clusters estimated by optimum average silhouette width:", data.best$nc, "\n")
  #visualized silhouette widthp
  #plot(pam(data.dist, data.best$nc))
  return(data.best)
}

clust.pam <- function(data) {
  par(mfrow=c(2,1))
  library(cluster)
  clust.num <- get.number.of.clusters(data)
  data.dist <- daisy(data, metric = "gower", stand = TRUE)
  pam.clust <- pam(data.dist, k=clust.num$nc)
  plot(pam(data.dist, clust.num$nc))
  clusplot(pam.clust) #printed clusters
  return(pam.clust)
}

clust.pam(republicans)
clust.pam(democrats)
clust.pam(subset(senators, select = -c(Class)))
