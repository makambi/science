data_path <- "Desktop/DataAnalysis/Soft/Classification.csv"
cols <- c("Disability program", "Water resourses project", "Adoption project",	"Law on helthcare",	"Salvador project",	"Law on religions",
          "Antisatellite project",	"Nicaragua assistance project",	"Rocket project",	"Law on immigrants",	"Alternative fuel sources",	"Law on education",
          "Project on funds",	"Project on crime",	"Project of customs", 	"Project exports",	"Class")
senators <- read.csv(data_path, sep = '\t', header = FALSE, col.names = cols)

republicans = subset(senators, Class=='republican')
republicans = subset(republicans, select = -c(Class))

democrats = subset(senators, Class=='democrat')
democrats = subset(democrats, select = -c(Class))

library(cluster)
republicans.dist <- daisy(republicans, metric = "gower", stand = TRUE) 
democrat.dist <- daisy(democrats, metric = "gower", stand = TRUE)

library(stats)
republicans.hc <- hclust(republicans.dist, method="complete")
democrat.hc <- hclust(democrat.dist, method="complete")

plot(republicans.hc)
View(republicans)
plot(senators[c(69,149),]) #different
plot(senators[c(109,124),]) #similiar

plot(democrat.hc)
View(democrat.hc)
plot(senators[c(4,12),]) #similiar
plot(senators[c(118,131),]) #similiar

library(fpc)
republicans.best <- pamk(republicans.dist)
cat("number of clusters estimated by optimum average silhouette width:", republicans.best$nc, "\n")
plot(pam(republicans.dist, republicans.best$nc))

clusplot(pam(republicans.dist, k=republicans.best$nc))

democrat.best <- pamk(democrat.dist)
cat("number of clusters estimated by optimum average silhouette width:", democrat.best$nc, "\n")
plot(pam(democrat.dist, democrat.best$nc))

clusplot(pam(democrat.dist, k=democrat.best$nc))
