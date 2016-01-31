data_path <- "Desktop/DataAnalysis/Soft/Classification.csv"
cols <- c("Disability program", "Water resourses project", "Adoption project",	"Law on helthcare",	"Salvador project",	"Law on religions",
          "Antisatellite project",	"Nicaragua assistance project",	"Rocket project",	"Law on immigrants",	"Alternative fuel sources",	"Law on education",
          "Project on funds",	"Project on crime",	"Project of customs", 	"Project exports",	"Class")
senators <- read.csv(data_path, sep = '\t', header = FALSE, col.names = cols)

barplot(table(senators$Class), col="grey")

barplot(table(senators$Law.on.religions), col="grey")

barplot(table(senators$Adoption.project), col="grey")

require(corrgram)

cor.senators <- senators[senators$Class == "republican",]
cor.senators <- subset(cor.senators, select = -c(Class))

cor.senator.reval <-  apply(cor.senators, 2, function(column){
  column <- as.numeric(factor(column, levels=c("no", "abstain", "yes")))
})

corrgram(cor.senator.reval, order = FALSE, cor.method = "kendall")

#corrgram(cor.senator.reval, order = FALSE, cor.method = "kendall", lower.panel=panel.ellipse, upper.panel=panel.pie, text.panel=panel.txt)

cor.senators <- senators[senators$Class == "democrat",]
cor.senators <- subset(cor.senators, select = -c(Class))

cor.senator.reval <-  apply(cor.senators, 2, function(column){
  column <- as.numeric(factor(column, levels=c("no", "abstain", "yes")))
})

corrgram(cor.senator.reval, order = FALSE, cor.method = "kendall")

barplot(table(senators$Rocket.project))

barplot(table(senators$Law.on.immigrants))

barplot(table(senators$Law.on.education))


mosaicplot(senators$Class ~ senators$Law.on.education, shade=FALSE, color=TRUE)

mosaicplot(senators$Class ~ senators$Law.on.immigrants, shade=FALSE, color=TRUE)

mosaicplot(senators$Class ~ senators$Law.on.religions, shade=FALSE, color=TRUE)


