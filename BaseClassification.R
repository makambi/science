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






