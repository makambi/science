data_path <- "Desktop/DataAnalysis/Soft/Classification.csv"
cols <- c("Disability program", "Water resourses project", "Adoption project",	"Law on helthcare",	"Salvador project",	"Law on religions",
          "Antisatellite project",	"Nicaragua assistance project",	"Rocket project",	"Law on immigrants",	"Alternative fuel sources",	"Law on education",
          "Project on funds",	"Project on crime",	"Project of customs", 	"Project exports",	"Class")
data <- read.csv(data_path, sep = '\t', header = FALSE, col.names = cols)


kc <- kmeans(data, 7)
