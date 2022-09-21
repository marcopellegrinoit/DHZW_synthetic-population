library(readr)
library("dplyr")
library(ggplot2)
library("this.path")
setwd(this.path::this.dir())

# Load ODiN dataset
setwd(paste0(this.path::this.dir(), "/data/Formatted"))
df_ODiN <- read_csv("df_DHZW.csv")

displacements <- df_ODiN %>%
  select(disp_id, disp_modal_choice) %>%
  distinct()
summary_displacements <- data.frame(table(displacements$disp_modal_choice))
summary_displacements$type <- 'Displacements'

rides <- df_ODiN %>%
  select(ride_id, ride_modal_choice) %>%
  distinct()
summary_rides <- data.frame(table(rides$ride_modal_choice))
summary_rides$type <- 'Rides'

summary <- rbind(summary_displacements, summary_rides)
colnames(summary) <- c('Modal choice', 'Frequency', 'Type')

ggplot(summary, aes(as.character(`Modal choice`), as.numeric(Frequency))) +
  geom_bar(aes(fill = Type),
           position = "dodge",
           stat = "identity",
           width=0.4)+
  ggtitle("Difference in distribution of modal choice in displacements and rides")+
  ylab("Frequency of modal choice")+
  xlab("Modal choice")+
  labs(fill = "Type")+
  theme(title = element_text(size = 20),
        legend.title=element_text(size=20),
        legend.text=element_text(size=15),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))