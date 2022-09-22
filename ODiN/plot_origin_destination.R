library(readr)
library(dplyr)

library(circlize)

library(od)
library(ggplot2)


setwd(paste0(this.path::this.dir(), "/data/Formatted"))
df_ODiN <- read_csv("df_DHZW.csv")

# load DHZW area
setwd(paste0(this.path::this.dir(), "/data"))
DHZW_neighborhood_codes <- read.csv("DHZW_neighbourhoods_codes.csv", sep = ";" ,header=F)$V1
DHZW_PC4_codes <- read.csv("DHZW_PC4_codes.csv", sep = ";" ,header=F)$V1

df_ODiN[!(df_ODiN$disp_start_PC4 %in% DHZW_PC4_codes),]$disp_start_PC4 = 'outside DHZW'
df_ODiN[!(df_ODiN$disp_arrival_PC4 %in% DHZW_PC4_codes),]$disp_arrival_PC4 = 'outside DHZW'

df_ODiN <- df_ODiN %>%
  select(disp_start_PC4, disp_arrival_PC4) %>%
  group_by(disp_start_PC4, disp_arrival_PC4) %>%
  summarise(freq = n())

od_matrix = od_to_odmatrix(x=df_ODiN, name_orig = 'disp_start_PC4', name_dest = 'disp_arrival_PC4', attrib = 'freq')

setwd(paste0(this.path::this.dir(), "/plots"))
png('origin_destination_graph.png', width = 2000, height = 2000, res = 400)
chordDiagram(od_matrix, link.border = "black")
title("Origin-destination graph", cex = 0.6)
dev.off()