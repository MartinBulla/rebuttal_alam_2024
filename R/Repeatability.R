# =============================================================
# ❗ The script runs relative to the project's root directory,
#  requires "Fig_2b.csv" and generates Fig. 2
# =============================================================

# tools
require(data.table)
require(ggplot2)
library(ggpmisc)
library(ggpubr)
library(grid)
library(patchwork)
library(rptR)
library(tidyr)

set.seed(5)

col_p = 'darkgrey'
col_R = 'red'
col_l = 'red'
col_ <- c("#46B8DAFF", "#EEA236FF")#c("#357EBDFF", "#D43F3AFF", "#46B8DAFF", "#5CB85CFF", "#EEA236FF", "#9632B8FF", "#9632B8FF")[7:1]

# data
b = fread(here::here('Data/Fig_2b.csv'), header = T)
d = pivot_longer(b, 
                    cols = `1`:`15`, 
                    names_to = "UMAP", 
                    values_to = "path_length")
ggplot(d, aes(x = path_length)) + geom_histogram()
# repeatability
R = rpt(path_length ~ (1 | bird), grname = "bird", data = d, datatype = "Gaussian")
RR = data.table(merge(data.frame(trait = "Path length", velocity = "Average path"), paste0(round(R$R * 100), "%"))) %>% setnames(new = c("trait", "specification", "repeatability"))
RR[, CI := paste0(paste(round(R$CI_emp*100)[1], round(R$CI_emp*100)[2], sep = "-"), '%')] 
RR[, pred := 100*R$R]
RR[, lwr := 100*R$CI_emp[1]]
RR[, upr := 100*R$CI_emp[2]]

# simulation umap repeatability
r = fread(here::here('Data/remya_distance_dataframe_1.csv'), header = T)
r[, id:=paste(digit1, digit2)]
Rr = rpt(distance ~ (1 | id), grname = "id", data = r, datatype = "Gaussian")
RRr = data.table(merge(data.frame(trait = "Path length", velocity = "Average path"), paste0(round(R$R * 100), "%"))) %>% setnames(new = c("trait", "specification", "repeatability"))
RRr[, CI := paste0(paste(round(R$CI_emp*100)[1], round(R$CI_emp*100)[2], sep = "-"), '%')] 
RRr[, pred := 100*R$R]
RRr[, lwr := 100*R$CI_emp[1]]
RRr[, upr := 100*R$CI_emp[2]]
VAP = RR
