# =============================================================
# ❗ The script runs relative to the project's root directory,
#  requires "Fig_4c.csv" and generates Fig. 1
# =============================================================

# tools
require(data.table)
require(ggplot2)
require(ggpmisc)
require(ggpubr)
require(patchwork)

set.seed(5)

g_r_x = 35
col_p = 'darkgrey'
col_R = 'red'
col_l = 'red'
per_ = 0.05

# top panels - random data
d = data.table(
x = rnorm(1000, mean = 37.5, sd = 4),
y = rnorm(1000, mean = 37.5, sd = 4)
)

g1=
ggplot(d, aes(x = x, y = y)) + 
    geom_point(col = col_p) +
    stat_poly_line(se = FALSE, col = col_l) +
    stat_cor(cor.coef.name = "r", aes(label = after_stat(r.label)),  col = col_R, r.accuracy = 0.1, label.x = g_r_x, label.y = 52-(52-20)*per_ , hjust = 0.5, cex = 3) + theme_bw() + 
    coord_cartesian(xlim = c(20, 50), ylim = c(20, 52)) +
    scale_x_continuous(breaks = seq(20,50, by=10), labels = seq(20,50, by=10), expand = c(0,0))+
    scale_y_continuous(breaks = seq(20,50, by=10), labels = seq(20,50, by=10), expand = c(0,0))

    #stat_poly_eq() 

g2=  
ggplot(d, aes(x = x, y = y-x)) + 
    geom_point(col = col_p) +
    stat_poly_line(se = FALSE, col = col_l) +
    stat_cor(cor.coef.name = "r", aes(label = after_stat(r.label)),  col = col_R, r.accuracy = 0.1, label.x = g_r_x, label.y = 18.2-36*per_, hjust = 0.5, cex = 3) + theme_bw() + 
    coord_cartesian(xlim = c(20, 50), ylim = c(-18, 18.2)) +
    scale_x_continuous(breaks = seq(20,50, by=10), labels = seq(20,50, by=10), expand = c(0,0))+
    scale_y_continuous(breaks = seq(-18,18, by=9), labels = seq(-18,18, by=9),expand = c(0,0))

g3=  
ggplot(d, aes(x = x, y = y+x)) + 
    geom_point(col = col_p) +
    stat_poly_line(se = FALSE, col = col_l) +
    stat_cor(cor.coef.name = "r", aes(label = after_stat(r.label)),  col = col_R, r.accuracy = 0.1, label.x = g_r_x, label.y = 95-(95-55)*per_, hjust = 0.5, cex = 3) + theme_bw() + 
    coord_cartesian(xlim = c(20, 50), ylim = c(55, 95)) +
    scale_x_continuous(breaks = seq(20,50, by=10), labels = seq(20,50, by=10), expand = c(0,0))+
    scale_y_continuous(breaks = seq(55,95, by=10), labels =seq(55,95, by=10),expand = c(0,0))

    #g1|g2|g3

    #ggsave(here::here('Output/Fig_1.png'), g1|g2|g3, width = 12*2, height = 4*2, units = 'cm')

# bottom panels - Alam et al's data
dd = fread('Data/Fig_4c.csv')
dd$tutor_path = dd$tutor_parth
dd[, pupil_path_full:=tutor_path+pupil_path]
dd[, tutor_minus_pupil:=pupil_path]

gg1=
ggplot(dd, aes(x =tutor_path, y = pupil_path_full)) + 
    geom_point(col = col_p) +
    annotate("text", x=g_r_x, y=52-(52-20)*per_*3, label= "Color indicates a tutor", cex = 2.75, hjust = 0.5)+ 
    stat_poly_line(se = FALSE, col = col_l) +
    stat_cor(cor.coef.name = "r", aes(label = after_stat(r.label)),  col = col_R, r.accuracy = 0.1, label.x = g_r_x, label.y = 52-(52-20)*per_, hjust = 0.5, cex = 3)+ 
    coord_cartesian(xlim = c(20, 50), ylim = c(20, 52)) +
    xlab('Tutor path length') + ylab('Pupil path length') +
   scale_x_continuous(breaks = seq(20,50, by=10), labels = seq(20,50, by=10), expand = c(0,0))+
    scale_y_continuous(breaks = seq(20,50, by=10), labels = seq(20,50, by=10), expand = c(0,0))+
    geom_point(aes(col = as.factor(tutor))) +
    theme_bw() + theme(legend.position="none")

gg2=  
ggplot(dd, aes(x =tutor_path, y = pupil_path_full-tutor_path)) + 
    geom_point(col = col_p) +
    stat_poly_line(se = FALSE, col = col_l) +
    stat_cor(cor.coef.name = "r", aes(label = after_stat(r.label)),  col = col_R, r.accuracy = 0.1, label.x = g_r_x, label.y = 15-(15+20)*per_, hjust = 0.5, cex = 3)+ 
    coord_cartesian(xlim = c(20, 50), ylim = c(-21, 15)) +
    xlab('Tutor path length') + ylab('Pupil minus tutor path length') +
    scale_x_continuous(breaks = seq(20,50, by=10), labels = seq(20,50, by=10), expand = c(0,0))+
    scale_y_continuous(breaks = seq(-20,15, by=5), labels =seq(-20,15, by=5),expand = c(0,0))+
    geom_point(aes(col = as.factor(tutor))) +
    theme_bw() + theme(legend.position="none")
 

gg3=  
ggplot(dd, aes(x =tutor_path, y = tutor_path+pupil_path_full)) + 
    geom_point(col = col_p) +
    stat_poly_line(se = FALSE, col = col_l) +
    stat_cor(cor.coef.name = "r", aes(label = after_stat(r.label)),  col = col_R, r.accuracy = 0.1, label.x = g_r_x, label.y = 90-(90-50)*per_, hjust = 0.5, cex = 3)+ 
    coord_cartesian(xlim = c(20, 50), ylim = c(50, 90)) +
    xlab('Tutor path length') + ylab('Pupil + tutor path length')+
    scale_x_continuous(breaks = seq(20,50, by=10), labels = seq(20,50, by=10), expand = c(0,0))+
    scale_y_continuous(breaks = seq(50,90, by=10), labels =seq(50,90, by=10),expand = c(0,0))+
    geom_point(aes(col = as.factor(tutor))) +
    theme_bw() + theme(legend.position="none")

    #gg1|gg2|gg3

    #ggsave(here::here('Output/Fig_2_v2.png'), gg1|gg2|gg3, width = 12*2, height = 4*2, units = 'cm')

# combine

combine_1 =  (g1|g2|g3) & plot_annotation(subtitle = "Random data; n = 1000") #& theme(plot.title = element_text(hjust = .5))
combine_2 =  (gg1|gg2|gg3) & plot_annotation(subtitle = "Alam et al. 2024 data; n = 17 pupils and 8 tutors ") #& theme(plot.title = element_text(hjust = .5))

wrap_elements(combine_1) / wrap_elements(combine_2)

ggsave(here::here('Output/Fig_point_1_v2.png'), wrap_elements(combine_1) / wrap_elements(combine_2), width = 11*2, height = 4*2*2, units = 'cm')

# END