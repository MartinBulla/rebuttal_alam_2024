# =============================================================
# ❗ The script runs relative to the project's root directory,
#  requires "Fig_4c.csv" and generates Fig. 1
# =============================================================

# tools
require(data.table)
require(ggplot2)
require(ggpmisc)
require(ggpubr)
require(grafify)
require(gridExtra)
require(patchwork)

set.seed(5)

g_r_x = 35
col_p = 'darkgrey'
col_pc = 'grey10'
col_R = 'red'#'black'#'red'
col_l = 'red'#'black'#'red'
col_ <- c("#357EBDFF", "#D43F3AFF", "#46B8DAFF", "#5CB85CFF", "#EEA236FF", "#9632B8FF", "#9632B8FF")
per_ = 0.05

# top panels - random data
d = data.table(
x = rnorm(1000, mean = 35, sd = 4),
y = rnorm(1000, mean = 35, sd = 4)
)
#save(file='Data/Dat_rnorm.Rdata', d)
load(file='Data/Dat_rnorm.Rdata')

g1=
ggplot(d, aes(x = x, y = y)) + 
    geom_point(col = col_p, size = 1.5, alpha = 0.6) +
    stat_poly_line(se = FALSE, col = col_l) +
    stat_cor(cor.coef.name = "r", aes(label = after_stat(r.label)),  col = col_R, r.accuracy = 0.1, label.x = g_r_x, label.y = 52-(52-20)*per_ , hjust = 0.5, cex = 3) + 
    coord_cartesian(xlim = c(20, 50), ylim = c(20, 52)) +
    scale_x_continuous(breaks = seq(20,50, by=10), labels = seq(20,50, by=10), expand = c(0,0))+
    scale_y_continuous(breaks = seq(20,50, by=10), labels = seq(20,50, by=10), expand = c(0,0)) +
    theme_bw() + 
    theme(axis.ticks = element_blank())

    #stat_poly_eq() 

g2=  
ggplot(d, aes(x = x, y = y-x)) + 
    geom_point(col = col_p, size = 1.5, alpha = 0.6) +#geom_point(fill = col_p, pch = 21, size = 1.5) +#geom_point(col = col_p) +
    stat_poly_line(se = FALSE, col = col_l) +
    stat_cor(cor.coef.name = "r", aes(label = after_stat(r.label)),  col = col_R, r.accuracy = 0.1, label.x = g_r_x, label.y = 20-36*per_, hjust = 0.5, cex = 3) +
    coord_cartesian(xlim = c(20, 50), ylim = c(-20, 20)) +
    scale_x_continuous(breaks = seq(20,50, by=10), labels = seq(20,50, by=10), expand = c(0,0))+
    scale_y_continuous(breaks = seq(-20,20, by=10), labels = seq(-20,20, by=10),expand = c(0,0)) +
    theme_bw() + 
    theme(axis.ticks = element_blank())

g3=  
ggplot(d, aes(x = x, y = y+x)) + 
    geom_point(col = col_p, size = 1.5, alpha = 0.6) +#geom_point(fill = col_p, pch = 21, size = 1.5) +
    stat_poly_line(se = FALSE, col = col_l) +
    stat_cor(cor.coef.name = "r", aes(label = after_stat(r.label)),  col = col_R, r.accuracy = 0.1, label.x = g_r_x, label.y = 90-(90-50)*per_, hjust = 0.5, cex = 3) + 
    coord_cartesian(xlim = c(20, 50), ylim = c(50, 90)) +
    scale_x_continuous(breaks = seq(20,50, by=10), labels = seq(20,50, by=10), expand = c(0,0))+
    scale_y_continuous(breaks = seq(50,90, by=10), labels =seq(55,95, by=10),expand = c(0,0)) +
    theme_bw() + 
    theme(axis.ticks = element_blank())

    #g1|g2|g3

    #ggsave(here::here('Output/Fig_1.png'), g1|g2|g3, width = 12*2, height = 4*2, units = 'cm')

# bottom panels - Alam et al's data
dd = fread('Data/Fig_4c.csv')
dd[, pupil_path_full:=tutor_path+pupil_path]
dd[, tutor_minus_pupil:=pupil_path]

gg1=
ggplot(dd, aes(x =tutor_path, y = pupil_path_full)) + 
    geom_point(aes(col = as.factor(tutor)), size = 2.2, alpha = 0.6) +
    scale_color_grafify(palette = 'fishy')+#, ColSeq = FALSE)+
    annotate("text", x=g_r_x, y=50-(50-20)*per_*4, label= "Color indicates a tutor", cex = 2.75, hjust = 0.5)+ 
    stat_poly_line(se = FALSE, col = col_l) +
    stat_cor(cor.coef.name = "r", aes(label = after_stat(r.label)),  col = col_R, r.accuracy = 0.1, label.x = g_r_x, label.y = 50-(50-20)*per_, hjust = 0.5, cex = 3)+ 
    coord_cartesian(xlim = c(20, 50), ylim = c(20, 50)) +
    xlab('Tutor path length') + ylab('Pupil path length') +
    scale_x_continuous(breaks = seq(20,50, by=10), labels = seq(20,50, by=10), expand = c(0,0))+
    scale_y_continuous(breaks = seq(20,50, by=10), labels = seq(20,50, by=10), expand = c(0,0))+
    theme_bw() + 
    theme(  legend.position="none",
            axis.ticks = element_blank())

gg2=  
ggplot(dd, aes(x =tutor_path, y = pupil_path_full-tutor_path)) + 
    geom_point(aes(col = as.factor(tutor)), size = 2.2, alpha = 0.6) +
    scale_color_grafify(palette = 'fishy')+#, ColSeq = FALSE)+
    stat_poly_line(se = FALSE, col = col_l) +
    stat_cor(cor.coef.name = "r", aes(label = after_stat(r.label)),  col = col_R, r.accuracy = 0.1, label.x = g_r_x, label.y = 20-(20+20)*per_, hjust = 0.5, cex = 3)+ 
    coord_cartesian(xlim = c(20, 50), ylim = c(-20, 20)) +
    xlab('Tutor path length') + ylab('Pupil minus tutor path length') +
    scale_x_continuous(breaks = seq(20,50, by=10), labels = seq(20,50, by=10), expand = c(0,0))+
    scale_y_continuous(breaks = seq(-20,20, by=10), labels =seq(-20,20, by=10),expand = c(0,0))+
    theme_bw() + 
    theme(  legend.position="none",
            axis.ticks = element_blank())
 

gg3=  
ggplot(dd, aes(x =tutor_path, y = tutor_path+pupil_path_full)) + 
    geom_point(aes(col = as.factor(tutor)), size = 2.2, alpha = 0.6) +
    scale_color_grafify(palette = 'fishy')+#, ColSeq = FALSE)+
    stat_poly_line(se = FALSE, col = col_l) +
    stat_cor(cor.coef.name = "r", aes(label = after_stat(r.label)),  col = col_R, r.accuracy = 0.1, label.x = g_r_x, label.y = 90-(90-50)*per_, hjust = 0.5, cex = 3)+ 
    coord_cartesian(xlim = c(20, 50), ylim = c(50, 90)) +
    xlab('Tutor path length') + ylab('Pupil + tutor path length')+
    scale_x_continuous(breaks = seq(20,50, by=10), labels = seq(20,50, by=10), expand = c(0,0))+
    scale_y_continuous(breaks = seq(50,90, by=10), labels =seq(50,90, by=10),expand = c(0,0))+
    theme_bw() + 
    theme(  legend.position="none",
            axis.ticks = element_blank())

    #gg1|gg2|gg3

    #ggsave(here::here('Output/Fig_2_v2.png'), gg1|gg2|gg3, width = 12*2, height = 4*2, units = 'cm')

# combine

combine_1 =  (g1|g2|g3) & plot_annotation(subtitle = "Random data; n = 1000") #& theme(plot.title = element_text(hjust = .5))
combine_2 =  (gg1|gg2|gg3) & plot_annotation(subtitle = "Alam et al. 2024 data; n = 17 pupils and 8 tutors ") #& theme(plot.title = element_text(hjust = .5))

wrap_elements(combine_1) / wrap_elements(combine_2)

ggsave(here::here('Output/Fig_point_1_rev_v3.png'), wrap_elements(combine_1) / wrap_elements(combine_2), width = 11*2, height = 4*2*2, units = 'cm')

# replies to the referees - residuals from the line of idenity
# point A
g0 = 
ggplot(d, aes(x = x, y = y)) + 
    geom_segment(aes(xend = x, yend = x), color = "skyblue") +    # Residual lines
    geom_point(col = col_p) + # raw data
    geom_abline(intercept = 0, slope = 1, color = col_l) +  # Line of identity (y = x)
    labs(title = "Residuals from Line of Identity", subtitle = "Random data; n = 1000") +  # Labels
    coord_cartesian(xlim = c(20, 50), ylim = c(20, 52)) +
    scale_x_continuous(breaks = seq(20,50, by=10), labels = seq(20,50, by=10), expand = c(0,0))+
    scale_y_continuous(breaks = seq(20,50, by=10), labels = seq(20,50, by=10), expand = c(0,0))+
    theme_minimal()   

gg0 = 
ggplot(dd, aes(x = tutor_path, y = pupil_path_full)) + 
    geom_segment(aes(xend = tutor_path, yend = tutor_path), color = "skyblue") +    # Residual lines
    geom_point(col = col_p) + # raw data
    geom_abline(intercept = 0, slope = 1, color = col_l) +  # Line of identity (y = x)
    labs(title = "", subtitle = "Alam et al. 2024 data; n = 17 pupils and 8 tutors") +  # Labels
    coord_cartesian(xlim = c(20, 50), ylim = c(20, 52)) +
    scale_x_continuous(breaks = seq(20,50, by=10), labels = seq(20,50, by=10), expand = c(0,0))+
    scale_y_continuous(breaks = seq(20,50, by=10), labels = seq(20,50, by=10), expand = c(0,0))+
    theme_minimal() 

ggsave(here::here('Output/Fig_point_1_rev.png'), grid.arrange(g0, gg0, ncol = 2), width = 14.7, height = 8, units = 'cm')

# point B
d[, res := y-x]
dr = rbind(d[order(x)][1:7], d[order(x)][95:1000])
d_spl = d[order(x)][1:7]
d_lpl = d[order(x)][95:1000]

dd[, res := pupil_path_full-tutor_path]
ddr = rbind(dd[order(tutor_path)][1:7], dd[order(tutor_path)][12:17])
dd_spl = dd[order(tutor_path)][1:7]
dd_lpl = dd[order(tutor_path)][12:17]

t.test(x = d_spl$res, y = dd_spl$res)
t.test(x = d_lpl$res, y = dd_lpl$res)




# END