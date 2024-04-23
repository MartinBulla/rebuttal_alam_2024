# tools
require(data.table)
require(ggplot2)
library(ggpmisc)
library(ggpubr)
library(grid)
library(patchwork)

set.seed(5)

col_p = 'darkgrey'
col_R = 'red'
col_l = 'red'
col_ <- c("#46B8DAFF", "#EEA236FF")#c("#357EBDFF", "#D43F3AFF", "#46B8DAFF", "#5CB85CFF", "#EEA236FF", "#9632B8FF", "#9632B8FF")[7:1]

# data
d = fread(here::here('Data/rebuttal_fig_2.csv'))

d[, path_delta:=long-short]
d[, time_in_long:= trial_long/(trial_short+trial_long)]
d[, long_vs_baseline:=(trial-(pre+post)/2)/100]

g1=
ggplot(d, aes(x = path_delta, y = long_vs_baseline)) + 
    geom_point(aes(fill = side_bias, shape = song_pair), data = d) +
    stat_poly_line(se = FALSE, col = col_l) +
    annotate("text", x = 15, y = 0, label = "pair 1", size =3) + 
    annotate("text", x = 30.29, y = 0, label = "pair 2", size =3) + 
    annotate("text", x = 5.72, y = 0, label = "pair 3", size =3) + 

    annotate("text", x = 10, y = 0.5, label = "Long-path song played in:", size =2.5, hjust = 0) + 
    annotate("text", x = 13, y = 0.475, label = "preffered arm", size =2.5, col = col_[2], hjust = 0) + 
    annotate("text", x = 13, y = 0.45, label = "non-preffered arm", size =2.5, col = col_[1], hjust = 0) + 

    stat_cor(cor.coef.name = "r", aes(label = after_stat(r.label)),  col = col_R, r.accuracy = 0.1, label.x = 30, hjust = 0.5, cex = 3) + theme_bw() + 
    coord_cartesian(xlim = c(0, 35), ylim = c(0, .58)) +
    scale_x_continuous(breaks = seq(0,30, by=10), expand = c(0,0))+
    #scale_y_continuous(breaks = seq(20,50, by=10), labels = seq(20,50, by=10), expand = c(0,0))

    scale_fill_manual(guide = FALSE, values = col_) +
    scale_shape_manual(guide = FALSE, values = c(21, 22, 23)) +
    #guides(fill = guide_legend(override.aes=list(shape=21))) +
    labs(x = 'Path-length difference of a stimulus pair', y = 'Time spent with a long-path song\n[trial - baseline]')+
    theme_bw()

g2=
ggplot(d, aes(x = path_delta, y = time_in_long)) + 
    geom_hline(yintercept=0.5, linetype='dotted', col = 'grey')+
    geom_point(aes(fill = side_bias, shape = song_pair), data = d) +
    stat_poly_line(se = FALSE, col = col_l) +
    annotate("text", x = 15, y = 0.3, label = "pair 1", size =3) + 
    annotate("text", x = 30.29, y = 0.3, label = "pair 2", size =3) + 
    annotate("text", x = 5.72, y = 0.3, label = "pair 3", size =3) + 
    stat_cor(cor.coef.name = "r", aes(label = after_stat(r.label)),  col = col_R, r.accuracy = 0.1, label.x = 30, hjust = 0.5, cex = 3) + theme_bw() + 
    coord_cartesian(xlim = c(0, 35), ylim = c(0.3, 1)) +
    scale_x_continuous(breaks = seq(0,30, by=10), expand = c(0,0))+
    scale_y_continuous(breaks = seq(0.4,1, by=0.1))+
    #scale_y_continuous(breaks = seq(20,50, by=10), labels = seq(20,50, by=10), expand = c(0,0))

    scale_fill_manual(guide = 'none', values = col_) +
    scale_shape_manual(guide = 'none', values = c(21, 22, 23)) +
    labs(x = 'Path-length difference of a stimulus pair', y = 'Proportion of time with a long-path song') +
    theme_bw()


png(here::here("Output/Fig_point_2_v2.png"),  width = 16, height = 8, unit = "cm", res = 600)

g1+xlab('')|g2+xlab('');grid::grid.draw(grid::textGrob('Path-length difference of a stimulus pair', y = 0.055, x = 0.55, gp = gpar(fontsize =12)))#

dev.off()

#ggsave(here::here('Output/Fig_point_2.png'), width = 16, height = 8, units = 'cm')

#(g0|g1|g2)
#grid::grid.draw(grid::textGrob('Day in the breading season\n ', y = 0.055, x = 0.6, gp = gpar(fontsize =7)))
#dev.off()