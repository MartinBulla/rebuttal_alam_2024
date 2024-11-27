# =============================================================
# ❗ The script runs relative to the project's root directory,
#  requires "rebuttal_fig_2.csv" and generates Fig. 2
# =============================================================

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
col_ = c("#46B8DAFF", "#EEA236FF")#c("#357EBDFF", "#D43F3AFF", "#46B8DAFF", "#5CB85CFF", "#EEA236FF", "#9632B8FF", "#9632B8FF")[7:1]
per_ = 0.05

# data
d = fread(here::here('Data/rebuttal_fig_2.csv'))

d[, path_delta:=long-short]
d[, time_in_long:= trial_long/(trial_short+trial_long)]
d[, long_vs_baseline:=(trial-(pre+post)/2)/100]

# baseline mean of pre and post
x = d[, mean(long_vs_baseline), by = path_delta] %>% setnames('V1', 'mean')

g=
ggplot(d, aes(x = path_delta, y = long_vs_baseline)) + 
    geom_point(aes(col = side_bias), data = d, cex = 2, alpha = 0.6) +
    stat_poly_line(se = FALSE, col = col_l) +
    geom_point(aes(x = path_delta, y = mean), data = x, pch = 23, cex = 2, fill = 'red', alpha = 0.6) +

    annotate("text", x = 10, y = 0.525, label = "Long-path song played in:", size =2.75, hjust = 0) + 
    annotate("text", x = 13, y = 0.49, label = "preferred arm", size =2.75, col = col_[2], hjust = 0) + 
    annotate("text", x = 13, y = 0.455, label = "non-preferred arm", size =2.75, col = col_[1], hjust = 0) + 

    stat_cor(cor.coef.name = "r", aes(label = after_stat(r.label)),  col = col_R, r.accuracy = 0.1, label.x = 17.5, label.y = 0.6-(0.6+.03)*per_*1.3,hjust = 0.5, vjust = 0, cex = 3) +

    coord_cartesian(xlim = c(0, 35), ylim = c(-.03, .6)) +
    scale_x_continuous(breaks = seq(0,30, by=10), expand = c(0,0))+
    scale_y_continuous(breaks = seq(0,.6, by=.2), expand = c(0,0))+
    scale_color_manual(guide = "none", values = col_) +
    
    annotate("text", x = 15, y = 0, label = "pair 1", size =3) + 
    annotate("text", x = 30.29, y = 0, label = "pair 2", size =3) + 
    annotate("text", x = 5.72, y = 0, label = "pair 3", size =3) + 
    #scale_shape_manual(guide = "none", values = c(21, 22, 23)) +
    #guides(fill = guide_legend(override.aes=list(shape=21))) +
    labs(x = 'Long-path minus short-path song\n ', y = 'Proportion of time with a long-path\n[trial minus baseline]')+
    theme_bw()+
    theme(axis.ticks = element_blank())

ggsave(here::here("Output/Fig_point_2_rev.png"), g, width = 8, height = 7.4, unit = "cm")

# pre as baseline
xp = d[, mean((trial-pre)/100), by = path_delta] %>% setnames('V1', 'mean')

gp=
ggplot(d, aes(x = path_delta, y = (trial-pre)/100)) + 
    geom_point(aes(col = side_bias), data = d, cex = 2, alpha = 0.6) +
    stat_poly_line(se = FALSE, col = col_l) +
    geom_point(aes(x = path_delta, y = mean), data = xp, pch = 23, cex = 2, fill = 'red', alpha = 0.6) +

    annotate("text", x = 10, y = 0.525, label = "Long-path song played in:", size =2.75, hjust = 0) + 
    annotate("text", x = 13, y = 0.49, label = "preferred arm", size =2.75, col = col_[2], hjust = 0) + 
    annotate("text", x = 13, y = 0.455, label = "non-preferred arm", size =2.75, col = col_[1], hjust = 0) + 

    stat_cor(cor.coef.name = "r", aes(label = after_stat(r.label)),  col = col_R, r.accuracy = 0.1, label.x = 17.5, label.y = 0.6-(0.6+.03)*per_*1.3,hjust = 0.5, vjust = 0, cex = 3) +

    coord_cartesian(xlim = c(0, 35), ylim = c(-.03, .6)) +
    scale_x_continuous(breaks = seq(0,30, by=10), expand = c(0,0))+
    scale_y_continuous(breaks = seq(0,.6, by=.2), expand = c(0,0))+
    scale_color_manual(guide = "none", values = col_) +
    
    annotate("text", x = 15, y = 0, label = "pair 1", size =3) + 
    annotate("text", x = 30.29, y = 0, label = "pair 2", size =3) + 
    annotate("text", x = 5.72, y = 0, label = "pair 3", size =3) + 
    #scale_shape_manual(guide = "none", values = c(21, 22, 23)) +
    #guides(fill = guide_legend(override.aes=list(shape=21))) +
    labs(x = 'Long-path minus short-path song\n ', y = 'Proportion of time with a long-path\n[trial minus pre-trial]')+
    theme_bw()+
    theme(axis.ticks = element_blank())

ggsave(here::here("Output/Fig_point_2_rev_pretrial-baseline.png"), gp, width = 8, height = 7.4, unit = "cm")

# post as baseline
xo = d[, mean((trial-post)/100), by = path_delta] %>% setnames('V1', 'mean')
go=
ggplot(d, aes(x = path_delta, y = (trial-post)/100)) + 
    geom_point(aes(col = side_bias), data = d, cex = 2, alpha = 0.6) +
    stat_poly_line(se = FALSE, col = col_l) +
    geom_point(aes(x = path_delta, y = mean), data = xo, pch = 23, cex = 2, fill = 'red', alpha = 0.6) +

    annotate("text", x = 10, y = 0.525, label = "Long-path song played in:", size =2.75, hjust = 0) + 
    annotate("text", x = 13, y = 0.49, label = "preferred arm", size =2.75, col = col_[2], hjust = 0) + 
    annotate("text", x = 13, y = 0.455, label = "non-preferred arm", size =2.75, col = col_[1], hjust = 0) + 

    stat_cor(cor.coef.name = "r", aes(label = after_stat(r.label)),  col = col_R, r.accuracy = 0.1, label.x = 17.5, label.y = 0.6-(0.6+.03)*per_*1.3,hjust = 0.5, vjust = 0, cex = 3) +

    coord_cartesian(xlim = c(0, 35), ylim = c(-.03, .6)) +
    scale_x_continuous(breaks = seq(0,30, by=10), expand = c(0,0))+
    scale_y_continuous(breaks = seq(0,.6, by=.2), expand = c(0,0))+
    scale_color_manual(guide = "none", values = col_) +
    
    annotate("text", x = 15, y = 0, label = "pair 1", size =3) + 
    annotate("text", x = 30.29, y = 0, label = "pair 2", size =3) + 
    annotate("text", x = 5.72, y = 0, label = "pair 3", size =3) + 
    #scale_shape_manual(guide = "none", values = c(21, 22, 23)) +
    #guides(fill = guide_legend(override.aes=list(shape=21))) +
    labs(x = 'Long-path minus short-path song\n ', y = 'Proportion of time with a long-path\n[trial minus post-trial]')+
    theme_bw()+
    theme(axis.ticks = element_blank())

ggsave(here::here("Output/Fig_point_2_rev_post-baseline.png"), go, width = 8, height = 7.4, unit = "cm")
