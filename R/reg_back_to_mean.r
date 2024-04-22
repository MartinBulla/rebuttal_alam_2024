require(data.table)
require(ggplot2)
library(ggpmisc)
library(ggpubr)
library(patchwork)

set.seed(5)


# random data
d = data.table(
maja = c(9,4,8,2,11,30,21,3,8,7),
bara = c(21,25, 7, 18, 28, 13, 15, 6, 3, 17)
)

d = data.table(
maja = sample(1:100, 1000, replace = TRUE),
bara = sample(1:100, 1000, replace = TRUE)
)


g1=
ggplot(d, aes(x = maja, y = bara)) + 
    geom_point() +
    stat_poly_line(se = FALSE) +
    stat_cor(aes(label = after_stat(r.label)), label.x = 50, label.y = 104,col = 'red', r.accuracy = 0.1, cex = 3, hjust = 0.5)
    #stat_poly_eq() 

g2=  
ggplot(d, aes(x = maja, y = bara-maja)) + 
    geom_point() +
    #stat_smooth(method = 'lm')
    stat_poly_line(se = FALSE) +
    stat_cor(aes(label = after_stat(r.label)), label.x = 50, label.y = 104, col = 'red',cex = 3, hjust = 0.5)
 

g3=  
ggplot(d, aes(x = maja, y = bara+maja)) + 
    geom_point() +
    stat_poly_line(se = FALSE) +
    stat_cor(aes(label = after_stat(r.label)), label.x = 50,label.y = 208, col = 'red',cex = 3, hjust = 0.5)

g1|g2|g3

ggsave(here::here('Output/Fig_1.png'), g1|g2|g3, width = 12*2, height = 4*2, units = 'cm')

# paper data
d = fread('Data/Fig_4c.csv')
d$tutor_path = d$tutor_parth
d[, pupil_path_full:=tutor_path+pupil_path]
d[, tutor_minus_pupil:=pupil_path]

ggplot(d, aes(x =tutor_path, y = pupil_path, col = as.factor(tutor))) + geom_point()
ggplot(d, aes(x =tutor_path, y = pupil_path_full, col = as.factor(tutor))) + geom_point()

ggplot(d, aes(x =tutor_path, y = pupil_path_full)) + geom_point() + stat_smooth(method='lm')

g1 = 
ggplot(d, aes(x =tutor_path, y = pupil_path_full)) + 
    stat_poly_line(se = FALSE) +
    stat_poly_eq() +
    geom_point()

g2 = 
ggplot(d, aes(x =tutor_path, y = tutor_minus_pupil)) + 
    stat_poly_line(se = FALSE) +
    stat_poly_eq() +
    geom_point()
g1|g2

gg1=
ggplot(d, aes(x =tutor_path, y = pupil_path_full)) + 
    geom_point() +
    stat_poly_line(se = FALSE) +
    stat_cor(aes(label = after_stat(r.label)), label.x = 37.5, col = 'red', r.accuracy = 0.1, cex = 3, hjust = 0.5) +
    xlab('Tutor path length') + ylab('Pupil path length') +
    annotate("text", x=37.5, y=42.5, label= "Color indicates a tutor", cex = 2.5) + 
    geom_point(aes(col = as.factor(tutor))) + theme(legend.position="none")
    #stat_poly_eq() 

gg2=  
ggplot(d, aes(x =tutor_path, y = pupil_path_full-tutor_path)) + 
    geom_point() +
    #stat_smooth(method = 'lm')
    stat_poly_line(se = FALSE) +
    stat_cor(aes(label = after_stat(r.label)), label.x = 37.5, col = 'red',cex = 3, hjust = 0.5)+
    xlab('Tutor path length') + ylab('Pupil minus tutor path length') +
    geom_point(aes(col = as.factor(tutor))) + theme(legend.position="none")
 

gg3=  
ggplot(d, aes(x =tutor_path, y = tutor_path+pupil_path_full)) + 
    geom_point() +
    stat_poly_line(se = FALSE) +
    stat_cor(aes(label = after_stat(r.label)), label.x = 37.5, col = 'red',cex = 3, hjust = 0.5)+
    xlab('Tutor path length') + ylab('Pupil + tutor path length')+
    geom_point(aes(col = as.factor(tutor))) + theme(legend.position="none")

gg1|gg2|gg3

ggsave(here::here('Output/Fig_2.png'), gg1|gg2|gg3, width = 12*2, height = 4*2, units = 'cm')

# combine

combine_1 =  (g1|g2|g3) & plot_annotation(subtitle = "Random data") #& theme(plot.title = element_text(hjust = .5))
combine_2 =  (gg1|gg2|gg3) & plot_annotation(subtitle = "Alam et al. 2024 data") #& theme(plot.title = element_text(hjust = .5))

combine_1/combine_2
wrap_elements(combine_1) / wrap_elements(combine_2)
ggsave(here::here('Output/Fig_1&2.png'), wrap_elements(combine_1) / wrap_elements(combine_2), width = 12*2, height = 4*2*2, units = 'cm')