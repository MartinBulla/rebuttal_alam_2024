require(data.table)
require(ggplot2)

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