# =============================================================
# ❗ The script runs relative to the project's root directory,
#  requires "Fig_2b.csv" and generates Fig. 2
# =============================================================

# tools
require(arm)
require(data.table)
require(foreach)     
require(ggplot2)
require(ggpmisc)
require(ggpubr)
require(grid)
require(patchwork)
require(rptR)
require(tidyr)

set.seed(5)

col_p = 'darkgrey'
col_R = 'red'
col_l = 'red'
col_ <- c("#46B8DAFF", "#EEA236FF")#c("#357EBDFF", "#D43F3AFF", "#46B8DAFF", "#5CB85CFF", "#EEA236FF", "#9632B8FF", "#9632B8FF")[7:1]
col_2 <- c("#357EBDFF", "#D43F3AFF", "#46B8DAFF", "#5CB85CFF", "#EEA236FF", "#9632B8FF", "#9632B8FF") # require(scales);show_col(col_2)
col_3 <- c("#46B8DAFF","#5CB85CFF", "#EEA236FF") # require(scales);show_col(col_2)
scale_size = 0.352778

# data
b = fread(here::here('Data/Fig_2b.csv'), header = T)
d = pivot_longer(b, 
                    cols = `1`:`15`, 
                    names_to = "UMAP", 
                    values_to = "path_length")
#ggplot(d, aes(x = path_length)) + geom_histogram()

s = fread(here::here('Data/combined_labels.csv'), header = T)
ss = s[, length(unique(syll_id)), by = list(bird_name)] %>% setnames(c('bird_name', 'V1'),c('bird','n_syll'))
d = data.table(merge(d,ss, all.x = TRUE))

# repeatability
R = rpt(path_length ~ (1 | bird), grname = "bird", data = d, datatype = "Gaussian")
RR = data.table(R =paste0(round(R$R * 100), "%"))
RRr[, CI := paste0(paste(round(Rr$CI_emp*100)[1], round(Rr$CI_emp*100)[2], sep = "-"), '%')] 
RRr
#RRr[, lwr := 100*R$CI_emp[1]]
#RRr[, upr := 100*R$CI_emp[2]]

Rs = rpt(path_length ~ n_syll+(1 | bird), grname = "bird", data = d, datatype = "Gaussian")
RRs = data.table(R =paste0(round(Rs$R * 100), "%"))
RRs[, CI := paste0(paste(round(Rs$CI_emp*100)[1], round(Rs$CI_emp*100)[2], sep = "-"), '%')] 
RRs
#RRs[, lwr := 100*R$CI_emp[1]]
#RRs[, upr := 100*R$CI_emp[2]]
m =  lmer(path_length ~ n_syll + (1 | bird),  data = d)
summary(m)

ggplot(d, aes(x = n_syll, y = path_length)) + stat_smooth() + geom_point(alpha = 0.2, bg = 'grey100', col = 'black', size = 4) + theme_bw()

dd = d[!is.na(n_syll), mean(path_length), by = list(bird, n_syll)]
names(dd)[3] = 'mean_path_length'
ggplot(dd, aes(x = n_syll, y = mean_path_length)) + geom_point(alpha = 0.2, bg = 'grey100', col = 'black', size = 4) + theme_bw()

# swapping 
d[n_syll%in%4 & UMAP==5 & bird =='O144', path_length] - d[n_syll%in%4 & UMAP==5 & bird =='G488', path_length] # 5.578464
d[n_syll%in%4 & UMAP==5 & bird =='O144', path_length] - d[n_syll%in%4 & UMAP==5 & bird =='B258', path_length] # 14.53614
d[n_syll%in%4 & UMAP==5 & bird =='O144', path_length] - d[n_syll%in%4 & UMAP==5 & bird =='S132', path_length] # 15.64421

ds = foreach(i = unique(d$UMAP), .combine = rbind) %do% {
    #i = 5
    di = data.table(UMAP=i, pair = factor(c('5.6','14.5','15.6'), levels =c('5.6','14.5','15.6')) , delta = c(
            d[n_syll%in%4 & UMAP==i & bird =='O144', path_length] - d[n_syll%in%4 & UMAP==i & bird =='G488', path_length],
            d[n_syll%in%4 & UMAP==i & bird =='O144', path_length] - d[n_syll%in%4 & UMAP==i & bird =='B258', path_length],
            d[n_syll%in%4 & UMAP==i & bird =='O144', path_length] - d[n_syll%in%4 & UMAP==i & bird =='S132', path_length]
            ))
    print(i)
    return(di)
}
g =
ggplot(ds, aes(x = pair, y = delta, fill = pair)) + 
    geom_rect(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax =0,fill = 'grey80',inherit.aes = FALSE)+
    geom_rect(xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf,fill = 'white',inherit.aes = FALSE)+
    geom_hline(yintercept = 0, lty = 3) +
    geom_point(alpha = 0.8, pch = 21, size = 2) + 
    geom_point(data = ds[UMAP==5], aes(x = pair, y = delta), col = '#D43F3AFF', size = 2) +
    annotate("text", x = factor(c('14.5'), levels =c('5.6','14.5','15.6')), y = 17, label = "Original difference", col = '#D43F3AFF', size = 1/scale_size) + 
    annotate("text", x = 3.4, y = 0, label = "Original long song remained the long one", size = 1/scale_size, angle = 90) + 
    annotate("text", x = 3.5, y = 13.5, label = "Original long song remained the long one", size = 1/scale_size, angle = 90) + 
    annotate("text", x = 3.5, y = -13.5, label = "Original long song became the short one", size = 1/scale_size, angle = 90) + 
    labs(x = 'Initial path-length difference of a pair', y = 'Path-length difference for each latent space rendition')+
    #scale_y_continuous(expand = c(0, 0), breaks=seq(-25,25, by = 5), labels = seq(-25,25, by = 5))+
    #scale_y_continuous(expand = c(0, 0))+
    scale_fill_manual(values = col_3) + 
    theme_bw() +
    theme(legend.position="none") 

g =
ggplot(ds, aes(x = pair, y = delta, fill = pair)) + 
    geom_rect(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax =0,fill = 'grey80',inherit.aes = FALSE)+
    geom_rect(xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf,fill = 'white',inherit.aes = FALSE)+
    geom_hline(yintercept = 0, lty = 3) +
    geom_point(alpha = 0.8, pch = 21, size = 2) + 
    geom_point(data = ds[UMAP==5], aes(x = pair, y = delta), fill = '#D43F3AFF',pch = 21, size = 2) +
    annotate("text", x = factor(c('14.5'), levels =c('5.6','14.5','15.6')), y = 17, label = "Original difference", col = '#D43F3AFF', size = 1/scale_size) + 
    annotate("text", x = 3.3, y = 1, label = "Original long song", size = 1/scale_size, angle = 90) + 
    annotate("text", x = 3.5, y = 13.5, label = "remained the long one", size = 1/scale_size, angle = 90) + 
    annotate("text", x = 3.5, y = -13.5, label = "became the short one", size = 1/scale_size, angle = 90) + 
    labs(x = 'Initial path-length difference of a pair\n', y = 'Path-length difference\nfor each latent space rendition')+
    #scale_y_continuous(expand = c(0, 0), breaks=seq(-25,25, by = 5), labels = seq(-25,25, by = 5))+
    #scale_y_continuous(expand = c(0, 0))+
    scale_fill_manual(values = col_3) + 
    theme_bw() +
    theme(legend.position="none", 
    axis.ticks = element_blank())    
ggsave(here::here("Output/Fig_2__width-55mm.png"), g, width = 8.5, height = 7.4*8.5/8 , unit = "cm")

p = ds[, sum(delta < 0), by = pair] %>% setnames(old = 'V1', new = 'swap')
p[, swap_per:=round(100*swap/length(unique(ds$UMAP)))]  
p[, swap_per_reversed:=100-round(100*swap/length(unique(ds$UMAP)))]  

1-(1-.47)*(1-.67)*(1-.40)   # probability that at least one pair swaps
1-(1-.53)*(1-.33)*(1-.60)   # probability that at least one pair swaps, if the initial assighnment would be oposite

# swapping for supplementary data
w = fread(here::here('Data/path_length_diff.csv'), header = T)
w = w[n_syll_diff ==0] #table(w$n_syll1)/20
w = w[n_syll1==5]
w[iteration==5]
w[, pr := paste(bird1,bird2)]
w = w[pr %in% c('1 28','1 10','11 30')]
w[pr=='1 28', pair := '6.5']
w[pr=='1 10', pair := '15.5']
w[pr=='11 30', pair := '30.1']
w[, pair:=factor(pair, levels =c('6.5','15.5','30.1'))]
w[pair=='15.5', path_length_diff:=path_length_diff*-1 ]# for plotting make all starting pair values from umap 5 positive

gw =
ggplot(w, aes(x = pair, y = path_length_diff, fill = pair)) + 
    geom_rect(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax =0,fill = 'grey80',inherit.aes = FALSE)+
    geom_rect(xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf,fill = 'white',inherit.aes = FALSE)+
    geom_hline(yintercept = 0, lty = 3, col = 'grey45') +
    geom_point(alpha = 0.8, pch = 21, size = 2) + 
    geom_point(data = w[iteration==5], aes(x = pair, y = path_length_diff), fill = '#D43F3AFF',pch = 21, size = 2) +
    annotate("text", x = factor(c('15.5'), levels =c('6.5','15.5','30.1')), y = 25, label = "Original difference", col = '#D43F3AFF', size = 1/scale_size) + 
    annotate("text", x = 3.3, y = 1.5, label = "Original long song", size = 1/scale_size, angle = 90) + 
    annotate("text", x = 3.5, y = 16.5, label = "remained the long one", size = 1/scale_size, angle = 90) + 
    annotate("text", x = 3.5, y = -16, label = "became the short one", size = 1/scale_size, angle = 90) + 
    labs(x = 'Initial path-length difference of a pair\n', y = 'Path-length difference\nfor each latent space rendition')+
    scale_y_continuous(lim = c(-30.2, 30.2), breaks=seq(-30,30, by = 10))+
    #scale_y_continuous(expand = c(0, 0))+
    scale_fill_manual(values = col_3) + 
    theme_bw() +
    theme(legend.position="none", 
    axis.ticks = element_blank())    
ggsave(here::here("Output/Fig_2_width-55mm_30birds.png"), gw, width = 8.5, height = 7.4*8.5/8 , unit = "cm")

wp = w[, sum(path_length_diff < 0), by = pair] %>% setnames(old = 'V1', new = 'swap')
wp[, swap_per:=round(100*swap/length(unique(w$iteration)))]  
wp[, swap_per_reversed:=100-round(100*swap/length(unique(w$iteration)))]  

1-(1-.70)*(1-.25)*(1-.15)   # probability that at least one pair swaps
1-(1-.30)*(1-.75)*(1-.85)   # probability that at least one pair swaps, if the initial assighnment would be oposite

# swapping for supplementary data # syllables = 4
w = fread(here::here('Data/path_length_diff.csv'), header = T)
w = w[n_syll_diff ==0] #table(w$n_syll1)/20
w = w[n_syll1==4]
w[, pr := paste(bird1,bird2)]
x = data.table(pr = w[iteration==18, pr], pair = w[iteration==18, round(abs(path_length_diff),3)], delta = w[iteration==18, round(path_length_diff,3)])
x[pair==delta,adjust:= 1]
x[is.na(adjust), adjust :=-1]
wx = merge(w,x[,.(pr, pair,adjust)],all.x=TRUE)
wx[, delta:=path_length_diff*adjust ]# for plotting make all starting pair values from umap 5 positive
nrow(wx)
wx[iteration==18]  

wxp = wx[, sum(path_length_diff < 0), by = pair] %>% setnames(old = 'V1', new = 'swap')
wxp[, swap_per:=paste0(round(100*swap/19),'%')]  #paste0(round(100*swap/20), %)]  
wxp[, swap_pr:=round(100*swap/19)]

orig_ = '#D43F3AFF'
furt_ = '#46B8DAFF'#"#357EBDFF"#

length(c(unique(wx$bird1),unique(wx$bird2)))

gwx =
ggplot(wx, aes(x = pair, y = path_length_diff)) + 
    geom_rect(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax =0,fill = 'grey80',inherit.aes = FALSE)+
    geom_rect(xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf,fill = 'white',inherit.aes = FALSE)+
    geom_vline(aes(xintercept = pair), col = 'grey75', lwd = 0.25) +
    geom_hline(yintercept = 0, lty = 3, col = 'grey45', lwd = 0.25) +
    geom_point(alpha = 0.8, pch = 21, size = 2, fill =furt_) + 
    geom_point(data = wx[iteration==18], aes(x = pair, y = delta), fill = orig_, pch = 21, size = 2) +
    #geom_text(data = wxp, aes(x = pair, y = -45, label = swap_per), size = 0.8/scale_size, angle = 90, hjust = 0)+
    annotate("text", x = 34.5, y = 36, label = "Initial difference", col = orig_, size = 0.9/scale_size, hjust = 1) + 
    annotate("text", x = 34.5, y = 18, label = "Further differences", col = furt_, size = 0.9/scale_size, hjust = 1) + 
    annotate("text", x = 37, y = 0, label = "Initial long song", size = 1/scale_size, angle = 90) + 
    annotate("text", x = 38, y = 25, label = "remained the long one", size = 1/scale_size, angle = 90) + 
    annotate("text", x = 38, y = -25, label = "became the short one", size = 1/scale_size, angle = 90) + 
    labs(x = 'Initial path-length difference of a pair\n[indicated also by red dots]', y = 'Path-length difference\nfor each latent space iteration', tag = 'a')+
    #scale_y_continuous(lim = c(-30.2, 30.2), breaks=seq(-30,30, by = 10))+
    scale_x_continuous(lim = c(-1,39), breaks = seq(0,35,by=5),expand = c(0,0))+
    scale_y_continuous(lim = c(-50,50), breaks = seq(-50,50,by=10), expand = c(0,0))+
    #scale_fill_manual(values = col_3) + 
    theme_bw() +
    theme(legend.position="none", 
    plot.tag = element_text(face = "bold", size = 10),
    axis.ticks = element_blank())    
ggsave(here::here("Output/Fig_2a_all_30birds_4syl_width-110mm_v2.png"), gwx, width = 17, height = 7.4*8.5/8 , unit = "cm")

summary(wxp)
m = lm(swap_pr~1, wxp)
bsim = sim(m,n.sim = 5000)
apply(bsim@coef, 2, quantile, prob = c(0.025,.5,.975))



gwx2 =
ggplot(wxp, aes(x=pair, y=swap_pr)) + 
    #stat_smooth(se=FALSE, col = 'black')+
    stat_smooth(method='lm',se=FALSE, col = orig_)+
    geom_point(pch=21, size =2, fill = furt_, alpha = 0.8) +
    labs(x = 'Initial path-length difference of a pair\n ', y = '% of latent-space iterations with\n original long-song → short one', tag = 'b')+
    scale_y_continuous(lim = c(0,100), breaks = seq(0,100,by=25), expand = c(0,0))+
    scale_x_continuous(lim = c(-4,40), breaks = seq(0,40,by=10), expand = c(0,0))+
    theme_bw() +
    theme(
        axis.ticks = element_blank(),
        plot.tag = element_text(face = "bold", size = 10),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank()   # Remove minor grid lines
        )     
ggsave(here::here("Output/Fig_2b_width-55mm.png"), gwx2, width = 8.5, height = 7.4*8.5/8 , unit = "cm")
ggsave(here::here("Output/Fig_S2b_all_30birds_4syl.png"), gwx2, width = 18, height = 7.4*8.5/8 , unit = "cm")

# simulation umap repeatability
r = fread(here::here('Data/path_length_df.csv'), header = T)
r[!duplicated(focal_bird_id), length(focal_bird_id), by = n_syll]

l = list()
for(i in 4:6){
    #i=3
    Ri = rpt(distance ~ (1 | focal_bird_id), grname = "focal_bird_id", data = r[n_syll %in%i], datatype = "Gaussian")
    RRi = data.table(R =paste0(round(Ri$R * 100), "%"))
    RRi[, CI := paste0(paste(round(Ri$CI_emp*100)[1], round(Ri$CI_emp*100)[2], sep = "-"), '%')]

    l[[i]] = RRi
    }
s = do.call(rbind,l)


Rr = rpt(distance ~ (1 | focal_bird_id), grname = "focal_bird_id", data = r, datatype = "Gaussian")
RRr = data.table(R =paste0(round(Rr$R * 100), "%"))
RRr[, CI := paste0(paste(round(Rr$CI_emp*100)[1], round(Rr$CI_emp*100)[2], sep = "-"), '%')] 
RRr

Rrk = rpt(kdistance ~ (1 | focal_bird_id), grname = "focal_bird_id", data = r, datatype = "Gaussian")
RRrk = data.table(R =paste0(round(Rrk$R * 100), "%"))
RRrk[, CI := paste0(paste(round(Rrk$CI_emp*100)[1], round(Rrk$CI_emp*100)[2], sep = "-"), '%')]
RRrk

Rrs = rpt(distance ~n_syll + (1 | focal_bird_id), grname = "focal_bird_id", data = r, datatype = "Gaussian")
RRrs = data.table(R =paste0(round(Rrs$R * 100), "%"))
RRrs[, CI := paste0(paste(round(Rrs$CI_emp*100)[1], round(Rrs$CI_emp*100)[2], sep = "-"), '%')] 
RRrs

Rrks = rpt(kdistance ~ n_syll + (1 | focal_bird_id), grname = "focal_bird_id", data = r, datatype = "Gaussian")
RRrks = data.table(R =paste0(round(Rrks$R * 100), "%"))
RRrks[, CI := paste0(paste(round(Rrks$CI_emp*100)[1], round(Rrks$CI_emp*100)[2], sep = "-"), '%')]
RRrks

rr = r[!duplicated(focal_bird_id), .(focal_bird_id, n_syll)]

# simulation umap repeatability pairwise
p = fread(here::here('Data/pairwise_distance_df.csv'), header = T)
p[, pair_ID:=paste(cluster1, cluster2)]
Rp = rpt(distance ~ (1 | pair_ID), grname = "pair_ID", data = p[iteration>4], datatype = "Gaussian")
RRp = data.table(R =paste0(round(Rp$R * 100), "%"))
RRp[, CI := paste0(paste(round(Rp$CI_emp*100)[1], round(Rp$CI_emp*100)[2], sep = "-"), '%')] 
RRp


m = lmer(distance ~ (1 | pair_ID),  data = p)
summary(m)

# simulation umap repeatability pairwise - MNIST
mn = fread(here::here('Data/MNIST_distance_dataframe.csv'), header = T)
mn[, pair_ID:=paste(digit1, digit2)]
Rmn = rpt(distance ~ (1 | pair_ID), grname = "pair_ID", data = mn, datatype = "Gaussian")
RRmn = data.table(R =paste0(round(Rmn$R * 100), "%"))
RRmn[, CI := paste0(paste(round(Rmn$CI_emp*100)[1], round(Rmn$CI_emp*100)[2], sep = "-"), '%')] 
RRmn

# simulaiton UMAP R by n_syll
s = fread(here::here('Data/subset_path_lengths_MINIST.csv'), header = T)

sr = foreach(i = unique(s$subset_size), .combine = rbind) %do% {
    #i = 2
    si = s[subset_size%in%i]
    Rp = rpt(path_length ~ (1 | combo), grname = "combo", data = si, datatype = "Gaussian")
    RRp = data.table(R =paste0(round(Rp$R * 100), "%"))
    RRp[, CI := paste0(paste(round(Rp$CI_emp*100)[1], round(Rp$CI_emp*100)[2], sep = "-"), '%')] 
    RRp[, subset_size:=i]

    print(RRp)
    return(RRp)
}

ggplot(sr, aes(x = subset_size, y = R)) + geom_point()

ggplot(s, aes(x=combo, y = path_length))+ geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


    Rp = rpt(path_length ~ (1 | combo), grname = "combo", data = s, datatype = "Gaussian")
    RRp = data.table(R =paste0(round(Rp$R * 100), "%"))
    RRp[, CI := paste0(paste(round(Rp$CI_emp*100)[1], round(Rp$CI_emp*100)[2], sep = "-"), '%')] 
    RRp

    Rp = rpt(path_length ~ subset_size + (1 | combo), grname = "combo", data = s, datatype = "Gaussian")
    RRp = data.table(R =paste0(round(Rp$R * 100), "%"))
    RRp[, CI := paste0(paste(round(Rp$CI_emp*100)[1], round(Rp$CI_emp*100)[2], sep = "-"), '%')] 
    RRp 
    subset_size