require(cowplot)  
require(ggpubr)  


## load data
w = fread(here::here('Data/Dat_path_length.csv'), header = T) # the dataset was created using Alam et al's Fig 2c data on 31 birds available from https://doi.org/10.18738/T8/WBQM4I/Q92O9A and using their procedure to generated 20 UMAPs - see https://github.com/ymir-k/UMAP_test/tree/main/AlamTests/3-RseedTest
setnames(w, c('kdistance'), c('path_length'))#w[, path_length := kdistance]

### prepares data & plots
  j=4
  wj = w[n_syll%in%j] 
  ## create pair dataset (for each UMAP iteration add the path length of the second bird in a pair)
  pj <- wj[, { 
  # Generate unique pairs of bird IDs
  bird_pairs <- CJ(bird_a = bird_id, bird_b = bird_id, sorted = TRUE)[bird_a < bird_b]
  
  # Merge path lengths for bird_a and bird_b
  bird_pairs <- merge(bird_pairs, .SD[, .(bird_id, path_length)], 
                      by.x = "bird_a", by.y = "bird_id", all.x = TRUE)
  setnames(bird_pairs, "path_length", "path_length_a")
  
  bird_pairs <- merge(bird_pairs, .SD[, .(bird_id, path_length)], 
                      by.x = "bird_b", by.y = "bird_id", all.x = TRUE)
  setnames(bird_pairs, "path_length", "path_length_b")
  
  bird_pairs
}, by = iteration]

pj[, path_length_diff := path_length_a-path_length_b]
pj[, delta := abs(path_length_diff)]
pj[, pr := paste(bird_a,bird_b)]

foreach(i in unique(w$iteration))
for(i in unique(w$iteration)){
  #i = 3
  xi = data.table(pr = pj[iteration==i, pr], pair = pj[iteration==i, round(abs(path_length_diff),4)], delta = pj[iteration==i, round(path_length_diff,4)])
  xi[pair==delta,adjust:= 1]
  xi[is.na(adjust), adjust :=-1]
  pxi = merge(pj,xi[,.(pr, pair,adjust)],all.x=TRUE)
  pxi[, delta:=path_length_diff*adjust ]# for plotting make all starting pair values from umap 5 positive
  
  pxip = pxi[!iteration%in%i, if(adjust[1]==-1){sum(path_length_diff > 0)}else{sum(path_length_diff <0)}, by = list(pr,pair)] %>% setnames(old = 'V1', new = 'flip')
  pxip[, flip_per:=paste0(round(100*flip/(length(unique(pxi$iteration))-1)),'%')]  #paste0(round(100*swap/20), %)]  
  pxip[, flip_pr:=round(100*flip/(length(unique(pxi$iteration))-1))]

  gpxi =
  ggplot(pxi, aes(x = pair, y = delta)) + 
      geom_rect(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax =0,fill = 'grey80',inherit.aes = FALSE)+
      geom_rect(xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf,fill = 'white',inherit.aes = FALSE)+
      geom_vline(aes(xintercept = pair), col = 'grey75', lwd = 0.25) +
      geom_hline(yintercept = 0, lty = 3, col = 'grey45', lwd = 0.25) +
      geom_point(alpha = 0.8, pch = 21, size = 2, fill =furt_) + 
      geom_point(data = pxi[iteration==i], aes(x = pair, y = delta), fill = orig_, pch = 21, size = 2) +
      #geom_text(data = wxp, aes(x = pair, y = -45, label = swap_per), size = 0.8/scale_size, angle = 90, hjust = 0)+
      #annotate("text", x = 34.5, y = 36, label = "Initial difference", col = orig_, size = 0.9/scale_size, hjust = 1) + 
      #annotate("text", x = 34.5, y = 18, label = "Further differences", col = furt_, size = 0.9/scale_size, hjust = 1) + 
      #annotate("text", x = 47.5, y = 0, label = "Initial long song", size = 1/scale_size, angle = 90) + 
      #annotate("text", x = 48.5, y = 25, label = "remained the long one", size = 1/scale_size, angle = 90) + 
      #annotate("text", x = 48.5, y = -25, label = "became the short one", size = 1/scale_size, angle = 90) + 
      labs(x = 'Initial path-length difference of a pair\n[indicated also by red dots]', y = 'Path-length difference\nfor each latent space iteration', subtitle = paste("Iteration #:",i))+
      #scale_y_continuous(lim = c(-30.2, 30.2), breaks=seq(-30,30, by = 10))+
      scale_x_continuous(lim = c(-1,50), breaks = seq(0,50,by=10),expand = c(0,0))+
      scale_y_continuous(lim = c(-50,50), breaks = seq(-50,50,by=10), expand = c(0,0))+
      #scale_fill_manual(values = col_3) + 
      theme_bw() +
      theme(legend.position="none", 
      plot.subtitle = element_text(face = 'bold', size = 10),
      axis.ticks = element_blank())    


ggsave(here::here(paste0("Output/ED_Fig_2/ED_Fig_2_syll",j,"_iter",i,".png")), gpxi, width = 12, height = 7.6*8.5/8 , unit = "cm") #16.575cm
 
print(paste("iter",i))
}
print(paste("syl",j))
}




### prepares mean values for plotting - 4 syl
load(here::here('Data/Dat_flip_kdistances.Rdata'))
zz4 = zz[n_syll==4]
zz4[, pair_bin := cut(pair, breaks = quantile(pair, probs = seq(0, 1, length.out = 11)), include.lowest = TRUE)]

zz4[, pair_bin := cut(pair, 
    breaks = seq(min(pair), max(pair), length.out = 11), 
    include.lowest = TRUE)]

summary_stats <- zz4[, .(
    median_flip_pr = median(flip_pr, na.rm = TRUE), 
    mean_pair = mean(pair),
    n = length(pair)
), by = pair_bin]

### prepares predictions for plotting
m = glmer(cbind(flip, no_flip) ~ pair + (1|pr)+(1|iteration), family = binomial, data = zz4)
bsim = sim(m, nsim)
v <- apply(bsim@fixef, 2, quantile, prob = c(0.5))
newD = data.table(pair = seq(0, max(zz4$pair), length.out =100))
X = model.matrix(~pair, newD)
predmatrix = matrix(nrow = nrow(newD), ncol = nsim)
for(j in 1:nsim) predmatrix[,j] <-plogis(X %*% bsim@fixef[j,])
newD$pred <- plogis(X %*% v) # fitted values
newD$lwr <- apply(predmatrix, 1, quantile, prob = 0.025)
newD$upr <- apply(predmatrix, 1, quantile, prob = 0.975)

### prepare plot legend
g_leg = 
ggplot() +
  geom_point(data = summary_stats, aes(x = mean_pair, y = median_flip_pr, size = n), col = point_out, fill = point_fill, pch = 21, alpha = 0.8) +
  scale_size_area(
    limits = c(0, max(summary_stats$n, na.rm = TRUE)), 
    max_size = 10, 
    breaks = c(5, 75, 250), 
    labels = c(5, 75, 250), 
    guide = guide_circles(vjust = 1)
  )+
  theme_bw() +
  theme(  legend.text=element_text(size=7),
          legend.title=element_text(hjust=0.5, margin = margin(b = -1)),
          legend.key = element_rect(colour = NA, fill = NA),
          #legend.margin = margin(0,0,0,0, unit="cm"),
          legend.background = element_blank())
leg <- get_legend(g_leg)
f2b = 
ggplot() + 
  #stat_smooth( method="glm", method.args=list(family="binomial"), col = night_, size=0.5) + 
  geom_point(data = zz4 , aes(y = flip_pr, x = pair), col = furt_, size =0.5, alpha = 0.5) + 
  geom_ribbon(data = newD,aes(ymin=lwr, ymax=upr, x=pair), fill = orig_, alpha = 0.2, show.legend = NA) +
  geom_line(data = newD,aes(x = pair, y = pred), col = orig_) +
  geom_point(data = summary_stats , aes(y = median_flip_pr, x = mean_pair, size = n), col = point_out, fill = "grey70", pch = 21, alpha = 0.8) + 
  
  #geom_jitter(data = xx[night_num == 1] , aes(y = night_num, x = midday_T,fill = temperature), width = 0, height = 0.025,  col = point_out2, pch = 21) + 
  #geom_jitter(aes(x = midday_T, y = night_num,  fill = temperature), data = xx, shape=21, col = point_out) +
  #scale_fill_viridis_c(option = "plasma", name = "°C\nwhen\npredated") +
  #scale_size(name = "n", breaks = c(10, 100, 300)) + 
  scale_size_area(
    limits = c(0, max(summary_stats$n, na.rm = TRUE)), 
    max_size = 10, 
    breaks = c(5, 75, 250), 
    labels = c(5, 75, 250), 
    guide = guide_legend(vjust = 1)
  )+
  scale_x_continuous(expand = c(0, 0), lim = c(0,46),  name = "Initial path-length difference of a pair\n") +
  scale_y_continuous(expand = c(0, 0), lim = c(0,1),  name = "Long-song flipping to short-song\n[%]",
  breaks = seq(0,1, by=0.25), labels = seq(0,100, by = 25)) +
  #labs(size "n")
  #coord_cartesian(xlim = c(30,61), clip = 'off') + 
  labs(tag = "b") +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        plot.tag = element_text(face = "bold", size = 10),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank()   # Remove minor grid lines
        )  

### combine
f2 = 
ggdraw() +
  draw_plot(f2a, x = 0, y = 0, width = 0.67, height = 1) +
  draw_plot(ggdraw(ggarrange(f2b)) +
              draw_grob(leg, x = 0.7, y = 0.65, width = 0.3, height = 0.3),
            x = 0.67, y = 0, width = 0.33, height = 1)

ggsave(here::here(paste0("Output/Fig_2ab_width-166mm_rev",j,"_iter",i,"v2.png")), f2, width = 17+8.5, height = 7.4*8.5/8 , unit = "cm") #16.575cm

### estimate the mean flipping probability - 4 syl
load(here::here('Data/Dat_flip_kdistances.Rdata'))
zz4 = zz[n_syll==4]

#### binomial
mi_b = glmer(cbind(flip, no_flip) ~ 1 + (1|pr)+(1|iteration), family = binomial, data = zz4) 
bsim = sim(mi_b,n.sim = 5000) # bayesian simulations
plogis(apply(bsim@fixef, 2, quantile, prob = c(0.025,.5,.975)))

mi_b_glm = glm(cbind(flip, no_flip) ~ 1 , family = binomial, data = zz4) 
bsim_glm = sim(mi_b_glm,n.sim = 5000) # bayesian simulations
plogis(apply(bsim_glm@coef, 2, quantile, prob = c(0.025,.5,.975)))

#### Gaussian
mi_g = lmer(flip_pr ~ 1 + (1|pr)+(1|iteration), data = zz4) 
bsim_g = sim(mi_g,n.sim = 5000)
apply(bsim_g@fixef, 2, quantile, prob = c(0.025,.5,.975))

mi_g_lm = lm(flip_pr ~ 1 , data = zz4) 
bsim_g_lm = sim(mi_g_lm,n.sim = 5000)
apply(bsim_g_lm@coef, 2, quantile, prob = c(0.025,.5,.975))

### prepares mean values for plotting - 5 syl
load(here::here('Data/Dat_flip.Rdata'))
zz5 = zz[n_syll==5]
zz5[, pair_bin := cut(pair, breaks = quantile(pair, probs = seq(0, 1, length.out = 11)), include.lowest = TRUE)]

zz5[, pair_bin := cut(pair, 
    breaks = seq(min(pair), max(pair), length.out = 11), 
    include.lowest = TRUE)]

summary_stats <- zz5[, .(
    median_flip_pr = median(flip_pr, na.rm = TRUE), 
    mean_pair = mean(pair),
    n = length(pair)
), by = pair_bin]

### prepares predictions for plotting
m = glmer(cbind(flip, no_flip) ~ pair + (1|pr)+(1|iteration), family = binomial, data = zz5)
v <- apply(bsim@fixef, 2, quantile, prob = c(0.5))
newD = data.table(pair = seq(0, max(zz5$pair), length.out =100))
X = model.matrix(~pair, newD)
predmatrix = matrix(nrow = nrow(newD), ncol = nsim)
for(j in 1:nsim) predmatrix[,j] <-plogis(X %*% bsim@fixef[j,])
newD$pred <- plogis(X %*% v) # fitted values
newD$lwr <- apply(predmatrix, 1, quantile, prob = 0.025)
newD$upr <- apply(predmatrix, 1, quantile, prob = 0.975)

### prepare plot legend
g_leg = 
ggplot() +
  geom_point(data = summary_stats, aes(x = mean_pair, y = median_flip_pr, size = n), col = point_out, fill = point_fill, pch = 21, alpha = 0.8) +
  scale_size_area(
    limits = c(0, max(summary_stats$n, na.rm = TRUE)), 
    max_size = 10, 
    breaks = c(5, 75, 250), 
    labels = c(5, 75, 250), 
    guide = guide_circles(vjust = 1)
  )+
  theme_bw() +
  theme(  legend.text=element_text(size=7),
          legend.title=element_text(hjust=0.5, margin = margin(b = -1)),
          legend.key = element_rect(colour = NA, fill = NA),
          #legend.margin = margin(0,0,0,0, unit="cm"),
          legend.background = element_blank())
leg <- get_legend(g_leg)
f2b = 
ggplot() + 
  #stat_smooth( method="glm", method.args=list(family="binomial"), col = night_, size=0.5) + 
  geom_point(data = zz5 , aes(y = flip_pr, x = pair), col = furt_, size =0.5, alpha = 0.5) + 
  geom_ribbon(data = newD,aes(ymin=lwr, ymax=upr, x=pair), fill = orig_, alpha = 0.2, show.legend = NA) +
  geom_line(data = newD,aes(x = pair, y = pred), col = orig_) +
  geom_point(data = summary_stats , aes(y = median_flip_pr, x = mean_pair, size = n), col = point_out, fill = "grey70", pch = 21, alpha = 0.8) + 
  
  #geom_jitter(data = xx[night_num == 1] , aes(y = night_num, x = midday_T,fill = temperature), width = 0, height = 0.025,  col = point_out2, pch = 21) + 
  #geom_jitter(aes(x = midday_T, y = night_num,  fill = temperature), data = xx, shape=21, col = point_out) +
  #scale_fill_viridis_c(option = "plasma", name = "°C\nwhen\npredated") +
  #scale_size(name = "n", breaks = c(10, 100, 300)) + 
  scale_size_area(
    limits = c(0, max(summary_stats$n, na.rm = TRUE)), 
    max_size = 10, 
    breaks = c(5, 75, 250), 
    labels = c(5, 75, 250), 
    guide = guide_legend(vjust = 1)
  )+
  scale_x_continuous(expand = c(0, 0), lim = c(0,46),  name = "Initial path-length difference of a pair\n") +
  scale_y_continuous(expand = c(0, 0), lim = c(0,1),  name = "Long-song flipping to short-song\n[%]",
  breaks = seq(0,1, by=0.25), labels = seq(0,100, by = 25)) +
  #labs(size "n")
  #coord_cartesian(xlim = c(30,61), clip = 'off') + 
  labs(tag = "b") +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        plot.tag = element_text(face = "bold", size = 10),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank()   # Remove minor grid lines
        )  

### combine
f2 = 
ggdraw() +
  draw_plot(f2a, x = 0, y = 0, width = 0.67, height = 1) +
  draw_plot(ggdraw(ggarrange(f2b)) +
              draw_grob(leg, x = 0.7, y = 0.65, width = 0.3, height = 0.3),
            x = 0.67, y = 0, width = 0.33, height = 1)

ggsave(here::here(paste0("Output/Fig_2ab_width-166mm_rev",j,"_iter",i,".png")), f2, width = 17+8.5, height = 7.4*8.5/8 , unit = "cm") #16.575cm

### estimate the mean flipping probability - 4 syl
mi_b = glmer(cbind(flip, no_flip) ~ 1 + (1|pr)+(1|iteration), family = binomial, data = zz4) 
bsim = sim(mi_b,n.sim = 5000) # bayesian simulations
plogis(apply(bsim@fixef, 2, quantile, prob = c(0.025,.5,.975)))

mi_g = lmer(flip_pr ~ 1 + (1|pr)+(1|iteration), data = zz4) 
bsim_g = sim(mi_g,n.sim = 5000)
apply(bsim_g@fixef, 2, quantile, prob = c(0.025,.5,.975))

### estimate the mean flipping probability - 5 syl
mi_b = glmer(cbind(flip, no_flip) ~ 1 + (1|pr)+(1|iteration), family = binomial, data = zz5) 
bsim = sim(mi_b,n.sim = 5000) # bayesian simulations
plogis(apply(bsim@fixef, 2, quantile, prob = c(0.025,.5,.975)))

mi_g = lmer(flip_pr ~ 1 + (1|pr)+(1|iteration), data = zz5) 
bsim_g = sim(mi_g,n.sim = 5000)
apply(bsim_g@fixef, 2, quantile, prob = c(0.025,.5,.975))