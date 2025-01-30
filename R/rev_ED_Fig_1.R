#' ---
#' title: "Estimating incorrect classification    probability"
#' author: "Martin Bulla"
#' date: "`r Sys.time()`"
#' output: 
#'     html_document:
#'         toc: true
#'         toc_float: true
#'         toc_depth: 5
#'         code_folding: hide
#'         bibliography: shorebird_bipInc_sex.bibtex
#'         link-citations: yes
#' ---

#+ r setup, include=FALSE 
knitr::opts_chunk$set(message = FALSE, warning = FALSE, cache = FALSE)

#' ##### Code to load tools & data
# load R-packages
require(arm)
require(data.table)
require(foreach)
require(ggplot2)
library(magrittr)

# define constants
nsim = 5000
orig_ = '#D43F3AFF'
furt_ = '#46B8DAFF'#"#357EBDFF"#
point_out = 'grey30'
point_fill = "#46B8DAFF"
inch = 0.393701
scale_size = 0.352778

#+ ed_f1, fig.width=8*inch,fig.height=8*inch
# load data
w = fread(here::here('Data/Dat_path_length.csv'), header = T) # the dataset was created using Alam et al's Fig 2c data on 31 birds available from https://doi.org/10.18738/T8/WBQM4I/Q92O9A and using their procedure to generated 20 UMAPs - see https://github.com/ymir-k/UMAP_test/tree/main/AlamTests/3-RseedTest
setnames(w, c('distance'), c('path_length'))
w = w[n_syll%in%5] 

# estimate the true value per male (best unbiased linear predictor)
m1 = lmer(path_length ~ 1 + (1|bird_id), w, REML=TRUE) # mixed effect model
blups_with_intercept <- data.table(coef(m1)$bird_id) %>% setnames(new = 'blup')# extracts blubs with added intercept  
blups_with_intercept[,bird_id:= unique(w$bird_id)]

wb = merge(w, blups_with_intercept, all.x = TRUE)

# plot the true vlaue agains the iterations
#ggplot(wb, aes(x = blup, y = path_length, fill = as.factor(blup))) +
#geom_point(pch = 21, col = "white", size = 4)+
#labs(x = "True inherent path length",  y = "Path length for each iteration") +
#coord_cartesian(xlim = c(10,70), ylim = c(10,70))+
#theme(legend.position="none")

# prepare the dataset for estimating the probability of incorrect assignment
pp = foreach(i = unique(wb$iteration), .combine = rbind) %do% {
# i = 0
    wi = wb[iteration==i]
    
    p = foreach(j = unique(wi$bird_id), .combine = rbind) %do% {
        #j = 1
        wij = wi[!bird_id%in%j] 
        wij = wij[order(path_length)]
        pair = data.table(
            pair = c('#1',"#2","#3"),
            bird_a = c(wij$bird_id[2], wij$bird_id[5], wij$bird_id[6]),
            bird_b = c(wij$bird_id[4], wij$bird_id[1], wij$bird_id[3]),
            delta = c(wij$path_length[4]-wij$path_length[2],
           wij$path_length[6]- wij$path_length[1],wij$path_length[5]- wij$path_length[3]
            ),
            delta_blub = c(wij$blup[4]-wij$blup[2],
            wij$blup[6] - wij$blup[1],wij$blup[5] - wij$blup[3]
            ),
        set = j
        )
        return(pair)
    }
    return(p[, iteration := i])
}
#nrow(pp)
pp[, incorrect := ifelse((delta >= 0 & delta_blub >= 0) | (delta < 0 & delta_blub < 0), 0, 1)] # assing 1 when the delta from one iteration is of oposite sign (long-path wrongly assigned) compare to the true difference

# estimate the probability
m2 = glm(incorrect~delta, pp, family = "binomial")

bsim = sim(m2,n.sim = nsim)
v <- apply(bsim@coef, 2, quantile, prob = c(0.5))
newD = data.table(delta = c(5,15,30,seq(0, max(pp$delta), length.out =100)))
newD = newD[order(delta)]
X = model.matrix(~delta, newD)
predmatrix = matrix(nrow = nrow(newD), ncol = nsim)
for(j in 1:nsim) predmatrix[,j] <-plogis(X %*% bsim@coef[j,])
newD$pred <- plogis(X %*% v) # fitted values
newD$lwr <- apply(predmatrix, 1, quantile, prob = 0.025)
newD$upr <- apply(predmatrix, 1, quantile, prob = 0.975)

# force through zero
force0=0.5-newD$pred[1] 
newD$pred_0 = newD$pred + force0
newD$lwr_0 = newD$lwr + force0
newD$upr_0 = newD$upr + force0

# prepare dataset for Alam et al's pair scenario
pa =newD[delta %in% c(5, 15, 30)]
pa[, pair:=c('Pair #3', 'Pair #1', 'Pair #3')]

ggplot() + 
  # Dots for incorrect = 0 (stacking up)
  geom_dotplot(data = pp[incorrect == 0], 
               aes(y = incorrect, x = delta), 
               binaxis = "x", 
               stackdir = "up",  # Stack only upwards from 0
               position = position_nudge(y = 0),
               dotsize = 0.25, 
               col = point_out,
               fill = "grey70", 
               alpha = 0.5) +  
  # Dots for incorrect = 1 (stacking down)
  geom_dotplot(data = pp[incorrect == 1], 
               aes(y = incorrect, x = delta), 
               binaxis = "x", 
               stackdir = "down",  # Stack only downwards from 1
               position = position_nudge(y = 1),
               dotsize = 0.25, 
               col = point_out,
               fill = "grey70", 
               alpha = 0.5) +  
  # prediction and 95%CI from the logistic regression             
  geom_ribbon(data = newD,aes(ymin=lwr_0, ymax=upr_0, x=delta), fill = orig_, alpha = 0.2, show.legend = NA) +
  geom_line(data = newD,aes(x = delta, y = pred_0), col = orig_) +
  # Alam et al's pairs highlighted 
  geom_point(data = pa, aes(x = delta, y = pred_0), pch = 21, fill = "red")+
   # Add labels above the three points
  geom_text(data = pa, aes(x = delta, y = pred_0, label = round(pred_0, 2)), vjust = -1, color = "red", fontface = "bold", size = 1/scale_size) +  
  geom_text(data = pa, aes(x = delta, y = pred_0, label = pair), vjust = -3, color = "darkgrey", size = 1/scale_size) +  
  scale_x_continuous(expand = c(0, 0),name = "Path-length difference of a pair\n[based on one iteration]") +
  scale_y_continuous(expand = c(0, 0), name = "Probability of incorrect assignement\n[of long-short path]")+
  coord_cartesian(xlim =c(0,50))+
  #breaks = seq(0,1, by=0.25), labels = seq(0,100, by = 25)) 
  #labs(tag = "b") +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        plot.tag = element_text(face = "bold", size = 10),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank()   # Remove minor grid lines
        ) 
  
#' 
#' <a name="ED_1">
#' **Extended Data Figure 1</a> | 
#' Probability of incorrect "long-path" song classification  within a stimulus pair given their path-length difference, estimated from a single iteration.** Points represent individual cases, stacked using `geom_dotplot()` R-function. The line with a shaded area represents the predicted probabiity with a 95% credible interval, based on the joint posterior distribution of 5,000 simulated values from a logistic regression using the `sim` function from the `arm` R-package[15]. If both songs in a pair have have the same path length, the expected probability of incorrect assignment is 50%. The regression line was thus forced through 50%, although the estimated probability from the data was 47.8%. The three red dots, along with their values, indicate the probability of incorrect assignment for the three stimulus pairs used by Alam et al. The data underlying this figure, consisting of 420 individual cases, were obtained as follows. We used spectograms[3](https://doi.org/10.18738/T8/WBQM4I/Q92O9A) from 31 tutored birds (Alam et al.’s Fig. 2c) and generated 20 latent space iterations[4](https://github.com/MartinBulla/rebuttal_alam_2024). Within each iteration, we calculated the shortest path length between syllable clusters. We focused on seven males with five-syllable songs to match the artifical stimuli used by Alam et al. To estimate the true inherent path lengths, we fitted an intercept-only mixed-effects model with bird identity as a random intercept. Using the `coef()` function in R, we extracted an unbiased estimate of the true inherent path length for each male (*what to cite here: https://doi.org/10.2307/2529430, https://doi.org/10.1016%2Fj.jmva.2008.01.004, https://zbmath.org/?format=complete&q=an:0955.62500*).
#' 
#' *Whereas the between-male variation in the ture inherent path length is small, the variation due to iteration is large (Extended Data Figure 1a).* **[This sentence and figure might not be necessary.]** 
#' 
#' To simulate Alam et al's playback scenario of three stimulus pair contrasting presumably "long path" against "short path" songs, we implemented a leave-one-out resampling apporach. For each iteration, we created a subset of six males by removing one male and repeated the procedure six times, ensuring that each male was removed once. This yielded, for each of the 20 iterations, seven subsets, each with a slightly different set of six males. Within each substet (following Alam et al's Extended Data Fig. 5d), songs were sorted by path length, and three song pairs were created: Pair #1 contrasts the 2nd- and 4th-ranked song, Pair #2 the 1st- and 6th-raanked song, and Pair #3 the 3rd- to 5th-ranked song. This process resulted in 420 comparisons (three pairs per subset across seven subsets and 20 iterations). For each pair, we noted whether a song appearing as "long-path" was actually "short-path" accoring to the pair's true inherent values estimated above, approximating an average over an infinite number of interations. A logistic regression was fitted to estimate how the probability of incorrect "long-path" clasification changes with the path-length difference of a pair from a single iteration.