## SETTING 3: p=0.1
#plots for accuracy measures

library(ggplot2)
library(gridExtra)
library(ggpubr)

#read file from drive
results_loc_beta1 <- "/Users/reetikasarkar/Dropbox/My Mac (Reetika’s MacBook Pro)/Documents/dissertation_research/codes/results/naive_setting_5d_10ae/p_0_1/beta_1"
results_loc_beta10 <- "/Users/reetikasarkar/Dropbox/My Mac (Reetika’s MacBook Pro)/Documents/dissertation_research/codes/results/naive_setting_5d_10ae/p_0_1/beta_10"
results_loc_beta50 <- "/Users/reetikasarkar/Dropbox/My Mac (Reetika’s MacBook Pro)/Documents/dissertation_research/codes/results/naive_setting_5d_10ae/p_0_1/beta_50"
results_loc_beta100 <- "/Users/reetikasarkar/Dropbox/My Mac (Reetika’s MacBook Pro)/Documents/dissertation_research/codes/results/naive_setting_5d_10ae/p_0_1/beta_100"

#read conf values for each beta 
conf_results_beta1 <- read.table(paste0(results_loc_beta1,"/conf.csv"), header = T)
conf_results_beta10 <- read.table(paste0(results_loc_beta10,"/conf.csv"), header = T)
conf_results_beta50 <- read.table(paste0(results_loc_beta50,"/conf.csv"), header = T)
conf_results_beta100 <- read.table(paste0(results_loc_beta100,"/conf.csv"), header = T)

#read prr values for each beta
prr_results_beta1 <- read.table(paste0(results_loc_beta1,"/prr.csv"), header = T)
prr_results_beta10 <- read.table(paste0(results_loc_beta10,"/prr.csv"), header = T)
prr_results_beta50 <- read.table(paste0(results_loc_beta50,"/prr.csv"), header = T)
prr_results_beta100 <- read.table(paste0(results_loc_beta100,"/prr.csv"), header = T)

#read rr values for each beta
rr_results_beta1 <- read.table(paste0(results_loc_beta1,"/rr.csv"), header = T)
rr_results_beta10 <- read.table(paste0(results_loc_beta10,"/rr.csv"), header = T)
rr_results_beta50 <- read.table(paste0(results_loc_beta50,"/rr.csv"), header = T)
rr_results_beta100 <- read.table(paste0(results_loc_beta100,"/rr.csv"), header = T)

#read ror values for each beta
ror_results_beta1 <- read.table(paste0(results_loc_beta1,"/ror.csv"), header = T)
ror_results_beta10 <- read.table(paste0(results_loc_beta10,"/ror.csv"), header = T)
ror_results_beta50 <- read.table(paste0(results_loc_beta50,"/ror.csv"), header = T)
ror_results_beta100 <- read.table(paste0(results_loc_beta100,"/ror.csv"), header = T)


betas <- as.factor(c(rep(1,4), rep(10,4), rep(50,4), rep(100,4)))
conf_thresh <- as.factor(rep(c(0.4,0.5,0.6,0.7),4))
prr_thresh <- as.factor(rep(c(1,1.2,1.5,2),4))
rr_thresh <- as.factor(rep(c(1,1.2,1.5,2),4))
ror_thresh <- as.factor(rep(c(1,1.2,1.5,2),4))

#sensitivity plots
#confidence thresholds
mean_sens_conf <- c(as.numeric(conf_results_beta1[1,]), as.numeric(conf_results_beta10[1,]), as.numeric(conf_results_beta50[1,]), as.numeric(conf_results_beta100[1,]))

#prr thresholds
mean_sens_prr <- c(as.numeric(prr_results_beta1[1,]), as.numeric(prr_results_beta10[1,]), as.numeric(prr_results_beta50[1,]), as.numeric(prr_results_beta100[1,]))

#rr thresholds
mean_sens_rr <- c(as.numeric(rr_results_beta1[1,]), as.numeric(rr_results_beta10[1,]), as.numeric(rr_results_beta50[1,]), as.numeric(rr_results_beta100[1,]))

#ror thresholds
mean_sens_ror <- c(as.numeric(ror_results_beta1[1,]), as.numeric(ror_results_beta10[1,]), as.numeric(ror_results_beta50[1,]), as.numeric(ror_results_beta100[1,]))

g1 <- ggplot() + 
  geom_col(aes(x=conf_thresh, y=mean_sens_conf, fill=betas), position="dodge") + 
  labs(y="Mean Sensitivity", x="Confidence thresholds", fill = "Beta values") +
  theme(legend.position = "none")

g2 <- ggplot() + 
  geom_col(aes(x=prr_thresh, y=mean_sens_prr, fill=betas), position="dodge") + 
  labs(y="Mean Sensitivity", x="PRR thresholds", fill = "Beta values") +
  theme(legend.position = "none")

g3 <- ggplot() + 
  geom_col(aes(x=rr_thresh, y=mean_sens_rr, fill=betas), position="dodge") + 
  labs(y="Mean Sensitivity", x="RR thresholds", fill = "Beta values") +
  theme(legend.position = "none")

g4 <- ggplot() + 
  geom_col(aes(x=ror_thresh, y=mean_sens_ror, fill=betas), position="dodge") + 
  labs(y="Mean Sensitivity", x="ROR thresholds", fill = "Beta values") +
  theme(legend.position = "none")

combined_plot <- grid.arrange(g1, g2, g3, g4, nrow=2, ncol=2)

g1_legend <- ggplot() + 
  geom_col(aes(x=conf_thresh, y=mean_sens_conf, fill=betas), position="dodge") + 
  labs(y="Mean Sensitivity", x="Confidence thresholds", fill = "Beta values") + 
  theme(legend.position = "bottom")

#function to extract legend from plot
get_only_legend <- function(plot) {
  plot_table <- ggplot_gtable(ggplot_build(plot))
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")
  legend <- plot_table$grobs[[legend_plot]]
  return(legend)
}

#extract legend from g1 using above function
legend <- get_only_legend(g1_legend)

#final combined plot with common legend
grid.arrange(combined_plot, legend, nrow=2, heights=c(10,1), top = "Sensitivity plots for p=0.1 setting")

#specificity plots
#confidence thresholds
mean_spec_conf <- c(as.numeric(conf_results_beta1[2,]), as.numeric(conf_results_beta10[2,]), as.numeric(conf_results_beta50[2,]), as.numeric(conf_results_beta100[2,]))

#prr thresholds
mean_spec_prr <- c(as.numeric(prr_results_beta1[2,]), as.numeric(prr_results_beta10[2,]), as.numeric(prr_results_beta50[2,]), as.numeric(prr_results_beta100[2,]))

#rr thresholds
mean_spec_rr <- c(as.numeric(rr_results_beta1[2,]), as.numeric(rr_results_beta10[2,]), as.numeric(rr_results_beta50[2,]), as.numeric(rr_results_beta100[2,]))

#ror thresholds
mean_spec_ror <- c(as.numeric(ror_results_beta1[2,]), as.numeric(ror_results_beta10[2,]), as.numeric(ror_results_beta50[2,]), as.numeric(ror_results_beta100[2,]))

#ppv plots
#confidence thresholds
mean_ppv_conf <- c(as.numeric(conf_results_beta1[3,]), as.numeric(conf_results_beta10[3,]), as.numeric(conf_results_beta50[3,]), as.numeric(conf_results_beta100[3,]))

#prr thresholds
mean_ppv_prr <- c(as.numeric(prr_results_beta1[3,]), as.numeric(prr_results_beta10[3,]), as.numeric(prr_results_beta50[3,]), as.numeric(prr_results_beta100[3,]))

#rr thresholds
mean_ppv_rr <- c(as.numeric(rr_results_beta1[3,]), as.numeric(rr_results_beta10[3,]), as.numeric(rr_results_beta50[3,]), as.numeric(rr_results_beta100[3,]))

#ror thresholds
mean_ppv_ror <- c(as.numeric(ror_results_beta1[3,]), as.numeric(ror_results_beta10[3,]), as.numeric(ror_results_beta50[3,]), as.numeric(ror_results_beta100[3,]))

#npv plots
#confidence thresholds
mean_npv_conf <- c(as.numeric(conf_results_beta1[4,]), as.numeric(conf_results_beta10[4,]), as.numeric(conf_results_beta50[4,]), as.numeric(conf_results_beta100[4,]))

#prr thresholds
mean_npv_prr <- c(as.numeric(prr_results_beta1[4,]), as.numeric(prr_results_beta10[4,]), as.numeric(prr_results_beta50[4,]), as.numeric(prr_results_beta100[4,]))

#rr thresholds
mean_npv_rr <- c(as.numeric(rr_results_beta1[4,]), as.numeric(rr_results_beta10[4,]), as.numeric(rr_results_beta50[4,]), as.numeric(rr_results_beta100[4,]))

#ror thresholds
mean_npv_ror <- c(as.numeric(ror_results_beta1[4,]), as.numeric(ror_results_beta10[4,]), as.numeric(ror_results_beta50[4,]), as.numeric(ror_results_beta100[4,]))

#fscore plots
#confidence thresholds
mean_fscore_conf <- c(as.numeric(conf_results_beta1[5,]), as.numeric(conf_results_beta10[5,]), as.numeric(conf_results_beta50[5,]), as.numeric(conf_results_beta100[5,]))

#prr thresholds
mean_fscore_prr <- c(as.numeric(prr_results_beta1[5,]), as.numeric(prr_results_beta10[5,]), as.numeric(prr_results_beta50[5,]), as.numeric(prr_results_beta100[5,]))

#rr thresholds
mean_fscore_rr <- c(as.numeric(rr_results_beta1[5,]), as.numeric(rr_results_beta10[5,]), as.numeric(rr_results_beta50[5,]), as.numeric(rr_results_beta100[5,]))

#ror thresholds
mean_fscore_ror <- c(as.numeric(ror_results_beta1[5,]), as.numeric(ror_results_beta10[5,]), as.numeric(ror_results_beta50[5,]), as.numeric(ror_results_beta100[5,]))

