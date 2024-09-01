library(plyr)

#betas = 1, 5, 10, 30, 50, 100
#add error term ~ N(0,1), N(0,5), N(0,10)
#performance measurement: check accuracy for every pair of drug-AE, etc.

# NAIVE SETTING 1
num_drugs <- 1 #5, 15, 20, 50
num_ae <- 3 #10, 20, 50, 100
num_reports <- 500 #100, 500, 1000
num_sim <- 1000 #50
p_ae <- 0.5 #prob of drug being given: 0.1, 0.3, 0.5

set.seed(1201)

##### NAIVE setting: 
#all AEs are 0s and 1s and independent of each other (bernoulli with prob=0.3)
#drug1 associated with AE1
#logit(d1) ~ b1*a1

imp_set <- c("d1", "a1")

#define beta coefficients (all positive) (only one beta in this setting)
beta <- matrix(0,num_drugs,num_ae)
beta[num_drugs,1] <- 10 #drug 1 associated with AE 1
#beta[1,1:5] <- 1 #naive value (drug1 associated with first 5 AEs)
#beta[2,1:3] <- 1 #naive value (drug2 associated with first 3 AEs)
#beta[3,1] <- 1 #naive value (drug3 associated with 1 AE)

#initial y and x arrays
y <- matrix(0, num_reports, num_drugs)
colnames(y) <- paste("d",1:num_drugs,sep="")
x <- matrix(0, num_reports, num_ae)
colnames(x) <- paste("a",1:num_ae,sep="")
#all AEs (xi's) are bernoulli with p=0.3

#set folder path to save data
data_path <- "/Users/reetikasarkar/Dropbox/My Mac (Reetika’s MacBook Pro)/Documents/dissertation_research/codes/data/naive_setting_1d_3ae/p_0_5/beta_10"

for (sim in 1:num_sim) {
  
  print(paste("iteration", sim, sep=" "))
  #t1 <- Sys.time()
  #generate data
  for (row in 1:num_reports) {
    for (ae in 1:num_ae) {
      x[row,ae] <- rbinom(1,1,p_ae) #all AEs are independent
    }
    p1 <- x[row,] %*% beta[1,] 
    y[row,1] <- rbinom(1,1,(exp(p1)/(1+exp(p1))))
  }
  
  data <- cbind(y,x)

  #save simulated data
  f <- paste0(data_path, "/sim_", sim, ".csv")
  #paste("/Users/reetikasarkar/Dropbox/My Mac (Reetika’s MacBook Pro)/Documents/dissertation_research/codes/naive_setting_1/data/sim_", sim, ".csv", sep="")
  write.csv(data, file=f, row.names = FALSE)
} 
