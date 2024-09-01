library(plyr)

#AEs indep, 100 reports, 5 drugs, 10 AEs
#betas = 1, 5, 10, 30, 50, 100
#add error term ~ N(0,1), N(0,5), N(0,10)
#performance measurement: check accuracy for every pair of drug-AE, etc.

#Setting 2: # drugs and 5 AEs -> drug 1 associated with ae1 and ae2

# NAIVE SETTING 1
num_drugs <- 1 #5, 15, 20, 50
num_ae <- 3 #10, 20, 50, 100
num_reports <- 500 #100, 500, 1000
num_sim <- 1000 #50
p_ae <- 0.1 #prob of drug being given: 0.1, 0.3, 0.5
p_drug <- 0.1

set.seed(1201)

##### NAIVE setting: 
#all AEs are 0s and 1s and independent of each other (bernoulli with prob=0.3)
#drug1 associated with first 5 AEs
#drug2 associated with first 3 AEs
#drug3 associated with 1 AE
#drug4 and drug5 are independent Bernoulli(p)

#performance assessment
#check for 2 items, then 3 items, then 4 items and so on
#check each individual item set within the list of selected sets

#12 important 2 itemsets are (d1, d2), (d1, d3), (d1, a1), (d1, a2), (d1, a3), 
#(d1, a4), (d1, a5), (d2, d3), (d2, a1), (d2, a2), (d2, a3), (d3, a1)

imp_set <- c("d1", "a1")
#imp_2_set <- rbind(c("d1", "d2"), c("d1", "d3"), c("d1", "a1"), c("d1", "a2"), 
#                   c("d1", "a3"), c("d1", "a4"), c("d1", "a5"), c("d2", "d3"), 
#                   c("d2", "a1"), c("d2", "a2"), c("d2", "a3"), c("d3", "a1"))

#19 important 3 itemsets are (d1, d2, d3), (d1, a1, a2), (d1, a1, a3), 
#(d1, a1, a4), (d1, a1, a5), (d1, a2, a3), (d1, a2, a4), (d1, a2, a5), 
#(d1, a3, a4), (d1, a3, a5), (d1, a4, a5), (d1, d2, a1), (d1, d2, a2), 
#(d1, d2, a3), (d2, a1, a2), (d2, a1, a3), (d2, a2, a3), (d1, d3, a1), (d2, d3, a1) 

#imp_3_set <- rbind(c("d1", "d2", "d3"), c("d1", "a1", "a2"), c("d1", "a1", "a3"), 
#                   c("d1", "a1", "a4"), c("d1", "a1", "a5"), c("d1", "a2", "a3"), 
#                   c("d1", "a2", "a4"), c("d1", "a2", "a5"), c("d1", "a3", "a4"), 
#                   c("d1", "a3", "a5"), c("d1", "a4", "a5"), c("d1", "d2", "a1"), 
#                   c("d1", "d2", "a2"), c("d1", "d2", "a3"), c("d2", "a1", "a2"), 
#                   c("d2", "a1", "a3"), c("d2", "a2", "a3"), c("d1", "d3", "a1"), 
#                   c("d2", "d3", "a1"))

#1 important 4 itemset is (d1, d2, d3, a1) 

#imp_4_set <- rbind(c("d1", "d2", "d3", "a1")) 

#0 important 5 itemsets

#define beta coefficients (all positive) (only one beta in this setting)
beta <- matrix(0,num_drugs,num_ae)
beta[1,1:2] <- 0 #drug 1 not associated with AE1 and AE2
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
data_path <- "/Users/reetikasarkar/Dropbox/My Mac (Reetika’s MacBook Pro)/Documents/dissertation_research/codes/data/naive_setting_1d_3ae/p_0_1/beta_0"

for (sim in 1:num_sim) {
  
  print(paste("iteration", sim, sep=" "))
  #t1 <- Sys.time()
  #generate data
  for (row in 1:num_reports) {
    for (ae in 1:num_ae) {
      x[row,ae] <- rbinom(1,1,p_ae) #all AEs are independent
    }
    #p1 <- x[row,] %*% beta[1,] 
    #y[row,1] <- rbinom(1,1,(exp(p1)/(1+exp(p1))))
    y[row,1] <- rbinom(1,1,p_drug)
  }
  
  data <- cbind(y,x)

  #save simulated data
  f <- paste0(data_path, "/sim_", sim, ".csv")
  #paste("/Users/reetikasarkar/Dropbox/My Mac (Reetika’s MacBook Pro)/Documents/dissertation_research/codes/naive_setting_2/data/sim_", sim, ".csv", sep="")
  write.csv(data, file=f, row.names = FALSE)
} 
