# generating correlated data
# 500 reports, 5 drugs, 10 AEs
# AE1, AE2, AE3, AE4 and AE5 are correlated

# U_ij ~ iid Bernoulli(r_i)
# Y_ij, Z_i ~ iid Bernoulli(p_i)
# A_j, U_ij, Y_ij, Z_i are mutually independent
# X_ij = (1-U_ij) Y_ij + U_ij Z_i
# X_ij ~ Bernoulli(p_i) with corr(X_ij, X_ik)=r_i

# model: logit(d1) = beta*a1 + beta*a2 + beta*a3 + beta*a4 + beta*a5

# important pairs
imp_2_set <- rbind(c("d1", "a1"), c("d1", "a2"), c("d1", "a3"), c("d1", "a4"), 
                   c("d1", "a5"), c("d2", "a1"), c("d2", "a2"), c("d2", "a3"), 
                   c("d3", "a1"))

set.seed(1201)

#specify number of observations, vaccines and AEs
num_obs <- 500
num_drugs <- 5
num_ae <- 10
num_sim <- 1000

#define beta coefficients (all positive) (only one beta in this setting)
beta <- matrix(0,num_drugs,num_ae)
beta[1,1:5] <- 10 #naive value (drug1 associated with first 5 AEs)
beta[2,1:3] <- 10 #naive value (drug2 associated with first 3 AEs)
beta[3,1] <- 10 #naive value (drug3 associated with 1 AE)

# correlation between AEs
rho <- 0.5 # 0.3, 0.5, 0.7
r <- sqrt(rho)
# probabilities
p <- 0.1 # 0.1, 0.3, 0.5
phi <- 1
dep_ae <- c(1:5) #dependent AEs

# initialize y and x arrays - y denotes drugs and x denotes AEs
# model 1: # all AE correlations are positive
y_1 <- matrix(0, num_obs, num_drugs)
colnames(y_1) <- paste0("d",1:num_drugs)

x <- matrix(0, num_obs, num_ae)
colnames(x) <- paste0("a",1:num_ae)

# negatively correlated AE4 and AE5
y_2 <- matrix(0, num_obs, num_drugs)
colnames(y_2) <- paste0("d",1:num_drugs)

x_neg <- matrix(0, num_obs, num_ae)
colnames(x_neg) <- paste0("a",1:num_ae)

# for generating correlated AEs, first generate U, V and Z variables
# where U~Ber(r), V,Z~Ber(p)
# initialize u, v

#set folder path to save data
data_path_1 <- "/Users/reetikasarkar/Dropbox/My Mac (Reetika’s MacBook Pro)/Documents/dissertation_research/codes/data/correlated_setting_5d_10ae/p_0_1/cor_0_5/beta_10/model_1"
data_path_2 <- "/Users/reetikasarkar/Dropbox/My Mac (Reetika’s MacBook Pro)/Documents/dissertation_research/codes/data/correlated_setting_5d_10ae/p_0_1/cor_0_5/beta_10/model_2"

for (sim in 1:num_sim) {
  for (i in 1:num_obs) {
    # generate U, V, A and Z
    U <- rep(0, num_ae)
    V <- rep(0, num_ae)
    A <- rep(0, num_ae)
    Z <- rbinom(1, 1, p)
    for (dep in dep_ae) {
      A[dep] <- rbinom(1, 1, phi)
      U[dep] <- rbinom(1, 1, r)
      V[dep] <- rbinom(1, 1, p)
    }
  
    for (j in 1:num_ae) {
      if(j %in% dep_ae) {
        x[i,j] <- A[j]*(((1-U[j]) * V[j]) + (U[j] * Z))
      }
      else {
        x[i,j] <- rbinom(1, 1, p)
      }
    }
    
    # flip values of ae4 and ae5 to reverse sign of correlation
    x_neg[i,4] <- abs(1-x[i,4])
    x_neg[i,5] <- abs(1-x[i,5])
    x_neg[i,1] <- x[i,1]
    x_neg[i,2] <- x[i,2]
    x_neg[i,3] <- x[i,3]
    x_neg[i,6:10] <- x[i,6:10]
    
    # Model 1
    p1 <- x[i,] %*% beta[1,] 
    y_1[i,1] <- rbinom(1,1,(exp(p1)/(1+exp(p1))))
    
    p2 <- x[i,] %*% beta[2,]
    y_1[i,2] <- rbinom(1,1,(exp(p2)/(1+exp(p2))))
    
    p3 <- x[i,] %*% beta[3,]
    y_1[i,3] <- rbinom(1,1,(exp(p3)/(1+exp(p3))))
    
    y_1[i,4] <- rbinom(1, 1, p)
    y_1[i,5] <- rbinom(1, 1, p)
    
    # Model 2
    p1_2 <- x_neg[i,] %*% beta[1,] 
    y_2[i,1] <- rbinom(1,1,(exp(p1_2)/(1+exp(p1_2))))
    
    p2_2 <- x_neg[i,] %*% beta[2,]
    y_2[i,2] <- rbinom(1,1,(exp(p2_2)/(1+exp(p2_2))))
    
    p3_2 <- x_neg[i,] %*% beta[3,]
    y_2[i,3] <- rbinom(1,1,(exp(p3_2)/(1+exp(p3_2))))
    
    y_2[i,4] <- rbinom(1, 1, p)
    y_2[i,5] <- rbinom(1, 1, p)
  }
  
  data_1 <- cbind(y_1,x)
  data_2 <- cbind(y_2,x_neg)

  #save simulated data in csv file
  # Model 2
  f_1 <- paste0(data_path_1, "/sim_", sim, ".csv")
  write.csv(data_1, file=f_1, row.names = FALSE)
  
  # Model 1
  f_2 <- paste0(data_path_2, "/sim_", sim, ".csv")
  write.csv(data_2, file=f_2, row.names = FALSE)
}
