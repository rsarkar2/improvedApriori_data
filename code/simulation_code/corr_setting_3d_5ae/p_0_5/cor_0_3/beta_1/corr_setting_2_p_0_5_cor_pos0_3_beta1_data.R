# generating correlated data
# 500 reports, 3 drugs, 5 AEs
# AE1 and AE2 are positively correlated
# Important pairs are (D1-A1), (D1-A2) 

# U_ij ~ iid Bernoulli(r_i)
# Y_ij, Z_i ~ iid Bernoulli(p_i)
# A_j, U_ij, Y_ij, Z_i are mutually independent
# X_ij = (1-U_ij) Y_ij + U_ij Z_i
# X_ij ~ Bernoulli(p_i) with corr(X_ij, X_ik)=r_i

# model 1: logit(d1) = beta*a1
# model 2: logit(d1) = beta*a1 + beta*a2

set.seed(1201)

#specify number of observations, vaccines and AEs
num_obs <- 500
num_drugs <- 3
num_ae <- 5
num_sim <- 1000

#define beta coefficients (all positive) (only one beta in this setting)
# model 1: logit(d1) = beta*a1
beta_1 <- matrix(0,num_drugs,num_ae)
beta_1[1,1] <- 1 #drug 1 associated with AE 1
# model 2: logit(d1) = beta*a1 + beta*a2
beta_2 <- matrix(0,num_drugs,num_ae)
beta_2[1,1:2] <- 1 #drug 1 associated with AE 1 and AE 2

# correlation between AEs
rho <- 0.3 # 0.3, 0.5, 0.7
r <- sqrt(rho)
# probabilities
p <- 0.5 # 0.1, 0.3, 0.5
phi <- 1
dep_ae <- c(1:2) #dependent AEs

# initialize y and x arrays - y denotes drugs and x denotes AEs
# model 1 
y_1 <- matrix(0, num_obs, num_drugs)
colnames(y_1) <- paste0("d",1:num_drugs)
# model 2
y_2 <- matrix(0, num_obs, num_drugs)
colnames(y_2) <- paste0("d",1:num_drugs)

x <- matrix(0, num_obs, num_ae)
colnames(x) <- paste0("a",1:num_ae)

# for generating correlated AEs, first generate U, V and Z variables
# where U~Ber(r), V,Z~Ber(p)
# initialize u, v

#set folder path to save data
data_path_1 <- "/Users/reetikasarkar/Dropbox/My Mac (Reetika’s MacBook Pro)/Documents/dissertation_research/codes/data/correlated_setting_3d_5ae/p_0_5/cor_0_3/beta_1/model_1"
data_path_2 <- "/Users/reetikasarkar/Dropbox/My Mac (Reetika’s MacBook Pro)/Documents/dissertation_research/codes/data/correlated_setting_3d_5ae/p_0_5/cor_0_3/beta_1/model_2"

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
    
    # Model 1
    p1 <- x[i,] %*% beta_1[1,] 
    y_1[i,1] <- rbinom(1,1,(exp(p1)/(1+exp(p1))))
    y_1[i,2] <- rbinom(1, 1, p)
    y_1[i,3] <- rbinom(1, 1, p)
    
    # Model 2
    p2 <- x[i,] %*% beta_2[1,] 
    y_2[i,1] <- rbinom(1,1,(exp(p2)/(1+exp(p2))))
    y_2[i,2] <- rbinom(1, 1, p)
    y_2[i,3] <- rbinom(1, 1, p)
  }
  
  data_1 <- cbind(y_1,x)
  data_2 <- cbind(y_2,x)

  #save simulated data in csv file
  # Model 1
  f1 <- paste0(data_path_1, "/sim_", sim, ".csv")
  write.csv(data_1, file=f1, row.names = FALSE)
  
  # Model 2
  f2 <- paste0(data_path_2, "/sim_", sim, ".csv")
  write.csv(data_2, file=f2, row.names = FALSE)
}
