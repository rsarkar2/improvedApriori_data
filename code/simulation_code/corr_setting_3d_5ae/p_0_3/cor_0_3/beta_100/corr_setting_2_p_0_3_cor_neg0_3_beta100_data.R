# generating correlated data
# 500 reports, 3 drugs, 5 AEs
# AE1, AE2 are negatively correlated
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
# model 1: logit(d1) = beta*a1 + beta*a2
beta_3 <- matrix(0,num_drugs,num_ae)
beta_3[1,1:2] <- 100 #drug 1 associated with AE 1 and AE2

# correlation between AEs
rho <- 0.3 # 0.3, 0.5, 0.7
r <- sqrt(rho)
# probabilities
p <- 0.3 # 0.1, 0.3, 0.5
phi <- 1
dep_ae <- c(1:2) #dependent AEs

# initialize y and x arrays - y denotes drugs and x denotes AEs
# model 3
y_3 <- matrix(0, num_obs, num_drugs)
colnames(y_3) <- paste0("d",1:num_drugs)

x <- matrix(0, num_obs, num_ae)
colnames(x) <- paste0("a",1:num_ae)

# for generating correlated AEs, first generate U, V and Z variables
# where U~Ber(r), V,Z~Ber(p)
# initialize u, v

#set folder path to save data
data_path_3 <- "/Users/reetikasarkar/Dropbox/My Mac (Reetika’s MacBook Pro)/Documents/dissertation_research/codes/data/correlated_setting_3d_5ae/p_0_3/cor_0_3/beta_100/model_3"

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
    
    # flip values of ae2 to reverse sign of correlation
    x[i,2] <- abs(1-x[i,2])
    
    # Model 3
    p3 <- x[i,] %*% beta_3[1,] 
    y_3[i,1] <- rbinom(1,1,(exp(p3)/(1+exp(p3))))
    y_3[i,2] <- rbinom(1, 1, p)
    y_3[i,3] <- rbinom(1, 1, p)
  }
  
  data_3 <- cbind(y_3,x)

  #save simulated data in csv file
  # Model 1
  f3 <- paste0(data_path_3, "/sim_", sim, ".csv")
  write.csv(data_3, file=f3, row.names = FALSE)
}
