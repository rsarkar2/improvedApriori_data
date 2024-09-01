library(dplyr)
library(corrr)

# read data from file path for various years
file_path <- "/Users/reetikasarkar/Documents/dissertation_research/apriori_project/vaers_data/"
vaers2010to2012rawdata <- read.csv(paste0(file_path,"VAERS2010to2012DataWithIDs_All.csv"), header=T)
vaers2013to2015rawdata <- read.csv(paste0(file_path,"VAERS2013to2015DataWithIDs_All.csv"), header=T)
vaers2016to2018rawdata <- read.csv(paste0(file_path,"VAERS2016to2018DataWithIDs_All.csv"), header=T)
vaers2019rawdata <- read.csv(paste0(file_path,"VAERS2019DataWithIDs_All.csv"), header=T)

# combine data for years 2010 to 2019
combined_rawdata <- rbind(vaers2010to2012rawdata,vaers2013to2015rawdata,vaers2016to2018rawdata,vaers2019rawdata)

# save combined data
#write.csv(combined_rawdata, file=paste0(file_path,"/combined_data.csv"), row.names = FALSE)
# empty lists to save vaccines and AEs
#vacs <- c()
#aes <- c()

# make list of all vaccine and ae codes in the data
#vacs <- append(vacs, unlist(strsplit(as.character(tab[row,2]), ", ")))
#aes <- append(aes, unlist(strsplit(as.character(tab[row,3]), ", ")))

# filter data to remove unknown vaccines
# unique vaccine types and their codes, counts, and frequency
count_unique_vacs <- combined_rawdata %>%
  filter(Vaccine.Type.Code != "UNK") %>%
  count(Vaccine.Type.Code, Vaccine.Type, sort = TRUE)
print(count_unique_vacs)

# unique AE types and their codes, counts, and frequency
count_unique_aes <- combined_rawdata %>%
  filter(Vaccine.Type.Code !="UNK") %>%
  count(Symptoms.Code, Symptoms, sort = TRUE)
print(count_unique_aes)

unique_ids_1 <- combined_rawdata %>%
  filter(Vaccine.Type.Code !="UNK") %>%
  distinct(VAERS.ID.Code)

# make a list of unique VAERS IDs, vaccine and ae codes in the data
unique_vacs <- count_unique_vacs[,1]
unique_aes <- count_unique_aes[,1]
unique_ids <- unique_ids_1[,1]

num_reports <- length(unique_ids)
num_aes <- nrow(count_unique_aes)
num_vacs <- nrow(count_unique_vacs)

# total possible pairs
tot_pairs <- choose(num_aes+num_vacs, 2); tot_pairs

#restructure data into array of binary variables corresponding to drugs and aes
data_vac <- matrix(0, nrow=num_reports, ncol=num_vacs)
colnames(data_vac) <- unique_vacs
rownames(data_vac) <- unique_ids

data_ae <- matrix(0, nrow=num_reports, ncol=num_aes)
colnames(data_ae) <- unique_aes
rownames(data_ae) <- unique_ids

for (row in 1:num_reports) {
  aa <- which(combined_rawdata[,2] == unique_ids[row])
  for (dr in 1:num_vacs) {
    if (any(combined_rawdata[aa,4] == unique_vacs[dr])) {
      data_vac[row,dr] = 1
    }
  }
  for (ae in 1:num_aes) {
    if (any(combined_rawdata[aa,6] == unique_aes[ae])) {
      data_ae[row,ae] = 1
    }
  }
}

# AE correlation
library(data.table)

ae_cor <- cor(data.frame(data_ae))

max_ae_cor = ae_cor %>%
  as.data.frame() %>%
  mutate(var1 = rownames(.)) %>%
  gather(var2, value, -var1) %>%
  arrange(desc(value)) %>%
  group_by(value) %>%
  filter(row_number()==1)
print(head(max_ae_cor, 10))

# X10001945 X10001782 1 (1 correlation)
print(count_unique_aes[which(count_unique_aes[,1]=="10001945"),1:2])
print(count_unique_aes[which(count_unique_aes[,1]=="10001782"),1:2])

# X10052231 X10005395 0.865 (0.865 correlation)
print(count_unique_aes[which(count_unique_aes[,1]=="10052231"),1:2])
print(count_unique_aes[which(count_unique_aes[,1]=="10005395"),1:2])

print(tail(max_ae_cor, 10))

# X10042434 X10011906 -0.279
print(count_unique_aes[which(count_unique_aes[,1]=="10042434"),1:2])
print(count_unique_aes[which(count_unique_aes[,1]=="10011906"),1:2])

# X10014625 X10011906 -0.162
print(count_unique_aes[which(count_unique_aes[,1]=="10014625"),1:2])
print(count_unique_aes[which(count_unique_aes[,1]=="10011906"),1:2])

data <- cbind.data.frame(data_vac, data_ae)

# save data in wide format
write.csv(data, file=paste0(file_path,"/combined_data_wide.csv"), row.names = FALSE)

# AE frequency information
min_ae_freq <- min(colMeans(data[,48:ncol(data)])); min_ae_freq
min_ae_count <- min(colSums(data[,48:ncol(data)])); min_ae_count
min_ae_names <- which(colMeans(data[,48:ncol(data)]) == min_ae_freq); min_ae_names
which_ae_min_freq <- data.frame(Symptoms.Code=character(), Symptoms=character(), min_ae_count=integer(), min_ae_freq=numeric(), stringsAsFactors = FALSE)
#names(which_ae_min_freq)=c("Symptoms.Code","Symptoms","min_ae_count","min_ae_freq")
for (nm in 1:length(min_ae_names)) {
  freq <- count_unique_aes[which(count_unique_aes[,1] == names(min_ae_names)[nm]),1:2]
  ae_freq <- cbind(freq,min_ae_count,min_ae_freq)
  which_ae_min_freq <- rbind(which_ae_min_freq, ae_freq)
}
print(which_ae_min_freq[5:10,])
max_ae_freq <- max(colMeans(data[,48:ncol(data)])); max_ae_freq
max_ae_count <- max(colSums(data[,48:ncol(data)])); max_ae_count
which_ae_max_freq <- count_unique_aes[which(count_unique_aes[,1] == names(which.max(colMeans(data[,48:ncol(data)])))),1:2]
print(cbind(which_ae_max_freq, max_ae_count, max_ae_freq))

# vaccine frequency information
min_vac_freq <- min(colMeans(data[,1:47])); min_vac_freq
min_vac_count <- min(colSums(data[,1:47])); min_vac_count
min_vac_names <- which(colMeans(data[,1:47]) == min_vac_freq); min_vac_names
which_vac_min_freq <- data.frame(Vaccine.Type.Code=character(), min_vac_count=integer(), min_vac_freq=numeric(), stringsAsFactors = FALSE)
#names(which_ae_min_freq)=c("Symptoms.Code","Symptoms","min_ae_count","min_ae_freq")
for (nm in 1:length(min_vac_names)) {
  freq <- count_unique_vacs[which(count_unique_vacs[,1] == names(min_vac_names)[nm]),1:2]
  vac_freq <- cbind(freq,min_vac_count,min_vac_freq)
  which_vac_min_freq <- rbind(which_vac_min_freq, vac_freq)
}
print(which_vac_min_freq)
max_vac_freq <- max(colMeans(data[,1:47])); max_vac_freq
max_vac_count <- max(colSums(data[,1:47])); max_vac_count
which_vac_max_freq <- count_unique_vacs[which(count_unique_vacs[,1] == names(which.max(colMeans(data[,1:47])))),1:2]
print(cbind(which_vac_max_freq, max_vac_count, max_vac_freq))

############################################################################
################# Apriori Implementation ###################################

# define thresholds
min_support <- as.integer(mean(colMeans(data[,48:ncol(data)]))*num_reports) #compute mean of all mean AE frequencies
# alternatively min_support <- 3

# parameter thresholds                                     
conf_val <- c(0.4, 0.5, 0.6, 0.7)
prr_val <- c(1, 1.2, 1.5, 2)
rr_val <- c(1, 1.2, 1.5, 2)
ror_val <- c(1, 1.2, 1.5, 2)

num_conf_val <- 4

#1 itemsets (include all drugs and AEs)
c1 <- data.frame(item=character(), count=integer(), 
                 stringsAsFactors = FALSE) #1 item candidate sets
for (col in 1:ncol(data)) {
  c1[col,1] <- colnames(data)[col]
  c1[col,2] <- sum(data[,col])
}

if (nrow(c1) == 0) {
  print("no frequent 1 itemsets")
} else{
  l1 <- data.frame(item=character(), count=integer(), 
                   stringsAsFactors = FALSE) #1 item item sets
  for (item in 1:nrow(c1)) {
    if((as.integer(c1[item,2])) >= min_support) {
      l1 <- rbind(l1, c1[item,])
    }
  }
  rownames(l1) <- 1:nrow(l1)
}

# 2 itemsets (only pairs with one drug/vaccine and one AE to be considered)
c2 <- data.frame(item1=character(), item2=character(), count=integer(), 
                 stringsAsFactors = FALSE) #2 item candidate sets
for (i in 1:(nrow(l1)-1)) {
  for (j in (i+1):nrow(l1)) {
    if(((l1[i,1] %in% unique_vacs) & (l1[j,1] %in% unique_aes)) | ((l1[i,1] %in% unique_aes) & (l1[j,1] %in% unique_vacs))){
      com <- c(l1[i,1], l1[j,1], sum((data[,l1[i,1]]==1) & (data[,l1[j,1]]==1)))
      c2 <- rbind(c2, com)
    }
  }
}
names(c2) <- c("item1","item2","count")

if (nrow(c2) == 0) {
  print("no frequent 2 itemsets")
} else{
  l2 <- data.frame(item1=character(), item2=character(), count=integer(), 
                   stringsAsFactors = FALSE) #2 item item sets
  for (item in 1:nrow(c2)) {
    if(as.integer(c2[item,ncol(c2)]) >= min_support) {
      l2 <- rbind(l2, c2[item,])
    }
  }
  if (nrow(l2)!=0){
    names(l2) <- c("item1","item2","count")
    rownames(l2) <- 1:nrow(l2)
  }
}

#### Confidence
freq2_conf_list <- list()

for (conf in 1:num_conf_val) {
  freq2_conf <- data.frame(item1=character(), item2=character(), 
             support=integer(), confidence=numeric())
  i1 <- 0
  cf <- 0
  for (row in 1:nrow(l2)) {
    i1 <- which(l1[,1] == l2[row,1])
    cf <- as.numeric(l2[row,3])/as.numeric(l1[i1,2])
    if (cf >= conf_val[conf]) {
      freq2_conf <- rbind(freq2_conf, c(unlist(l2[row,]), cf))
    }
  }
  colnames(freq2_conf) <- c("item1","item2","support","confidence")
  
  freq2_conf_list[[conf]] <- freq2_conf
}
names(freq2_conf_list) <- c("conf_thresh=0.4","conf_thresh=0.5","conf_thresh=0.6","conf_thresh=0.7")

f2_conf_0_4 <- freq2_conf_list[[1]]
f2_conf_0_5 <- freq2_conf_list[[2]]
f2_conf_0_6 <- freq2_conf_list[[3]]
f2_conf_0_7 <- freq2_conf_list[[4]]

f2_conf_0_4 <- f2_conf_0_4[order(as.numeric(f2_conf_0_4$confidence)),]
f2_conf_0_5 <- f2_conf_0_5[order(as.numeric(f2_conf_0_5$confidence)),]
f2_conf_0_6 <- f2_conf_0_6[order(as.numeric(f2_conf_0_6$confidence)),]
f2_conf_0_7 <- f2_conf_0_7[order(as.numeric(f2_conf_0_7$confidence)),]

#### PRR, RR, ROR
freq2_prr_list <- list()
freq2_rr_list <- list()
freq2_ror_list <- list()

for (conf in 1:num_conf_val) {
  freq2_prr <- data.frame(item1=character(), item2=character(), 
                          support=integer(), prr=numeric())
  freq2_rr <- data.frame(item1=character(), item2=character(), 
                         support=integer(), rr=numeric())
  freq2_ror <- data.frame(item1=character(), item2=character(), 
                          support=integer(), ror=numeric())
  
  for (row in 1:nrow(l2)) {
    a <- sum((data[,l2[row,1]] == 1) & (data[,l2[row,2]] == 1))
    b <- sum((data[,l2[row,1]] == 1) & (data[,l2[row,2]] == 0))
    c <- sum((data[,l2[row,1]] == 0) & (data[,l2[row,2]] == 1))
    d <- sum((data[,l2[row,1]] == 0) & (data[,l2[row,2]] == 0))
    
    prr <- (a/(a + b + 0.00001))/((c+0.00001)/(c+d+0.00001))
    
    rr <- num_reports*a/((a + c)*(a + b)+0.00001)
    
    ror <- (a*d)/(c*b+0.00001)
    
    if(prr >= prr_val[conf]){
      freq2_prr <- rbind(freq2_prr, c(unlist(l2[row,]), prr))
    }
    if(rr >= rr_val[conf]){
      freq2_rr <- rbind(freq2_rr, c(unlist(l2[row,]), rr))
    }
    if(ror >= ror_val[conf]){
      freq2_ror <- rbind(freq2_ror, c(unlist(l2[row,]), ror))
    }
  }
  colnames(freq2_prr) <- c("item1","item2","support","prr")
  colnames(freq2_rr) <- c("item1","item2","support","rr")
  colnames(freq2_ror) <- c("item1","item2","support","ror")
  
  freq2_prr_list[[conf]] <- freq2_prr
  freq2_rr_list[[conf]] <- freq2_rr
  freq2_ror_list[[conf]] <- freq2_ror
}

names(freq2_prr_list) <- c("prr_thresh=1","prr_thresh=1.2","prr_thresh=1.5","prr_thresh=2")
names(freq2_rr_list) <- c("rr_thresh=1","rr_thresh=1.2","rr_thresh=1.5","rr_thresh=2")
names(freq2_ror_list) <- c("ror_thresh=1","ror_thresh=1.2","ror_thresh=1.5","ror_thresh=2")

f2_prr_1 <- freq2_prr_list[[1]]
f2_prr_1_2 <- freq2_prr_list[[2]]
f2_prr_1_5 <- freq2_prr_list[[3]]
f2_prr_2 <- freq2_prr_list[[4]]

f2_prr_1 <- f2_prr_1[order(as.numeric(f2_prr_1$prr)),]
f2_prr_1_2 <- f2_prr_1_2[order(as.numeric(f2_prr_1_2$prr)),]
f2_prr_1_5 <- f2_prr_1_5[order(as.numeric(f2_prr_1_5$prr)),]
f2_prr_2 <- f2_prr_2[order(as.numeric(f2_prr_2$prr)),]

#selected rules containing death as AE
f2_prr_1[which(f2_prr_1$item2 == "10011906"),]
f2_prr_1_2[which(f2_prr_1_2$item2 == "10011906"),]
f2_prr_1_5[which(f2_prr_1_5$item2 == "10011906"),]
f2_prr_2[which(f2_prr_2$item2 == "10011906"),]

f2_rr_1 <- freq2_rr_list[[1]]
f2_rr_1_2 <- freq2_rr_list[[2]]
f2_rr_1_5 <- freq2_rr_list[[3]]
f2_rr_2 <- freq2_rr_list[[4]]

f2_rr_1 <- f2_rr_1[order(as.numeric(f2_rr_1$rr)),]
f2_rr_1_2 <- f2_rr_1_2[order(as.numeric(f2_rr_1_2$rr)),]
f2_rr_1_5 <- f2_rr_1_5[order(as.numeric(f2_rr_1_5$rr)),]
f2_rr_2 <- f2_rr_2[order(as.numeric(f2_rr_2$rr)),]

#selected rules containing death as AE
f2_rr_1[which(f2_rr_1$item2 == "10011906"),]
f2_rr_1_2[which(f2_rr_1_2$item2 == "10011906"),]
f2_rr_1_5[which(f2_rr_1_5$item2 == "10011906"),]
f2_rr_2[which(f2_rr_2$item2 == "10011906"),]

f2_ror_1 <- freq2_ror_list[[1]]
f2_ror_1_2 <- freq2_ror_list[[2]]
f2_ror_1_5 <- freq2_ror_list[[3]]
f2_ror_2 <- freq2_ror_list[[4]]

f2_ror_1 <- f2_ror_1[order(as.numeric(f2_ror_1$ror)),]
f2_ror_1_2 <- f2_ror_1_2[order(as.numeric(f2_ror_1_2$ror)),]
f2_ror_1_5 <- f2_ror_1_5[order(as.numeric(f2_ror_1_5$ror)),]
f2_ror_2 <- f2_ror_2[order(as.numeric(f2_ror_2$ror)),]

#selected rules containing death as AE
f2_ror_1[which(f2_ror_1$item2 == "10011906"),]
f2_ror_1_2[which(f2_ror_1_2$item2 == "10011906"),]
f2_ror_1_5[which(f2_ror_1_5$item2 == "10011906"),]
f2_ror_2[which(f2_ror_2$item2 == "10011906"),]

#pairs selected by each paramter at different thresholds
pairs_selected <- c(nrow(f2_conf_0_4), nrow(f2_conf_0_5), nrow(f2_conf_0_6), nrow(f2_conf_0_7), 
                    nrow(f2_prr_1), nrow(f2_prr_1_2), nrow(f2_prr_1_5), nrow(f2_prr_2),
                    nrow(f2_rr_1), nrow(f2_rr_1_2), nrow(f2_rr_1_5), nrow(f2_rr_2),
                    nrow(f2_ror_1), nrow(f2_ror_1_2), nrow(f2_ror_1_5), nrow(f2_ror_2))

t <- head(f2_ror_2, 10)
t

vec_ae_name <- c()
vec_vac_name <- c()
prr_sel <- c()
rr_sel <- c()
conf_sel <- c()

for (row in 1:nrow(t)) {
  ae <- count_unique_aes[which(count_unique_aes[,1] == t[row,2]),2]
  vec_ae_name <- append(vec_ae_name, ae)
  vac <- count_unique_vacs[which(count_unique_vacs[,1] == t[row,1]),2]
  vec_vac_name <- append(vec_vac_name, vac)
  
  a <- sum((data[,t[row,1]] == 1) & (data[,t[row,2]] == 1))
  b <- sum((data[,t[row,1]] == 1) & (data[,t[row,2]] == 0))
  c <- sum((data[,t[row,1]] == 0) & (data[,t[row,2]] == 1))
  d <- sum((data[,t[row,1]] == 0) & (data[,t[row,2]] == 0))
  
  prr <- (a/(a + b + 0.00001))/((c+0.00001)/(c+d+0.00001))
  prr_sel <- append(prr_sel, prr)
  rr <- num_reports*a/((a + c)*(a + b)+0.00001)
  rr_sel <- append(rr_sel, rr)
  conf <- a/(a+b+0.00001)
  conf_sel <- append(conf_sel, conf)
}

nrow(f2_conf_0_7[which(f2_conf_0_7[,1] == "HEP"),])
nrow(f2_prr_2[which(f2_prr_2[,1] == "HEP"),])
nrow(f2_rr_2[which(f2_rr_2[,1] == "HEP"),])
nrow(f2_ror_2[which(f2_ror_2[,1] == "HEP"),])

# check if the first two columns of prr, rr and ror (343 drug-AE pairs) are same
comb_f2_prr_1 <- c()
comb_f2_rr_1 <- c()
comb_f2_ror_1 <- c()

for (row in 1:nrow(f2_prr_1)) {
  comb_f2_prr_1 <- append(comb_f2_prr_1, paste0(f2_prr_1[row,1], f2_prr_1[row,2]))
  comb_f2_rr_1 <- append(comb_f2_rr_1, paste0(f2_rr_1[row,1], f2_rr_1[row,2]))
  comb_f2_ror_1 <- append(comb_f2_ror_1, paste0(f2_ror_1[row,1], f2_ror_1[row,2]))
}
setequal(comb_f2_prr_1,comb_f2_rr_1)
setequal(comb_f2_prr_1,comb_f2_ror_1)
setequal(comb_f2_rr_1,comb_f2_ror_1) #they are all TRUE

#count occurrences of each AE and vaccine in t (first ten rows of ROR at threshold of 2)
count_unique_vacs_in_t <- c()
count_unique_aes_in_t <- c()
vec_ind_ae <- c()
vec_ind_vac <- c()

for (item in 1:length(unique(t$item1))) {
  uq <- as.character(unique(t$item1)[item])
  count_unique_vacs_in_t[item] <- sum(data[,uq])
  vec_ind_vac[item] <- which(as.character(count_unique_vacs[,1]) == unique(t$item1)[item])
}
count_unique_vacs_in_t_df <- cbind.data.frame(unique(t$item1), count_unique_vacs[vec_ind_vac,2], count_unique_vacs_in_t)
count_unique_vacs_in_t_df <- count_unique_vacs_in_t_df[order(count_unique_vacs_in_t_df$count_unique_vacs_in_t, decreasing=FALSE),]
names(count_unique_vacs_in_t_df) <- c("Vaccine code","Vaccine", "count")
print(count_unique_vacs_in_t_df)

for (item in 1:length(unique(t$item2))) {
  uq <- as.character(unique(t$item2)[item])
  count_unique_aes_in_t[item] <- sum(data[,uq])
  vec_ind_ae[item] <- which(as.character(count_unique_aes[,1]) == unique(t$item2)[item])
}
count_unique_aes_in_t_df <- cbind.data.frame(unique(t$item2), count_unique_aes[vec_ind_ae,2], count_unique_aes_in_t)
count_unique_aes_in_t_df <- count_unique_aes_in_t_df[order(count_unique_aes_in_t_df$count_unique_aes_in_t, decreasing=FALSE),]
names(count_unique_aes_in_t_df) <- c("AE code","AE", "count")
print(count_unique_aes_in_t_df)

#count occurrences of each important AE and vaccine in ROR (all rows of ROR at threshold of 2)
count_unique_vacs_in_ror <- c()
count_unique_aes_in_ror <- c()
for (item in 1:length(unique(f2_ror_2$item1))) {
  uq <- as.character(unique(f2_ror_2$item1)[item])
  count_unique_vacs_in_ror[item] <- sum(data[,uq])
}
print(count_unique_vacs_in_ror)
length(count_unique_vacs_in_ror)

for (item in 1:length(unique(f2_ror_2$item2))) {
  uq <- as.character(unique(f2_ror_2$item2)[item])
  count_unique_aes_in_ror[item] <- sum(data[,uq])
}
count_unique_aes_in_ror_df <- cbind.data.frame(unique(f2_ror_2$item2), count_unique_aes[which(count_unique_aes[,1] %in% unique(f2_ror_2$item2)),2], count_unique_aes_in_ror)
count_unique_aes_in_ror_df <- count_unique_aes_in_ror_df[order(count_unique_aes_in_ror_df$count_unique_aes_in_ror, decreasing=FALSE),]
names(count_unique_aes_in_ror_df) <- c("AE code","AE", "count")
print(count_unique_aes_in_ror_df)
nrow(count_unique_aes_in_ror_df)

print(f2_prr_2[which(f2_prr_2[,2] %in% count_unique_aes_in_ror_df[1:6,1]),])
print(f2_rr_2[which(f2_rr_2[,2] %in% count_unique_aes_in_ror_df[1:6,1]),])
print(f2_conf_0_7[which(f2_conf_0_7[,2] %in% count_unique_aes_in_ror_df[1:6,1]),])

#print prr, rr, and ror of f2_conf_0_7 selected pairs
t_2 <- f2_conf_0_4

vec_ae_name_2 <- c()
vec_vac_name_2 <- c()
prr_sel_2 <- c()
rr_sel_2 <- c()
ror_sel_2 <- c()

for (row in 1:nrow(t_2)) {
  ae <- count_unique_aes[which(count_unique_aes[,1] == t_2[row,2]),2]
  vec_ae_name_2 <- append(vec_ae_name_2, ae)
  vac <- count_unique_vacs[which(count_unique_vacs[,1] == t_2[row,1]),2]
  vec_vac_name_2 <- append(vec_vac_name_2, vac)
  
  a <- sum((data[,t_2[row,1]] == 1) & (data[,t_2[row,2]] == 1))
  b <- sum((data[,t_2[row,1]] == 1) & (data[,t_2[row,2]] == 0))
  c <- sum((data[,t_2[row,1]] == 0) & (data[,t_2[row,2]] == 1))
  d <- sum((data[,t_2[row,1]] == 0) & (data[,t_2[row,2]] == 0))
  
  prr <- (a/(a + b + 0.00001))/((c+0.00001)/(c+d+0.00001))
  prr_sel_2 <- append(prr_sel_2, prr)
  rr <- num_reports*a/((a + c)*(a + b)+0.00001)
  rr_sel_2 <- append(rr_sel_2, rr)
  ror <- (a*d)/(c*b+0.00001)
  ror_sel_2 <- append(ror_sel_2, ror)
}

t_2 <- cbind(as.character(t_2$item1), as.character(t_2$item2), 
             as.character(vec_ae_name_2), as.numeric(t_2$support), 
             as.numeric(t_2$confidence), as.numeric(prr_sel_2), 
             as.numeric(rr_sel_2), as.numeric(ror_sel_2))
row.names(t_2) <- c(1:nrow(t_2))
colnames(t_2) <- c("Vaccine Code", "AE Code", "AE", "Support", "Confidence", "PRR", "RR", "ROR")
print(t_2)
write.csv(t_2,file=paste0(file_path,"/conf_0_4_results.csv"), row.names = FALSE)
