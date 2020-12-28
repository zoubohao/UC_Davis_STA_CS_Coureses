###################################################################### 
# simulation if cox PH model is assumed, with some continuous covariates; 
# baseline event time is assumed to follow Weibull/exponential distribution
# indepedent (uniform) censoring and Right censoring
###################################################################### 
library(MASS)

sim_cox<- function(lambda0, beta,censor.right = 125)
{
  # N = Total sample size 
  # beta = PH coefficients : c()
  # lambda0 = rate parameter of the exponential distribution for baseline
  N = 328
  # sex
  sex = sample(c(rep("Male", 252), rep("Female", 76)), size = N, replace = F)
  
  # age 26-83, mean 58,2
  age = sample(x = c(26:83), size = N, replace = T)
  
  # The predominant etiology of liver disease was hepatitis B
  liver_disease = sample(x=c("No Disease", "Hepatitis B"), 
                           size=N, replace=TRUE, prob=c(1-0.735, 0.735))
  
  # RLI type
  rli_type = sample(c(sample("None", size = 124, replace = T),
                      sample("Marginal", size = 106, replace = T),
                      # severe RLI
                      sample("Partial", size = 63, replace = T),
                      sample("segmental", size = 16, replace = T),
                      sample("necrotic", size = 19, replace = T)), size = N, replace = F)
  # Minimal RLI or Severe RLI
  min_Severe_RLI = rep("Severe", N)
  min_Severe_RLI[which(rli_type == "None" | rli_type == "Marginal")] = "Min"
  
  # AH or NAH
  ah_nah = sample(c("AH", "NAH"), size = N, replace = T, prob = c(0.634, 1-0.634))
  
  # major or minor liver resection
  major_minor_liver_resection = sample(c(rep("Major", 105), rep("Minor", 223)), size = N, replace = F)
  
  # Previous TACE
  p_TACE = sample(c("TACE", "NoTACE"), size = N, replace = T)
  
  # Child-Pugh classification B or C
  chile_class = sample(c("B", "C"), size = N, replace = T)
  
  # use of the Pringle maneuver
  pringle_maneuver = sample(c("Yes", "No"), size = N, replace = T)
  
  # Intraoperative transfusion
  intra_trans = sample(c("Yes", "No"), size = N, replace = T)
  
  # Serum albumin level 3.4 to 5.4 g/dL
  serum_alb_level = abs(rnorm(N, mean = 4.4, sd = 3))
  
  # Serum ALT level 29 to 33 units per liter (IU/L) for males and 19 to 25 IU/L for females,
  serum_alt_level = abs(rnorm(N, mean = 26, sd = 7))
  
  # Longer operative time
  long_op_time = sample(c("Yes", "No"), size = N, replace = T)
  
  # Stage T3 or T4
  stage = sample(c("T3", "T4"), size = N, replace = T)
  
  # Open surgery
  open_surgery = sample(c("Yes", "No"), size = N, replace = T)
  
  # Nonanatomical resection
  nonanatom_resection = sample(c("Yes", "No"), size = N, replace = T)
  
  # Presence of a satellite nodule
  satellite_nodule = sample(c("exist", "NoExist"), size = N, replace = T)
  
  # Microscopic vascular invasion
  micro_vascular_invasion = sample(c("Yes", "No"), size = N, replace = T)
  
  # Multinodular confluent or infiltrative gross tumor type
  mcig_type = sample(c("A", "B", "C", "D"), size = N, replace = T)
  
  # Histologically confirmed cirrhosis
  h_confirmed_c = sample(c("Yes", "No"), size = N, replace = T)
  
  # randomization to treatment or control  
  A = rep(1,N)
  A[which(min_Severe_RLI == "Min")] = 0
  
  # generate continuous covariates, mutually indepedent
  
  # generate underlying event time (range, 1-125 months),
  Time = rweibull(n=N, shape=1, scale = lambda0*exp(beta[1]*A+beta[2]*age+beta[3]*serum_alb_level 
                                                    + beta[4]*serum_alt_level)) + 1
  time_new = c()
  for (t in Time){
    if (t > 125){
      time_new = c(time_new, 125)
    }
    else{
      time_new = c(time_new, t)
    }
  }
  time_new = round(time_new)
  censor = sample(c(0,1),size=N,replace = T, prob = c(0.25, 0.75))
  # data set
  dataset = data.frame(sex = sex, age = age,liver_disease = liver_disease, rli_type = rli_type,
             min_Severe_RLI = min_Severe_RLI, ah_nah = ah_nah, major_minor_liver_resection = major_minor_liver_resection,
             p_TACE = p_TACE, chile_class = chile_class, pringle_maneuver = pringle_maneuver,
             intra_trans = intra_trans, serum_alb_level= serum_alb_level, serum_alt_level= serum_alt_level,
             long_op_time = long_op_time, stage = stage, open_surgery = open_surgery,
             nonanatom_resection = nonanatom_resection, satellite_nodule = satellite_nodule,
             micro_vascular_invasion = micro_vascular_invasion,mcig_type= mcig_type,
             h_confirmed_c=h_confirmed_c,
             time=time_new,
             censor=censor)
  return(dataset)
}
mydata=sim_cox(lambda0 = 1, beta = c(1.15, 0.02, 0.2,0.03), censor.right = 125)
write.csv(mydata, file="d:\\simulation.csv", row.names = F)

 