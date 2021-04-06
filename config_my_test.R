# create a script that they will SOURCE and it will create their datasets that they will use.
# in the script, want to set the seed to their student ID

config_my_test <- function(sn){
  # please enter all digits of your student number
  
  # load faux
  if(!require(faux)){install.packages("faux")}
  library(faux)
  print("faux package succesfully loaded.")
  
  if(!require(ggplot2)){install.packages("ggplot2")}
  library(ggplot2)
  print("ggplot2 package succesfully loaded.")

  if(!require(xlsx)){install.packages("xlsx")}
  library(xlsx)
  print("xlsx package succesfully loaded.")
  
  # set the seed
  set.seed(sn)
  print(paste0("seed successfully set to ",as.character(sn),"."))
  
  # set the sample size
  dat_n = round(runif(1, 80,120))
  
  dat1_mean = runif(1, 10,200)
  dat1_sd = runif(1,10,20)
  
  dat2_means = runif(2,10,200)
  dat2_sd = runif(2,10,20)
  dat2_r = runif(1,0.7,0.9)
  
  dat3_means = runif(2,10,200)
  dat3_sd = runif(2,10,20)
  dat3_r = runif(1,0.1,0.2)
  
  # generate data

  alldata <- data.frame(var1 = rnorm(dat_n, dat1_mean, dat1_sd),
             rnorm_multi(dat_n, vars=2, mu=dat2_means, sd=dat2_sd, r=dat2_r, varnames=c("var2","var3")),
             rnorm_multi(dat_n, vars=2, mu=dat3_means, sd=dat3_sd, r=dat3_r, varnames=c("var4","var5")))
  
  print("your data has been generated.")
  print("Please make sure you saved it in a variable")
  print("(i.e., myData <- setup_test(studentnumber)")
  
  
  return(alldata)
  
}


