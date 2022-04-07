config_my_test <- function(sn){
  # please enter all digits of your student number
  
  # set the seed
  set.seed(sn)
  print(paste0("seed successfully set to ",as.character(sn),"."))
  
  # set the sample size
  dat_n = sample(101:199,1)
  
  dat1_mean = runif(1, 10,200)
  dat1_sd = runif(1,10,50)
  
  dat2_mean = runif(1,10,200)
  dat2_sd = runif(1,10,50)
  dat2_3_r = sample(200:1400, 1)
  
  dat4_mean = runif(1,10,200)
  dat4_sd = runif(1,10,50)
  
  # generate data
  
  subjID = factor(sample(1:dat_n,dat_n,replace=FALSE), label="s")
  var1 = round(rnorm(dat_n, dat1_mean, dat1_sd),1)
  var2 = round(rnorm(dat_n, dat2_mean, dat2_sd),1)
  var3 = round((jitter(var2, dat2_3_r) + rnorm(dat_n,0,50)) / 10, 1)
  var4 = round(rnorm(dat_n, dat4_mean, dat4_sd),1)
  alldata <- data.frame(subjID, ratings=var1, stroop=var2, vision=var3, bias=var4)
  
  print("Your data was generated.")
  print("Please make sure there is a variable called testData (or whatever you called it) in your workspace.")
  print("If it isn't there, copy and paste the following line of code into your console (without the quotations), and press enter.")
  print(paste0("testData <- config_my_test(",sn,")"))
  
  testData <- alldata
  #return(alldata)
  
}

