createPNBdata <- function(studentNumber){
  # please enter all digits of your student number
  
  # set the seed
  set.seed(studentNumber)
  print(paste0("seed successfully set to ",as.character(studentNumber),"."))
  
  # set the sample size
  dat_n = sample(11:42,1)
  
  dat1_mean = runif(1, 40,90)
  dat1_sd = runif(1,10,15)
  
  dat2_mean = runif(1,40,90)
  dat2_sd = runif(1,10,15)
  dat2_3_r = sample(200:1400, 1) # value used for adding jitter to dat2
  
  dat4_mean = runif(1,90,120)
  dat4_sd = 15
  
  # generate data
  
  participant = factor(sample(1:dat_n,dat_n,replace=FALSE), label="p")
  
  test1tmp = round(rnorm(dat_n, dat1_mean, dat1_sd),1)
  test1tmp[test1tmp>100] = test1tmp[test1tmp>100]-(test1tmp[test1tmp>100]-100)*2
  test1tmp[test1tmp<0] = test1tmp[test1tmp<0]-(test1tmp[test1tmp<0])*2
  
  test2tmp = round(rnorm(dat_n, dat2_mean, dat2_sd),1)
  test2tmp[test2tmp>100] = test2tmp[test2tmp>100]-(test2tmp[test2tmp>100]-100)*2
  test2tmp[test2tmp<0] = test2tmp[test2tmp<0]-(test2tmp[test2tmp<0])*2
  
  tmp <- data.frame(participant,
                        test1=test1tmp,
                        test2=test2tmp,
                        test2duration=round(abs(jitter(test2tmp/100*50, dat2_3_r) + rnorm(dat_n,0,4)), 0),
                        emotionQuotient=round(rnorm(dat_n, dat4_mean, dat4_sd),0))
  

  print("Your data was generated.")
  print("Please make sure there is a variable called PNBdata (or whatever you called it) in your workspace.")
  print("If it isn't there, copy and paste the following line of code into your console (without the quotations), and press enter.")
  print(paste0("PNBdata <- createPNBdata(",studentNumber,")"))
  
  PNBdata <- tmp
  #return(PNBdata)
  
}

