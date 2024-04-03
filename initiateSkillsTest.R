generateData <- function(studentNumber=123456789, todaysDate=20240101){
  # please enter all digits of your student number

  # set the seed
  set.seed(abs(studentNumber - todaysDate))
  print(paste0("seed successfully configured."))
  print("generating data now...")
  
  # set the sample size
  dat_n = sample(20:40,1)
  
  dat1_mean = runif(1,40,90)
  dat1_sd = runif(1,10,15)
  
  dat2_mean = runif(1,40,90)
  dat2_sd = runif(1,10,15)
  
  dat2_3_r = dat2_mean*10 # value used for adding jitter to dat2
  
  dat4_mean = runif(1,90,120)
  dat4_sd = runif(1,9,13)
  
  # generate data
  
  participant = factor(sample(1:dat_n,dat_n,replace=FALSE), label="sub")
  
  tmp1 = round(rnorm(dat_n, dat1_mean, dat1_sd),1)
  tmp1[tmp1>100] = tmp1[tmp1>100]-(tmp1[tmp1>100]-100)*2
  tmp1[tmp1<0] = tmp1[tmp1<0]-(tmp1[tmp1<0])*2
  
  tmp2 = round(rnorm(dat_n, dat2_mean, dat2_sd),1)
  tmp2[tmp2>100] = tmp2[tmp2>100]-(tmp2[tmp2>100]-100)*2
  tmp2[tmp2<0] = tmp2[tmp2<0]-(tmp2[tmp2<0])*2
  
  tmp3 = round(runif(dat_n, 1,10))
  tmp4 = rgamma(dat_n, sample(1:10,1))
  
  tmpAge = round(runif(dat_n, 18,25))
  tmpEthnicity = sample(c("caucasian","black","middle-eastern","east-asian","south-asian","indigenous","latino"), dat_n, replace=T)
  tmpHand = sample(c("left","left","right","right","right","right","ambidextrous"), dat_n, replace=T)
  tmp <- data.frame(subjectID=participant,
                    age=tmpAge,
                    ethnicity=tmpEthnicity,
                    handedness=tmpHand,
                    faceTest=tmp1,
                    memoryTest=tmp3,
                    stressTest=tmp4,
                    attentionTest=tmp2,
                    attentionDuration=round(abs(jitter(tmp2/2, dat2_3_r) + rnorm(dat_n,600,20)), 0),
                    emotionIntel=round(rnorm(dat_n, dat4_mean, dat4_sd),0))
  
  

  print("Your data was generated.")
  print("Please make sure there is a variable called McMasterData (or whatever you called it) in your workspace.")
  print("If it isn't there, copy and paste the following line of code into your console (without the quotations), and press enter.")
  print(paste0("McMasterData <- generateData(",studentNumber,", ",todaysDate,")"))
  
  McMasterData <- tmp
  #return(McMasterData)

}


# create a function that does all the magic
simulate_CI_new = function(n=30, num_samples=100, pop_mean=0, pop_sd=1, studentNumber=sample(1:999999,1)){
  # function written by Ali Hashemi
  # this script will generate a plot showing the sample mean +/- 95% confidence intervals for a desired number of samples of a specific sample size from a population with a desired mean and SD.
  # n = sample size (default=30)
  # num_samples = number of samples (default=100)
  # pop_mean = true mean of the theoretical population (default=0)
  # pop_sd = true standard deviation of the theoretical population (default=1)
  
  set.seed(studentNumber)
  
  sample_params = array(NA, c(num_samples,4)) # create an empty array to store sample means and 95% confidence intervals, and other things..
  for(ii in 1:num_samples){ # a for loop to cycle through each sample
    cur_sample = rnorm(n,pop_mean,pop_sd) # simulate current sample
    sample_mean = mean(cur_sample) # calcualte sample's mean
    sample_sd = sd(cur_sample) # calculate sample's SD
    ci_lower = sample_mean - (1.96*(sample_sd/sqrt(n))) # calculate lower 95% confidence interval
    ci_upper = sample_mean + (1.96*(sample_sd/sqrt(n))) # calculate upper 95% confidence interval
    
    # does this sample's 95% CI include the population mean? in other words, is the population mean smaller than the lowest value in the CI inerval OR is it greater than the highest value in the CI?
    includes_mu = as.logical(ci_lower<=pop_mean & ci_upper>=pop_mean) # note the | reflects "or" in many programming languages
    
    sample_params[ii,] = c(sample_mean, ci_lower, ci_upper, includes_mu) # store these into the empty array in row ii
  }
  
  # calculate how many samples' confidence intervals did not include the true population mean
  reject_rate = round(1-mean(sample_params[,4]),2)*100
  
  # make the figure!
  par(mar=c(3,3,2.5,.5), mgp=c(2,1,0)) # set some margin sizes
  plot(x=sample_params[,1], y=c(1:num_samples), type="p", xlim=c(min(sample_params[,2]),max(sample_params[,3])), xlab="mean +/- 1.96 SEM", ylab="sample #", main=paste0(num_samples, " samples of n=",n, " each.")) # create a pretty customized plot... each argument should be self-explanatory
  segments(sample_params[,2], 1:num_samples, sample_params[,3], 1:num_samples) # add the confidence interval lines to each point
  points(sample_params[,1], y=c(1:num_samples), pch=21, bg="white") # replot points to make the filled in
  points(sample_params[sample_params[,4]==FALSE,1], c(1:num_samples)[sample_params[,4]==FALSE], pch=21, bg="red") # make the extreme ones red
  segments(sample_params[sample_params[,4]==FALSE,2], c(1:num_samples)[sample_params[,4]==FALSE], sample_params[sample_params[,4]==FALSE,3], c(1:num_samples)[sample_params[,4]==FALSE], col="red") # make their lines red too
  abline(v=pop_mean, lwd=2) # add a vertical line for the true population mean
}
