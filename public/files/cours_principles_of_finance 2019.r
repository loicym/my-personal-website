efficient_frontier = function(MRet,rangeMu)
{
  uM = dim(MRet)[1]
  pM = dim(MRet)[2]
  #compute the portfolio's expected returns
  expRet = colMeans(MRet)

  #use sample variance matrix (does not change anything compared to pop. so this one
  #is unbiased!)

  Omega = var(MRet)
  # expVar = t(weights)%*%Omega%*%weights
  
  #create a matrix of expected returns and constraints: t(E) and t(1) right and only one
  #cause its the returns of each and the weight of each that we scale to one
  unityVec = rep(1,pM)
  
  A = rbind(expRet,unityVec)
  print(A)

  # compute var in function of expected returns independently of the weights:
  n = length(rangeMu)
  myVar = rep(NA,n)
  myWeights = matrix(data=NA,nrow=n,ncol=pM)
  
  for(i in 1:n)
  {
    b = matrix(data=c(rangeMu[i],1),nrow=2)
    myVar[i] = t(b)%*%solve(A%*%solve(Omega)%*%t(A))%*%b
    myWeights[i,] = solve(Omega)%*%t(A)%*%solve(A%*%solve(Omega)%*%t(A))%*%b
  }

  # dev.new()
  mySd = myVar^0.5
  return(mySd)
  
  # plot(mySd,rangeMu,t='l')
  # Ones = matrix(data=c(1,1),nrow=2)
  # weightsMinVar = solve(Omega)%*%Ones%*%solve(t(Ones)%*%solve(Omega)%*%Ones)
  # muMinVar = t(weightsMinVar)%*%expRet
  #case where we want the sum of the weights to sum to one (less efficient and more risky
  #(we want to achieve a kind of zero investment portfolio.)
  # bMax = matrix(data=c(targetMu,1),nrow=2)
  # print(bMax)
  
  # bMax = rbind(targetMu,)
  # case where target weights sum to zero - better for us.
  # bMaxZero = matrix(data=c(targetMu,0),nrow=2)
  # bMaxZero = rbind(targetMu,0)
  # weightsMinVarTarget = solve(Omega)%*%t(A)%*%solve(A%*%solve(Omega)%*%t(A))%*%bMaxZero
}

efficient_frontier_single = function(MRet)
{
  rangeMu = seq(0.001,0.025,0.01)
  mySd = efficient_frontier(MRet,rangeMu)
  
  dev.new()
  plot(mySd*100,rangeMu*100,xlab="volatility (%)",ylab="expected return (%)")
}

efficient_frontier_simul = function(MRet)
{
  pM = dim(MRet)[2]
  rangeMu = seq(-0.01,0.05,0.001)
  mySd = efficient_frontier(MRet[,1:9],rangeMu)
  
  dev.new()
  plot(mySd*100,rangeMu*100,xlab="volatility (%)",ylab="expected return (%)",t='l',xlim=c(0,max(mySd*100)),ylim=c(-1,5))
  
  for(j in 10:pM)
  {
    mySd = efficient_frontier(MRet[,1:j],rangeMu)
    lines(mySd*100,rangeMu*100,col='red')
    # Sys.sleep(0.5)
  }
}
