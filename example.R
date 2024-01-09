#' Example of a dgp function
#'
#' This function shows how a dgp can be constructed.
#'
#'@param Model is a Model object with given paramters (PARAM) and resid or Model$RD="norm"
#'@param T the number of data points to be generated.
#'@param M usually takes the value 1.
#'@return A list of Model objact with generated data (ENDOG) and the parameters used to generate the data.
#'@export
#'
dgp = function(Model,T, M=1)
{
  Co    = Model$PARAM[[1]]
  Sigma = Model$PARAM[[2]]

  if ( Model$RD=="resid" )  {
    resid = as.matrix(Model$resid)
  }
  if  ( Model$RD  == "norm" ) {
    resid = as.matrix(stats::rnorm(T))%*%as.matrix(sqrt(Sigma))
  }
  Y = Co + resid
  Model$ENDOG   = as.matrix(Y)
  Model$resid   = as.matrix(resid)
  return(Model)
}

#' Example of a est function
#'
#' This function shows how an est can be constructed to estimate the model parameter in PARAM
#'
#'@param Model is a Model object with generated data.
#'@return A list of Model objact with estimated parameter (PARAM) values and the generated data (ENDOG).
#'@export
#'
est = function(Model) {
  ## this is a program estimating VAR(p) with exogenous variables via LS
  n       = Model$H_PARAM[[1]]
  Y       = Model$ENDOG
  Co      = mean(Y)
  Sigma   = stats::var(Y)
  T       = length(Model$ENDOG)
  PARAM   = list(Co,Sigma);   names(PARAM) = c("Co","Sigma")
  Model$PARAM = PARAM

  AIC =   2*2-2*(-(T*n/2)*log(2*pi) -(T*n/2) +(T/2)*log(det(solve(Sigma))))
  BIC =   log(T)*2- 2*(-(T*n/2)*log(2*pi) -(T*n/2) +(T/2)*log(det(solve(Sigma))))
  INFOC = list(AIC,BIC); names(INFOC) =c("AIC","BIC")
  Model$INFOC = INFOC
  return(Model)
}


Cshareo =c(0,0)
PRP.PARAM = list(Cshareo); names(PRP.PARAM) = c("Cshareo")

#' Example of a prp function
#'
#' This function shows how a prp function can be constructed to test the designed properties.
#'
#'@param Model is an estimated Model object.
#'@param PRP.PARAM a list containing the parameters of the to be tested properties. Typitcally it includs the values of the test statistics under the null hypothesis (Cshareo).
#'@return A list of bootstraped test statistics.
#'@export
#'
prp = function(Model,PRP.PARAM)
{
  n = Model$H_PARAM[[1]]
  B = Model$PARAM[[1]]
  Co= Model$PARAM[[2]]
  Cshare=c(B[1]-1,Co[1]-2)
  results = list(Cshare)
  names(results) = c("Cshare")
  return(results)
}

