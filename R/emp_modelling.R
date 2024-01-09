#' MODEL.DGP
#'
#' This function completes the Model by generating the endogeous variables for given model parametrs and given/generated residuals.
#' In application it will be implemented through dgp(Model,T,M).
#'
#'@param Model object of an incomplete Model with known H_PARAM, PARAM,EXOG,resid and RD (For RD="norm" resid will be gnerated for RD="resid" resid must be provides.
#'@param T the number of data to be generated. If EXOG is given, T must be consistent with the number of observations in EXOG.
#'@param M the dimension of the data to be generated
#'@return an object of Model with generated ENDOG and resid.
#'
#'@export

MODEL.DGP = function(Model,T,M)
{
	END_RES = dgp(Model,T,M)
     	return(END_RES)
}


#' MODEL.EST
#'
#' This function completes the Model by providing PARAM and resid for given endogeneous and exogeneous variables.
#' In application it will be implemented through est(Model).
#'
#'@param Model object of an incomplete Model with known H_PARAM, ENDOG and EXOG.
#'@return Model object with estimated PARAM, INFOC, and resid.
#'
#'@export

MODEL.EST <- function(Model)
{

  estimate = est(Model)
  Model$PARAM = estimate$PARAM
  Model$resid = estimate$resid
  Model$INFOC = estimate$INFOC
  return(Model)
}

#' MODEL.PRP
#'
#' This function calculates certain properties of a complete Model for given PRP.PARAM. The properties are usually for some application purposes.
#' In application it will be implemented through prp(Model,PRP.PARAM).
#'
#'@param Model object of a complete Model with known H_PARAM, PARAM, ENDOG, EXOG and resid.
#'@param PRP.PARAM a list containing parameters that must be used to calculate the properties.
#'@return a list of PRP of interest.
#'
#'@export


MODEL.PRP = function(Model,PRP.PARAM)
{
      PROP = prp(Model,PRP.PARAM)

      return(PROP)
}


#' MODEL.BOOT
#'
#' This function bootstraps the Properties of a complete Model inlcuding bootstrap of the estimated parametrs. Often it is used for model validation purpose.
#'
#' @param Model object of a complete Model with known RD. RD="resid" non-parametric bootstrap, RD="norm" parametric boostrap.
#' @param PRP object of the MODEL.PRO output which will be boostraped.
#' @param PRP.PARAM the parameter that is used to calculate the PRP.
#' @param nrun number of bootrap runs
#' @param Method a parameter that determines how the boostrap samples are generated.
#' @return a list of bootstraped PARAM, bootstraped resid and bootstraped PRP.
#' @examples
#' ENDOG = NA
#' n = 1
#' EXOG = NA
#' Co = 2
#' Sigma = 1.5
#' H_PARAM = list(n)
#' PARAM   = list(Co,Sigma);names(PARAM) = c("Co","Sigma")
#' resid   = NA
#' RD      = c("norm")
#' INI = NA
#' AIC = NA
#' BIC = NA
#' INFOC = list(AIC,BIC)
#' EXTRA = NA
#' Model = list(H_PARAM,PARAM,INFOC,ENDOG,EXOG,resid,RD,INI,EXTRA)
#' names(Model) = c("H_PARAM","PARAM", "INFOC" ,"ENDOG","EXOG","resid',"RD","INI","EXTRA")
#'
#'
#' DGP = MODEL.DGP(Model=Model,T=200,M=1)
#' EST <- MODEL.EST(DGP)
#' prp(EST,PRP.PARAM)
#' Cshareo =c(0,0)
#' PRP.PARAM = list(Cshareo); names(PRP.PARAM) = c("Cshareo")
#' Method = "norm"
#' nrun   = 200
#' dim(EST$resid)
#' bootresult = MODEL.BOOT(EST,PRP,PRP.PARAM,nrun,Method)
#' SM<-Summary(OUT = bootresult,Model=EST,PRP.PARAM)
#' @export
MODEL.BOOT <- function(Model,PRP,PRP.PARAM,nrun,Method)
{
  T = dim(as.matrix(Model$ENDOG))[1]
  M = dim(Model$ENDOG)[2]
  PARAM.BOOT = BOOT(Model$PARAM,nrun)
  PRP.BOOT   = BOOT(PRP,nrun)
  #if (!(Method==Model$H_PARAM$DGP))  {print("Incompatible"); break }


  resid.BOOT = bstrp(Model,nrun,Method,T)


  for (r in 1:nrun) {
    Model$resid   		= resid.BOOT[r,1][[1]]
    DGP_B         		= MODEL.DGP(Model,T,M)
    EST_B         		= MODEL.EST(DGP_B)
    PARAM.BOOT[[r,1]]    	= EST_B$PARAM
    PRP.BOOT[[r,1]]         = MODEL.PRP(EST_B,PRP.PARAM)
    #print(r)
  }
  results = list(PARAM.BOOT,resid.BOOT,PRP.BOOT,PRP.PARAM)
  names(results) = c("PARAM.BOOT","resid.BOOT","PRP.BOOT","PRP.PARAM")
  return(results)
}





#' MODEL.SELECT
#'
#' This function calculates model selection values for a set of given H_PARAMs for selection of the hypo-parameter.
#'
#'@param Model object of a incomplete Model with a set of K H_PARAMs, ENDOG and EXOG.
#'@param H_PARAML a list of K H_PARAMs.
#'@param K number of H_PARAM for selection.
#'@return a array of K model selection values.
#'
#'@export

MODEL.SELECT = function(Model,H_PARAML,K)
{
  L = length(H_PARAML)
  Select = matrix(0,L,K)
  for ( i in 1:L) {
    Model$H_PARAM = H_PARAML[[i]]
    estimate = est(Model)
    Select[i,] = estimate$CRITERIA
  }
  return(Select)
}



#' BOOT
#'
#' This creates a list of PRP object to contain the bootrap results.
#'
#'@param C list of resid used as container for bootstraped residuals
#'@param nrun number of bootstrap runs.
#'@return an array to contain the list of bootstrap and number of bootstrap runs.
#'@export

BOOT = function(C,nrun)
{
  MyArray = array(list(NULL),c(nrun,1))
  return(MyArray)
}


#' bstrp
#'
#' This function calculates bootstrap residuals.
#'
#'@param Model an estimated Model object.
#'@param nrun number of bootstrap runs.
#'@param Method a parameter that determines how the bootstrap residuals are to to generate. Method="residuals" for residuals' bootstrap, Method="norm" for parametric bootstrap from a normal distribution, and Method = "binomial" for parametric bootstrap from a binomial distribution.
#'@param T is the length of the bootstrap residuals.
#'@return a list of Model residuals.
#'
#'@export
bstrp <- function(Model,nrun,Method,T)
{
  resid = Model$resid
  n = dim(resid)[2]
  N = dim(resid)[3]
  residl = list(resid)
  resid.BOOT = BOOT(residl,nrun)
  if (Method == "residuals")   {
    for (r in 1:nrun ) {
      u_index = as.integer(stats::runif(T,1,dim(resid)[1]))
      if ( !is.na(N) ) {
        U =   resid[u_index,,];
        dim(U) = c(T,n,N)
        resid.BOOT[r,1][[1]]  = U;
      }   else  {
        resid.BOOT[r,1][[1]]  = resid[u_index,]
      }

    }
  }
  if ((Method == "norm" ))   {
    for (r in 1:nrun ) {
      if (!is.na(N))  {
        U = resid
        for (j in 1:N) U[,,j] = rnormSIGMA(dim(resid)[1],as.matrix(Model$PARAM$Sigma[,,j]));
        dim(U) = c(T,n,N)
        resid.BOOT[r,1][[1]] = U
      }    else   {
        U = rnormSIGMA(dim(resid)[1],Model$PARAM$Sigma);
        resid.BOOT[r,1][[1]] = U;
      }
    }
  }
  if ((Method == "binomial" ))   {
    U = Model$resid
    for (r in 1:nrun ) {
      resid.BOOT[r,1][[1]] = U*NA;
    }
  }

  return(resid.BOOT)
}

#' Multivariate normal random series
#'
#' This function will generate iid multivariate normal random time series.
#'
#' @param T Length of the generated time series
#' @param sigma An (n x n) covariance matrix of the normal series
#' @return T x n matrix of iid normal time series
#' @export
#' @keywords internal
rnormSIGMA = function(T,sigma) {
  # generate random numbers from iid multivariate normal distribution with covariance matrix Sigma
  n = dim(sigma)[1]
  U = stats::rnorm(T*n)
  dim(U) = c(T,n)
  U = U%*%chol(sigma)
  return(U)
}


#' Summary of bootstrap results
#'
#' This function summarizes the bootstrap results.
#'
#' @param OUT is the output of MODEL.BOOT
#' @param Model is an estimated Model object
#' @param PRP.PARAM is the bootstrap parameter used in MODEL.BOOT
#' @return a list containing parameter bootstrapped confidence interval, the bootstrapped interval of test statistics in Cshare, and the impulse response functions.
#' @export
#'
Summary <- function(OUT = bootresult,Model=EST,PRP.PARAM) {
  PARAM.BOOT <- OUT[[1]]
  PRP.BOOT   <- OUT[[3]]

  #### output parameter bootstrap
  r = 1
  nrun   = length( PARAM.BOOT)
  NPARAM = length(unlist(PARAM.BOOT[[r]]))

  param.array= matrix(0,NPARAM,nrun)
  param.out  = matrix(0,NPARAM,3)
  for (i in 1: nrun) {
    param.array[,i] = unlist(PARAM.BOOT[[i]])
  }

  for (i in 1:NPARAM) {
    paramout<-print(c(quantile(param.array[i,],c(.05,.95)),unlist(Model$PARAM)[i]))
    param.out[i,] <- c(quantile(param.array[i,],c(.05,.95)),unlist(Model$PARAM)[i])
  }
  #### output PRP.BOOT point statistic such as restrictions
  NPRP1 = length(unlist(PRP.BOOT[[r]][[1]]))
  prp1.array= matrix(0,NPRP1,nrun)
  prp1.out  = matrix(0,NPRP1,3)

  for (i in 1: nrun) {
    prp1.array[,i] = unlist(PRP.BOOT[[i]][[1]])
  }
  Cshareo = PRP.PARAM$Cshareo

  for (i in 1:NPRP1) {
    prp1out <- print(c(quantile(prp1.array[i,],c(.05,.95)),unlist(Cshareo)[i]))
    prp1.out[i,] <-  c(quantile(prp1.array[i,],c(.05,.95)),unlist(Cshareo)[i])
  }

  #### output PRP.BOOT path statistic such as IRF

  out = list(param.out,prp1.out)

  PRP = prp(EST,PRP.PARAM)


  if (length(PRP.BOOT[[1]])==2) {

    bootarray = c(1:(nrun*n*n*nstep))*0
    dim(bootarray) = c(n,n,nstep,nrun)
    bootconf = c(1:(3*n*n*nstep))*0
    dim(bootconf) = c(n,n,nstep,3)
    for (i in 1:nrun) {
      bootarray[,,,i]= bootresult$PRP.BOOT[[i]]$IRF
    }

    for (i in 1:(n))        {
      for (j in 1:(n))     {
        for (k in 1:nstep ) {
          bootconf[i,j,k,2:3] = quantile(bootarray[i,j,k,],c(.05,.95))
        }}}
    bootconf[,,,1] = PRP$IRF
    ACCbootconf3N = ACCIRFconf(bootconf)

    dim(bootconf) = c(n,n,nstep,3)
    dim(ACCbootconf3N) = c(n,n,nstep,3)
    IRF_list <-IRF_graph(bootconf)
    out = list(param.out,prp1.out,bootconf,ACCbootconf3N)
  }
  if (length(PRP.BOOT[[1]])==3) {
    bootarray1 = c(1:(nrun*n*n*nstep))*0
    dim(bootarray1) = c(n,n,nstep,nrun)
    bootconf1 = c(1:(3*n*n*nstep))*0
    dim(bootconf1) = c(n,n,nstep,3)
    for (i in 1:nrun) {
      bootarray1[,,,i]= bootresult$PRP.BOOT[[i]]$IRF1
    }

    for (i in 1:(n))        {
      for (j in 1:(n))     {
        for (k in 1:nstep ) {
          bootconf1[i,j,k,2:3] = quantile(bootarray1[i,j,k,],c(.05,.95))
        }}}
    bootconf1[,,,1] = PRP$IRF1
    ACCbootconf3N1 = ACCIRFconf(bootconf1)
    IRF_list <-IRF_graph(bootconf1)



    bootarray2 = c(1:(nrun*n*n*nstep))*0
    dim(bootarray2) = c(n,n,nstep,nrun)
    bootconf2 = c(1:(3*n*n*nstep))*0
    dim(bootconf2) = c(n,n,nstep,3)
    for (i in 1:nrun) {
      bootarray2[,,,i]= bootresult$PRP.BOOT[[i]]$IRF2
    }

    for (i in 1:(n))        {
      for (j in 1:(n))     {
        for (k in 1:nstep ) {
          bootconf2[i,j,k,2:3] = quantile(bootarray2[i,j,k,],c(.05,.95))
        }}}
    bootconf2[,,,1] = PRP$IRF2
    ACCbootconf3N2 = ACCIRFconf(bootconf2)

    IRF_list <-IRF_graph(bootconf2)
    out = list(param.out,prp1.out,bootconf1,ACCbootconf3N1,bootconf2,ACCbootconf3N2)
  }
  return(out)
}

#' Multivariate normal random series
#'
#' This function calculates the accumulative impulse response function.
#'
#' @param IRF is an impulse response function
#' @return an accumulative impulse response function with the same dimension of IRF.
#' @export
#'
ACCIRFconf = function(IRF) {
  ACCirf = IRF
  dm = dim(IRF)
  for (t in 2:dm[3])          {
    ACCirf[,,t,] =    ACCirf[,,t-1,] + IRF[,,t,]
  }
  return(ACCirf)
}


