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
#'@param Model object of a complete Model with known RD. RD="resid" non-parametric bootstrap, RD="norm" parametric boostrap.
#'@param PRP object of the MODEL.PRO output which will be boostraped.
#'@param PRP.PARAM the parameter that is used to calculate the PRP.
#'@param nrun number of bootrap runs
#'@param Method a parameter that determines how the boostrap samples are generated.
#'@return a list of bootstraped PARAM, bootstraped resid and bootstraped PRP.
#'
#'@examples
#'
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
#'
#'
#'
#'@export


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
    print(r)
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
    for (r in 1:nrun ) {
      resid.BOOT[r,1][[1]] = U*NA;
    }
  }

  return(resid.BOOT)
}



