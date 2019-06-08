# COSTS   --------------------------------------------------------------------

#' Key Source: 
#' Table 3. Gebo et al. AIDS 2012
#' Contemporary costs of HIV healthcare in the HAART era. 
#' AIDS. 2010 Nov 13;24(17):2705-15. doi: 10.1097/QAD.0b013e32833f3c14.
#' Table 3. Mean annualized costs (and 95% confidence interval) of HIV care by CD4 stratum and cost category
#' 

# i need to take away the drug costs for the people in the untreated compartment

prob = FALSE


#'  INPUTS:
#'  
#'  cVaccine : price for one doses of 5-dose series vaccine (excluding delivery costs)


GetCostParams <- function(prob = FALSE, nsims, cVaccine, cPrep, societal = FALSE){
  
  medCPI_06to17 <- 1.41379  # medical cost inflation from 2006 to 2017
  
  #gamma distribution defined as: f(x)= 1/(s^a Gamma(a)) x^(a-1) e^-(x/s)
  
  cAcute       <- 0 # does acute infection have a cost? what is it?
  
  c500_mean    <- 16614*medCPI_06to17/4 # (16052 – 17177) CD4 >=500
  c500_sd      <- (17177-16052)/(2*1.96)*medCPI_06to17/4
  c500_shape   <- (c500_mean/c500_sd)^2
  c500_scale   <- (c500_sd^2/c500_mean)
  c500         <- if(prob){rgamma(nsim, shape = c500_shape, scale = c500_scale)}else{c500_mean}
  
  c350_mean    <- 16859*medCPI_06to17/4 # (15798–17920) CD4 350-499
  c350_sd      <- (17920-15798)/(2*1.96)*medCPI_06to17/4
  c350_shape   <- (c350_mean/c500_sd)^2
  c350_scale   <- (c350_sd^2/c350_mean)
  c350         <- if(prob){rgamma(nsim, shape = c350_shape, scale = c350_scale)}else{c350_mean}
  
  c200_mean    <- 19565*medCPI_06to17/4 # (18472–20658) CD4 200-349
  c200_sd      <- (20658-18472)/(2*1.96)*medCPI_06to17/4
  c200_shape   <- (c200_mean/c200_sd)^2
  c200_scale   <- (c200_sd^2/c200_mean)
  c200         <- if(prob){rgamma(nsim, shape = c200_shape, scale = c200_scale)}else{c200_mean}
  
  cAIDS_mean    <- 40678*medCPI_06to17/4 # (33566–47789) CD4 <50
  cAIDS_sd      <- (47789-33566)/(2*1.96)*medCPI_06to17/4
  cAIDS_shape   <- (cAIDS_mean/cAIDS_sd)^2
  cAIDS_scale   <- (cAIDS_sd^2/cAIDS_mean)
  cAIDS         <- if(prob){rgamma(nsim, shape = cAIDS_shape, scale = cAIDS_scale)}else{cAIDS_mean}
  
  cPrep_mean    <- NA   
  cPrep_sd      <- NA   
  cPrep_shape   <- NA
  cPrep_scale   <- NA
  #cPrep         <- (1049.91*3) # FSS pricing of $1,049.91 from CCST in March 2018
                                # cost for 3-months supply
                                # THIS HAS MOVED TO Main.R AS INPUT FOR THIS FUNCTION
  
  cPreventionLabs <- 44+22+37+25+19+7+10 
                    # $44 HIV antibody test CMS reimbursement for 4th gen HIV test CPT code 87389
                    # $22 chalmydia CPT code 86631
                    # $37 gonorrhea cpt 87590
                    # $25 syphillis cpt 86780
                    # $19 hep B cpt 87340 
                    # $7 urea nitrogen 84520
                    # $10 serum creatinine 82565
  cPreventionVisit <- 51 # HCPCS 99402
  

  #cVaccine <- (1623.72*(3/5)*1.3) #upfront one-time cost for 5-dose series of vaccine  
                #Benchmarking reference: FSS price of Guardasil 9 is 1623.72 per dose
                # THIS HAS MOVED TO Main.R AS INPUT FOR THIS FUNCTION

  nDoses <- 5
  
  cFI <- 0 # cost of financial incentives
  
  
  # alpha = shape; beta = scale
  # quarterly costs
  # only U and T are on ART
  c.SL   <- 0
  c.SH   <- 0
  c.SPL  <- cPrep + cPreventionLabs + cPreventionVisit
  c.SPH  <- cPrep + cPreventionLabs + cPreventionVisit
  c.SVL  <- 0 # apply the vaccine costs to the newly vaccinated group that quarter
  c.SVH  <- 0 # apply the vaccine costs to the newly vaccinated group that quarter
  c.SPVL <- cPrep + cPreventionLabs + cPreventionVisit
  c.SPVH <- cPrep + cPreventionLabs + cPreventionVisit
  c.I1L <- cAcute
  c.I1H <- cAcute
  c.I2L <- cAcute
  c.I2H <- cAcute
  c.I3L <- cAcute
  c.I3H <- c350
  c.I4L <- c200
  c.I4H <- c200
  c.I5L <- cAIDS
  c.I5H <- cAIDS
  c.D1L <- cAcute
  c.D1H <- cAcute
  c.D2L <- c500
  c.D2H <- c500
  c.D3L <- c350
  c.D3H <- c350
  c.D4L <- c200
  c.D4H <- c200
  c.D5L <- cAIDS
  c.D5H <- cAIDS
  c.E1L <- cAcute
  c.E1H <- cAcute
  c.E2L <- c500
  c.E2H <- c500
  c.E3L <- c350
  c.E3H <- c350
  c.E4L <- c200
  c.E4H <- c200
  c.E5L <- cAIDS
  c.E5H <- cAIDS
  c.U1L <- cAcute
  c.U1H <- cAcute
  c.U2L <- c500
  c.U2H <- c500
  c.U3L <- c350
  c.U3H <- c350
  c.U4L <- c200
  c.U4H <- c200
  c.U5L <- cAIDS
  c.U5H <- cAIDS
  c.T1L <- cAcute
  c.T1H <- cAcute
  c.T2L <- c500
  c.T2H <- c500
  c.T3L <- c350
  c.T3H <- c350
  c.T4L <- c200
  c.T4H <- c200
  c.T5L <- cAIDS
  c.T5H <- cAIDS
  c.NewI  <- 0
  c.NewD <- 0
  c.NewD1 <- 0
  c.NewD2 <- 0
  c.NewD3 <- 0
  c.NewD4 <- 0
  c.NewD5 <- 0
  c.CumVacc <- 0
  c.Deaths <- 0
  c.popsize <- 0
  c.pInfected <- 0
  c.nPrep <- 0
  c.nVacc <- (cVaccine + cPreventionVisit + cPreventionLabs)*nDoses 
  c.nCare <- 0
  c.FI <- 0
  c.nyr <- 0 # keep variable for discounting later
  c.pPrEP <- 0
  c.pVacc <- 0
  c.totalLY <- 0
  c.totalCostHIVcare <- 0
  c.totalCost <- 0
  
  
  for (i in 1: nsims){
    C <- c( c.SL[i], c.SH[i], c.SPL[i], c.SPH[i],  c.SVL[i],  c.SVH[i], c.SPVL[i], c.SPVH[i],
            c.I1L[i], c.I1H[i], c.I2L[i], c.I2H[i], c.I3L[i], c.I3H[i], c.I4L[i], c.I4H[i], c.I5L[i], c.I5H[i], 
            c.D1L[i], c.D1H[i], c.D2L[i], c.D2H[i], c.D3L[i], c.D3H[i], c.D4L[i], c.D4H[i], c.D5L[i], c.D5H[i], 
            c.E1L[i], c.E1H[i], c.E2L[i], c.E2H[i], c.E3L[i], c.E3H[i], c.E4L[i], c.E4H[i], c.E5L[i], c.E5H[i], 
            c.U1L[i], c.U1H[i], c.U2L[i], c.U2H[i], c.U3L[i], c.U3H[i], c.U4L[i], c.U4H[i], c.U5L[i], c.U5H[i], 
            c.T1L[i], c.T1H[i], c.T2L[i], c.T2H[i], c.T3L[i], c.T3H[i], c.T4L[i], c.T4H[i], c.T5L[i], c.T5H[i], 
            c.NewI[i], c.NewD[i], c.NewD1[i], c.NewD2[i], c.NewD3[i], c.NewD4[i], c.NewD5[i], 
            c.CumVacc[i], c.Deaths[i], c.popsize[i], c.pInfected[i],  c.nPrep[i], c.nVacc[i], 
            c.nCare[i], c.FI[i], c.nyr[i], c.pPrEP[i], c.pVacc[i], c.totalLY[i], c.totalCostHIVcare[i], 
            c.totalCost[i])
  }
  return(as.matrix(C))
}




