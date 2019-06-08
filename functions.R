
# ---------
# FUNCTIONS
# ---------
#          PopSize : Adds the mean population size to each row of 
#                    dynamic transmission model output
#
#   GetPersonTime : converts dynamic output into quarterly person time
#'  
#' UtilityWeights : adds column to health state matrix with corresponding utilty weight
#' 
#'   SumPrepUsers : sums the total number of people on prep at each time
#'   
#' GetResultsTable: Table 2 for chapter with main results summary
#' 
#'     CostingCal : 
#
#
# ------
# INPUTS
# ------
#          hiv_trt : data frame from .csv of dyanmic tranmission model simulations
#              trt : the control or intervention label


PopSize <- function(hiv_trt, trt){
  hiv_trt[,13] <- as.numeric(hiv_trt[,13])
  hiv_trt[,14] <- as.numeric(hiv_trt[,14])
  hiv_trt[,15] <- as.numeric(hiv_trt[,15])
  hiv_trt[,16] <- as.numeric(hiv_trt[,16])
  hiv_trt$popsize <- rowSums(hiv_trt[,3:60]) # sum the popsize at each time point
  hiv_trt$pInfected <- rowSums(hiv_trt[,11:60])/rowSums(hiv_trt[,3:60]) # probability of being HIV infected
  hiv_trt$nPrep <- rowSums(hiv_trt[,c(4,6)]) # number of people using PrEP during quarter
  #hiv_trt$nVaccine <- hiv_trt$CumVacc - hiv_trt$CumVacc
  hiv_trt$nVacc <- NA
  hiv_trt$nVacc[1] <- 0
 
   for(i in 2:nrow(hiv_trt)){
    hiv_trt$nVacc[i] <- hiv_trt$CumVacc[i] - hiv_trt$CumVacc[c(i-1)]
   }
  
  hiv_trt$nCare <- 0 # replace this with the number engaged in care that would be eligible to participate in an FI
  hiv_trt$nFI <- 0
  hiv_trt$nyr <- trunc(hiv_trt$Time, digits = 0)
  hiv_trt$nyr <- hiv_trt$nyr - 2025
  hiv_trt$nyr[hiv_trt$Time < 2025] <- 0
  return(hiv_trt)
}

GetPersonTime <- function(hiv_trt){
  ly <- hiv_trt
  ly[3:56] <- ly[3:56]/4
  ly$totalLY <- rowSums(ly[,3:56])
  return(ly)
}


UtilityWeights <- function(state){
  st <- state
  st$utility <- NA
  st$utility[st$hiv == 'susceptible'] <- 1
  st$utility[st$cd4 == 'Acute'] <- uAcute
  st$utility[st$cd4 == '>500'] <- u500
  st$utility[st$cd4 == '350-500'] <- u350
  st$utility[st$cd4 == '200-349'] <- u200
  st$utility[st$cd4 == '<200'] <- uAIDS
  st$utility[st$aware == "T"] <- u500
  return(st)
}


# QALYs: calculate discounted QALYs
GetQALYs <- function(hiv_trt_ly){
  hiv_trt_qaly <- hiv_trt_ly
  hiv_trt_qaly[3:60] <- NA
  for(i in 1:nrow(hiv_trt_ly)){
    hiv_trt_qaly[i,3:60] <- hiv_trt_ly[i,3:60] * states$utility * (1/((1+discount)^hiv_trt_ly$nyr[i]))
  }
  hiv_trt_qaly[hiv_trt_qaly$Time < 2025, 3:60] <- 0
  hiv_trt_qaly$totalQALY <- rowSums(hiv_trt_qaly[,3:60])
  return(hiv_trt_qaly)
}


CostingCalc <- function(hiv_trt_ly, C){
  hiv_trt_cost <- hiv_trt_ly
  hiv_trt_cost[3:length(C)] <- NA
  for(i in 1:nrow(hiv_trt_ly)){
    hiv_trt_cost[i,3:c(length(hiv_trt_cost))] <- hiv_trt_ly[i,3:c(length(hiv_trt_ly))] * C * (1/((1+discount)^hiv_trt_ly$nyr[i]))
  }
  hiv_trt_cost[hiv_trt_cost$Time < 2025, 3:c(length(hiv_trt_cost))] <- 0
  hiv_trt_cost$totalCost <- rowSums(hiv_trt_cost[3:length(hiv_trt_cost)])
  hiv_trt_cost$totalCostHIVcare <- rowSums(hiv_trt_cost[11:60])
  
  return(hiv_trt_cost)
}

# Collapse life years into annual and by care cascade categories
# add the pVS and other CDC metrics
CareCascadeSummary <- function(hiv_trt){
  hiv_trt$sum_Susceptible <- rowSums(hiv_trt[,3:10])
  hiv_trt$sum_Infected <- rowSums(hiv_trt[,11:60])
  #hiv_trt <- hiv_trt %>% 
  #  mutate(VS = rowSums(T1L:T5H, na.rm = TRUE))
  
  return(hiv_trt)
}


GetResultsTable <- function (hiv_ref, hiv_ref_ly, hiv_ref_qaly, hiv_ref_cost, 
                             hiv_pv, hiv_pv_ly, hiv_pv_qaly, hiv_pv_cost){
  Outcome <- c(
    "HIV BURDEN    ----------------",
    "New Infections 2025-2045",
    "Diagnoses 2025-2045", 
    "PLWH 2045", 
    "HIV Prevalence (%) 2045", 
    
    "UTILIZATION    ---------------",
    "PrEP Users in 2025", 
    "Total Protected in 2045",
    "---PrEP alone", 
    "---HIV vaccine alone",
    "---PrEP and HIV vaccine",
    
    "OUTCOMES    ------------------",
    "Life Years, total", 
    "Life Years, per capita",
    "QALYs, total", 
    "QALYs, per capita", 
    
    "COSTS    ---------------------",
    "Total Cost, $", 
    "---PrEP cost, $", 
    "_PrEP cost per susceptible, $",
    "---Vaccine cost, $", 
    "_Vaccine cost per susceptible, $",
    "---HIV Care Cost, $",
    "ICER, $/QALY")
  
  pop <- hiv_ref$popsize[hiv_ref$Time == 2025]
  
  Spop <- sum(hiv_ref %>% 
                filter(Time == 2025) %>% 
                  select(SL, SH, SPL, SPH, SVL, SVH, SPVL, SPVH))
  
  Reference   <- c("",
                   #HIV BURDEN - reference
                   comma(max(hiv_ref$NewI) - hiv_ref$NewI[hiv_ref$Time == 2025], 0),      # Infections
                   comma(max(hiv_ref$NewD) - hiv_ref$NewD[hiv_ref$Time == 2025]),      # Diagnoses
                   comma(sum(hiv_ref$sum_Infected[hiv_ref$Time == 2045]), 0), # PLWH
                   scales::percent(hiv_ref$pInfected[hiv_ref$Time == 2045]), # HIV Prevalence 2045
                   
                   "",#UTILIZATION - reference
                   comma(sum(hiv_ref %>% 
                               filter(Time == 2025) %>% 
                                  select(SPL, SPH)), 0),  # PrEP Users in 2025
                   comma(sum(hiv_ref %>% 
                               filter(Time == 2045) %>% 
                                  select(SPL, SPH)), 0),  # Any PrEP Users in 2045
                   comma(sum(hiv_ref %>% 
                               filter(Time == 2045) %>% 
                                  select(SPL, SPH)), 0),  # PrEP-only Users in 2045
                   "0", # no HIV vaccine users in ref group
                   "0", # no HIV vaccine users in ref group
                   
                   "",#HEALTH OUTCOMES - reference
                   comma(sum(hiv_ref_ly$totalLY[hiv_ref_ly$Time >= 2025]), 0),  # Life Years, total
                   comma(sum(hiv_ref_ly$totalLY[hiv_ref_ly$Time >= 2025])/pop, 3),
                   comma(sum(hiv_ref_qaly$totalQALY[hiv_ref_ly$Time >= 2025])), # QALYs, total
                   comma(sum(hiv_ref_qaly$totalQALY[hiv_ref_ly$Time >= 2025])/pop, 3), # QALYs per person
        
                   "",#COSTS - reference
                   scales::dollar(sum(hiv_ref_cost$totalCost[hiv_ref_cost$Time >=2025])), # Total Cost
                   scales::dollar(sum(hiv_ref_cost %>% 
                                filter(Time >=2025) %>% 
                                  select(SPL, SPH, SPVL, SPVH))), # PrEP Cost
                   scales::dollar(sum(hiv_ref_cost %>% 
                                filter(Time >=2025) %>% 
                                select(SPL, SPH, SPVL, SPVH))/Spop), # PrEP Cost per capita susceptible
                   scales::dollar(sum(hiv_ref_cost$nVacc[hiv_ref_cost$Time >=2025])), # Vaccine costs
                   scales::dollar(sum(hiv_ref_cost$nVacc[hiv_ref_cost$Time >=2025])/Spop), # Vaccine costs per capita susceptible
                   scales::dollar(sum(hiv_ref_cost$totalCostHIVcare[hiv_ref_cost$Time >=2025])), # HIV care costs
                   
                   " " # ICER row blank 
                   ) 
  
  Vaccine    <- c("",
                  #HIV BURDEN -vaccine
                  scales::comma(max(hiv_pv$NewI) - hiv_pv$NewI[hiv_pv$Time == 2025], 0), #infections
                  scales::comma(max(hiv_pv$NewD)- hiv_pv$NewD[hiv_pv$Time == 2025], 0), #diagnoses
                  scales::comma(sum(hiv_pv$sum_Infected[hiv_pv$Time == 2045]), 0), #PLWH
                  scales::percent(hiv_pv$pInfected[hiv_pv$Time == 2045]), # HIV Prevalence (%) 2045
                  
                  "",#UTILIZATION - vaccine
                  comma(sum(hiv_pv %>% 
                              filter(Time == 2025) %>% 
                                select(SPL, SPH)), 0),  # PrEP Users in 2025
                  comma(sum(hiv_pv %>% 
                              filter(Time == 2045) %>% 
                                select(SPL:SPVH)), 0),  # All protected in 2045
                  comma(sum(hiv_pv %>% 
                              filter(Time == 2045) %>% 
                                select(SPL, SPH)), 0),  # PrEP Users in 2045
                  comma(sum(hiv_pv %>% 
                              filter(Time == 2045) %>% 
                                select(SVL, SVH)), 0),  # HIV vaccine alone protected in 2045
                  comma(sum(hiv_pv %>% 
                              filter(Time == 2045) %>% 
                                select(SPVL:SPVH)), 0),  # PrEP + vaccine protection in 2045
                  
                  "",#OUTCOMES - vaccine
                  comma(sum(hiv_pv_ly$totalLY[hiv_pv_ly$Time >= 2025]), 0), # LY total
                  comma(sum(hiv_pv_ly$totalLY[hiv_pv_ly$Time >= 2025])/pop, 3), #LY per capita
                  comma(sum(hiv_pv_qaly$totalQALY[hiv_pv_ly$Time >= 2025]), 0), # total QALY
                  comma(sum(hiv_pv_qaly$totalQALY[hiv_pv_ly$Time >= 2025])/pop, 3), #QALY per capita
                  
                  "",#COST - vaccine
                  scales::dollar(sum(hiv_pv_cost$totalCost[hiv_pv_cost$Time >= 2025])), # total cost
                  scales::dollar(sum(hiv_pv_cost %>% 
                              filter(Time >=2025) %>% 
                                select(SPL, SPH, SPVL, SPVH))), # PrEP Cost
                  scales::dollar(sum(hiv_pv_cost %>% 
                              filter(Time >=2025) %>% 
                                select(SPL, SPH, SPVL, SPVH))/Spop), # PrEP Cost per capita susceptible
                  scales::dollar(sum(hiv_pv_cost$nVacc[hiv_pv_cost$Time >= 2025])), # hiv vaccine cost
                  scales::dollar(sum(hiv_pv_cost$nVacc[hiv_pv_cost$Time >= 2025])/Spop),# hiv vaccine cost per capita
                  scales::dollar(sum(hiv_pv_cost$totalCostHIVcare[hiv_pv_cost$Time >= 2025])),# hiv care cost total
                  
                  " " # ICER row
                  )
  
  Incremental <- c("",
                   #HIV BURDEN - inc
                   comma(max(hiv_pv$NewI) - max(hiv_ref$NewI), 0), # reduction in new HIV infections
                   comma(max(hiv_pv$NewD) - max(hiv_ref$NewD), 0), # reduction in new HIV diagnoses
                   comma(sum(hiv_pv$sum_Infected[hiv_pv$Time == 2045]) 
                         - sum(hiv_ref$sum_Infected[hiv_ref$Time == 2045]), 0),# PLWH
                   scales::percent(hiv_pv$pInfected[hiv_pv$Time == 2045] - 
                                     hiv_ref$pInfected[hiv_ref$Time == 2045]), # HIV Prevalence 2045
                   
                   "",#UTILIZATION - inc
                   "0",
                   comma(sum(hiv_pv %>% 
                               filter(Time == 2045) %>% 
                               select(SPL:SPVH)) - sum(hiv_ref %>% 
                                                         filter(Time == 2045) %>% 
                                                         select(SPL:SPVH)), 0),  # Incremental protected in 2045
                   comma(sum(hiv_pv %>% 
                               filter(Time == 2045) %>% 
                               select(SPL, SPH)) - sum(hiv_ref %>% 
                                                         filter(Time == 2045) %>% 
                                                         select(SPL, SPH)), 0),  # Incremental PrEP Users in 2045
                   comma(sum(hiv_pv %>% 
                               filter(Time == 2045) %>% 
                               select(SVL, SVH)) - 
                           sum(hiv_ref %>% 
                                 filter(Time == 2045) %>% 
                                 select(SVL, SVH)), 0),  # Incremental HIV vaccine alone protected in 2045
                   comma(sum(hiv_pv %>% 
                               filter(Time == 2045) %>% 
                               select(SPVL:SPVH)) - sum(hiv_ref %>% 
                                                          filter(Time == 2045) %>% 
                                                          select(SPVL:SPVH)), 0),  # Incremental PrEP + vaccine protection in 2045
                   
                   "",#OUTCOMES - inc
                   comma(sum(hiv_pv_ly$totalLY[hiv_pv$Time>=2025]) 
                         - sum(hiv_ref_ly$totalLY[hiv_pv$Time>=2025]), 0), 
                   comma((sum(hiv_pv_ly$totalLY[hiv_pv$Time>=2025]) 
                          - sum(hiv_ref_ly$totalLY[hiv_pv$Time>=2025]))/pop, 2), 
                   sum(hiv_pv_qaly$totalQALY[hiv_pv$Time>=2025]) 
                         - sum(hiv_ref_qaly$totalQALY[hiv_ref$Time>=2025]), 
                   comma((sum(hiv_pv_qaly$totalQALY[hiv_pv$Time>=2025]) 
                          - sum(hiv_ref_qaly$totalQALY[hiv_ref$Time>=2025]))/pop, 2), 
                   
                   "",#COST - incremental
                   scales::dollar((sum(hiv_pv_cost$totalCost[hiv_pv_cost$Time >= 2025])) 
                         - (sum(hiv_ref_cost$totalCost[hiv_ref_cost$Time >= 2025]))), # total incremental cost 
                   scales::dollar(sum(hiv_pv_cost %>% 
                                        filter(Time >=2025) %>% 
                                          select(SPL, SPH, SPVL, SPVH)) - 
                                    sum(hiv_ref_cost %>% 
                                          filter(Time >=2025) %>% 
                                          select(SPL, SPH, SPVL, SPVH))),  # inc Prep cost
                   scales::dollar((sum(hiv_pv_cost %>% 
                                        filter(Time >=2025) %>% 
                                        select(SPL, SPH, SPVL, SPVH)) - 
                                    sum(hiv_ref_cost %>% 
                                          filter(Time >=2025) %>% 
                                          select(SPL, SPH, SPVL, SPVH)))/Spop),  # inc Prep per capita susceptible
                   scales::dollar(sum(hiv_pv_cost$nVacc) - sum(hiv_ref_cost$nVacc)), # inc vaccine cost
                   scales::dollar((sum(hiv_pv_cost$nVacc) - sum(hiv_ref_cost$nVacc))/Spop), # inc vaccine cost
                   scales::dollar((sum(hiv_pv_cost$totalCostHIVcare[hiv_pv_cost$Time >=2025]) 
                          - sum(hiv_ref_cost$totalCostHIVcare[hiv_pv_cost$Time >=2025]))), #inc HIV care costs
                   
                   scales::dollar((sum(hiv_pv_cost$totalCost[hiv_pv_cost$Time >= 2025]) # ICER row
                   - sum(hiv_ref_cost$totalCost[hiv_ref_cost$Time >= 2025]))
                   / (sum(hiv_pv_qaly$totalQALY[hiv_pv$Time>=2025]) 
                      - sum(hiv_ref_qaly$totalQALY[hiv_ref$Time>=2025]))) 
                   )
  
  Percent_Change <- c("",
              #HIV BURDEN - % diff
              scales::percent((max(hiv_pv$NewI) - max(hiv_ref$NewI))
                    /(max(hiv_ref$NewI)- hiv_ref$NewI[hiv_ref$Time == 2025])), #infections
              scales::percent((max(hiv_pv$NewD) - max(hiv_ref$NewD))
                    /(max(hiv_ref$NewD)- hiv_ref$NewD[hiv_ref$Time == 2025])), #diagnoses
              scales::percent((sum(hiv_pv$sum_Infected[hiv_pv$Time == 2045]) 
                    - sum(hiv_ref$sum_Infected[hiv_ref$Time == 2045])) 
              / sum(hiv_ref$sum_Infected[hiv_ref$Time == 2045])), #PLWH in 2045
              scales::percent((hiv_pv$pInfected[hiv_pv$Time == 2045] - hiv_ref$pInfected[hiv_ref$Time == 2045])/
                                 hiv_ref$pInfected[hiv_ref$Time == 2045]), # HIV Prevalence 2045
              
              "",#UTILIZATION - % diff
              "0", # change in total protected in 2025
              "-",  # change in total protected in 2045
              "-",  # change in PrEP alone at 2045
              "-",  # Vaccine alone in 2045
              "-",  # Vaccine alone in 2045
              
              "",#OUTCOMES - % diff
              scales::percent((sum(hiv_pv_ly$totalLY[hiv_pv$Time>=2025]) 
                     - sum(hiv_ref_ly$totalLY[hiv_pv$Time>=2025]))
                    /sum(hiv_ref_ly$totalLY[hiv_pv$Time>=2025])), 
              "-", # no need for per capita diff, same as above
              scales::percent((sum(hiv_pv_qaly$totalQALY[hiv_pv$Time>=2025]) 
                     - sum(hiv_ref_qaly$totalQALY[hiv_ref$Time>=2025]))
                    /sum(hiv_ref_qaly$totalQALY[hiv_ref$Time>=2025])), 
              "-", # no need, same as one line above
              
              "",#COSTS - % diff
              scales::percent((sum(hiv_pv_cost$totalCost[hiv_pv_cost$Time >= 2025]) 
                     - sum(hiv_ref_cost$totalCost[hiv_ref_cost$Time >= 2025]))
                    /sum(hiv_ref_cost$totalCost[hiv_ref_cost$Time >= 2025])), # Total Cost
              "-", #Prep Costs
              " ",# Prep costs per capita
              "-", # Vaccine costs
              "-", # hiv vaccine cost per capita
              scales::percent(((sum(hiv_pv_cost$totalCostHIVcare[hiv_pv_cost$Time >=2025]) 
                      - sum(hiv_ref_cost$totalCostHIVcare[hiv_pv_cost$Time >=2025])))
                    /sum(hiv_ref_cost$totalCostHIVcare[hiv_pv_cost$Time >=2025])), # hiv care cost total
              
              "" # ICER
              ) 
  
  table_results <- as.data.frame(cbind(Outcome, Reference, Vaccine, Incremental, Percent_Change))
  
  return(table_results)
  
}


