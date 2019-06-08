
# --------
# FUNCTION
# --------
# StateAttributes
#
# Adds indicator variables to 
# ------
# INPUTS
# ------
#          state : vector of names of model health states
# -------
# RETURNS
# -------
# a data frame describing the model health states


DefineStates <- function(state){
  st <- data.frame(state)
  st$state <- as.character(st$state)
  
  # PrEP indicator
  st$prep <- NA
  st$prep[st$state == "S"] <- 0
  st$prep[st$state == "SV"] <- 0
  st$prep[st$state == "SP"] <- 1
  st$prep[st$state == "SPV"] <- 1
  
  # Vaccinated
  st$vax <- NA
  st$vax[st$state == "S"] <- 0
  st$vax[st$state == "SP"] <- 0
  st$vax[st$state == "SV"] <- 1
  st$vax[st$state == "SPV"] <- 1
  
  # Preventative medication
  st$med <- NA
  st$med[st$state == "SP"] <- "none"
  st$med[st$state == "SP"] <- "prep"
  st$med[st$state == "SV"] <- "vaccine"
  st$med[st$state == "SPV"] <- "both"
  
  # HIV infection status
  st$hiv[9:58] <- "infected"
  st$hiv[1:8] <- "susceptible"
  
  # phase of the HIV care cascade
  # I: Infected, D: Diagnosed, E: Enaged (not on ART), U: Unsuppressed and ON ART, T: Suppressed and on ART
  st$aware <- NA
  st$aware[1:8] <- "S"       # Susceptible
  st$aware[9:18] <- "I"      # Infected and unaware
  st$aware[19:28] <- "D"     # Diagnosed
  st$aware[29:38] <- "E"     # Enaged (not on ART)
  st$aware[39:48] <- "U"     # Unsuppressed and ON ART
  st$aware[49:58] <- "T"     # Suppressed and on ART
  aware_labels <- unique(st$aware)
  
  # HIV Care cascade labels
  st$care <- NA
  st$care[1:8] <- "Susceptible"
  st$care[9:18] <- "Infected and unaware"
  st$care[19:28] <- "Diagnosed"
  st$care[29:38] <- "Enaged (not on ART)"
  st$care[39:48] <- "Unsuppressed and ON ART"
  st$care[49:58] <- "Suppressed"
  care_labels <- unique(st$care)
  
  # CD4 Count of 1: ACUTE, 2: >500 CD4, 3: 350-500 CD4, 4: 200-350, 5: <200
  st$cd4_cat <- NA
  temp <- c(seq(9,58,10),seq(10,58,10))
  for(i in temp){
    st$cd4_cat[i] <- 1
    st$cd4[i] <- "Acute"
  }
  temp <- c(seq(11,58,10),seq(12,58,10))
  for(i in temp){
    st$cd4_cat[i] <- 2
    st$cd4[i] <- ">500"
  }
  temp <- c(seq(13,58,10),seq(14,58,10))
  for(i in temp){
    st$cd4_cat[i] <- 3
    st$cd4[i] <- "350-500"
  }
  temp <- c(seq(15,58,10),seq(16,58,10))
  for(i in temp){
    st$cd4_cat[i] <- 4
    st$cd4[i] <- "200-349"
  }
  temp <- c(seq(17,58,10),seq(18,58,10))
  for(i in temp){
    st$cd4_cat[i] <- 5
    st$cd4[i] <- "<200"
  }
  cd4_labels <- unique(st$cd4)
  
  # Risk level for low or high sexual activity
  st$risk <- NA
  temp <- c(seq(9,58,2)) # rows of low risk health states
  for(i in temp){
    st$risk[i] <- "low"
  }
  temp <- c(seq(10,58,2)) # rows of high risk health states
  for(i in temp){
    st$risk[i] <- "high"
  }
  
  # Treatment with ART
  st$art <- 0
  st$art[st$care == 'Susceptible'] <- NA
  st$art[st$aware=='U' | st$aware == 'T'] <- 1
  
  return(st)
}
