

# Blythe Adamson
# seattleHIV Model
# Doctoral Dissertation
# University of Washington
# February 2018


#-------------------------------------------------------------------------------
# FIGURES FOR MANUSCRIPT
#-------------------------------------------------------------------------------

Fig_competition <- ggplot(hiv_ref[hiv_ref$Time>=2018,], aes(Time, NewI), alpha = .7) + 
  geom_smooth(linetype = 'dashed', colour = 'red', show.legend = TRUE) + 
  geom_smooth(data = hiv_pv[hiv_pv$Time>=2018,], aes(Time, NewI), colour = 'red', alpha = .7, show.legend = TRUE) +
  geom_smooth(data = hiv_pv[hiv_pv$Time>=2018,], aes(Time, SP), colour = 'blue', se = FALSE, alpha = .7) +
  geom_smooth(data = hiv_pv[hiv_pv$Time>=2018,], aes(Time, SV), colour = 'green', se = FALSE, alpha = .7) +
  geom_smooth(data = hiv_pv[hiv_pv$Time>=2018,], aes(Time, SPV), colour = 'purple', se = FALSE, alpha = .7) +
  geom_smooth(aes(Time, nPrep), colour = 'blue', linetype = 'dashed', se = FALSE, alpha = .7) +
  labs(y = 'Utilization', x = 'Year', colour = "Colour\nlegend") +
  theme_classic()
Fig_competition


Fig_pCoverage <- ggplot(hiv_pv[hiv_pv$Time>=2018,], aes(Time, pPrEP)) + 
  geom_smooth(color = "navy", se = FALSE) +
  geom_smooth(data = hiv_pv[hiv_pv$Time>=2025,], aes(Time, pVacc), colour = 'red', alpha = .7, se = FALSE) +
  geom_smooth(data = hiv_ref[hiv_ref$Time>=2018,], aes(Time, pPrEP), linetype = 'dashed', colour = 'navy', show.legend = TRUE, se = FALSE) +
  labs(y = 'Fraction of Susceptible People Covered', x = 'Year') +
  ylim(0,1) +
  scale_colour_manual(values = c(pVacc = 'red', pPrEP = 'navy')) +
  theme(text = element_text(size=12)) +
  theme_classic()
Fig_pCoverage

# same as above, but trying to get labels for the colors and make the text size bigger
Fig_pCoverage <- ggplot(hiv_pv[hiv_pv$Time>=2018,], aes(Time, pPrEP)) + 
  geom_smooth(aes(Time, pPrEP, colour = "PrEP"), se = FALSE) +
  geom_smooth(data = hiv_pv[hiv_pv$Time>=2025,], aes(Time, pVacc), colour = 'red', alpha = .7, se = FALSE) +
  geom_smooth(data = hiv_ref[hiv_ref$Time>=2018,], aes(Time, pPrEP), linetype = 'dashed', colour = 'navy', show.legend = TRUE, se = FALSE) +
  labs(y = 'Fraction of Susceptible People Covered', x = 'Year') +
  ylim(0,1) +
  scale_colour_manual(values = c(pVacc = 'red', pPrEP = 'navy'), labels = c("Vaccinated", "Using PrEP")) +
  theme(text = element_text(size=12)) +
  theme_classic()
Fig_pCoverage

#' I want to see a graph showing the percent of all MSM covered by something
#' either PrEP or vaccine and how that changes over time. 


# make a figure of the HIV epidemic in Seattle in the absence of HIV vaccines
ggplot(hiv_ref, aes(Time, sum_Infected), alpha = .7) + 
  geom_smooth(data = hiv_ref[hiv_ref$Time<2015,], aes(color = 'ref')) + 
  geom_smooth(data = hiv_ref[hiv_ref$Time>=2015,], linetype = 'dashed', aes(color = 'ref')) + 
  geom_smooth(data = hiv_pv[hiv_pv$Time>=2015,], aes(Time, sum_Infected, color = "pv"), linetype = 'dashed') + 
  labs(y = 'People living with HIV', x = 'Year') +
  scale_colour_manual(values = c("pv" = 'red', "ref" = 'black'), labels = c("Vaccine", "No Vaccine")) +
  ylim(0,7000) + 
  theme_classic()

