## Question: How many animals do we need in a survival experiment to be able
## to determine t_S50 within a range of +/- 5% of the true value? 

source("./Code/global.R", encoding = "UTF-8")

##### pre-investigation of Kaplan Meier Estimates in pre-experiments with immune incompetent mice #####

## create event table strain 3
Survdat <- copy(Data) %>%
  .[Mausstamm == 3] %>% 
  #.[Instilldauer == 120] %>% 
  .[, time :=  Abbruch] %>% 
  .[is.na(time), time := BLI] %>% 
  .[is.na(time), time := 80] %>% 
  .[, Event := 0] %>% 
  .[!is.na(Abbruch), Event := 1] 

## calculate Kaplan Meier Estimate and plot survival curves
sfit <- survfit(Surv(time, Event) ~ Instilldauer, data = Survdat)
km <- Surv(Survdat$time, Survdat$Event)
sdifffit <- survdiff(km ~ Instilldauer, data = Survdat)

ggsurvplot(fit = sfit, conf.int = TRUE)+
  ggtitle("Stamm 3")

ggsave(
  paste0("./Results/Survival-Instilldauer.png"), 
  width = 15, height= 10, units = "cm"
) 


## create event table all strains
Survdat <- copy(Data) %>%
  #.[Mausstamm == 3] %>% 
  .[Instilldauer == 120] %>% 
  .[, time :=  Abbruch] %>% 
  .[is.na(time), time := BLI] %>% 
  .[is.na(time), time := 80] %>% 
  .[, Event := 0] %>% 
  .[!is.na(Abbruch), Event := 1] 


sfit <- survfit(Surv(time, Event) ~ Mausstamm, data = Survdat)
km <- Surv(Survdat$time, Survdat$Event)
sdifffit <- survdiff(km ~ Mausstamm, data = Survdat)


ggsurvplot(fit = sfit, conf.int = TRUE) +
  ggtitle("Instilldauer = 120 min")

ggsave(
  paste0("./Results/Survival-Mausstaemme.png"), 
  width = 15, height= 10, units = "cm"
) 


##### simulate survival experiments #####

## hyperparameters
animals <- seq(1,30) # animal numbers for simulatin experiments
n.bootstrap <- 1000  # repetitions of simulation experiments
Stamm <- 3           # mouse strain of interest

## empty data structures to save simulation results
res.sd <- res.median <- matrix(NA, nrow = length(animals), ncol = n.bootstrap)

## simulation loop for different animal numbers
for(i in seq_along(animals)) {
  message(paste0("kalkuliere Tierzahl ", animals[i]))
  
  ## simulation loop for experiment repetitions
  for(jj in 1:n.bootstrap) {
    ## calculate events on real data and assume slower cancer growth due
    ## to immune competent versus immune incompetent mice issue. 
    ## Than sample as many animals as needed
    Survdat <- Data %>%
      .[, time :=  Abbruch*1.5] %>% 
      .[is.na(time), time := BLI*1.5] %>% 
      .[is.na(time), time := 80] %>% 
      .[, Event := 0] %>% 
      .[!is.na(Abbruch), Event := 1] %>% 
      sampleMice(animals[i], .Data = .)
    
    ## calculate Kaplan Meier Estimator
    sfit <- survfit(Surv(time, Event) ~ 1, data = Survdat)
    
    ## Extract t_S50 from the survival model and check if it is within the 
    ## required precsion range
    res.median[i, jj] <- (summary(sfit)$table["median"] >= 35) & (summary(sfit)$table["median"] <= 39)
  }
}


res.median[is.na(res.median)] <- FALSE

##### calculate percentage of simulations meeting the precision requirements = Power #####
erg <- rowSums(res.median)/n.bootstrap  * 100 
erg <- data.table(Tierzahl = animals, Power = erg)


##### visualize result #####
ggplot(erg, aes(Tierzahl, round(Power/100,2))) +
  geom_coltext() +
  ylab("Power") +
  ylab("Power") +
  geom_hline(aes(yintercept = 0.8, color = "organge"))

ggsave(paste0("./Results/Power-Experiment3-Stamm",Stamm,"-Median.png"), width = 25, height= 10, units = "cm")

