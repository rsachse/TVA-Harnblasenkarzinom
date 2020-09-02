## Question: How many animals do we need to be able to calculate a significant 
## and reliable correlation between BLI-signal and Cancer-Biomass?

source("./Code/global.R")

##### hyperparameters #####
animals.max <- 10         # maximal animals
mu <- 0.5                 # mean cancer growth rate retrieved from literature
mu.min <- 0.2 * 0.5       # minimal cancer growth rate retrieved from literature
mu.max <- 0.8 * 0.5       # maximal cancer growth rate retrieved from literature
times <- c(7,14,21,28,35) # experiment duration times
n.bootstrap <- 1000       # simulation repetitions
bli.min <- 0.1            # minimal cancer biomass at which a BLI signal can be measure (conservativeley estmated form literature)
tumor.max <- 6            # maximal tumor count at which experiment will be terminated anyway
bli.measure.error <- 0.2  # relative error of BLI-measurement (+/- 20% measure error assumed)
Stamm <- 3                # mouse strain for experiment



##### calculate starting cancer size at t0 #####
## to simulate cancer growth, we need to know how much cancer was manifested at 
## t0 after instillation. This will be the starting tumor-biomass. We back-calculate 
## this starting-biomass by using the mean cancer growth rate published for this 
## cancer. This we retrieve a randomized individual starting biomass like it 
## would happen also when the experiment will be performed in reality.

## calculate starting tumor biomass
Data[, X0.tumorzahl := Tumoranzahl/exp(mu * Wachstumsdauer)]


##### calculate minimum tumor amount needed for BLI signal and conservatively round #####
## up the number to be on the save side for animal number calculation
Data[, min.tumor.nachweis := X0.tumorzahl * exp(mu * Nachweisdauer)]
mean(Data[, min.tumor.nachweis], na.rm = TRUE) %>% round(1)
## median detection limit 0.007 tumors
## mean detection limit 0.07 tumors
## rounded result for bli.min = 0.1 tumors



##### simulation and power calculation #####

res <- data.table(Tierzahl = 1:animals.max)

## simulation loop for different animal numbers
for(i in 1:animals.max){

  message(paste0("kalkuliere Tierzahl ", i))
  
  ## sample as many mice as needed for simulation repetitions
  tdat <- sampleMice(length(times) * i * n.bootstrap, .Stamm = Stamm) %>% 
    ## slow down cancer growth due to the fact that we use immune competent mice
    ## in pre experiments this ratio was found in direct comparison to 
    ## immune incompetent mice, which we have in our sample data set
    .[,Wachstumsdauer := Wachstumsdauer * 1.5] %>% 
    ## fill variable with experiment duration
    .[,Zeit.sim := rep(times, each = i * n.bootstrap)] %>%
    ## fill variable with id of simulation run
    .[,sim.run := rep(1:n.bootstrap, length(times)*i)] %>% 
    ## sample a random tumor growth within the reported tumor growth rate range
    .[, tumor.growth := runif(.N, mu.min, mu.max)] %>% 
    ## calculate initial tumor biomass
    .[, X0.tumorzahl := Tumoranzahl/exp(tumor.growth * Wachstumsdauer)] %>% 
    ## simulate exponential tumor growth
    .[,Tumor.sim := X0.tumorzahl * exp(tumor.growth * Zeit.sim)] %>% 
    ## simulate BLI-signal according to bli.min and tumor biomass and add 
    ## random measurement error on the signal
    .[,BLI.sim := NA %>% as.numeric] %>% 
    ## when tumer biomass is lower than BLI.min than no signal can be measured
    .[Tumor.sim < bli.min, BLI.sim := 0] %>% 
    ## the BLI-signal is assumed to be linearly related to the cancer biomass
    .[Tumor.sim >= bli.min & Tumor.sim <= tumor.max, BLI.sim := 1.1*Tumor.sim - bli.min + rnorm(.N, 0, bli.measure.error)] %>% 
    ## abort experiment if too many tumors have grown
    .[Tumor.sim <= tumor.max] %>% 
    ## if no tumor has grown, than remove animal from data set
    .[!is.na(Tumor.sim) & !is.na(BLI.sim)] %>% 
    ## count remaining animals
    .[, NperGroup := .N, sim.run] %>% 
    ## for a correlation we need at least two animals
    .[NperGroup > 2] %>%
    ## only animals with tumor can have a BLI-signal
    .[Tumor.sim > 0] %>% 
    .[BLI.sim > 0] %>% 
    ## test if animals contributing to BLI signal have a significant correlation 
    ## between simulated tumor biomass and simulated BLI signal with 
    ## a correlation-coefficient > 0.7
    .[, .(corr = sqrt(summary(lm(Tumor.sim ~ BLI.sim))$r.squared) %>%
            as.data.table() %>% 
            .[NROW(.), 1] %>% 
            unlist(), 
          #p-value for whole model is the same es for regressor when only one
          #regressor is fitted, otherwise use pf() to retrieve p-value from
          #fstatistics
          #pf(ff[1], ff[2], ff[3], lower.tail=FALSE)
          cor.p = summary(lm(Tumor.sim ~ BLI.sim))$coefficients %>% 
            as.data.table() %>% 
            .[NROW(.),4] %>% 
            unlist(),
          cor.n = summary(lm(Tumor.sim ~ BLI.sim))$coefficients %>% 
            as.data.table() %>% 
            .[, .N] %>% 
            unlist(),
          N = .N), sim.run] %>% 
    .[, cor.sig := FALSE] %>% 
    .[cor.n ==2 & cor.p <= 0.05 & corr > 0.95, cor.sig := TRUE]

  ## calculate percentage of experiments meeting the requirements = Power
  res[i, Power := tdat[, sum(cor.sig)/n.bootstrap * 100]]

}


##### visualize result #####
ggplot(res[Tierzahl %in% seq(2,10,2)], aes(Tierzahl, Power/100)) +
  geom_coltext()+
  #ggtitle(paste0("Experiment 2, Stamm ", Stamm)) +
  #ylim(0,100) +
  ylab("Power") +
  xlab("Tierzahl je Zeitstufe")+
  scale_x_continuous(labels = seq(2,10,2), breaks = seq(2,10,2))+
  geom_hline(aes(yintercept = 0.8, color = "organge"))

ggsave(paste0("./Results/Power-Experiment2-Stamm",Stamm,".png"), width = 15, height= 10, units = "cm")

