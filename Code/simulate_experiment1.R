## Question: How many mice do we need to have at least two mice which 
## have developed cancer when optimal installation-conditions have been applied?

source("./Code/global.R", encoding = "UTF-8")

##### hyperparameters #####
n.max <- 8         #maximal number of animials to test
n.bootstrap <- 1000 #repetitions of simulation
Stamm <- 3          #selected mouse strain



##### Variationsbreite ####
d <- data.table(d = rpois(n = 1000, lambda = 2))

Data[Mausstamm == Stamm & Instilldauer == 120, ] %>% 
  ggplot(aes(Tumoranzahl), fill = "blue") +
    #geom_histogram() +
    geom_density(fill = "darkblue") +
    geom_density(data = d, aes(d, colour = "Poisson"), bw = 1, size = 1.5) +
    ggtitle("Mausstamm 3, 120min Instillation, n = 20 Tiere") +
    xlim(0,5)

ggsave("./Results/Verteilung_Tumore.jpg", dpi = 300, units = "cm", width = 17, height = 13)


##### simulation ####

## randomly samples n.bootstrap times 1 to n.max animals from the dataset 
## of pre-experiments and counts how many anmimals have developed more 
## than 1 tumor
res <- matrix(NA, nrow = n.max, ncol = n.bootstrap)

## loop over animal number
for(i in seq(2,n.max,2)){
  message(paste0("kalkuliere Tieranzahl = ", i))
  ## repetitions
  for(jj in 1:n.bootstrap){
    ## sampling from dataset
    res[i, jj] <- 
      sampleMice(i, .Stamm = Stamm) %>%
      .[, Tumoranzahl := Tumoranzahl] %>%
      ## counting animals with more than 1 tumor
      .[Tumoranzahl >= 1, .N]
    ## drawing from poisson distribution
    # res[i, jj] <- 
    #   data.table(Tumoranzahl = rpois(i, lambda = 2)) %>% 
    #   .[Tumoranzahl >= 1, .N]
    res[i, jj] <- binom.test(res[i,jj], i, 0.40, alternative = "greater")$p.value
  }
}

##### power calculation for different animal numbers ####

## calculate percentage of simulation runs fullfilling the requirement
erg <- (res < 0.05) %>% 
  rowSums(.)/n.bootstrap  * 100 

erg <- data.table(Tierzahl = 1:n.max, Power = erg)


##### visualize result #####
ggplot(erg, aes(Tierzahl, Power/100)) +
  geom_coltext() +
  ylab("Power") +
  xlab("Tierzahl je Gruppe") +
  scale_x_continuous(labels = seq(2,n.max,2), breaks = seq(2,n.max,2))+
  geom_hline(aes(yintercept = 0.8, color = "organge"))

ggsave(
  paste0("./Results/Power-Experiment1-Stamm",Stamm,".png"), 
  width = 15, height= 10, units = "cm"
)


