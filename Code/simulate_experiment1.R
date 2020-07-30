## Question: How many mice do we need to have at least two mice which 
## have developed cancer when optimal installation-conditions have been applied?

source("./Code/global.R", encoding = "UTF-8")

##### hyperparameters #####
n.max <- 10         #maximal number of animials to test
n.bootstrap <- 1000 #repetitions of simulation
Stamm <- 3          #selected mouse strain


##### simulation ####

## randomly samples n.bootstrap times 1 to n.max animals from the dataset 
## of pre-experiments and counts how many anmimals have developed more 
## than 1 tumor
res <- matrix(NA, nrow = n.max, ncol = n.bootstrap)

## loop over animal number
for(i in 1:n.max){
  message(paste0("kalkuliere Tieranzahl = ", i))
  ## repetitions
  for(jj in 1:n.bootstrap){
    res[i, jj] <- sampleMice(i, .Stamm = Stamm) %>% 
      .[, Tumoranzahl := Tumoranzahl] %>% 
      ## counting animals with more than 1 tumor
      .[Tumoranzahl >= 2, .N]
  }
}

##### power calculation for different animal numbers ####

## calculate percentage of simulation runs fullfilling the requirement
erg <- (res >= 2) %>% 
  rowSums(.)/n.bootstrap  * 100 

erg <- data.table(Tierzahl = 1:n.max, Power = erg)


##### visualize result #####
ggplot(erg, aes(Tierzahl, Power/100)) +
  geom_coltext() +
  ylab("Power") +
  xlab("Tierzahl je Gruppe") +
  scale_x_continuous(labels = 1:10, breaks = 1:10)+
  geom_hline(aes(yintercept = 0.8, color = "organge"))

ggsave(
  paste0("./Results/Power-Experiment1-Stamm",Stamm,".png"), 
  width = 15, height= 10, units = "cm"
)


