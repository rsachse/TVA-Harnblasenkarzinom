##### packages #####
library(openxlsx)
library(data.table)
library(magrittr)
library(ggplot2)
library(stringr)
library(forcats)
library(digitize)
library(scales)
library(survival)
library(survminer)
library(lubridate)


##### define extra functions #####
source("./Code/geom_coltext.R")

`%notin%` <- Negate(`%in%`)

## samples .n animals from a given strain instillation duration of a given data-set
## sampling allows replacing
sampleMice <- function(.n, .Stamm = 3, .tInstill = 120, .Data = Data){
  res <- Data[Mausstamm == .Stamm & Instilldauer ==  .tInstill] %>% 
    .[sample(1:NROW(.), .n, replace = TRUE),]
  res
  
}


##### read in data from pre-experiments #####
Data <- read.xlsx("./Data/Datensatz_20200415_Rheader.xlsx") %>% 
  setDT %>% 
  .[, Tumaranzahl := Tumaranzahl %>% as.numeric] %>% 
  setnames("Tumaranzahl", "Tumoranzahl") %>% 
  .[, Wachstumsdauer := Abbruch] %>% 
  .[, Nachweisdauer := BLI] %>%
  .[Nachweisdauer < 0, Nachweisdauer := 0] %>% 
  .[, Dauer := Abbruch - BLI] 






