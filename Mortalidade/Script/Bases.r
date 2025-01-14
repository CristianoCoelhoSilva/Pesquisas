rm(list = ls())
library(data.table)
library(bit64)
library(tidyverse)
library(stargazer)
library(lfe)
library(lubridate)
library(gridExtra)
library(grid)

FAST_DEATH_LOADING = TRUE

if(FAST_DEATH_LOADING == FALSE) {
  
  base2002 = fread("./DADOS/SIM2002.csv")
  base2003 = fread("./DADOS/SIM2003.csv")
  base2004 = fread("./DADOS/SIM2004.csv")
  base2005 = fread("./DADOS/SIM2005.csv")
  base2006 = fread("./DADOS/SIM2006.csv")
  base2007 = fread("./DADOS/SIM2007.csv")
  base2008 = fread("./DADOS/SIM2008.csv")
  base2009 = fread("./DADOS/SIM2009.csv")
  base2010 = fread("./DADOS/SIM2010.csv")
  base2011 = fread("./DADOS/SIM2011.csv")
  base2012 = fread("./DADOS/SIM2012.csv")
  base2013 = fread("./DADOS/SIM2013.csv")
  base2014 = fread("./DADOS/SIM2014.csv")
  base2015 = fread("./DADOS/SIM2015.csv") 
  base2016 = fread("./DADOS/SIM2016.csv")
  base2017 = fread("./DADOS/SIM2017.csv")
  base2018 = fread("./DADOS/SIM2018.csv")
  base2019 = fread("./DADOS/SIM2019.csv")
  
  base = rbindlist(
    l = list(base2002,base2003,base2004,base2005
             ,base2006,base2007,base2008,base2009
             ,base2010,base2011,base2012,base2013
             ,base2014,base2015,base2016,base2017
             ,base2018,base2019),
    use.names = T
  )
  
  rm(base2002,base2003,base2004,base2005
     ,base2006,base2007,base2008,base2009
     ,base2010,base2011,base2012,base2013
     ,base2014,base2015,base2016,base2017
     ,base2018,base2019)
  
  fwrite(base, "./DADOS/SIM2002_2019.csv")
} else {
  base = fread("./DADOS/SIM2002_2019.csv", encoding = "UTF-8")
}

ibge = 
  fread("./DADOS/ibge_2002_2019_comPopulacao.csv") %>% 
  filter(POPULACAO > 0 & PIB > 0) %>%
  mutate(PIB_PER_CAPITA = PIB / POPULACAO) %>% 
  mutate(ibge2 = floor(IBGE / 10)) %>%
  group_by(IBGE) %>%
  arrange(ANO) %>% 
  mutate(lag_PIB = dplyr::lag(PIB, n = 1),
         lag_PIB_PER_CAPITA = dplyr::lag(PIB_PER_CAPITA, n = 1)) %>% 
  ungroup() %>% 
  mutate(growth_pib = log(PIB) - log(lag_PIB),
         growth_pib_per_capita = log(PIB_PER_CAPITA) - log(lag_PIB_PER_CAPITA)) 

