library(maptools)
library(spdep)
library(cartography)
library(leaflet)
library(dplyr)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(tidyverse)
library(readr)
library(data.table)
library(stringr)
library(lubridate)
library(RColorBrewer)
library(sqldf)
library(rgdal)
library(maptools)

if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()

shpMun <- readOGR("IBGE//.", "BRMUE250GC_SIR", stringsAsFactors=FALSE, encoding="UTF-8")
mapaMun <- fortify(shpMun, region = 'CD_GEOCMU')

shpUFs <- readOGR("IBGE//.", "BRUFE250GC_SIR", stringsAsFactors=FALSE, encoding="UTF-8")

base = sqldf("select codmunres, count(1) qtd_mortes from base where ano_obito = 2019 and  causabas_capitulo like '%IV.  Doenças endócrinas nutricionais e metabólicas%' group by codmunres")

ibge = sqldf("select ano, ibge, ibge2, populacao, nome, uf from ibge where ano = '2019'")

df = sqldf("select ano, ibge, ibge2, codmunres, nome, ROUND(populacao) populacao, qtd_mortes, uf from ibge inner join base on ibge.ibge2 = base.codmunres")

df = sqldf("select ano, ibge, ibge2, codmunres, nome, uf, populacao, qtd_mortes, round(((qtd_mortes/populacao)*100000)) taxa from df")

ggplot(df) +
  geom_map(
    map = mapaMun,
    color = NA, 
    size= .5,
    aes(map_id = IBGE, fill = taxa)) +
  expand_limits(
    x = mapaMun$long,
    y = mapaMun$lat
  ) +
  scale_fill_gradient(
    low = 'lightyellow',
    high = 'darkred'
  ) +
  geom_path(
    data = shpUFs,
    color = 'black',
    size = 0.001,
    aes(long, lat, group = group)
  ) +
  coord_map() +
  theme_void()  + 
  ggtitle("") +
  theme(legend.position = c(0.1,0.3)) + 
  labs(fill = "Rate")

ggsave(path = "./IMAGES",
  filename = "fg31.png",
       device = "png",
       dpi = 320,
       width = 9,
       height = 5,
       limitsize = FALSE)
