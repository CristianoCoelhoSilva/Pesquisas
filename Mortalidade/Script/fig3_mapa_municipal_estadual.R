# Carregando os pacotes
rm(list=ls())
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

base = sqldf("select codmunres, count(1) qtd_mortes from base where ano_obito = 2019 group by codmunres")

ibge = sqldf("select ano, ibge, ibge2, populacao, nome, uf from ibge where ano = '2019'")

df = sqldf("select ano, ibge, ibge2, codmunres, nome, populacao, qtd_mortes, uf from ibge inner join base on ibge.ibge2 = base.codmunres")

df = sqldf("select ano, uf, ibge, ((qtd_mortes * 100) / populacao) taxa from df")

ggplot(df) +
  geom_map(
    map = mapaMun,
    color = NA, 
    size= .5,
    aes(map_id = IBGE, fill = psych::winsor(taxa / 100, trim = 0.05))) +
  expand_limits(
    x = mapaMun$long,
    y = mapaMun$lat
  ) +
  scale_fill_gradient(
    low = 'lightyellow',
    high = 'darkred',
    label = scales::percent_format(accuracy = .1)
    
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
  labs(fill = "Taxa")

ggsave(path = "./IMAGES",
       filename = "fig3_mapa_municipal.png",
       device = "png",
       dpi = 320,
       width = 9,
       height = 5,
       limitsize = FALSE)

#Mapa Estadual
df2 = sqldf("select ano, ibge, ibge2, codmunres, nome, populacao, qtd_mortes, uf from ibge inner join base on ibge.ibge2 = base.codmunres")

df2 = sqldf("select ano, uf, sum(qtd_mortes) qtd_mortes, SUM(populacao) populacao from df2 group by ano, uf")

df2 = sqldf("select ano, uf, ((qtd_mortes * 100) / populacao) taxa FROM df2")

df2 = sqldf("select ibge, df2.taxa FROM df2 inner join df on (df2.ano = df.ano and df2.uf = df.uf)")

ggplot(df2) +
  geom_map(
    map = mapaMun,
    color = NA, 
    size= .5,
    aes(map_id = IBGE, fill = psych::winsor(taxa / 100, trim = 0.05))) +
  expand_limits(
    x = mapaMun$long,
    y = mapaMun$lat
  ) +
  scale_fill_gradient(
    low = 'lightyellow',
    high = 'darkred',
    label = scales::percent_format(accuracy = .1)
    
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
  labs(fill = "Taxa")

ggsave(path = "./IMAGES",
       filename = "fig3_mapa_estadual.png",
       device = "png",
       dpi = 320,
       width = 9,
       height = 5,
       limitsize = FALSE)
