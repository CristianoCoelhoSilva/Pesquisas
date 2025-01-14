# Carregando os pacotes
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

base2002 = sqldf("select codmunres, count(1) qtd_mortes from base where ano_obito = 2002 group by codmunres")
base2019 = sqldf("select codmunres, count(1) qtd_mortes from base where ano_obito = 2019 group by codmunres")

ibge2002 = sqldf("select ano, ibge, ibge2, populacao, nome, uf from ibge where ano = '2002'")
ibge2019 = sqldf("select ano, ibge, ibge2, populacao, nome, uf from ibge where ano = '2019'")

rm(base)
rm(ibge)
gc()

df2002 = sqldf("select ano, ibge, ibge2, codmunres, nome, populacao, qtd_mortes, uf from ibge2002 inner join base2002 on ibge2002.ibge = base2002.codmunres")
df2019 = sqldf("select ano, ibge, ibge2, codmunres, nome, populacao, qtd_mortes, uf from ibge2019 inner join base2019 on ibge2019.ibge2 = base2019.codmunres")

rm(base2002)
rm(base2019)
rm(ibge2002)
rm(ibge2019)
gc()

df2002 = sqldf("select ano, uf, ibge, ((qtd_mortes * 100) / populacao) taxa from df2002")
df2019 = sqldf("select ano, uf, ibge, ((qtd_mortes * 100) / populacao) taxa from df2019")

df = sqldf("select uf, IBGE, df2002.taxa taxa2002, df2019.taxa taxa2019, df2002.taxa - df2019.taxa taxa from df2002 inner join df2019 using (uf, IBGE)")

df = sqldf("select uf, IBGE, taxa, case when taxa > 0 then 1 else 2 end indicador from df")

ggplot(df) +
  geom_map(
    map = mapaMun,
    color = NA, 
    size= .5,
    aes(map_id = IBGE, fill = indicador)) +
  expand_limits(
    x = mapaMun$long,
    y = mapaMun$lat
  ) +
  scale_fill_gradient(
    low = 'green',
    high = 'tomato'
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
  labs(fill = "Rate increase?")

ggsave(path = "./IMAGES",
  filename = "fig4_mapa_diferenca.png",
       device = "png",
       dpi = 320,
       width = 9,
       height = 5,
       limitsize = FALSE)
