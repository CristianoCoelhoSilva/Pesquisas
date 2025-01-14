options(scipen = 999)
library(sqldf)
library(ggrepel)
library(ggthemes)
library(ggplot2)

base = sqldf("select codmunres, ano_obito, count(1) qtd_mortes from base group by codmunres, ano_obito")

ibge = sqldf("select populacao
                   , ibge2
                   , regiao
                   , uf
                    , case 
                      when estado = 'S<e3>o Paulo' then 'São Paulo'
                      when estado = 'Rond<f4>nia' Then 'Rondônia'
                      when estado = 'Piau<ed>' then 'Piauí'
                      when estado = 'Maranh<e3>o' then 'Maranhão'
                      when estado = 'Par<e1>' then 'Pará'
                      when estado = 'Cear<e1>' then 'Ceará'
                      when estado = 'Amap<e1>' then 'Amapá'
                      when estado = 'Para<ed>ba' then 'Paraíba'
                      when estado = 'Paran<e1>' then 'Paraná'
                      when estado = 'Esp<ed>rito Santo' then 'Espírito Santo'
                      when estado = 'Goi<e1>s' then 'Goiás'
                     else estado
                     end estado
                   , ano from ibge where ano IN ('2019')")

data = sqldf("select populacao, qtd_mortes, regiao, uf, estado, ano from ibge inner join base on (ibge.ibge2 = base.codmunres and ibge.ano = base.ano_obito)")

data = sqldf("select SUM(populacao) populacao, SUM(qtd_mortes) mortes, uf, estado, ano, regiao from data group by uf, estado, ano, regiao")

data = sqldf("select ano, ROUND(populacao) populacao, mortes, ROUND(((mortes * 100)/populacao),2) taxa, uf, estado from data")

data = sqldf("select *  from data where ano = '2019'")

ggplot(data, aes(x=populacao, y=taxa))+
  geom_point(size=7, colour=ifelse(data$taxa>=0.64, 'red',
                                   '#66FFFF'),
             alpha=ifelse(data$populacao>=100000&data$taxa<0.5, 1,.4))+
  geom_smooth(method='lm', colour='black', size=1, linetype='dashed', se=FALSE)+
  geom_text_repel(size=5, aes(label=estado),
                  colour='black',
                  box.padding = .5,
                  point.padding = .7,
                  data=subset(data, data$populacao>=100000))+
  scale_color_brewer(palette = "Dark2") +  
  ggthemes::theme_clean(base_size = 20) +
   xlab('Population')+
  ylab('Rate %')

ggsave(
  path = "./IMAGES",
  filename = "fig5_estados.png",
  device = "png",
  width = 12,
  height = 6,
  dpi = 320
)
