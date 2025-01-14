options(scipen = 999)
rm(list = ls())
library(ggplot2)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)
library(sqldf)

causa = fread("./DADOS/CAUSA/respiratorio.csv", encoding = "UTF-8")
totalMortes = fread("./DADOS/TotalMortesAno.csv", encoding = "UTF-8")
populacaoAno = fread("./DADOS/populacao_ano.csv", encoding = "UTF-8")

quantidade = sqldf("select ano_obito ano, count(1) qtd from causa group by ano_obito")

ggplot() + theme_bw() +
  geom_line(aes(y = qtd, x = ano), size=1.5, colour='blue', data = quantidade) +
  geom_point(aes(y = qtd, x = ano), size=3, colour='blue', data = quantidade) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=4)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n=7)) +
  ggthemes::theme_clean(base_size = 20) +
  geom_point(size = 2.8) +
  theme(text = element_text(size=30), axis.text.x = element_text(angle=45, hjust=1))  +
  labs(x="Year", y="Amount")

ggsave(
  path = "./IMAGES",
  filename = "fig24_a.png",
  device = "png",
  width = 12,
  height = 7,
  dpi = 320
)

porcentagem = sqldf("select ano_obito ANO, totalMortes.qtd, ROUND(((quantidade.qtd * 100) / totalMortes.qtd),2) porcentagem from totalMortes inner join quantidade on totalMortes.ano_obito = quantidade.ANO")

ggplot()  + theme_bw() +
  geom_line(aes(y = porcentagem, x = ANO), size=1, colour='blue', data = porcentagem) +
  geom_point(aes(y = porcentagem, x = ANO), size=2, colour='blue', data = porcentagem) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=5)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n=7)) +
  ggthemes::theme_clean(base_size = 20) +
  geom_point(size = 2.8) +
  geom_hline(yintercept = 0.1, alpha = 0.1) +
  theme(text = element_text(size=30), axis.text.x = element_text(angle=45, hjust=1))  +
  labs(x="Year", y="Rate %")

ggsave(
  path = "./IMAGES",
  filename = "fig24_b.png",
  device = "png",
  width = 12,
  height = 7,
  dpi = 320
)

genero = sqldf("select ano_obito, def_sexo, count(1) qtd from causa where def_sexo not in ('Ignorado') group by ano_obito, def_sexo")
genero = sqldf("select ano_obito, case when def_sexo = 'Feminino' then 'Female' when def_sexo = 'Masculino' then 'Male' else null end def_sexo, qtd from genero")

total = sqldf("select ano_obito, sum(qtd) sum from genero group by ano_obito")
baseFinal =  sqldf("select def_sexo, total.ano_obito, qtd, sum from genero inner join total on genero.ano_obito = total.ano_obito")
base =  sqldf("select def_sexo, ano_obito, ((qtd * 100) / sum) porcentagem from baseFinal")

ggplot()  + theme_bw() +
  geom_line(aes(y = porcentagem, x = ano_obito, colour = def_sexo), size=1, data = base) +
  geom_point(aes(y = porcentagem, x = ano_obito, colour = def_sexo), size=2, data = base) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=5)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n=7)) +
  ggthemes::theme_clean(base_size = 20) +
  geom_point(size = 2.8) +
  geom_hline(yintercept = 30, alpha = 0.1) +
  theme(text = element_text(size=20), axis.text.x = element_text(angle=45, hjust=1))  +
  labs(x="Year", y="%") + 
  theme(legend.position="bottom", legend.text=element_text(size=15)) + labs(color='')

ggsave(
  path = "./IMAGES",
  filename = "fig24_c.png",
  device = "png",
  width = 12,
  height = 7,
  dpi = 320
)

raca = sqldf("select ano_obito, def_raca_cor, count(1) qtd from causa where def_raca_cor not in ('Ignorado','Amarela','Indígena') group by ano_obito, def_raca_cor")

raca = sqldf("select ano_obito
                  , case 
                      when def_raca_cor = 'Parda' then 'Black or mixed race'
                      when def_raca_cor = 'Preta' then 'Black or mixed race'
                      when def_raca_cor = 'Branca' then 'White'
                      else def_raca_cor
                    end raca
                  , qtd from raca")

raca = sqldf("select ano_obito
                  , raca
                  , sum(qtd) qtd from raca
              group by ano_obito, raca")

total = sqldf("select ano_obito, sum(qtd) sum from raca group by ano_obito")

baseFinal =  sqldf("select raca, total.ano_obito, qtd, sum from raca inner join total on raca.ano_obito = total.ano_obito")

base =  sqldf("select raca, ano_obito, ((qtd * 100) / sum) porcentagem from baseFinal")

ggplot()  + theme_bw() +
  geom_line(aes(y = porcentagem, x = ano_obito, colour = raca), size=1, data = base) +
  geom_point(aes(y = porcentagem, x = ano_obito, colour = raca), size=2, data = base) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=5)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n=7)) +
  ggthemes::theme_clean(base_size = 20) +
  geom_point(size = 2.8) +
  geom_hline(yintercept = 20, alpha = 0.1) +
  theme(text = element_text(size=20), axis.text.x = element_text(angle=45, hjust=1))  +
  labs(x="Year", y="%") + 
  theme(legend.position="bottom", legend.text=element_text(size=15)) + labs(color='')

ggsave(
  path = "./IMAGES",
  filename = "fig24_d.png",
  device = "png",
  width = 12,
  height = 7,
  dpi = 320
)

#Gráficos por Sexo, Genero e Escolaridade
escolaridade = sqldf("select ano_obito, def_escol, count(1) qtd from causa where def_escol not in ('Ignorado') group by ano_obito, def_escol")

total = sqldf("select ano_obito, sum(qtd) sum from escolaridade group by ano_obito")

baseFinal =  sqldf("select case 
                            when def_escol = '1 a 3 anos' then '1 to 3 years'
                            when def_escol = '4 a 7 anos' then '4 to 7 years'
                            when def_escol = '8 a 11 anos' then '8 to 11 years'
                            when def_escol = '12 e mais' then '12 or more'
                            when def_escol = 'Nenhuma' then 'None'
                            else def_escol
                            end def_escol
                         , total.ano_obito
                         , qtd
                         , sum 
                      from escolaridade 
                     inner join total 
                        on escolaridade.ano_obito = total.ano_obito")

base =  sqldf("select def_escol, ano_obito, ((qtd * 100) / sum) porcentagem from baseFinal")

ggplot()  + theme_bw() +
  geom_line(aes(y = porcentagem, x = ano_obito, colour = def_escol), size=1, data = base) +
  geom_point(aes(y = porcentagem, x = ano_obito, colour = def_escol), size=2, data = base) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=5)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n=7)) +
  ggthemes::theme_clean(base_size = 20) +
  geom_point(size = 2.8) +
  geom_hline(yintercept = 20, alpha = 0.1) +
  theme(text = element_text(size=30), axis.text.x = element_text(angle=45, hjust=1))  +
  labs(x="Year", y="%") + 
  theme(legend.position="bottom", legend.text=element_text(size=15)) + labs(color='')

ggsave(
  path = "./IMAGES",
  filename = "fig24_e.png",
  device = "png",
  width = 12,
  height = 7,
  dpi = 320
)
