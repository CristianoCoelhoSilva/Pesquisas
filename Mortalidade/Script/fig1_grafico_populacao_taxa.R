options(scipen = 999)
library(sqldf)
library(ggrepel)
library(ggthemes)
library(ggplot2)

populacao = fread("./DADOS/populacao_ano.csv", encoding = "UTF-8")
totalMortes = fread("./DADOS/TotalMortesAno.csv", encoding = "UTF-8")
data = sqldf("select ano, populacao, qtd, ((qtd * 100)/populacao) taxa from populacao inner join totalMortes on ano = ano_obito")

#qtd = sqldf("select ano_obito ano, count(1) qtd from base group by ano_obito")
#fwrite(qtd, "./DADOS/mortes_ano.csv")
#ibge = sqldf("select ano, sum(populacao) populacao from ibge group by ano")
#fwrite(ibge, "./DADOS/populacao_ano.csv")

quantidade  <- ggplot() + theme_bw() +
  geom_line(aes(y = qtd, x = ANO), size=1, colour='blue', data = data) +
  geom_point(aes(y = qtd, x = ANO), size=2, colour='blue', data = data) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=4)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n=7)) +
  ggthemes::theme_clean(base_size = 30) +
  geom_point(size = 2.8) +
  #geom_hline(yintercept = 0.1, linetype = "dashed", alpha = 0.5) +
  theme(text = element_text(size=50), axis.text.x = element_text(angle=45, hjust=1))  +
  labs(x="Year", y="Amount")

quantidade

ggsave(
  path = "./IMAGES",
  filename = "fig1_populacao.png",
  device = "png",
  width = 9,
  height = 6,
  dpi = 320
)

#porcentagem = sqldf("select ibge.ano, ROUND(((qtd * 100) / populacao),2) porcentagem from ibge inner join data on ibge.ano = qtd.ANO")

porcentagem  <- ggplot()  + theme_bw() +
  geom_line(aes(y = taxa, x = ANO), size=1, colour='blue', data = data) +
  geom_point(aes(y = taxa, x = ANO), size=2, colour='blue', data = data) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=5)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n=7)) +
  ggthemes::theme_clean(base_size = 30) +
  geom_point(size = 2.8) +
  #geom_hline(yintercept = 0.1, linetype = "dashed", alpha = 0.5) +
  theme(text = element_text(size=50), axis.text.x = element_text(angle=45, hjust=1))  +
  labs(x="Year", y="Rate %")


porcentagem

ggsave(
  path = "./IMAGES",
  filename = "fig1_taxa.png",
  device = "png",
  width = 9,
  height = 6,
  dpi = 320
)
