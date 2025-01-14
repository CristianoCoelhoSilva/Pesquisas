options(scipen = 999)
library(sqldf)
library(ggrepel)
library(ggthemes)
library(ggplot2)

ibge =  sqldf("select sum(populacao) populacao, ano from ibge where ano in ('2002','2019') group by ano")

data = sqldf("select causabas_capitulo causa
                      , ano_obito ano
                      , case ano_obito
                          when '2002' then 1
                          else 2
                        end cor
                      , case 
                          when causabas_capitulo = 'IX.  Doenças do aparelho circulatório' then 'c) Diseases of the circulatory system'
                          when causabas_capitulo = 'II.  Neoplasias (tumores)' then 'e) Neoplasms'
                          when causabas_capitulo = 'X.   Doenças do aparelho respiratório' then 'd) Diseases of the respiratory system'
                          when causabas_capitulo = 'XX.  Causas externas de morbidade e mortalidade' then 'a) External causes of morbidity and mortality'
                          when causabas_capitulo = 'IV.  Doenças endócrinas nutricionais e metabólicas' then 'b) Endocrine, nutritional and metabolic diseases'
                          else causabas_capitulo
                        end doenca
                      , COUNT(1) total
                  from base 
                 where causabas_capitulo 
              IN ('IX.  Doenças do aparelho circulatório'
                 ,'II.  Neoplasias (tumores)'
                 ,'X.   Doenças do aparelho respiratório'
                 ,'IV.  Doenças endócrinas nutricionais e metabólicas'
                 ,'XX.  Causas externas de morbidade e mortalidade')
            group by causabas_capitulo, ano_obito")

data = sqldf("select data.ano, doenca, total, cor, populacao, ((total/populacao)*100000) taxa from data inner join ibge on ibge.ano = data.ano")

data2019 = sqldf("select ano, doenca, taxa taxa_2019 from data where ano = '2019'")

data =  sqldf("select data.ano, data.doenca, total, cor, populacao, taxa, taxa_2019, ROUND((taxa_2019 - taxa)) dif from data left join data2019 on data2019.doenca = data.doenca")

ggplot(data, aes(x=taxa, y=doenca))+
  geom_line(aes(y = doenca, x = taxa), size=1, colour='gray', data = data) +
  geom_point(size=4, colour=ifelse(data$cor>=2, '#014d64',
                                   '#76c0c1')) +
  scale_x_continuous(breaks=seq(0,200,25)) +
  geom_text(size = 6,  nudge_y = 0.4, colour = "black", mapping = 
      aes(label = ifelse(doenca == "e Doenças do aparelho circulatório",
                 ifelse(ano == "2002","2002", "2019" ), "" ))) + 
  geom_rect(
                   mapping = aes(xmin = 200, xmax = Inf , ymin = -Inf, ymax = Inf),
                   fill = "white",
                   color = "white"
                 ) +
  geom_text(
    size = 6,
    mapping = 
      aes(
        x = 210,
        y = doenca,
        label = ifelse(dif!=0, dif,
                       '')
      )
  ) +
  # Inserir Titulo das Diferencas
  geom_text(
    size = 6,
    # Cor
    colour = "black",
    nudge_y = 0.4,
    # Posicao
    mapping = 
      aes(
        x = 211,
        y = doenca,
        label = 
          # Plotar acima apenas dos valores do primeiro grupo (germany)
          ifelse(doenca == "e Doenças do aparelho circulatório",
                 # Verdadeiro
                 "Dif.",
                 #Falso
                 ""
          )
      )
  ) +
  ggthemes::theme_clean(base_size = 20) +
  xlab('Amount')+
  ylab('Cause')

ggsave(
  path = "./IMAGES",
  filename = "fig7_diferenca_principal_causa.png",
  device = "png",
  width = 15,
  height = 6,
  dpi = 320
)