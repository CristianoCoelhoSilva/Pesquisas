library(sqldf)
library(plotrix)

base = sqldf("select ano_obito ano, idade_obito, causabas_capitulo, def_sexo sexo from base where causabas_capitulo like '%IV.  Doenças endócrinas nutricionais e metabólicas%'")

base = sqldf("select ano, idade_obito, sexo, count(1) total from base  where ano = '2019' group by ano, idade_obito, sexo")

sum =  sqldf("select sum(total) total from base where sexo not in ('Ignorado')")

feminino = sqldf("select ano
                    , idade_obito
                    , total
                    , sexo
                    , case 
                       when idade_obito IN (0,1,2,3,4) then 1
                       when idade_obito IN (5,6,7,8,9) then 2
                       when idade_obito IN (10,11,12,13,14) then 3
                       when idade_obito IN (15,16,17,18,19) then 4
                       when idade_obito IN (20,21,22,23,24) then 5
                       when idade_obito IN (25,26,27,28,29) then 6
                       when idade_obito IN (30,31,32,33,34) then 7
                       when idade_obito IN (35,36,37,38,39) then 8 
                       when idade_obito IN (40,41,42,43,44) then 9 
                       when idade_obito IN (45,46,47,48,49) then 10 
                       when idade_obito IN (50,51,52,53,54) then 11 
                       when idade_obito IN (55,56,57,58,59) then 12 
                       when idade_obito IN (60,61,62,63,64) then 13 
                       when idade_obito IN (65,66,67,68,69) then 14 
                       when idade_obito IN (70,71,72,73,74) then 15 
                       when idade_obito IN (75,76,77,78,79) then 16 
                       when idade_obito IN (80,81,82,83,84) then 17 
                       else 18
                      end faixa
              from base where sexo = 'Feminino' and idade_obito is not null")

feminino =  sqldf("select faixa,((sum(total)*1000) / 83481) total  from feminino group by faixa")

masculino = sqldf("select ano
                    , idade_obito
                    , total
                    , sexo
                    , case 
                       when idade_obito IN (0,1,2,3,4) then 1
                       when idade_obito IN (5,6,7,8,9) then 2
                       when idade_obito IN (10,11,12,13,14) then 3
                       when idade_obito IN (15,16,17,18,19) then 4
                       when idade_obito IN (20,21,22,23,24) then 5
                       when idade_obito IN (25,26,27,28,29) then 6
                       when idade_obito IN (30,31,32,33,34) then 7
                       when idade_obito IN (35,36,37,38,39) then 8 
                       when idade_obito IN (40,41,42,43,44) then 9 
                       when idade_obito IN (45,46,47,48,49) then 10 
                       when idade_obito IN (50,51,52,53,54) then 11 
                       when idade_obito IN (55,56,57,58,59) then 12 
                       when idade_obito IN (60,61,62,63,64) then 13 
                       when idade_obito IN (65,66,67,68,69) then 14 
                       when idade_obito IN (70,71,72,73,74) then 15 
                       when idade_obito IN (75,76,77,78,79) then 16 
                       when idade_obito IN (80,81,82,83,84) then 17 
                       else 18
                      end faixa
              from base where sexo = 'Masculino' and idade_obito is not null")

masculino =  sqldf("select faixa,((sum(total) * 1000) / 83481) total  from masculino group by faixa")

#Metabolica
xy.pop<-c(0.3,0.0,0.0,0.0,0.1,0.2,0.4,0.6,1.0,1.5,2.5,3.7,4.8,5.9,6.3,6.1,5.1,6.9)
xx.pop<-c(0.3,0.0,0.0,0.1,0.1,0.2,0.3,0.5,0.8,1.2,1.9,2.9,4.3,5.8,6.5,7.3,7.5,12.9)

agelabels<-c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
             "35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74",
             "75-79","80-84","85+")

par(mar=pyramid.plot(xy.pop,xx.pop,labels=agelabels,top.labels=c("Male","","Female")
                     ,main="",lxcol='#3399FF',rxcol='#FF0033',
                     gap=1.2,show.values=TRUE))
