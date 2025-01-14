rm(list = ls())
library(data.table)
library(bit64)
library(tidyverse)
library(stargazer)
library(lfe)
library(lubridate)
library(gridExtra)
library(grid)
library(sqldf)

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

ibge = sqldf("select ano, regiao, uf, estado, ibge, ibge2, nome, pib, populacao from ibge WHERE ano = '2002'")

base2020 = fread("./DADOS/Mortalidade_Geral_2002.csv")

icd10 = fread("./DADOS/ICD-10_MIT_2021_Excel_16-March_2021.csv")

icd10 = sqldf ("SELECT Chapter_No
                            , Chapter_Desc
                            , Group_Code
                            , Group_Desc
                            , ICD10_3_Code
                            , ICD10_3_Code_Desc
                            , replace( ICD10_Code , '.', '') as ICD10_Code
                            , WHO_Full_Desc
                         FROM icd10")

base2020 = sqldf("select racacor
                       , sexo
                       , idade
                       , substr(idade,1,1) ind_idade
                       , substr(idade,2,4) idade_calc
                       , codmunres
                       , ESC
                       , causabas
                    from base2020")

causa = sqldf("select CASE racacor
                        WHEN 1 THEN 'Branca'
                        WHEN 2 THEN 'Preta'
                        WHEN 3 THEN 'Amarela'
                        WHEN 4 THEN 'Parda'
                        WHEN 5 THEN 'Indígena'
                        else racacor
                      end raca
                    , CASE sexo 
                        WHEN 1 THEN 'masculino'
                        WHEN 2 THEN 'feminino'
                        WHEN 0 THEN 'ignorado'
                        ELSE NULL
                      END sexo
                    , CASE
                       WHEN ind_idade in (1,2,3) THEN '0'
                       WHEN ind_idade = 4 THEN idade_calc
                       WHEN ind_idade = 5 THEN '> 100'
                       WHEN ind_idade IN (6,7,8) THEN 'Erro'
                       WHEN ind_idade = 9 THEN 'Ignorado'
                       ELSE 'Erro'
                      end idade_calc
                    , CASE ESC
                        WHEN 1 THEN 'Nenhuma'
                        WHEN 2 THEN 'de 1 a 3 anos'
                        WHEN 3 THEN 'de 4 a 7 anos'
                        WHEN 4 THEN 'de 8 a 11 anos'
                        WHEN 5 THEN '12 anos e mais'
                        WHEN 9 THEN 'Ignorado'
                        ELSE 'Erro'
                      END ESCOLARIDADE
                    , codmunres
                    , causabas
                    , ind_idade
                    , racacor
                 FROM base2020")

base = sqldf("SELECT sexo
                   , raca
                   , escolaridade
                   , idade_calc
                   , codmunres
                   , causabas
                   , chapter_no
                   , chapter_desc
                   , group_code
                   , group_desc
                   , ICD10_3_code
                   , ICD10_3_code_desc
                   , ICD10_code
                   , who_full_desc
                   , regiao
                   , uf
                   , estado
                   , ibge
                   , nome
                   , pib
                   , populacao
                FROM causa
               LEFT JOIN icd10
                 ON (causabas = ICD10_Code)
               LEFT JOIN ibge
                 ON (codmunres = ibge2)")

rm(base2020)
rm(causa)
rm(icd10)
rm(ibge)
gc()