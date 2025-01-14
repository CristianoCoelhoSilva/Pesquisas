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

base2020 = fread("./DADOS/Mortalidade_Geral_2020.csv")

cid10 = fread("./DADOS/cid.csv")

cid10 = sqldf("select replace(code,'.','') code, title from cid10 where code not like '%-%'")

base2020 = sqldf("select idade
                       , substr(idade,1,1) ind_idade
                       , substr(idade,2,4) idade_calc
                       , codmunnatu
                       , codmunocor
                       , ESC2010
                       , ESCFALAGR1
                       , causabas
                    from base2020")

causa = sqldf("select idade
                    , ind_idade
                    , idade_calc
                    , codmunnatu
                    , codmunocor
                    , ESC2010
                    , ESCFALAGR1
                    , causabas
                    , substr(causabas,1,1) codigo
                    , substr(causabas,1,3) doenca
                 from base2020")

causa = sqldf("select idade
                    , ind_idade
                    , idade_calc
                    , codmunnatu
                    , codmunocor
                    , ESC2010
                    , ESCFALAGR1
                    , codigo
                    , causabas
                    , title causa_base
                    , code codigo_base
                    , case
                      WHEN codigo =  'A' THEN 'Infectious and parasitic diseases'
                      WHEN codigo =  'B' THEN 'Infectious and parasitic diseases'
                      WHEN codigo =  'C' THEN 'Cancer'
                      WHEN codigo =  'D' THEN 'Neoplasms, blood, and blood-forming organs'
                      WHEN codigo =  'E' THEN 'Endocrine, nutritional, or metabolic'
                      WHEN codigo =  'F' THEN 'Mental and behavioral disorders'
                      WHEN codigo =  'G' THEN 'Nervous system'
                      WHEN codigo =  'H' THEN 'Eyes, ears, nose, and throat'
                      WHEN codigo =  'I' THEN 'Circulatory system'
                      WHEN codigo =  'J' THEN 'Respiratory system'
                      WHEN codigo =  'K' THEN 'Digestive system'
                      WHEN codigo =  'L' THEN 'Skin'
                      WHEN codigo =  'M' THEN 'Musculoskeletal system'
                      WHEN codigo =  'N' THEN 'Genitourinary system'
                      WHEN codigo =  'O' THEN 'Pregnancy and childbirth'
                      WHEN codigo =  'P' THEN 'Perinatal conditions'
                      WHEN codigo =  'Q' THEN 'Congenital and chromosomal abnormalities'
                      WHEN codigo =  'R' THEN 'Abnormal clinical and lab findings'
                      WHEN codigo =  'S' THEN 'Injury, poisoning, and other external causes'
                      WHEN codigo =  'T' THEN 'Injury, poisoning, and other external causes'
                      WHEN codigo =  'U' THEN 'Used for emergency designation'
                      WHEN codigo =  'V' THEN 'External causes of morbidity'
                      WHEN codigo =  'W' THEN 'External causes of morbidity'
                      WHEN codigo =  'X' THEN 'External causes of morbidity'
                      WHEN codigo =  'Y' THEN 'External causes of morbidity'
                      WHEN codigo =  'Z' THEN 'Factors influencing health status and contact with health services'
                      end descricao_causa
                 from causa
                 left join cid10
                   on (doenca = code)")

causa = sqldf("select idade
                    , ind_idade
                    , idade_calc
                    , codmunnatu
                    , codmunocor
                    , ESC2010
                    , ESCFALAGR1 
                    , codigo
                    , descricao_causa
                    , codigo_base
                    , causa_base
                    , causabas
                    , title
                from causa
                left join cid10
                  on (causabas = code)
                where codigo = 'V'")