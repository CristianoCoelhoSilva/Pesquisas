options(scipen = 999)
library(sqldf)
library(ggrepel)
library(ggthemes)
library(ggplot2) 

base = sqldf("select causabas_capitulo causa
                   , count(1) total 
                from base group by causabas_capitulo")

base = sqldf("select case
                      when causa = 'I.   Algumas doenças infecciosas e parasitárias' then  'Certain infectious and parasitic diseases'
                      when causa = 'II.  Neoplasias (tumores)' then 'Neoplasms'
                      when causa = 'III. Doenças sangue órgãos hemat e transt imunitár' then 'Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism'
                      when causa = 'IV.  Doenças endócrinas nutricionais e metabólicas' then 'Endocrine, nutritional and metabolic diseases'
                      when causa = 'IX.  Doenças do aparelho circulatório' then 'Diseases of the circulatory system'
                      when causa = 'V.   Transtornos mentais e comportamentais' then 'Mental and behavioural disorders'
                      when causa = 'VI.  Doenças do sistema nervoso' then 'Diseases of the nervous system'
                      when causa = 'VII. Doenças do olho e anexos' then 'Diseases of the eye and adnexa'
                      when causa = 'VIII.Doenças do ouvido e da apófise mastóide' then 'Diseases of the ear and mastoid process'
                      when causa = 'X.   Doenças do aparelho respiratório' then 'Diseases of the respiratory system'
                      when causa = 'XI.  Doenças do aparelho digestivo' then 'Diseases of the digestive system'
                      when causa = 'XII. Doenças da pele e do tecido subcutâneo' then 'Diseases of the skin and subcutaneous tissue'
                      when causa = 'XIII.Doenças sist osteomuscular e tec conjuntivo' then 'Diseases of the musculoskeletal system and connective tissue'
                      when causa = 'XIV. Doenças do aparelho geniturinário' then 'Diseases of the genitourinary system'
                      when causa = 'XV.  Gravidez parto e puerpério' then 'Pregnancy, childbirth and the puerperium'
                      when causa = 'XVI. Algumas afec originadas no período perinatal' then 'Certain conditions originating in the perinatal period'
                      when causa = 'XVII.Malf cong deformid e anomalias cromossômicas' then 'Congenital malformations, deformations and chromosomal abnormalities'
                      when causa = 'XVIII.Sint sinais e achad anorm ex clín e laborat' then 'Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified'
                      when causa = 'XX.  Causas externas de morbidade e mortalidade' then 'External causes of morbidity and mortality'
                    end causa
                  , total 
              from base")

ggplot(base, aes(reorder(str_wrap(causa), total, sum), total)) +
  geom_col() +
  coord_flip() +  
  ggthemes::theme_clean(base_size = 11.5) +
  theme(text = element_text(size=11.5), axis.text.x = element_text(angle=45, hjust=1)) +
  xlab('Cause')+
  ylab('Total')+
  theme(plot.title = element_text(size=8))

ggsave(
  path = "./IMAGES",
  filename = "fig6_cause.png",
  device = "png",
  width = 12,
  height = 7,
  dpi = 320
)