rm(list = ls())
library(data.table)
library(bit64)
library(tidyverse)
library(stargazer)
library(lfe)
library(lubridate)
library(gridExtra)
library(grid)
library(fixest)

source("./Script/plot_utils.R")

FAST_DEATH_LOADING = FALSE

if(FAST_DEATH_LOADING == FALSE) {
  
  base2002 = fread("./DADOS/SIM2002.csv")
  base2003 = fread("./DADOS/SIM2003.csv")
  base2004 = fread("./DADOS/SIM2004.csv")
  base2005 = fread("./DADOS/SIM2005.csv")
  base2006 = fread("./DADOS/SIM2006.csv")
  base2007 = fread("./DADOS/SIM2007.csv")
  base2008 = fread("./DADOS/SIM2008.csv")
  base2009 = fread("./DADOS/SIM2009.csv")
  base2010 = fread("./DADOS/SIM2010.csv")
  base2011 = fread("./DADOS/SIM2011.csv")
  base2012 = fread("./DADOS/SIM2012.csv")
  base2013 = fread("./DADOS/SIM2013.csv")
  base2014 = fread("./DADOS/SIM2014.csv")
  base2015 = fread("./DADOS/SIM2015.csv") 
  base2016 = fread("./DADOS/SIM2016.csv")
  base2017 = fread("./DADOS/SIM2017.csv")
  base2018 = fread("./DADOS/SIM2018.csv")
  base2019 = fread("./DADOS/SIM2019.csv")
  
  base = rbindlist(
    l = list(base2002,base2003,base2004,base2005
             ,base2006,base2007,base2008,base2009
             ,base2010,base2011,base2012,base2013
             ,base2014,base2015,base2016,base2017
             ,base2018,base2019),
    use.names = T
  )
  
  rm(base2002,base2003,base2004,base2005
     ,base2006,base2007,base2008,base2009
     ,base2010,base2011,base2012,base2013
     ,base2014,base2015,base2016,base2017
     ,base2018,base2019)
  
  fwrite(base, "./DADOS/SIM2002_2019.csv")
} else {
  base = fread("./DADOS/SIM2002_2019.csv", encoding = "UTF-8")
}

base = sqldf("select CODMUNRES
                   , ano_obito
                   , idade_obito
                   , case
                      when causabas_capitulo = 'I.   Algumas doenças infecciosas e parasitárias' then  'I.   Certain infectious and parasitic diseases'
                      when causabas_capitulo = 'II.  Neoplasias (tumores)' then 'II.  Neoplasms'
                      when causabas_capitulo = 'III. Doenças sangue órgãos hemat e transt imunitár' then 'III. Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism'
                      when causabas_capitulo = 'IV.  Doenças endócrinas nutricionais e metabólicas' then 'IV.  Endocrine, nutritional and metabolic diseases'
                      when causabas_capitulo = 'IX.  Doenças do aparelho circulatório' then 'IX.  Diseases of the circulatory system'
                      when causabas_capitulo = 'V.   Transtornos mentais e comportamentais' then 'V.   Mental and behavioural disorders'
                      when causabas_capitulo = 'VI.  Doenças do sistema nervoso' then 'VI.  Diseases of the nervous system'
                      when causabas_capitulo = 'VII. Doenças do olho e anexos' then 'VII. Diseases of the eye and adnexa'
                      when causabas_capitulo = 'VIII.Doenças do ouvido e da apófise mastóide' then 'VIII.Diseases of the ear and mastoid process'
                      when causabas_capitulo = 'X.   Doenças do aparelho respiratório' then 'X.   Diseases of the respiratory system'
                      when causabas_capitulo = 'XI.  Doenças do aparelho digestivo' then 'XI.  Diseases of the digestive system'
                      when causabas_capitulo = 'XII. Doenças da pele e do tecido subcutâneo' then 'XII. Diseases of the skin and subcutaneous tissue'
                      when causabas_capitulo = 'XIII.Doenças sist osteomuscular e tec conjuntivo' then 'XIII.Diseases of the musculoskeletal system and connective tissue'
                      when causabas_capitulo = 'XIV. Doenças do aparelho geniturinário' then 'XIV. Diseases of the genitourinary system'
                      when causabas_capitulo = 'XV.  Gravidez parto e puerpério' then 'XV.  Pregnancy, childbirth and the puerperium'
                      when causabas_capitulo = 'XVI. Algumas afec originadas no período perinatal' then 'XVI. Certain conditions originating in the perinatal period'
                      when causabas_capitulo = 'XVII.Malf cong deformid e anomalias cromossômicas' then 'XVII.Congenital malformations, deformations and chromosomal abnormalities'
                      when causabas_capitulo = 'XVIII.Sint sinais e achad anorm ex clín e laborat' then 'XVIII.Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified'
                      when causabas_capitulo = 'XX.  Causas externas de morbidade e mortalidade' then 'XX.  External causes of morbidity and mortality'
                     end causabas_capitulo
                   , case 
                      when def_sexo = 'Feminino' then 'Female' 
                      when def_sexo = 'Masculino' then 'Male' 
                      else def_sexo
                     end def_sexo
                   , case 
                      when def_raca_cor = 'Parda' then 'Brown'
                      when def_raca_cor = 'Preta' then 'Black'
                      when def_raca_cor = 'Branca' then 'White'
                      else def_raca_cor
                     end def_raca_cor
                   , case 
                      when def_escol = '1 a 3 anos' then '1 to 3 years'
                      when def_escol = '4 a 7 anos' then '4 to 7 years'
                      when def_escol = '8 a 11 anos' then '8 to 11 years'
                      when def_escol = '12 e mais' then '12 or more'
                      when def_escol = 'Nenhuma' then 'None'
                      else def_escol
                     end def_escol
                from base")

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


geographic_city_info = 
  ibge %>% 
  group_by(IBGE) %>% 
  mutate(last_seen = max(ANO, na.rm = T)) %>% 
  ungroup()  %>% 
  filter(ANO == last_seen) %>% 
  select(IBGE, ESTADO, REGIAO, NOME_MICRORREGIAO, NOME_MESORREGIAO)

tudo = 
  base %>% 
  #  mutate(DTOBITO = dmy(DTOBITO)) %>% 
  #  mutate(MES_ANO = year(DTOBITO)*1e2 + month(DTOBITO)) %>% 
  mutate(discrete_idade_obito = dplyr::ntile(idade_obito, 10)) %>% 
  group_by(ano_obito, CODMUNRES, discrete_idade_obito, def_sexo, def_raca_cor, def_escol, causabas_capitulo) %>% 
  summarize(number_deaths = n()) %>% 
  ungroup() %>% 
  left_join(ibge %>% 
              select(ano_obito = ANO,
                     CODMUNRES = ibge2,
                     PIB,
                     PIB_PER_CAPITA,
                     POPULACAO,
                     REGIAO,
                     ESTADO,
                     CODIGO_MESORREGIAO,
                     CODIGO_MICRORREGIAO,
                     growth_pib,
                     growth_pib_per_capita
              ))

toRegress = 
  tudo %>% 
  mutate(frac_deaths = 100 * number_deaths / POPULACAO) 


# Regressions (Male vs Female) -------------------------------------------------------------

model_baseline1_sexo = 
  feols(
    data = toRegress %>%
      filter(def_sexo != "Ignorado") %>% 
      mutate(def_sexo = stats::relevel(factor(def_sexo), ref = "Female")) 
    ,fml = log(1+number_deaths) ~  
      def_sexo |
      ano_obito^CODMUNRES^causabas_capitulo^discrete_idade_obito^def_raca_cor^def_escol
    ,cluster = ~ CODMUNRES + causabas_capitulo
  )


model_baseline2_sexo = 
  feols(
    data = toRegress %>%
      filter(def_sexo != "Ignorado") %>% 
      mutate(def_sexo = stats::relevel(factor(def_sexo), ref = "Female")) 
    ,fml = log(1+number_deaths) ~  
      causabas_capitulo*def_sexo - def_sexo |
      ano_obito^CODMUNRES^discrete_idade_obito^def_raca_cor^def_escol + causabas_capitulo
    ,cluster = ~ CODMUNRES + causabas_capitulo
  )



etable(model_baseline1_sexo, model_baseline2_sexo)

# Joga o output desse commando no Latex/Overleaf e ajeita por ali. Depois, pega a imagem e coloca no word. 
etable(model_baseline1_sexo, model_baseline2_sexo,
       tex = T,
       fixef_sizes = T,
       digits.stats = 4)


coeftable = coeftable(model_baseline2_sexo) 

to_plot = 
  coeftable %>% 
  as.data.table() %>% 
  mutate(coefficient = rownames(coeftable)) %>% 
  janitor::clean_names() %>% 
  mutate(
    formatted_coefficient = str_match(coefficient, "causabas_capitulo.+\\.[ ]*(.*):.*")[, 2]
  ) %>%
  mutate(coefficient = str_replace_all(coefficient, "(causabas_capitulo|:def_sexoMale)", "")) %>% 
  arrange(estimate) %>% 
  mutate(rank = row_number()) %>% 
  mutate(
    formatted_coefficient = factor(formatted_coefficient,
                                   ordered = T,
                                   .$formatted_coefficient[.$rank])
  ) %>% 
  mutate(positive_estimate = estimate >= 0)

to_plot %>% 
  ggplot(aes(x = formatted_coefficient, y = estimate)) +
  geom_errorbar(aes(ymin=estimate-1.96*std_error,
                    ymax=estimate+1.96*std_error,
                    color = positive_estimate)
  ) +
  geom_point(aes(color = positive_estimate), size = 2.8) + 
  scale_y_continuous(label = scales::percent_format(accuracy = 1), 
                     breaks = scales::pretty_breaks())  +
  coord_flip() +
  labs(y = "",
       x = "",
       color = "Dep. variable") + 
  scale_color_brewer(palette = "Set1", direction = -1) +  
  geom_hline(yintercept = 0, linetype = "dashed") + 
  # viridis::scale_color_viridis(discrete = T, option = "B") + 
  ggthemes::theme_clean(base_size = 22)  +
  theme(
    legend.position = "hide",
    plot.background = element_blank()
  )  

ggsave(
  path = "./results",
  filename = "dotplot_death_male_vs_female.pdf",
  device = "pdf",
  width = 18,
  height = 10,
  dpi = 640
)

model_sexo_ao_longo_tempo = 
  feols(
    data = 
      toRegress %>%
      filter(causabas_capitulo %in% 
               (to_plot %>%
                  filter(!str_detect(formatted_coefficient, "(Symptoms|Diseases of the ear|Congenital|eye|Mental)")) %>% 
                  pull(coefficient))) %>% 
      # filter(str_detect(causabas_capitulo, "Causas externas de morbidade")) %>% 
      filter(def_sexo != "Ignorado") %>% 
      mutate(log_deaths = log(1+number_deaths)) %>% 
      mutate(is_male = as.integer(def_sexo == "Male"))
    ,fml = log_deaths ~  
      i(ano_obito, is_male) - is_male |
      ano_obito^CODMUNRES^discrete_idade_obito^def_raca_cor^def_escol
    ,cluster = ~ CODMUNRES
    ,fixef.rm = "both"
    ,split = ~ causabas_capitulo
  )

plot_coefplot_black(
  regression_model = model_sexo_ao_longo_tempo,
  coefficientName = "is_male",
  curves_to_plot = 
    to_plot %>% 
    filter(positive_estimate == TRUE) %>%
    # filter(!str_detect(formatted_coefficient, "(Symptoms|Diseases of the ear|apófise|olho|cromoss)")) %>%
    pull(formatted_coefficient)
)

ggsave(
  path = "./results",
  filename = "coefplot_death_male_most_likely_to_die.pdf",
  device = "pdf",
  width = 18,
  height = 12,
  dpi = 640
)


plot_coefplot_black(
  regression_model = model_sexo_ao_longo_tempo,
  coefficientName = "is_male",
  curves_to_plot = 
    to_plot %>% 
    filter(positive_estimate == FALSE) %>%
    # filter(!str_detect(formatted_coefficient, "(olho)")) %>% 
    pull(formatted_coefficient)
)

ggsave(
  path = "./results",
  filename = "coefplot_death_male_least_likely_to_die.pdf",
  device = "pdf",
  width = 18,
  height = 12,
  dpi = 640
)

# Regressions (Race) -------------------------------------------------------------

toRegress$def_raca_cor %>% table()

to_regress_raca = 
  toRegress %>%
  filter(def_raca_cor != "Ignorado")  %>% 
  mutate(def_raca_cor = case_when(
    def_raca_cor %in% c("Amarela", "White") ~ "White",
    TRUE ~ "Not White",
  )) %>%
  mutate(is_white = def_raca_cor == "White")

model_baseline1_raca = 
  feols(
    data = 
      to_regress_raca 
    ,fml = log(1+number_deaths) ~  
      is_white |
      ano_obito^CODMUNRES^causabas_capitulo^discrete_idade_obito^def_sexo^def_escol
    ,fixef.rm = "both"
    ,cluster = ~ CODMUNRES + causabas_capitulo
  )

model_baseline2_raca = 
  feols(
    data = 
      to_regress_raca
    ,fml = log(1+number_deaths) ~  
      causabas_capitulo*is_white - is_white |
      ano_obito^CODMUNRES^discrete_idade_obito^def_sexo^def_escol + causabas_capitulo
    ,fixef.rm = "both"
    ,cluster = ~ CODMUNRES + causabas_capitulo
  )

etable(model_baseline1_raca, model_baseline2_raca)

# Joga o output desse commando no Latex/Overleaf e ajeita por ali. Depois, pega a imagem e coloca no word. 
etable(model_baseline1_raca, model_baseline2_raca,
       tex = T,
       fixef_sizes = T,
       digits.stats = 4)


coeftable = coeftable(model_baseline2_raca) 

to_plot = 
  coeftable %>% 
  as.data.table() %>% 
  mutate(coefficient = rownames(coeftable)) %>% 
  janitor::clean_names() %>% 
  mutate(
    formatted_coefficient = str_match(coefficient, "causabas_capitulo.+\\.[ ]*(.*):.*")[, 2]
  ) %>%
  mutate(coefficient = str_replace_all(coefficient, "(causabas_capitulo|:is_whiteTRUE)", "")) %>% 
  arrange(estimate) %>% 
  mutate(rank = row_number()) %>% 
  mutate(
    formatted_coefficient = factor(formatted_coefficient,
                                   ordered = T,
                                   .$formatted_coefficient[.$rank])
  ) %>% 
  mutate(positive_estimate = estimate >= 0)

to_plot %>% 
  ggplot(aes(x = formatted_coefficient, y = estimate)) +
  geom_errorbar(aes(ymin=estimate-1.96*std_error,
                    ymax=estimate+1.96*std_error,
                    color = positive_estimate)
  ) +
  geom_point(aes(color = positive_estimate), size = 2.8) + 
  scale_y_continuous(label = scales::percent_format(accuracy = 1), 
                     breaks = scales::pretty_breaks())  +
  coord_flip() +
  labs(y = "",
       x = "",
       color = "Dep. variable") + 
  scale_color_brewer(palette = "Set1", direction = -1) +  
  geom_hline(yintercept = 0, linetype = "dashed") + 
  ggthemes::theme_clean(base_size = 22)  +
  theme(
    legend.position = "hide",
    plot.background = element_blank()
  )  

ggsave(
  path = "./results",
  filename = "dotplot_death_white_vs_non-white.pdf",
  device = "pdf",
  width = 18,
  height = 10,
  dpi = 640
)

model_raca_ao_longo_tempo = 
  feols(
    data = 
      to_regress_raca
    ,fml = log(1+number_deaths) ~  
      i(ano_obito, is_white) - is_white |
      ano_obito^CODMUNRES^discrete_idade_obito^def_sexo^def_escol
    ,cluster = ~ CODMUNRES
    ,split = ~ causabas_capitulo
  )

plot_coefplot_black(
  regression_model = model_raca_ao_longo_tempo,
  coefficientName = "is_white",
  curves_to_plot = 
    to_plot %>% 
    filter(positive_estimate == TRUE) %>%
     pull(formatted_coefficient)
)

ggsave(
  path = "./results",
  filename = "coefplot_death_white_most_likely_to_die.pdf",
  device = "pdf",
  width = 20,
  height = 14,
  dpi = 640
)

plot_coefplot_black(
  regression_model = model_raca_ao_longo_tempo,
  coefficientName = "is_white",
  curves_to_plot = 
    to_plot %>% 
    filter(positive_estimate == FALSE) %>%
    filter(!str_detect(formatted_coefficient, "(eye|ear)")) %>%
    pull(formatted_coefficient)
)

ggsave(
  path = "./results",
  filename = "coefplot_death_white_least_likely_to_die.pdf",
  device = "pdf",
  width = 18,
  height = 12,
  dpi = 320
)


# Regressions (Escolaridade) -------------------------------------------------------------

table(toRegress$def_escol)

to_regress_escolaridade = 
  toRegress %>%
  filter(def_escol != "Ignorado")  %>% 
  mutate(def_escol = case_when(
    def_escol %in% c("None", "1 to 3 years", "4 to 7 years") ~ "Elementary School", #tive de fazer essa gambiarra para balancear os dados. com as populacoes por escolaridade, não há necessidade de fazer isso
    TRUE ~ "High School or Higher Education",
  )) %>%
  mutate(is_well_educated = def_escol == "High School or Higher Education")

table(to_regress_escolaridade$is_well_educated)

model_baseline1_escolaridade = 
  feols(
    data = 
      to_regress_escolaridade 
    ,fml = log(1+number_deaths) ~  
      is_well_educated |
      ano_obito^CODMUNRES^causabas_capitulo^discrete_idade_obito^def_sexo^def_raca_cor
    ,fixef.rm = "both"
    ,cluster = ~ CODMUNRES + causabas_capitulo
  )


model_baseline2_escolaridade = 
  feols(
    data = 
      to_regress_escolaridade
    ,fml = log(1+number_deaths) ~  
      causabas_capitulo*is_well_educated - is_well_educated |
      ano_obito^CODMUNRES^discrete_idade_obito^def_sexo^def_raca_cor + causabas_capitulo
    ,fixef.rm = "both"
    ,cluster = ~ CODMUNRES + causabas_capitulo
  )


etable(model_baseline1_escolaridade, model_baseline2_escolaridade)

# Joga o output desse commando no Latex/Overleaf e ajeita por ali. Depois, pega a imagem e coloca no word. 
etable(model_baseline1_escolaridade, model_baseline2_escolaridade,
       tex = T,
       fixef_sizes = T,
       digits.stats = 4)


coeftable = coeftable(model_baseline2_escolaridade) 

to_plot = 
  coeftable %>% 
  as.data.table() %>% 
  mutate(coefficient = rownames(coeftable)) %>% 
  janitor::clean_names() %>% 
  mutate(
    formatted_coefficient = str_match(coefficient, "causabas_capitulo.+\\.[ ]*(.*):.*")[, 2]
  ) %>%
  mutate(coefficient = str_replace_all(coefficient, "(causabas_capitulo|:is_well_educatedTRUE)", "")) %>% 
  arrange(estimate) %>% 
  mutate(rank = row_number()) %>% 
  mutate(
    formatted_coefficient = factor(formatted_coefficient,
                                   ordered = T,
                                   .$formatted_coefficient[.$rank])
  ) %>% 
  mutate(positive_estimate = estimate >= 0)

to_plot %>% 
  ggplot(aes(x = formatted_coefficient, y = estimate)) +
  geom_errorbar(aes(ymin=estimate-1.96*std_error,
                    ymax=estimate+1.96*std_error,
                    color = positive_estimate)
  ) +
  geom_point(aes(color = positive_estimate), size = 2.8) + 
  scale_y_continuous(label = scales::percent_format(accuracy = 1), 
                     breaks = scales::pretty_breaks())  +
  coord_flip() +
  labs(y = "",
       x = "",
       color = "Dep. variable") + 
  scale_color_brewer(palette = "Set1", direction = -1) +  
  geom_hline(yintercept = 0, linetype = "dashed") + 
  # viridis::scale_color_viridis(discrete = T, option = "B") + 
  ggthemes::theme_clean(base_size = 22)  +
  theme(
    legend.position = "hide",
    plot.background = element_blank()
  )  

ggsave(
  path = "./results",
  filename = "dotplot_death_educated_vs_non-educated.pdf",
  device = "pdf",
  width = 18,
  height = 12,
  dpi = 640
)

model_education_ao_longo_tempo = 
  feols(
    data = 
      to_regress_escolaridade
    ,fml = log(1+number_deaths) ~  
      i(ano_obito, is_well_educated) - is_well_educated |
      ano_obito^CODMUNRES^discrete_idade_obito^def_sexo^def_raca_cor
    ,cluster = ~ CODMUNRES
    # ,fixef.rm = "both"
    ,split = ~ causabas_capitulo
  )


plot_coefplot_black(
  regression_model = model_education_ao_longo_tempo,
  coefficientName = "is_well_educated",
  curves_to_plot = 
    to_plot %>%
    filter(positive_estimate == FALSE) %>%
    filter(!str_detect(formatted_coefficient, "(eye|ear|conditions)")) %>% 
    pull(formatted_coefficient)
)

ggsave(
  path = "./results",
  filename = "coefplot_death_wellEducated_most_likely_to_die.pdf",
  device = "pdf",
  width = 20,
  height = 15,
  dpi = 320
)
