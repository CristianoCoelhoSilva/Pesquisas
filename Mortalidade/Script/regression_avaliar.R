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

source("plot_utils.R")

model_education_ao_longo_tempo = readRDS("./DADOS/fastloading/model_education_ao_longo_tempo.R")
model_sexo_ao_longo_tempo = readRDS("./DADOS/fastloading/model_sexo_ao_longo_tempo.R")
model_raca_ao_longo_tempo = readRDS("./DADOS/fastloading/model_raca_ao_longo_tempo.R")

model_baseline1_raca = readRDS("./DADOS/fastloading/model_baseline1_raca.R")
model_baseline2_raca = readRDS("./DADOS/fastloading/model_baseline2_raca.R")

model_baseline1_sexo = readRDS("./DADOS/fastloading/model_baseline1_sexo.R")
model_baseline2_sexo = readRDS("./DADOS/fastloading/model_baseline2_sexo.R")

model_baseline1_escolaridade = readRDS("./DADOS/fastloading/model_baseline1_escolaridade.R")
model_baseline2_escolaridade = readRDS("./DADOS/fastloading/model_baseline2_escolaridade.R")

etable(model_sexo_ao_longo_tempo)

etable(m, model_baseline2_raca, tex = T, fixef_sizes = T, digits.stats = 4)

coeftable = coeftable(model_raca_ao_longo_tempo)

to_plot = 
  coeftable %>% 
  as.data.table() %>% 
  mutate(coefficient = rownames(coeftable)) %>% 
  janitor::clean_names() %>% 
  mutate(
    formatted_coefficient = str_match(coefficient, "causabas_capitulo.+\\.[ ]*(.*):.*")[, 2]
  ) %>%
  mutate(coefficient = str_replace_all(coefficient, "(causabas_capitulo|:def_sexoMasculino)", "")) %>% 
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
  labs(y = "Diferencial na prob. de morte (relativo à mulher)",
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

plot_coefplot(
  regression_model = model_raca_ao_longo_tempo,
  coefficientName = "is_male",
  curves_to_plot = 
    to_plot %>% 
    filter(positive_estimate == FALSE) %>%
    filter(!str_detect(formatted_coefficient, "(hem|achad anorm|mentais|apófise|olho|cromoss)")) %>%
    pull(formatted_coefficient)
)

ggsave(
  path = "./results",
  filename = "coefplot_death_male_least_likely_to_die.png",
  device = "png",
  width = 12,
  height = 9.5,
  dpi = 320
)
