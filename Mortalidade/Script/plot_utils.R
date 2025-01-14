require(fixest)
require(tidyverse)


plot_coefplot = function(regression_model, coefficientName, curves_to_plot = c()) {
  
  Sys.setlocale(category = "LC_TIME", locale = "English_United States.1252")
  
  todos_coeficientes = lapply(regression_model, fixest::coeftable)
  names(todos_coeficientes) = names(regression_model)
  
  todos_coeficientes =
    map_df(todos_coeficientes, ~ as.data.frame(.x), .id="id") %>%
    mutate(coefficient_name = rownames(.)) %>%
    mutate(coefficient_name = str_match(coefficient_name, "ano_obito::([0-9]{4}):.*")[, 2]) %>% 
    mutate(coefficient_name = as.integer(coefficient_name)) %>% 
    janitor::clean_names() %>% 
    mutate(causa_base = id) %>% 
    mutate(id = str_match(id, ".*\\.(.*)")[, 2] %>% str_trim(side = "both"))
  
  if(length(curves_to_plot) > 0) { 
    todos_coeficientes =
      todos_coeficientes %>% 
      filter(str_detect(causa_base, paste0("(", paste0(curves_to_plot, collapse = "|"), ")")))
  }
  
  todos_coeficientes %>%
    ggplot(aes(x = coefficient_name, y = estimate)) +
    geom_errorbar(aes(ymin=estimate-1.96*std_error,
                      ymax=estimate+1.96*std_error,
                      color = id),
                  size = 1
    ) +
    geom_point(aes(color = id), size = 2.8) +
    labs(y = "Coefficient value",
         x = "",
         color = "Dep. variable") +
    scale_color_brewer(palette = "Dark2") +  
    ggthemes::theme_clean(base_size = 19) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1e-1), breaks = scales::pretty_breaks(n=5)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n=7)) + 
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.8) + 
    theme(plot.background = element_blank(),
          legend.position = "hide",
          legend.background = element_rect(color = NA),
          aspect.ratio = 3/5,
          legend.margin=margin(t = -1.2, unit='cm')
    ) +
    guides(
      color=guide_legend(ncol=1)
    ) + 
    facet_wrap(~ str_wrap(id, 25), scales = "free")
  
}

plot_coefplot_black = function(regression_model, coefficientName, curves_to_plot = c()) {
  
  Sys.setlocale(category = "LC_TIME", locale = "English_United States.1252")
  
  todos_coeficientes = lapply(regression_model, fixest::coeftable)
  names(todos_coeficientes) = names(regression_model)
  
  todos_coeficientes =
    map_df(todos_coeficientes, ~ as.data.frame(.x), .id="id") %>%
    mutate(coefficient_name = rownames(.)) %>%
    mutate(coefficient_name = str_match(coefficient_name, "ano_obito::([0-9]{4}):.*")[, 2]) %>% 
    mutate(coefficient_name = as.integer(coefficient_name)) %>% 
    janitor::clean_names() %>% 
    mutate(causa_base = id) %>% 
    mutate(id = str_match(id, ".*\\.(.*)")[, 2] %>% str_trim(side = "both"))
  
  if(length(curves_to_plot) > 0) { 
    todos_coeficientes =
      todos_coeficientes %>% 
      filter(str_detect(causa_base, paste0("(", paste0(curves_to_plot, collapse = "|"), ")")))
  }
  
  todos_coeficientes %>%
    ggplot(aes(x = coefficient_name, y = estimate)) +
    geom_errorbar(aes(ymin=estimate-1.96*std_error,
                      ymax=estimate+1.96*std_error),
                  size = 1
    ) +
    geom_point(size = 2.8) +
    labs(y = "Coefficient value",
         x = "",
         color = "Dep. variable") +
    scale_color_brewer(palette = "Dark2") +  
    ggthemes::theme_clean(base_size = 19) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1e-1), breaks = scales::pretty_breaks(n=5)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n=7)) + 
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.8) + 
    theme(plot.background = element_blank(),
          legend.position = "hide",
          legend.background = element_rect(color = NA),
          aspect.ratio = 3/5,
          legend.margin=margin(t = -1.2, unit='cm')
    ) +
    guides(
      color=guide_legend(ncol=1)
    ) + 
    facet_wrap(~ str_wrap(id, 25), scales = "free")
  
}