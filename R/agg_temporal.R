# agg_temporal <- function(df, nbins){
#   
#   df %>% 
#     mutate(yearbin = cut(year, breaks = nbins, labels = FALSE)) %>%
#     # group_by(id_species_site, yearbin) %>%
#     group_by(id_site, yearbin) %>%
#     summarise(on = mean(on, na.rm = TRUE), off = mean(off, na.rm = TRUE)) %>% 
#     ungroup() %>% 
#     # group_by(id_species_site) %>%
#     group_by(id_site) %>%
#     nest() %>% 
#     mutate(linmod = purrr::map(data, ~lm(off ~ on, data = .))) %>% 
#     mutate(summ = purrr::map(linmod, ~summary(.))) %>% 
#     mutate(df_coef = purrr::map(linmod, ~tidy(.))) %>% 
#     mutate(coef_on = purrr::map_dbl(df_coef, ~get_coef_on(.))) %>% 
#     mutate(p_value_on = purrr::map_dbl(df_coef, ~get_p_on(.))) %>% 
#     select(-linmod) %>% 
#     mutate(rsq = purrr::map_dbl(summ, "r.squared"),
#            adj_rsq = purrr::map_dbl(summ, "adj.r.squared")) %>% 
#     select(-summ)
#   
# }

bins <- c(25, 15, 10, 7, 6, 5, 4, 3, 2)

# list_temporal_longterm <- list()
list_median_coef <- list()
list_gg <- list()

for (ibreaks in bins){
  
  print(paste0("breaks_", ibreaks))

  list_temporal_longterm[[paste0("breaks_", ibreaks)]] <- df_anl %>%
    # mutate(yearbin = ntile(year, n = ibreaks)) %>%
    mutate(yearbin = cut(year, breaks = ibreaks, labels = FALSE)) %>%
    # group_by(id_species_site, yearbin) %>%
    group_by(id_site, yearbin) %>%
    summarise(on = mean(on, na.rm = TRUE), off = mean(off, na.rm = TRUE)) %>%
    ungroup() %>%
    # group_by(id_species_site) %>%
    group_by(id_site) %>%
    nest() %>%
    mutate(linmod = purrr::map(data, ~lm(off ~ on, data = .))) %>%
    mutate(summ = purrr::map(linmod, ~summary(.))) %>%
    mutate(df_coef = purrr::map(linmod, ~tidy(.))) %>%
    mutate(coef_on = purrr::map_dbl(df_coef, ~get_coef_on(.))) %>%
    mutate(p_value_on = purrr::map_dbl(df_coef, ~get_p_on(.))) %>%
    select(-linmod) %>%
    mutate(rsq = purrr::map_dbl(summ, "r.squared"),
           adj_rsq = purrr::map_dbl(summ, "adj.r.squared")) %>%
    select(-summ)
  
  list_median_coef[[paste0("breaks_", ibreaks)]] <- median(list_temporal_longterm[[paste0("breaks_", ibreaks)]]$coef_on, na.rm = TRUE)
  
  list_gg[[paste0("breaks_", ibreaks)]] <- list_temporal_longterm[[paste0("breaks_", ibreaks)]] %>% 
    # filter(p_value_on < 0.05) %>% 
    ggplot(aes(coef_on, ..density..)) +
    geom_histogram() +
    xlim(-20,20) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    geom_vline(xintercept = median(list_temporal_longterm[[paste0("breaks_", ibreaks)]]$coef_on, na.rm = TRUE), color = "red", linetype = "dashed") +
    geom_vline(xintercept = mean(list_temporal_longterm[[paste0("breaks_", ibreaks)]]$coef_on, na.rm = TRUE), color = "red")
  
}

vec_slopes <- c(list_median_coef %>% unlist(), out_modobs$results %>% pull(slope))

tibble(nbins =  c(bins, 1), mean_slope = vec_slopes) %>% 
  ggplot(aes(nbins, mean_slope)) +
  geom_point(size = 2)

save(list_temporal_longterm, file = "../data/list_temporal_longterm.RData")
