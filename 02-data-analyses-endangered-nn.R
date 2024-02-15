source("00-preamble.R")

# load data
dt <-  read_csv("Data/seed-redlist-data-complete-v2.csv")

# how many endangered vs. native non-endangered vs. neophytes have data
dt <- dt %>%
  mutate(threatened = ifelse(threatened == "yes", "endangered", "non-endangered")) %>%
  mutate(species_cat = ifelse(native == "native",
                              paste(native, threatened), "non-native"))

names(dt)
navsnona <- dt %>%
  group_by(species_cat) %>%
  summarize(
    count_seedmass_na = sum(is.na(seedmass)),
    count_seedmass_data = sum(!is.na(seedmass)),
    count_dispersal_mode_na = sum(is.na(dispersal_mode)),
    count_dispersal_mode_data = sum(!is.na(dispersal_mode)),
    count_dispersal_distance_na = sum(is.na(dispersal_distance)),
    count_dispersal_distance_data = sum(!is.na(dispersal_distance)),
    count_germination_freq_na = sum(is.na(germination_freq_mean)),
    count_germination_freq_data = sum(!is.na(germination_freq_mean)),
    count_termvel_na = sum(is.na(termvel_mean)),
    count_termvel_data = sum(!is.na(termvel_mean)),
    count_most_frequent_seed_str_na = sum(is.na(`No appendages`)),
    count_most_frequent_seed_str_data = sum(!is.na(`No appendages`)),
    count_most_frequent_seedbank_type_na = sum(is.na(most_frequent_seedbank_type)),
    count_most_frequent_seedbank_type_data = sum(!is.na(most_frequent_seedbank_type)),
    count_mean_seedbank_index_na = sum(is.na(mean_seedbank_index)),
    count_mean_seedbank_index_data = sum(!is.na(mean_seedbank_index))
  ) %>%
  pivot_longer(cols = starts_with("count_"),
               names_to = "variable",
               values_to = "count") %>%  mutate(
                 na_non_na = ifelse(str_detect(variable, "_na$"), "NA", "Data"),
                 variable = sub("_na$|_data$", "", variable)
               )

# plot
# create labels
facet_labels <- c(
  "count_seedmass" = "Seed mass",
  "count_dispersal_mode" = "Dispersal mode",
  "count_dispersal_distance" = "Dispersal distance",
  "count_germination_freq" = "Germination rate",
  "count_termvel" = "Terminal velocity",
  "count_most_frequent_seed_str" = "Seed structure",
  "count_most_frequent_seedbank_type" = "Seedbank type",
  "count_mean_seedbank_index" = "Seedbank index"
)


# display percentages alongside
total_counts <- navsnona %>%
  group_by(species_cat, variable) %>%
  summarize(total_count = sum(count)) %>% ungroup

# calculate percentages and join with the data
navsnona <- navsnona %>%
  left_join(total_counts, by = c("species_cat", "variable")) %>%
  mutate(percentage = count / total_count * 100)

# specify panel and axis order
custom_order <- c("native non-endangered", "native endangered", 
                  "non-native")
navsnona$species_cat <- factor(navsnona$species_cat, levels = custom_order)

custom_order2 <- c("count_seedmass",
                   "count_termvel",
                   "count_dispersal_distance",
                   "count_germination_freq",
                   "count_dispersal_mode",
                   "count_most_frequent_seed_str",
                   "count_most_frequent_seedbank_type")
navsnona$variable <- factor(navsnona$variable, levels = custom_order2)

navsnona <- left_join(navsnona,
                      navsnona %>%
                        group_by(species_cat, variable) %>%
                        summarize(percentage_text = paste0(paste0(round(percentage, 0), "%"),
                                                           collapse = " vs. "), " vs. ", sum(percentage)) %>% 
                        select(species_cat, variable, percentage_text)
)

# plot
navsnona %>% 
  filter(variable != "count_mean_seedbank_index") %>% 
ggplot(aes(x = species_cat, y = count, fill = na_non_na)) +
  geom_chicklet(position = ggplot2::position_stack(reverse = FALSE), col = "gray60") +
  geom_text(aes(label = percentage_text, x = species_cat, y = 2600), family = "Arial Narrow",
            size = 4, fontface = "italic", check_overlap = T, col = "gray60") +
  coord_flip() +
  facet_wrap(vars(variable),
             labeller = labeller(variable = as_labeller(facet_labels))) +
  scale_fill_manual(values = c("NA" = "#f6f6f6", "Data" = "gray60")) +
  scale_y_continuous(limits = c(0,2900), breaks = c(0, 1000, 2000)) +
  labs(title = "Data availability per trait",
       x = "",
       y = "Number of species",
       fill = "NA vs Data") +
  theme_ipsum(grid = "", 
              axis_title_size = 14,
              axis_text_size = 14,
              strip_text_size = 14,
              strip_text_face = "italic",
              plot_title_size = 18,
              axis_title_just = "mm") +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size= 14),
        plot.margin = margin(0, 0, 0, 0),
        panel.spacing = unit(0, "lines")) +
  guides(fill = guide_legend(reverse = TRUE))

# save plot
showtext_opts(dpi=600)
ggsave(width = 11, height = 7, bg = "white",
         file = "Figures/fig0.png",
         dpi = 600)
showtext_opts(dpi=96)

  


# seed weight -------------------------------------------------------------

(dt %>% filter(!is.na(seedmass)) %>% 
ggplot(aes(species_cat, y = seedmass, 
               fill = species_cat)) +
  geom_violinhalf(position = position_nudge(x = 0.15, y = 0)) +
  geom_point(aes(col = species_cat), position = position_jitter(width = 0.05), 
             size = .5)   +
  geom_boxplot(
    outlier.shape = NA,
    width = 0.2,
    alpha = 0.3
  ) +
  stat_summary(
    fun = "mean",
    geom = "point",
    shape = 21,      
    size = 2,
    fill = "white",
    color = "#030708", 
    position = position_dodge(width = 0.2)  # Dodge points for each category
  ) +
  stat_n_text(size = 4, fontface = 3, y.pos = 4, vjust =.5, 
              family = "Arial Narrow", color = "gray40") +
  theme_ipsum(
    grid = "",
    axis_title_size = 14,
    axis_text_size = 14,
    strip_text_size = 14,
    strip_text_face = "italic",
    plot_title_size = 14,
    axis_title_just = "mm"
  ) +
  scale_y_log10()+
  theme(
    legend.position = "none",
    legend.title = element_blank() ,
    axis.line.x = element_blank(),
    axis.text.x = element_blank()) +
  labs(y = "Seed mass (mg)", 
       x = "",
       title = "Seed mass") +
  scale_fill_manual(values = c(`native non-endangered` = "#e8e9eb" , 
                               `native endangered` = "#fbcbe0" , 
                               `non-native` = "#adffe4" )) +
  scale_color_manual(values = c(`native non-endangered` = "#e8e9eb" , 
                                `native endangered` = "#fbcbe0" , 
                                `non-native` = "#adffe4" )) -> fig_sw)


  
mod_sm <- lm(log10(seedmass) ~ species_cat, data = dt)
summary(mod_sm)
emmeans(mod_sm, pairwise ~ species_cat, type = "response")

# check if there is an influence of phylogeny by including family, genera
# as random effect
dt <- dt %>% mutate(genus = word(taxon_name, 1)) %>% left_join(plant_lookup())

mod_sm <- lmer(log10(seedmass) ~ species_cat + (1|family), data = dt)
summary(mod_sm)
(emmeans(mod_sm, pairwise ~ species_cat, type = "response")[[2]] %>% plot() +
  geom_vline(xintercept = 1) +
  theme_ipsum(
    grid = "",
    axis_title_size = 14,
    axis_text_size = 14,
    strip_text_size = 14,
    axis_title_just = "mm"
  ) +
  labs(y = "", x = "Log-ratio of seed weight") -> fig_sw_supp)
  
# terminal velocity -------------------------------------------------------

(dt %>% filter(!is.na(termvel_mean)) %>% 
ggplot(aes(species_cat, y = termvel_mean, 
               fill = species_cat)) +
  geom_violinhalf(position = position_nudge(x = 0.15, y = 0)) +
  geom_point(aes(col = species_cat), position = position_jitter(width = 0.05), 
             size = .5)   +
  geom_boxplot(
    outlier.shape = NA,
    width = 0.2,
    alpha = 0.3
  ) +
  stat_summary(
    fun = "mean",
    geom = "point",
    shape = 21,      
    size = 2,
    fill = "white",
    color = "#030708", 
    position = position_dodge(width = 0.2)  # Dodge points for each category
  ) +
  stat_n_text(size = 4, fontface = 3, y.pos = 6, vjust =.5, 
              family = "Arial Narrow",
              color = "gray40") +
  theme_ipsum(
    grid = "",
    axis_title_size = 14,
    axis_text_size = 14,
    plot_title_size = 14,
    axis_title_just = "mm"
  ) +
  theme(
    legend.position = "none",
    legend.title = element_blank() ,
    axis.line.x = element_blank(),
    axis.text.x = element_blank()) +
  labs(y = "Terminal velocity (m/s)", 
       x = "",
       title = "Terminal velocity") +
  scale_fill_manual(values = c(`native non-endangered` = "#e8e9eb" , 
                               `native endangered` = "#fbcbe0" , 
                               `non-native` = "#adffe4" )) +
  scale_color_manual(values = c(`native non-endangered` = "#e8e9eb" , 
                                `native endangered` = "#fbcbe0" , 
                                `non-native` = "#adffe4" )) -> fig_tv)


mod_tv <- lm(termvel_mean ~ species_cat, data = dt)
summary(mod_tv)
emmeans(mod_tv, pairwise ~ species_cat, type = "response")

# check if there is an influence of phylogeny by including family, genera
# as random effect
mod_tv <- lmer(termvel_mean ~ species_cat + (1|family), data = dt)
summary(mod_tv)
(emmeans(mod_tv, pairwise ~ species_cat, type = "response")[[2]] %>% plot() +
  geom_vline(xintercept = 0) +
  theme_ipsum(
    grid = "",
    axis_title_size = 14,
    axis_text_size = 14,
    strip_text_size = 14,
    axis_title_just = "mm"
  ) +
  labs(y = "", x = "Difference in terminal velocity (m/s)") -> fig_tv_supp)

# dispersal distance ------------------------------------------------------

(dt %>% filter(!is.na(dispersal_distance)) %>% 
ggplot(aes(species_cat, y = dispersal_distance, 
               fill = species_cat)) +
  geom_violinhalf(position = position_nudge(x = 0.15, y = 0)) +
  geom_point(aes(col = species_cat), position = position_jitter(width = 0.05,
                                                                height =0.1), 
             size = .5)   +
  geom_boxplot(
    outlier.shape = NA,
    width = 0.2,
    alpha = 0.3
  ) +
  stat_summary(
    fun = "mean",
    geom = "point",
    shape = 21,      
    size = 2,
    fill = "white",
    color = "#030708", 
    position = position_dodge(width = 0.2)  # Dodge points for each category
  ) +
  stat_n_text(size = 4, fontface = 3, y.pos = 7, vjust =.5, 
              family = "Arial Narrow", color = "gray40") +
  theme_ipsum(
    grid = "",
    axis_title_size = 14,
    axis_text_size = 14,
    plot_title_size = 14,
    axis_title_just = "mm"
  ) +
  theme(
    legend.position = "none",
    legend.title = element_blank() ,
    axis.line.x = element_blank(),
    axis.text.x = element_blank()) +
  labs(y = "Distance class", 
       x = "",
       title = "Dispersal distance") +
  scale_fill_manual(values = c(`native non-endangered` = "#e8e9eb" , 
                               `native endangered` = "#fbcbe0" , 
                               `non-native` = "#adffe4" )) +
  scale_color_manual(values = c(`native non-endangered` = "#e8e9eb" , 
                                `native endangered` = "#fbcbe0" , 
                                `non-native` = "#adffe4" )) -> fig_dd)



mod_dd <- lm(dispersal_distance ~ species_cat, data = dt)
summary(mod_dd)
emmeans(mod_dd, pairwise ~ species_cat, type = "response")

# check if there is an influence of phylogeny by including family, genera
# as random effect
mod_dd <- lmer(dispersal_distance ~ species_cat + (1|family), data = dt)
summary(mod_dd)
(emmeans(mod_dd, pairwise ~ species_cat, type = "response")[[2]] %>% plot() +
  geom_vline(xintercept = 0) +
  theme_ipsum(
    grid = "",
    axis_title_size = 14,
    axis_text_size = 14,
    strip_text_size = 14,
    axis_title_just = "mm"
  ) +
  labs(y = "", x = "Difference in dispersal distance") -> fig_dd_supp)

# germination frequency ---------------------------------------------------

custom_order <- c("native non-endangered", "native endangered", 
                  "non-native")
dt$species_cat <- factor(dt$species_cat, levels = custom_order)

(dt %>% filter(!is.na(germination_freq_mean)) %>% 
    ggplot(aes(species_cat, y = germination_freq_mean, 
               fill = species_cat)) +
    geom_violinhalf(position = position_nudge(x = 0.15, y = 0)) +
    geom_point(aes(col = species_cat), position = position_jitter(width = 0.05), 
               size = .5)   +
    geom_boxplot(
      outlier.shape = NA,
      width = 0.2,
      alpha = 0.3
    ) +
    stat_summary(
      fun = "mean",
      geom = "point",
      shape = 21,      
      size = 2,
      fill = "white",
      color = "#030708", 
      position = position_dodge(width = 0.2)  # Dodge points for each category
    ) +
    stat_n_text(size = 4, fontface = 3, y.pos = 1, vjust =.5, 
                family = "Arial Narrow", color = "gray40") +
    theme_ipsum(
      grid = "",
      axis_title_size = 14,
      axis_text_size = 14,
      plot_title_size = 14,
      axis_title_just = "mm"
    ) +
    theme(
      legend.position = "none",
      legend.title = element_blank() ,
      axis.line.x = element_blank(),
      axis.text.x = element_blank()) +
    labs(y = "Germination rate (%)", 
         x = "",
         title = "Germination rate") +
    scale_fill_manual(values = c(`native non-endangered` = "#e8e9eb" , 
                                 `native endangered` = "#fbcbe0" , 
                                 `non-native` = "#adffe4" )) +
    scale_color_manual(values = c(`native non-endangered` = "#e8e9eb" , 
                                  `native endangered` = "#fbcbe0" , 
                                  `non-native` = "#adffe4" )) -> fig_germ)


mod_germ <- lm(germination_freq_mean ~ species_cat, data = dt)
summary(mod_germ)
emmeans(mod_germ, pairwise ~ species_cat)

# check if there is an influence of phylogeny by including family, genera
# as random effect
mod_germ <- lmer(germination_freq_mean ~ species_cat + (1|family), data = dt)
summary(mod_germ)
(emmeans(mod_germ, pairwise ~ species_cat)[[2]] %>% plot() +
  geom_vline(xintercept = 0) +
  theme_ipsum(
    grid = "",
    axis_title_size = 14,
    axis_text_size = 14,
    strip_text_size = 14,
    axis_title_just = "mm"
  ) +
  labs(y = "", x = "Difference in germination rate (%)") -> fig_germ_supp)


# dispersal mode ----------------------------------------------------------

# to make this visually interpretable display proportion of species in each
# of these classes per native/endangered
dispmo <- dt %>% 
  filter(!is.na(dispersal_mode)) %>%
  group_by(species_cat) %>% 
  count(dispersal_mode)
totals <- dt %>% 
  filter(!is.na(dispersal_mode)) %>%
  count(species_cat) %>% 
  rename(total = n)
dispmo <- left_join(dispmo, totals) %>% mutate(prop = n/total)


# axis order
custom_order <- c("native non-endangered", "native endangered", 
                  "non-native")
dispmo$species_cat <- factor(dispmo$species_cat, levels = custom_order)

# facet order
custom_order2 <- c("Local non-specific dispersal",
                   "Anemochory",
                   "Myrmecochory",
                  "Endozoochory",
                  "Epizoochory",
                  "Hydrochory",
                  "Dyszoochory") # removed later on, because this is so few species
dispmo$dispersal_mode <- factor(dispmo$dispersal_mode, levels = custom_order2)

dispmo %>%
  filter(dispersal_mode == "Dyszoochory") 

dispmo <- dispmo %>%
  filter(dispersal_mode != "Dyszoochory")

# plot
(dispmo %>%
  ggplot(aes(x = species_cat, y = prop, fill = species_cat)) +
  facet_wrap(. ~ dispersal_mode, ncol = 1) +
  geom_chicklet(col = "#030708", radius = unit(2, "pt"),
                position = ggplot2::position_stack(reverse = FALSE)) +
    geom_text(aes(label = 
                    paste(
                      paste0(round(prop*100, 0), "%"),
                      paste0("(n=", n, ")")
                    ), y = prop, x = species_cat),   size = 3.5,
              position = position_dodge(0.9),
              hjust = -0.1,
              col = "gray40",
              family = "Arial Narrow", fontface = "italic") +
  scale_y_continuous(limits = c(0,0.8)) +
  coord_flip() +
  theme_ipsum(
    grid = "",
    axis_title_size = 14,
    axis_text_size = 14,
    strip_text_size = 14,
    strip_text_face = "italic",
    plot_title_size = 14,
    axis_title_just = "mm"
  ) +
    scale_fill_manual(values = c(`native non-endangered` = "#e8e9eb" , 
                                 `native endangered` = "#fbcbe0" , 
                                 `non-native` = "#adffe4" ))  +
  theme(legend.position = "none",
        legend.title = element_blank()) +
  labs(x = "", y = "",
       title = "Dispersal mode") -> fig_dispmo)


# Create a contingency table
contingency_table  <- dispmo %>% select(species_cat, dispersal_mode, n) %>%
  pivot_wider(names_from = species_cat, values_from = n) %>% 
  column_to_rownames(var = colnames(.[1])) %>%
  as.matrix()
contingency_table 


# endangered vs non-endangered
comp <- NULL
for (i in 1:nrow(contingency_table)){
test <- prop.test(x = c(contingency_table[i,1], contingency_table[i,2]), 
          n = c(
          colSums(contingency_table)[1], 
          colSums(contingency_table)[2]))
pval <- test$p.value
chisq <- test$statistic
mode <- row.names(contingency_table)[i]
comp[[i]] <- data.frame(pval = pval, chisq = chisq, mode= mode)
}
enat <- bind_rows(comp)
# endangered vs non-native
comp <- NULL
for (i in 1:nrow(contingency_table)){
  test <- prop.test(x = c(contingency_table[i,2], contingency_table[i,3]), 
                    n = c(
                      colSums(contingency_table)[2], 
                      colSums(contingency_table)[3]))
  pval <- test$p.value
  chisq <- test$statistic
  mode <- row.names(contingency_table)[i]
  comp[[i]] <- data.frame(pval = pval, chisq = chisq, mode= mode)
}
enon <- bind_rows(comp)
# non-native vs non-endangered
comp <- NULL
for (i in 1:nrow(contingency_table)){
  test <- prop.test(x = c(contingency_table[i,1], contingency_table[i,3]), 
                    n = c(
                      colSums(contingency_table)[1], 
                      colSums(contingency_table)[3]))
  pval <- test$p.value
  chisq <- test$statistic
  mode <- row.names(contingency_table)[i]
  comp[[i]] <- data.frame(pval = pval, chisq = chisq, mode= mode)
}
natnon <- bind_rows(comp)

comps <- bind_rows(list(
  enat %>% mutate(comp = "endangered vs non-endangered"),
  enon  %>% mutate(comp = "endangered vs non-native"),
  natnon %>% mutate(comp = "non-native vs non-endangered")
))
row.names(comps)<- NULL
options(scipen = 999)
comp <- comps %>% arrange(mode) %>% 
  mutate(pval = round(pval, 5), chisq = round(chisq, 3)) %>% 
  select(`Dispersal mode` = mode, Comparison = comp, `Chi square` = chisq, `P value` = pval)
comp

write_csv(comp, "Data/dispersal_mode_comp.csv")


# seed structure ----------------------------------------------------------

sestr <- bind_rows(
  list(
    dt %>% 
      filter(!is.na(`No appendages`)) %>% 
      group_by(species_cat) %>% 
      count(`No appendages`) %>% 
      pivot_longer(`No appendages`, names_to = "seed_str", values_to = "present") %>% 
      pivot_wider(values_from = n, names_from = present),
    dt %>% 
      filter(!is.na(`Elongated appendages`)) %>% 
      group_by(species_cat) %>% 
      count(`Elongated appendages`) %>% 
      pivot_longer(`Elongated appendages`, names_to = "seed_str", values_to = "present") %>% 
      pivot_wider(values_from = n, names_from = present),
    dt %>% 
      filter(!is.na(`Flat appendages`)) %>% 
      group_by(species_cat) %>% 
      count(`Flat appendages`) %>% 
      pivot_longer(`Flat appendages`, names_to = "seed_str", values_to = "present") %>% 
      pivot_wider(values_from = n, names_from = present),
    dt %>% 
      filter(!is.na(`Balloon structures`)) %>% 
      group_by(species_cat) %>% 
      count(`Balloon structures`) %>% 
      pivot_longer(`Balloon structures`, names_to = "seed_str", values_to = "present") %>% 
      pivot_wider(values_from = n, names_from = present),
    dt %>% 
      filter(!is.na(`Nutrient containing structures`)) %>% 
      group_by(species_cat) %>% 
      count(`Nutrient containing structures`) %>% 
      pivot_longer(`Nutrient containing structures`, names_to = "seed_str", values_to = "present") %>% 
      pivot_wider(values_from = n, names_from = present)
  )
) %>% mutate(total = No + Yes,
             prop = Yes/total)

# axis order
custom_order <- c("native non-endangered", "native endangered", 
                  "non-native")
sestr$species_cat <- factor(sestr$species_cat, levels = custom_order)

# facet order
custom_order2 <- c(
  "No appendages",
  "Elongated appendages",
  "Balloon structures",
  "Flat appendages",
  "Nutrient containing structures")
sestr$seed_str <- 
  factor(sestr$seed_str, levels = custom_order2)

# plot
(sestr %>%
  ggplot(aes(x = species_cat, y = prop, fill = species_cat)) +
  facet_wrap(. ~ seed_str, ncol = 1) +
  geom_chicklet(col = "#030708", radius = unit(2, "pt"),
                position = ggplot2::position_stack(reverse = FALSE)) +
  geom_text(aes(label = 
                  paste(
                  paste0(round(prop*100, 0), "%"),
                  paste0("(n=", Yes, ")")
                  ), y = prop, x = species_cat), size = 3.5,
            position = position_dodge(0.9),
            hjust = -0.1,
            col = "gray40",
            family = "Arial Narrow", fontface = "italic") +
  scale_y_continuous(limits = c(0,0.8)) +
  coord_flip() +
  theme_ipsum(
    grid = "",
    axis_title_size = 14,
    axis_text_size = 14,
    strip_text_size = 14,
    strip_text_face = "italic",
    plot_title_size = 14,
    axis_title_just = "mm"
  ) +
    scale_fill_manual(values = c(`native non-endangered` = "#e8e9eb" , 
                                 `native endangered` = "#fbcbe0" , 
                                 `non-native` = "#adffe4" ))  +
  theme(legend.position = "none",
        legend.title = element_blank() ) +
  labs(x = "", 
       y = "Proportion of species",
       title = "Seed structures") -> fig_sestr)


# Create a contingency table
contingency_table  <- sestr %>% select(species_cat, seed_str, n = Yes) %>%
  pivot_wider(names_from = species_cat, values_from = n) %>% 
  column_to_rownames(var = colnames(.[1])) %>%
  as.matrix()
contingency_table 


# endangered vs non-endangered
comp <- NULL
for (i in 1:nrow(contingency_table)){
  test <- prop.test(x = c(contingency_table[i,1], contingency_table[i,2]), 
                    n = c(
                      colSums(contingency_table)[1], 
                      colSums(contingency_table)[2]))
  pval <- test$p.value
  chisq <- test$statistic
  mode <- row.names(contingency_table)[i]
  comp[[i]] <- data.frame(pval = pval, chisq = chisq, mode= mode)
}
enat <- bind_rows(comp)
# endangered vs non-native
comp <- NULL
for (i in 1:nrow(contingency_table)){
  test <- prop.test(x = c(contingency_table[i,2], contingency_table[i,3]), 
                    n = c(
                      colSums(contingency_table)[2], 
                      colSums(contingency_table)[3]))
  pval <- test$p.value
  chisq <- test$statistic
  mode <- row.names(contingency_table)[i]
  comp[[i]] <- data.frame(pval = pval, chisq = chisq, mode= mode)
}
enon <- bind_rows(comp)
# non-native vs non-endangered
comp <- NULL
for (i in 1:nrow(contingency_table)){
  test <- prop.test(x = c(contingency_table[i,1], contingency_table[i,3]), 
                    n = c(
                      colSums(contingency_table)[1], 
                      colSums(contingency_table)[3]))
  pval <- test$p.value
  chisq <- test$statistic
  mode <- row.names(contingency_table)[i]
  comp[[i]] <- data.frame(pval = pval, chisq = chisq, mode= mode)
}
natnon <- bind_rows(comp)

comps <- bind_rows(list(
  enat %>% mutate(comp = "endangered vs non-endangered"),
  enon  %>% mutate(comp = "endangered vs non-native"),
  natnon %>% mutate(comp = "non-native vs non-endangered")
))
row.names(comps)<- NULL
options(scipen = 999)
comp <- comps %>% arrange(mode) %>% 
  mutate(pval = round(pval, 5), chisq = round(chisq, 3)) %>% 
  select(`Seed structure` = mode, Comparison = comp, `Chi square` = chisq, `P value` = pval)
comp

write_csv(comp, "Data/seedstr_comp.csv")



# most_frequent_seedbank_type ---------------------------------------------

sbank <- dt %>% 
  filter(!is.na(most_frequent_seedbank_type)) %>% 
  group_by(species_cat) %>% 
  count(most_frequent_seedbank_type)
totals <- dt %>% 
  filter(!is.na(most_frequent_seedbank_type)) %>% 
  count(species_cat) %>% 
  rename(total = n)
sbank <- left_join(sbank, totals) %>% mutate(prop = n/total)

# axis order
custom_order <- c("native non-endangered", "native endangered", 
                  "non-native")
sbank$species_cat <- factor(sbank$species_cat, levels = custom_order)

# facet order
custom_order2 <- c(
  "transient",
  "short-term persistent",
  "long-term persistent",
  "present")
sbank$most_frequent_seedbank_type <- 
  factor(sbank$most_frequent_seedbank_type, levels = custom_order2)

# plot
(sbank %>%
    ggplot(aes(x = species_cat, y = prop, fill = species_cat)) +
    facet_wrap(. ~ most_frequent_seedbank_type, ncol = 1) +
    geom_chicklet(col = "#030708", radius = unit(2, "pt"),
                  position = ggplot2::position_stack(reverse = FALSE)) +
    geom_text(aes(label = 
                    paste(
                      paste0(round(prop*100, 0), "%"),
                      paste0("(n=", n, ")")
                    ), y = prop, x = species_cat),   size = 3.5,
              position = position_dodge(0.9),
              hjust = -0.1,
              col = "gray40",
              family = "Arial Narrow", fontface = "italic") +
    scale_y_continuous(limits = c(0,1)) +
    coord_flip() +
    theme_ipsum(
      grid = "",
      axis_title_size = 14,
      axis_text_size = 14,
      strip_text_size = 14,
      strip_text_face = "italic",
      plot_title_size = 14,
      axis_title_just = "mm"
    ) +
    scale_fill_manual(values = c(`native non-endangered` = "#e8e9eb" , 
                                 `native endangered` = "#fbcbe0" , 
                                 `non-native` = "#adffe4" ))  +
    theme(legend.position = "none",
          legend.title = element_blank()) +
    labs(x = "", 
         y = "",
         title = "Seedbank type") -> fig_seedb)

# Create a contingency table
contingency_table  <- sbank %>% select(species_cat, most_frequent_seedbank_type, n) %>%
  pivot_wider(names_from = species_cat, values_from = n) %>% 
  column_to_rownames(var = colnames(.[1])) %>%
  as.matrix()
contingency_table 


# endangered vs non-endangered
comp <- NULL
for (i in 1:nrow(contingency_table)){
  test <- prop.test(x = c(contingency_table[i,1], contingency_table[i,2]), 
                    n = c(
                      colSums(contingency_table)[1], 
                      colSums(contingency_table)[2]))
  pval <- test$p.value
  chisq <- test$statistic
  mode <- row.names(contingency_table)[i]
  comp[[i]] <- data.frame(pval = pval, chisq = chisq, mode= mode)
}
enat <- bind_rows(comp)
# endangered vs non-native
comp <- NULL
for (i in 1:nrow(contingency_table)){
  test <- prop.test(x = c(contingency_table[i,2], contingency_table[i,3]), 
                    n = c(
                      colSums(contingency_table)[2], 
                      colSums(contingency_table)[3]))
  pval <- test$p.value
  chisq <- test$statistic
  mode <- row.names(contingency_table)[i]
  comp[[i]] <- data.frame(pval = pval, chisq = chisq, mode= mode)
}
enon <- bind_rows(comp)
# non-native vs non-endangered
comp <- NULL
for (i in 1:nrow(contingency_table)){
  test <- prop.test(x = c(contingency_table[i,1], contingency_table[i,3]), 
                    n = c(
                      colSums(contingency_table)[1], 
                      colSums(contingency_table)[3]))
  pval <- test$p.value
  chisq <- test$statistic
  mode <- row.names(contingency_table)[i]
  comp[[i]] <- data.frame(pval = pval, chisq = chisq, mode= mode)
}
natnon <- bind_rows(comp)

comps <- bind_rows(list(
  enat %>% mutate(comp = "endangered vs non-endangered"),
  enon  %>% mutate(comp = "endangered vs non-native"),
  natnon %>% mutate(comp = "non-native vs non-endangered")
))
row.names(comps)<- NULL
options(scipen = 999)
comp <- comps %>% arrange(mode) %>% 
  mutate(pval = round(pval, 5), chisq = round(chisq, 3)) %>% 
  select(`Seedbank type` = mode, Comparison = comp, `Chi square` = chisq, `P value` = pval)
comp

write_csv(comp, "Data/seedbank_comp.csv")


# create multiplot --------------------------------------------------------
# customize theme for each plot
(fig_dispmo <- fig_dispmo + theme(plot.margin = margin(0, 0, 0, 0),
                                 panel.spacing = unit(0, "lines"), 
                                 axis.text.y = element_blank(),
                                 plot.tag = element_text(face = "bold", 
                                                         family = "Arial Narrow")))

(fig_sestr <- fig_sestr + theme(plot.margin = margin(0, 0, 0, 0),
                                  panel.spacing = unit(0, "lines"), 
                                  axis.text.y = element_blank(),
                                  plot.tag = element_text(face = "bold", 
                                                          family = "Arial Narrow")))

(fig_seedb <- fig_seedb + theme(plot.margin = margin(0, 0, 0, 0),
                                panel.spacing = unit(0, "lines"), 
                                axis.text.y = element_blank(),
                                plot.tag = element_text(face = "bold", 
                                                        family = "Arial Narrow")))


(fig_germ <- fig_germ + theme(plot.margin = margin(0, .5, 0, 0),
                                plot.tag = element_text(face = "bold", 
                                                        family = "Arial Narrow")) +
  guides(colour = "none", fill = "none"))

(fig_sw <- fig_sw + theme(plot.margin = margin(0, .5, 0, 0),
                              plot.tag = element_text(face = "bold", 
                                                      family = "Arial Narrow")) +
  guides(colour = "none", fill = "none"))



(fig_tv <- fig_tv + theme(plot.margin = margin(0, .5, 0, 0),
                              plot.tag = element_text(face = "bold", 
                                                      family = "Arial Narrow")) +
  guides(colour = "none", fill = "none"))



(fig_dd <- fig_dd + theme(plot.margin = margin(0, .5, 0, 0),
                              plot.tag = element_text(face = "bold", 
                                                      family = "Arial Narrow")) +
  guides(colour = "none", fill = "none"))




plot_upper <- (fig_sw + fig_tv) / ( fig_dd + fig_germ)
plot_lower <- (fig_dispmo + fig_sestr + fig_seedb) 


plot_upper / plot_lower + plot_layout(heights = c(.5, .5, 2), guides = "collect") +
  plot_annotation(tag_levels = "a", title = "Dispersal ecology of endangered versus native and non-native plants",
                  theme = theme(plot.title = element_text(size = 18, 
                                                          family = "Arial Narrow",
                                                          face = "bold"))) &
  theme(legend.position = "bottom", legend.text=element_text(size=14))



# save plot
showtext_opts(dpi=600)
ggsave(width = 11, height = 11, bg = "white",
       file = "Figures/fig1.png",
       dpi = 600)
showtext_opts(dpi=96)


# supp plots for accounting for phylogeny
fig_tv_supp <- fig_tv_supp + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
fig_germ_supp <- fig_germ_supp + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())


(fig_sw_supp + fig_tv_supp) / (fig_dd_supp + fig_germ_supp) + 
  plot_annotation(title = "Contrasts between endangered, native and non-native plants when accounting for phylogeny", 
                  theme = theme(plot.title = element_text(hjust = 0.5, family = "Arial Narrow", face = 2, size = 14)),
                  tag_levels = 'a') +
  theme(axis.title.y = element_text(angle = 90, vjust = 2, family = "Arial Narrow"))


showtext_opts(dpi=600)
ggsave(width = 8.5, height = 5.45, bg = "white",
       file = "Figures/fig1-supp.png",
       dpi = 600)
showtext_opts(dpi=96)
