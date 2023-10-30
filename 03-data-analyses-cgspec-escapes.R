source("00-preamble.R")


# which endangered species are amenable to gardening ----------------------

cg_spec <- read_csv("Data/german_rl_naturadb.csv") %>% 
  filter(naturadb_common_name == "naturadb")


# which non-natives escaped gardens? --------------------------------------

cultivated <- read_delim("Data/redlist_rothmaler_t.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

nn_esc <- cultivated %>% filter(native == "established non-native") %>% 
  filter(cultivated == "yes")



# bind rows ---------------------------------------------------------------

dx <- bind_rows(
  cg_spec %>% select(species = species_cleaned) %>% 
    mutate(type = "conservation gardening"),
  nn_esc %>% select(species = taxon_name) %>% 
    mutate(type = "non-native escape")
)

dx %>% select(species) %>% group_by(species) %>% count %>% arrange(desc(n))

# tax harmon --------------------------------------------------------------
wcvp_names <- wcvp_names
wcvp_match_dx <- wcvp_match_names(dx, wcvp_names = wcvp_names,
                                  name_col = "species",
                                  fuzzy = T)

wcvp_acc_dx <- wcvp_match_dx %>% select(species, wcvp_status, wcvp_accepted_id)
View(wcvp_acc_dx)

# give preference to accepted (and then synonyms) names, 
# if there are multiple rows for one and 
# the same species
wcvp_acc_dx <- wcvp_acc_dx %>% 
  arrange(desc(wcvp_status %in% c("Accepted", "Synonym")), 
          desc(wcvp_status == "Accepted")) %>%
  distinct(species, .keep_all = TRUE)

names_dx <- wcvp_names %>% select(wcvp_accepted_id = plant_name_id, taxon_name)
wcvp_acc_dx <- left_join(wcvp_acc_dx, names_dx) 

dx_wcvp <- left_join(dx, wcvp_acc_dx)


# join with dispersal data ------------------------------------------------

dt <-  read_csv("Data/seed-redlist-data-complete-v2.csv")

dt <- dx_wcvp %>% select(taxon_name, type) %>% distinct %>% 
  left_join(dt)

# similar analysis as in comparison to all endangered vs nonnative --------


# prepare data
dt <- dt %>% rename(species_cat = type)
dt <- dt %>% mutate(species_cat = ifelse(species_cat == "conservation gardening", "conservation gardening species",
                                         "non-native garden escapees"))

dt %>% count(species_cat)


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
   scale_fill_manual(values = c( `conservation gardening species` = "#4004ae" , 
                                 `non-native garden escapees` = "#ecebed"
   )) +
   scale_color_manual(values = c( `conservation gardening species` = "#4004ae" , 
                                  `non-native garden escapees` = "#9d9aa1"
   )) -> fig2_sw)


mod_sw <- lm(log10(seedmass) ~ species_cat, data = dt)
summary(mod_sw)
emmeans(mod_sw, pairwise ~ species_cat, type = "response")

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
   scale_fill_manual(values = c( `conservation gardening species` = "#4004ae" , 
                                 `non-native garden escapees` = "#ecebed"
   )) +
   scale_color_manual(values = c( `conservation gardening species` = "#4004ae" , 
                                  `non-native garden escapees` = "#9d9aa1"
   )) -> fig2_tv)


mod_tv <- lm(termvel_mean ~ species_cat, data = dt)
summary(mod_tv)
emmeans(mod_tv, pairwise ~ species_cat, type = "response")

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
   scale_fill_manual(values = c( `conservation gardening species` = "#4004ae" , 
                                 `non-native garden escapees` = "#ecebed"
   )) +
   scale_color_manual(values = c( `conservation gardening species` = "#4004ae" , 
                                  `non-native garden escapees` = "#9d9aa1"
   )) -> fig2_dd)


mod_dd <- lm(dispersal_distance ~ species_cat, data = dt)
summary(mod_dd)
emmeans(mod_dd, pairwise ~ species_cat, type = "response")


# germination frequency ---------------------------------------------------

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
   scale_fill_manual(values = c( `conservation gardening species` = "#4004ae" , 
                                 `non-native garden escapees` = "#ecebed"
   )) +
   scale_color_manual(values = c( `conservation gardening species` = "#4004ae" , 
                                  `non-native garden escapees` = "#9d9aa1"
   )) -> fig2_germ)


mod_germ <- lm(germination_freq_mean ~ species_cat, data = dt)
summary(mod_germ)
emmeans(mod_germ, pairwise ~ species_cat, type = "response")


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

# facet order
custom_order2 <- c("Local non-specific dispersal",
                   "Anemochory",
                   "Myrmecochory",
                   "Endozoochory",
                   "Epizoochory",
                   "Hydrochory",
                   "Dyszoochory") # removed later on, because this is so few species
dispmo$dispersal_mode <- factor(dispmo$dispersal_mode, levels = custom_order2)

# plot
(dispmo %>%
    filter(dispersal_mode != "Dyszoochory") %>%
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
    scale_y_continuous(limits = c(0,0.9)) +
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
    scale_fill_manual(values = c( `conservation gardening species` = "#4004ae" , 
                                  `non-native garden escapees` = "#ecebed"
    )) +
    theme(legend.position = "none",
          legend.title = element_blank()) +
    labs(x = "", y = "",
         title = "Dispersal mode") -> fig2_dispmo)



# create a contingency table
contingency_table  <- dispmo %>% 
  filter(dispersal_mode != "Dyszoochory") %>%
  select(species_cat, dispersal_mode, n) %>%
  pivot_wider(names_from = species_cat, values_from = n) %>% 
  column_to_rownames(var = colnames(.[1])) %>%
  as.matrix()
contingency_table


# endangered vs garden escapee
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
comps <- bind_rows(comp) %>% mutate(comp = "conservation gardening vs garden escapee")
row.names(comps)<- NULL
options(scipen = 999)


comp <- comps %>% arrange(mode) %>% 
  mutate(pval = round(pval, 5), chisq = round(chisq, 3)) %>% 
  select(`Dispersal mode` = mode, Comparison = comp, `Chi square` = chisq, `P value` = pval)
comp

write_csv(comp, "Data/dispersal_mode_comp_cg_es.csv")


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
    scale_fill_manual(values = c( `conservation gardening species` = "#4004ae" , 
                                  `non-native garden escapees` = "#ecebed"
    )) +
    theme(legend.position = "none",
          legend.title = element_blank() ) +
    labs(x = "", 
         y = "Proportion of species",
         title = "Seed structures") -> fig2_sestr)


# create a contingency table
contingency_table  <- sestr %>% 
  select(species_cat, seed_str, n = Yes) %>%
  pivot_wider(names_from = species_cat, values_from = n) %>% 
  column_to_rownames(var = colnames(.[1])) %>%
  as.matrix()
contingency_table


# endangered vs garden escapee
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
comps <- bind_rows(comp) %>% mutate(comp = "conservation gardening vs garden escapee")
row.names(comps)<- NULL
options(scipen = 999)


comp <- comps %>% arrange(mode) %>% 
  mutate(pval = round(pval, 5), chisq = round(chisq, 3)) %>% 
  select(`Seed structure` = mode, Comparison = comp, `Chi square` = chisq, `P value` = pval)
comp

write_csv(comp, "Data/seedstructure_comp_cg_es.csv")


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
    scale_y_continuous(limits = c(0,1.08)) +
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
    scale_fill_manual(values = c( `conservation gardening species` = "#4004ae" , 
                                  `non-native garden escapees` = "#ecebed"
    )) +
    theme(legend.position = "none",
          legend.title = element_blank()) +
    labs(x = "", 
         y = "",
         title = "Seedbank type") -> fig2_seedb)


# create a contingency table
contingency_table  <- sbank %>% 
  select(species_cat, most_frequent_seedbank_type, n) %>%
  pivot_wider(names_from = species_cat, values_from = n) %>% 
  column_to_rownames(var = colnames(.[1])) %>%
  as.matrix()
contingency_table


# endangered vs garden escapee
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
comps <- bind_rows(comp) %>% mutate(comp = "conservation gardening vs garden escapee")
row.names(comps)<- NULL
options(scipen = 999)


comp <- comps %>% arrange(mode) %>% 
  mutate(pval = round(pval, 5), chisq = round(chisq, 3)) %>% 
  select(`Seedbank type` = mode, Comparison = comp, `Chi square` = chisq, `P value` = pval)
comp

write_csv(comp, "Data/seedbank_comp_cg_es.csv")


# create multiplot --------------------------------------------------------
# customize theme for each plot
(fig2_dispmo <- fig2_dispmo + theme(plot.margin = margin(0, 0, 0, 0),
                                  panel.spacing = unit(0, "lines"), 
                                  axis.text.y = element_blank(),
                                  plot.tag = element_text(face = "bold", 
                                                          family = "Arial Narrow")))

(fig2_sestr <- fig2_sestr + theme(plot.margin = margin(0, 0, 0, 0),
                                panel.spacing = unit(0, "lines"), 
                                axis.text.y = element_blank(),
                                plot.tag = element_text(face = "bold", 
                                                        family = "Arial Narrow")))

(fig2_seedb <- fig2_seedb + theme(plot.margin = margin(0, 0, 0, 0),
                                panel.spacing = unit(0, "lines"), 
                                axis.text.y = element_blank(),
                                plot.tag = element_text(face = "bold", 
                                                        family = "Arial Narrow")))


(fig2_germ <- fig2_germ + theme(plot.margin = margin(0, .5, 0, 0),
                              plot.tag = element_text(face = "bold", 
                                                      family = "Arial Narrow")) +
    guides(colour = "none", fill = "none"))

(fig2_sw <- fig2_sw + theme(plot.margin = margin(0, .5, 0, 0),
                          plot.tag = element_text(face = "bold", 
                                                  family = "Arial Narrow")) +
    guides(colour = "none", fill = "none"))



(fig2_tv <- fig2_tv + theme(plot.margin = margin(0, .5, 0, 0),
                          plot.tag = element_text(face = "bold", 
                                                  family = "Arial Narrow")) +
    guides(colour = "none", fill = "none"))



(fig2_dd <- fig2_dd + theme(plot.margin = margin(0, .5, 0, 0),
                          plot.tag = element_text(face = "bold", 
                                                  family = "Arial Narrow")) +
    guides(colour = "none", fill = "none"))


plot_upper <- (fig2_sw + fig2_tv) / ( fig2_dd + fig2_germ)
plot_lower <- (fig2_dispmo + fig2_sestr + fig2_seedb) 


plot_upper / plot_lower + plot_layout(heights = c(.5, .5, 2), guides = "collect") +
  plot_annotation(tag_levels = "a", title = "Dispersal ecology of non-native garden escapees versus conservation gardening species",
                  theme = theme(plot.title = element_text(size = 14, 
                                                          family = "Arial Narrow",
                                                          face = "bold"))) &
  theme(legend.position = "bottom", legend.text=element_text(size=14))



# save plot
showtext_opts(dpi=600)
ggsave(width = 11, height = 11, bg = "white",
       file = "Figures/fig2.png",
       dpi = 600)
showtext_opts(dpi=96)
