source("00-preamble.R")

# red list data -----------------------------------------------------------

# load red list
rl <- read_excel("Data/02_Datentabelle_RL_Farn-_und_Bluetenpflanzen_2018_Deutschland_20210317-1607.xlsx")

# prune rl species names
spec_vec <- NULL
for (i in 1:nrow(rl)){
  spec_vec[i] <- remove.authors(rl$Name[i])
}

rl$name_abb <- spec_vec

# introduce classifier for threatened
rl <- rl %>% 
  filter(Arten == "Arten") %>% 
  select(Name, name_abb, rl_cat =`RL Kat.`, native = Status) %>%  
  mutate(threatened = ifelse(rl_cat == "0" | 
                                          rl_cat == "1"|
                                          rl_cat == "2"|
                                          rl_cat == "3"|
                                          rl_cat == "G", "yes", "no"))
rl <- rl %>% mutate(native = ifelse(
  native == "I",
  "native",
  ifelse(
    native == "N",
    "established non-native",
    ifelse(native == "U", "transient", "doubtful")
  )
))

# get author
for (i in 1:nrow(rl)) {
  common_text <- str_detect(rl$Name[i], rl$name_abb[i])
  if (common_text) {
    rl$Name[i] <- str_remove(rl$Name[i], rl$name_abb[i])
  }
}

# remove leading and trailing whitespaces
rl$Name <- str_trim(rl$Name)

# taxonomic harmonization
# perform other check with rwcvp for rl
wcvp_names <- wcvp_names
wcvp_match_rl <- wcvp_match_names(rl, wcvp_names = wcvp_names,
                                  name_col = "name_abb",
                                  author_col = "Name",
                                  fuzzy = F)

wcvp_acc_rl <-
  wcvp_match_rl %>% select(species = name_abb, author = Name, wcvp_status, wcvp_accepted_id)


# give preference to accepted (and then synonyms) names, 
# if there are multiple rows for one and 
# the same species
wcvp_acc_rl <- wcvp_acc_rl %>% 
  arrange(desc(wcvp_status %in% c("Accepted", "Synonym")), 
          desc(wcvp_status == "Accepted")) %>%
  distinct(species, .keep_all = TRUE)

names_rl <- wcvp_names %>% select(wcvp_accepted_id = plant_name_id, taxon_name)
wcvp_acc_rl <- left_join(wcvp_acc_rl, names_rl) 

rl_wcvp <- left_join(rl %>% rename(species = name_abb) %>% select(-Name), wcvp_acc_rl)

# check if some synonyms resulted in one species having different rl statuses
rl_wcvp %>% select(-species, -author) %>% distinct %>% 
  count(taxon_name) %>% 
  arrange(desc(n)) %>% 
  View

# there seems to be some cases! pick the one with the lowest threat status
threat_levels <- c("nb","0", "1", "2", "3", "G", "R", "V", "*")
rl_wcvp <- rl_wcvp %>%
  mutate(rlcat_num = match(rl_cat, threat_levels))

rl_wcvp <- rl_wcvp %>%
  group_by(taxon_name) %>%
  filter(rlcat_num == max(rlcat_num)) %>%
  ungroup() %>% 
  select(taxon_name, threatened, native, rl_cat) %>% 
  distinct

# check
rl_wcvp %>% distinct %>% 
  count(taxon_name) %>% 
  arrange(desc(n)) %>% 
  View


# dispersal data ----------------------------------------------------------

# load dispersal data
sd <- read_excel("Data/Lososova_et_al_2023_Dispersal.xlsx")

# select relevant columns
sd <- sd %>% select(Taxon, 
              seedmass = `Seed mass (mg)`, 
              dispersal_mode = `Efficient dispersal mode - common`,
              dispersal_distance = `Dispersal distance class (1-6)`)


# taxonomic harmonization
# perform other check with rwcvp for rl
wcvp_match_sd <- wcvp_match_names(sd, wcvp_names = wcvp_names,
                                  name_col = "Taxon",
                                  fuzzy = F)
wcvp_acc_sd <-
  wcvp_match_sd %>% select(species = Taxon, wcvp_status, wcvp_accepted_id)

# give preference to accepted (and then synonyms) names, 
# if there are multiple rows for one and 
# the same species
wcvp_acc_sd <- wcvp_acc_sd %>% 
  arrange(desc(wcvp_status %in% c("Accepted", "Synonym")), 
          desc(wcvp_status == "Accepted")) %>%
  distinct(species, .keep_all = TRUE)

names_sd <- wcvp_names %>% select(wcvp_accepted_id = plant_name_id, taxon_name)
wcvp_acc_sd <- left_join(wcvp_acc_sd, names_sd) 

sd_wcvp <- left_join(sd %>% rename(species = Taxon), wcvp_acc_sd)

# check if some synonyms resulted in one species having different rl statuses
sd_wcvp %>% select(-species, -wcvp_status) %>%  distinct %>% 
  count(taxon_name) %>% 
  arrange(desc(n)) %>% 
  View

# just always prefer the accepted species in case of multiple options
sd_wcvp <- sd_wcvp %>% select(-species) %>% 
  arrange(desc(wcvp_status %in% c("Accepted", "Synonym")), 
          desc(wcvp_status == "Accepted")) %>%
  distinct(taxon_name, .keep_all = TRUE)

sd_wcvp %>% View

# germiantion data ------------------------------------------------------

# data were downloaded from TRY
try <- fread("Data/27544.txt")

# which traits, focus on germination rate and seed longevity
try %>% select(TraitID, TraitName) %>% distinct


# germination rate from KEW Seed Information Database
germination <- try %>% 
  filter(Dataset == "KEW Seed Information Database (SID)") %>% 
  filter(DataID == 291 |
           DataID == 292 |
           DataID == 293 |
           DataID == 294 ) %>% 
  select(ObservationID, AccSpeciesName, OriglName, OrigValueStr) %>% 
  pivot_wider(names_from = "OriglName", values_from = "OrigValueStr")

# there is lots of complex protocols, there is no way this can be made consistent
# simply take the mean germination rate, and say that they are somewhat not comparable
# given that different treatments were used across species
germination <- germination %>% select(species = AccSpeciesName, germination_freq = "%Germination") %>% 
  group_by(species) %>% 
  summarize(germination_freq_mean = mean(as.numeric(germination_freq), na.rm = T)) %>% 
  na.omit()

# taxonomic harmonization
wcvp_match_germination <- wcvp_match_names(germination, wcvp_names = wcvp_names,
                                  name_col = "species",
                                  fuzzy = F)
wcvp_acc_germination <-
  wcvp_match_germination %>% select(species, wcvp_status, wcvp_accepted_id)

# give preference to accepted (and then synonyms) names, 
# if there are multiple rows for one and 
# the same species
wcvp_acc_germination <- wcvp_acc_germination %>% 
  arrange(desc(wcvp_status %in% c("Accepted", "Synonym")), 
          desc(wcvp_status == "Accepted")) %>%
  distinct(species, .keep_all = TRUE)

names_germinaton <- wcvp_names %>% select(wcvp_accepted_id = plant_name_id, taxon_name)
wcvp_acc_germination <- left_join(wcvp_acc_germination, names_germinaton ) 

germination_wcvp <- left_join(germination, wcvp_acc_germination)

# check if harmon led to species having more values
germination_wcvp %>% select(taxon_name, germination_freq_mean, wcvp_status, wcvp_accepted_id) %>% 
  group_by(taxon_name) %>% count %>% arrange(desc(n))

class(germination_wcvp$germination_freq_mean)
# ok simply calc mean
germination_wcvp <-
  germination_wcvp %>% select(taxon_name,
                              germination_freq_mean,
                              wcvp_status,
                              wcvp_accepted_id) %>%
  group_by(taxon_name) %>% summarise(germination_freq_mean = 
                                       mean(germination_freq_mean, na.rm = T))

germination_wcvp %>% View

# leda seed data ----------------------------------------------------------

# seed shape
seedshape <- read.delim("Data/seedshape.txt", header=FALSE, skip=3, sep = ";")
colnames(seedshape) <- seedshape[1,]
seedshape <- seedshape[-1,]
View(seedshape)

# seed number
seednumber <- read.delim("Data/seednumber.txt", header=FALSE, skip=4, sep = ";")
colnames(seednumber) <- seednumber[1,]
seednumber <- seednumber[-1,]
View(seednumber)

# terminal velocity
terminalvelocity <- read.delim2("Data/terminalvelocity.txt", header=FALSE, skip=3, sep = ";")
colnames(terminalvelocity) <- terminalvelocity[1,]
terminalvelocity <- terminalvelocity[-1,]
View(terminalvelocity)

# morphology dispersal unit
dispmo <- read.delim2("Data/morphology_dispersalunit.txt", header=FALSE, skip=6, sep = ";")
colnames(dispmo) <- dispmo[1,]
dispmo <- dispmo[-1,]
View(dispmo)

# seed longevity
seedlongevity <- read.delim2("Data/seedlongevity.txt", header=FALSE, skip=3, sep = ";")
colnames(seedlongevity) <- seedlongevity[1,]
seedlongevity <- seedlongevity[-1,]
View(seedlongevity)

# seed bank, seems similar in a way to longevity
seedbank <- read.delim2("Data/seedbank.txt", header=FALSE, skip=6, sep = ";")
colnames(seedbank) <- seedbank[1,]
seedbank <- seedbank[-1,]
View(seedbank)

# bring all together
# seed longevity
seedlongevity <- seedlongevity %>% select(
  species = `SBS name`,
  seedbank_type = `seed bank type`,
  seedbank_index = `SSB seed longevity index`
) %>%
  filter(seedbank_type != "present") %>% 
  # pick the most frequent category per species, calculate mean index
  group_by(species) %>%
  summarize(
    most_frequent_seedbank_type = names(sort(table(seedbank_type),
                                             decreasing = TRUE))[1],
    mean_seedbank_index = mean(as.numeric(seedbank_index), na.rm = T)
  )

# morphology dispersal uni

# combinations are possible, but if no appendage occurs together with an appendage
# remove
dispmo <- dispmo %>% select(species = `SBS name`, 
                  seed_str = `gen. seed structure`) %>% 
  group_by(species, seed_str) %>%
  filter(seed_str != "") %>% 
  filter(seed_str != "Other specialisation") %>% 
  mutate(present = "Yes") %>%
  distinct() %>% 
  pivot_wider(names_from = seed_str, values_from = present, values_fill = "No") %>%
  ungroup() %>%
  mutate(`No appendages` = ifelse(`Elongated appendages` == "Yes" |
                                    `Flat appendages` == "Yes" |
                                    `Balloon structures` == "Yes" |
                                    `Nutrient containing structures` == "Yes", 
                                  "No", `No appendages`))

# terminal velocity
terminalvelocity %>% group_by(`diaspore type`) %>% count

terminalvelocity <- terminalvelocity %>% select(
  species = `SBS name`,
  termvel = `single value [m/s]`,
  diaspore_type = `diaspore type`,
  sample_size = `sample size`
) %>%
  group_by(species, diaspore_type) %>%
  # calculate mean
  summarize(termvel_mean = mean(as.numeric(termvel, na.rm = T))) %>%
  # pick one diaspore type, give perference to one-seeded
  arrange(species, desc(diaspore_type == "one-seeded generative dispersule")) %>%
  distinct(species, .keep_all = TRUE)

# seed number, very few species
seednumber %>% select(
  species = `SBS name`,
  seedn = `single value`,
  rep_uni = `reproduction unit measured`,
  sample_size = `sample size`
) %>%
  group_by(species, rep_uni) %>%
  # calculate weighted mean based on sample size col
  summarize(seedn_mean = mean(as.numeric(seedn, na.rm = T))) %>%
  # pick per ramet
  filter(rep_uni == "per ramet/tussock or individual plant ")

# combine
leda_seeds <- full_join(terminalvelocity %>% select(-diaspore_type), dispmo) %>% 
  full_join(seedlongevity)


# taxonomic harmonization
wcvp_match_ledas <- wcvp_match_names(leda_seeds, wcvp_names = wcvp_names,
                                           name_col = "species",
                                           fuzzy = F)

wcvp_acc_ledas <-
  wcvp_match_ledas %>% select(species, wcvp_status, wcvp_accepted_id)

# give preference to accepted (and then synonyms) names, 
# if there are multiple rows for one and 
# the same species
wcvp_acc_ledas <- wcvp_acc_ledas %>% 
  arrange(desc(wcvp_status %in% c("Accepted", "Synonym")), 
          desc(wcvp_status == "Accepted")) %>%
  distinct(species, .keep_all = TRUE)

names_ledas <- wcvp_names %>% select(wcvp_accepted_id = plant_name_id, taxon_name)
wcvp_acc_ledas <- left_join(wcvp_acc_ledas, names_ledas) 

ledas_wcvp <- left_join(leda_seeds, wcvp_acc_ledas)

# check if harmon led to species having more values
ledas_wcvp %>% select(taxon_name) %>% 
  group_by(taxon_name) %>% count %>% arrange(desc(n))

# in these cases (due to conflicting and na info), simply prefer the acc sp
ledas_wcvp <- ledas_wcvp %>% ungroup %>% select(-species) %>% 
  arrange(desc(wcvp_status %in% c("Accepted", "Synonym")), 
          desc(wcvp_status == "Accepted")) %>%
  distinct(taxon_name, .keep_all = TRUE)

ledas_wcvp %>% View

# join data ---------------------------------------------------------------

d <- left_join(rl_wcvp, sd_wcvp) %>% 
  left_join(germination_wcvp) %>% 
  left_join(ledas_wcvp) %>% 
  select(-wcvp_status, - wcvp_accepted_id)

View(d)

# save
write_csv(d, "Data/seed-redlist-data-complete-v2.csv")


