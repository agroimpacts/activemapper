# Summarize label risk for each AOI
# Builds on risk metrics calculated for all AOIs in consensus_risk.R

library(dplyr)

load(system.file("extdata/", "consensus_risk.rda", package = "activemapper"))
load(system.file("extdata/", "instance_tbls.rda", package = "activemapper"))

mgrid <- data.table::fread(
  system.file("extdata/ghana_grid.csv", package = "activemapper")
)

# select AOIs and unique names:
aoinms <- c(
  "labeller",
  paste0("labeller", c("trainsouth", "trainsouthnl", "trainsw", 1:16))
)

# get usages
usages <- instance_tbls$incoming_names %>%
  filter(!(aoi == "labeller3" & run == 0)) %>%
  filter(label == TRUE) %>%
  filter(aoi %in% aoinms) %>%
  dplyr::select(aoi, name, usage) %>%
  left_join(., mgrid %>% dplyr::select(name, x, y), by = "name")

# checks on duplicates
# dupnms <- usages %>% group_by(name) %>% count() %>% filter(n > 1) %>% pull(name)
# length(dupnms)  # 503 duplicates
# dupnms2 <- usages %>% filter(name %in% dupnms) %>% arrange(name) %>%
#   filter(!aoi %in% c("labellertrainsouth", "labellertrainsouthnl"))
# nrow(dupnms2) # 7 of those in other AOIs besides 2 runs of labellertrainsouth
# usages %>% filter(name == "GH0328463")
# this site was validate in labeller7 and in both labellertrainsouth versions

label_risk_pts <- left_join(consensus_risk, usages, by = c("aoi", "name")) %>%
  dplyr::select(aoi, name, usage, !!names(.))
# check on duplicates
# nm <- label_risk %>% group_by(name) %>% count() %>% filter(n > 1) %>% pull(name)
# label_risk %>% filter(name %in% nm) %>% arrange(name) %>% View()
# label_risk %>%
#   filter(name %in% nm) %>%
#   filter(!aoi %in% c("labellertrainsouth", "labellertrainsouthnl")) %>%
#   dplyr::select(aoi, name, usage) #%>% distinct(name)
# label_risk %>% filter(name == "GH0328463")
# same as from usage

# Rename some columns and values
clusters <- paste0("labeller", c("", "trainsouth", "trainsouthnl", "trainsw"))
new_cluster_nms <- paste0("Cl", c("1", "2a", "2b", "3"))

label_risk_pts <- label_risk_pts %>%
  rename(Risk = meanrisk, Riskprop = prop_risky) %>%
  mutate(usage = stringr::str_to_title(usage)) %>%
  mutate(aoi = case_when(
    aoi == clusters[1] ~ "-3",
    aoi == clusters[2] ~ "-2",
    aoi == clusters[3] ~ "-1",
    aoi == clusters[4] ~ "0",
    TRUE ~ gsub("labeller", "", aoi)
  )) %>%
  mutate(ord = as.numeric(aoi)) %>% arrange(ord) %>%
  mutate(aoi = case_when(
    aoi == "-3" ~ new_cluster_nms[1],
    aoi == "-2" ~ new_cluster_nms[2],
    aoi == "-1" ~ new_cluster_nms[3],
    aoi %in% "0" ~ new_cluster_nms[4],
    TRUE ~ aoi
  )) %>%
  mutate(aoi = forcats::fct_reorder(aoi, ord)) %>%
  select(-ord)

# summarize
label_risk_stats <- label_risk_pts %>% group_by(aoi, usage) %>%
  summarize(Risk = mean(Risk, na.rm = TRUE)) %>%
  ungroup()

label_risk <- list(pts = label_risk_pts, stats = label_risk_stats)

usethis::use_data(label_risk, overwrite = TRUE)
