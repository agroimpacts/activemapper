## Create map reference sample
## Reads in intermediate data from data_from_segmenter.R

library(dplyr)
library(here)

train_val <- readr::read_csv(
  system.file("extdata", "train_val_sites.csv", package = "activemapper")
)
mgrid_tb <- data.table::fread(
  system.file("extdata/ghana_grid.csv", package = "activemapper")
) %>% as_tibble()

# intermediate results: stratified points
fpoints <- data.table::fread(here("external/data/field_points005.csv")) %>%
  as_tibble %>% filter(aoi != "3N")
fpointsr <- fpoints %>% filter(!id %in% unique(train_val$id))

#!!!!!!!!! Add sample size analysis to SI
# Going with sample size of 800 for cropland class, and 683 for non-cropland,
# per SI.  MOE of 0.03 and 95% confidence interval on assumed cropland

# Take sample from each stratum and combine
set.seed(111)
fsample <- fpointsr %>% filter(class == 1) %>% sample_n(size = 800)
set.seed(111)
nofsample <- fpointsr %>% filter(class == 0) %>% sample_n(size = 683)
ref_sample <- bind_rows(fsample, nofsample) %>%
  mutate(class = as.character(class))

# Join with main grid to get name
# !!! Note: this step probably not need if fix lines 33-39 in
# !!! data_from_segmenter.R)
ref_sample <- left_join(ref_sample %>% select(id, aoi, class), mgrid_tb,
                        by = "id") %>% select(id, name, aoi, class, x, y)

# Write to package
# data.table::fwrite(
#   ref_sample, file = here("external/reference/map_reference_sample.csv")
# ) # original location
data.table::fwrite(
  ref_sample, file = here("inst/extdata/map_reference_sample.csv")
)

# convert to format for incoming_names* to set up new validator instance
ref_sample_labeller <- ref_sample %>% select(name) %>%
  mutate(run = 0, iteration = 0, processed = TRUE, usage = "train",
         label = TRUE)

# write to s3 bucket as input for new validation instance
aws.s3::s3write_using(
  ref_sample_labeller,
  FUN = readr::write_csv, bucket = "activemapper",
  object = glue::glue("planet/incoming_names_static_validator.csv")
)

# usethis::use_data("map_reference_sample")
