library(groundhog)
groundhog.library(c("tidyverse", "entropy", "ggrepel", "cowplot", "knitr", "readr",
                    "RColorBrewer", "plotly", "gglorenz", "rio", "hrbrthemes"),
                  date = "2024-02-24")
theme_set(theme_minimal(base_size = 14))

calc_norm_entropy <- function(n) {
  n <- n[n>0]
  entropy::entropy(n)/log(length(n))
}
psyctests_info <- readRDS("../sober_rubric/raw_data/psyctests_info.rds")
records_wide <- readRDS("../sober_rubric/raw_data/preprocessed_records.rds")
psyctests_info <- psyctests_info %>%
  left_join(records_wide %>% select(DOI, Acronym = first_acronym), by = "DOI")

tests <- psyctests_info %>%
  mutate(shortName = coalesce(Acronym, Name)) %>%
  group_by(DOI, shortName, Name) %>%
  summarise(n = sum(usage_count, na.rm = T),
            parent = "") %>%
  ungroup() %>%
  sample_frac(1)
entropy = entropy(tests$n)
norm_entropy = calc_norm_entropy(tests$n)

tests <- bind_rows(tests,
                   tests %>% filter(parent != "") %>%
                     ungroup() %>%
                     select(Name = parent) %>% distinct() %>% mutate(n=0, parent = ""))
tests <- tests %>% arrange(runif(n()))

fig <- plot_ly(
  type='treemap',
  labels = tests$shortName,
  parents = tests$parent,
  values= tests$n,
  text = str_c(if_else(tests$Name == tests$shortName, "", str_c(tests$Name, "<br>")), tests$DOI),
  marker = list(line = list(width = 0.1)
                , colors = rep(c("#69D2E7", "#A7DBD8", "#E0E4CC", "#F38630", "#FA6900"), length.out = nrow(tests))
  ),
  tiling = list(pad = 0.1,
                packing = "squarify",
                squarifyratio = (1 + sqrt(5)) / 2),
  # insidetextfont = list(size = I(50), color = "green", mode = "hide"),
  hoverinfo="label+value+text",
  textinfo="label+value")

fig %>% layout(
  autosize = TRUE,
  uniformtext=list(minsize=15, mode='hide'),
  margin = list(l = 0, t = 0, r = 0 , b = 0)
)
