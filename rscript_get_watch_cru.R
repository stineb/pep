#!/usr/bin/env Rscript

load("data/df_zani_cells.RData")
load("data/df_mine_cells.RData")

df_zani_cells <- df_zani_cells %>% 
  mutate(sitename = paste0("icell_", 1:n()))

df_mine_cells <- df_mine_cells %>% 
  mutate(sitename = paste0("icell_", 1:n()))


# ddf_watch_zani <- ingest(
#   siteinfo = df_zani_cells,
#   source    = "watch_wfdei",
#   getvars   = c("temp", "prec", "ppfd", "vpd", "patm"),
#   dir       = "~/data/watch_wfdei/"  # adjust this with your local path
# )
# save(ddf_watch_zani, file = "data/ddf_watch_zani.RData")

ddf_watch_mine <- ingest(
  siteinfo = df_mine_cells,
  source    = "watch_wfdei",
  getvars   = c("temp", "prec", "ppfd", "vpd", "patm"),
  dir       = "~/data/watch_wfdei/"  # adjust this with your local path
)
save(ddf_watch_mine, file = "data/ddf_watch_mine.RData")


ddf_cru_zani <- ingest(
  siteinfo = df_zani_cells,
  source    = "cru",
  getvars   = "ccov",
  dir       = "~/data/cru/ts_4.01/"  # adjust this with your local path
)
save(ddf_cru_zani, file = "data/ddf_cru_zani.RData")

ddf_cru_mine <- ingest(
  siteinfo = df_mine_cells,
  source    = "cru",
  getvars   = "ccov",
  dir       = "~/data/cru/ts_4.01/"  # adjust this with your local path
)
save(ddf_cru_mine, file = "data/ddf_cru_mine.RData")
