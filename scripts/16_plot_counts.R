library(tidyverse)

source('scripts/15_sync_drive_exports.R')


df_1 = read_csv('data/from_drive/sidney_collectedData_earthjt_fire_plots_on_171026_072512.csv')
df_2 = read_csv('data/from_drive/riannasamson_collectedData_earthjt_fire_plots_on_161026_204330_CSV.csv')
df_3 = read_csv('data/from_drive/anitachamraj_collectedData_earthjt_fire_plots_on_191026_000032_CSV.csv')


df = bind_rows(df_1, df_2, df_3) %>%
  pivot_longer(cols = c(jt_post_fire, jt_pre_fire), names_to = 'fire_status', values_to = 'count') %>%
  mutate(fire_status = factor(fire_status, levels = c('jt_pre_fire', 'jt_post_fire'), labels = c('Pre-fire', 'Post-fire')))

obs_cols <- c("sidney", "riannasamson", "anitachamraj")

df_agreement = df %>%
  select(fire_name, id, operator,fire_status, count) %>%
  pivot_wider(names_from = operator, values_from = count) %>%
  mutate(n_counts = rowSums(!is.na(pick(all_of(obs_cols)))),
         max = pmax(sidney, riannasamson, anitachamraj, na.rm = TRUE),
         min = pmin(sidney, riannasamson, anitachamraj, na.rm = TRUE),
         delta = if_else(n_counts >1, max - min, NA_real_),
         mean_count = if_else(n_counts >1, (min + max) / 2, NA_real_)) %>%
  mutate(within_2_trees = if_else(delta <= 2, max(df$count, na.rm = TRUE) + 1, NA_real_),
         within_20_pct  = if_else(delta <= 0.2 * mean_count, max(df$count, na.rm = TRUE) + 1, NA_real_)) %>%
  select(-max, -min)


ggplot() + theme_bw() +
  facet_grid(cols = vars(fire_name), rows = vars(fire_status), scales = 'free_x') +
  geom_line(data = df, aes(x = id, y = count, group = id)) +
  geom_point(data = df, aes(x = id, y = count, shape = operator, fill = operator),
             size = 2, alpha = .7, color = 'black') +
  geom_point(data = df_agreement, aes(x = id, y = within_2_trees), shape = 3, size = 2, color = 'black') +
  geom_point(data = df_agreement, aes(x = id, y = within_20_pct), shape = 4, size = 2, color = 'black') +
  scale_shape_manual(values = c(21, 22, 23)) +
  scico::scale_fill_scico_d(palette = 'batlow', begin = .2, end = .8) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))


