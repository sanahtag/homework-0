library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)


bat_99_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarize(mean_singles = mean(singles), mean_bb = mean(bb))


sum(bat_99_01$mean_singles > 0.2)

sum(bat_99_01$mean_bb > 0.2)

bat_02_03 <- inner_join(bat_02, bat_03_mean, by = "playerID")

cor(bat_02_03$singles.x,bat_02_03$singles.y)

dat <- inner_join(bat_02, bat_99_01)
cor(dat$singles, dat$mean_singles)

bat_02_03_df <- fortify(bat_02_03)
ggplot(bat_02_03, aes(mean_singles, singles)) + 
  +     geom_point(alpha = 0.5)

bat_02_03_df$mean_singles <- as.factor(bat_02_03_df$mean_singles)
bat_02_03_df$singles <- as.factor(bat_02_03_df$singles)

fit <- lm(singles ~ mean_singles, data = bat_02_03)
fit$coef
fit$coef[2]


