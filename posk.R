razlike <- slovenija %>% filter(Leto %in% 2019:2020) %>%
  pivot_wider(names_from=Leto, values_from=Stevilo) %>%
  mutate(razlika=`2020`-`2019`) 

  

