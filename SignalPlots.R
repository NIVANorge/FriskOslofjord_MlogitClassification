# Signal Plots
# Guri Sogn Anderen, 2020


# Channels ----------------------------------------------------------------

signals_aker %>% pivot_longer(cols = blue:red, names_to = "Channel", values_to = "Signal") %>%
  filter(Category != "") %>% 
  ggplot(aes(x = Category, y = Signal)) +
  geom_jitter(aes(colour = Category)) +
  geom_boxplot(alpha = 0.2) +
  facet_wrap(~ Channel) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))

# The signals from rock, sand and roof seems to be divided into groups

# Ground truth and signals
frisk_dataset %>% select(Dataset, Sp04_la, blue:red.edge) %>% 
  pivot_longer(cols = blue:red.edge, names_to = "Channel", values_to = "Signal") %>% 
  mutate(Sp04_la = str_to_sentence(Sp04_la)) %>% 
  filter(Channel == "nir") %>% 
  ggplot(aes(x = Dataset, y = Signal, fill = Dataset)) +
  geom_boxplot() +
  facet_wrap(~ Sp04_la)

# Single spec and signals
singlespec %>% pivot_longer(cols = blue:nir, names_to = "Channel", values_to = "Signal") %>% 
  ggplot(aes(x = species, y = Signal, fill = species)) +
  geom_boxplot() +
  facet_wrap(~ Channel) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))

