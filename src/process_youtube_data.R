library(tidyverse)

df <- read_csv("uyghur_gestures_annotated.csv") %>%
  filter(!is.na(label_wd) & !is.na(label_ph)) %>%
  mutate(Subject = ifelse(Subject == 'Gulnisa', 'A', 
                      ifelse(Subject == 'Mustafa', 'B', 'C'))) # %>%
  # filter(Subject != 'C')

df %>% 
  ggplot(aes(x = Subject)) + 
  geom_bar() +
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20)) +
  ylab("Number of gestures")

df %>%
  ggplot(aes(x = seg_type)) +
  geom_bar() +
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20)) +
  ylab("Number of gestures")

df %>%
  filter(seg_type == 'consonant') %>%
  ggplot(aes(x = as.factor(is_onset))) +
  geom_bar() +
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20)) +
  ylab("Number of gestures") +
  xlab("Onset consonant?")

df %>%
  ggplot(aes(x = fct_infreq(label_ph))) +
  geom_bar() +
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20)) +
  ylab("Number of gestures") +
  xlab("Segment")

poly_df <- df %>%
  filter(num_syls > 1) %>%
  mutate(pos = ifelse(gesture_syl == 1, 'initial', 
                      ifelse(gesture_syl < num_syls, 'medial',
                             'final')))

poly_df %>%
  ggplot(aes(x=fct_relevel(pos, c('initial', 'medial', 'final')))) +
  geom_bar() +
  theme_minimal() + 
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20)) +
  ylab("Count") +
  xlab("Position of syllable in word")

poly_df %>%
  ggplot(aes(x=fct_relevel(pos, c('initial', 'medial', 'final')), y=as.factor(closed_syl))) +
  geom_bin2d() +
  theme_minimal() + 
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20)) +
  ylab("Closed syllable?") +
  xlab("Position of syllable in word")


poly_df %>%
  ggplot(aes(x=as_factor(closed_syl))) +
  geom_bar() +
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20)) +
  ylab("Number of gestures") +
  xlab("Is closed syllable?")

poly_df %>%
  ggplot(aes(x=as_factor(has_suffix), y=gesture_syl)) +
  geom_violin() +
  geom_boxplot(width=0.1) +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20)) +
  ylab("Gesture syllable location") +
  xlab("Has suffixes?")

poly_df %>%
  ggplot(aes(x=as_factor(final), y=as_factor(closed_syl))) +
  geom_bin_2d()  +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20)) +
  ylab("Gesture syllable location") +
  xlab("Has suffixes?")

poly_df %>%
  filter(!is.na(dict_stress) & dict_stress != '?' & !has_suffix) %>%
  ggplot(aes(x=as.factor(dict_stress), y=as.factor(gesture_syl))) +
  geom_count() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20)) +
  ylab("Gesture syllable location") +
  xlab("Dictionary stressed syllable location")

unsuffixed <- poly_df %>%
  filter(!is.na(dict_stress) & dict_stress != '?' & !has_suffix) %>%
  mutate(dict_stress = as.numeric(dict_stress))

cor(unsuffixed$dict_stress, unsuffixed$gesture_syl)
