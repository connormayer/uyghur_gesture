library(tidyverse)
library(lme4)


df <- read_csv("data/full_syllable_data.csv", na=c("", "NA", "--undefined--")) %>%
  mutate(position = ifelse(is_initial, 'initial',
                           ifelse(is_final, 'final',
                           'medial')),
         template = str_replace_all(word, '[aeiouüöé]', 'V'),
         template = str_replace_all(template, '[^V\\.]', 'C'),
         template = str_replace_all(template, '[CV]*C(\\W|$)', 'H\\1'),
         template = str_replace_all(template, '[CV]*V(\\W|$)', 'L\\1'),
         other_heavy = preceding_heavy | following_heavy) %>%
  group_by(subject) %>%
  mutate(maxf0 = scale(maxf0),
         minf0 = scale(minf0),
         meanf0 = scale(meanf0),
         mean_intensity = scale(mean_intensity),
         duration = scale(duration))

df_gestures <- df %>% filter(gesture == 1)

df_gestures %>% 
  ggplot(aes(x = other_heavy)) + 
  geom_bar() +
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20)) +
  ylab("Number of gestures") +
  xlab("Other heavy syllable in word?") +
  facet_wrap(~ closed_syl)
ggsave('figs/other_heavy_gestures.png')

df_gestures %>% 
  ggplot(aes(x = subject)) + 
  geom_bar() +
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20)) +
  ylab("Number of gestures") +
  xlab("Subject")
ggsave('figs/subject_gestures.png')

df_gestures %>%
  ggplot(aes(x = closed_syl)) +
  geom_bar() +
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20)) +
  ylab("Number of gestures") + 
  xlab("Heavy syllable?")
ggsave('figs/heavy_gestures.png')

df_gestures %>%
  filter(word_len_syls > 1) %>%
  ggplot(aes(x = position)) +
  geom_bar() +
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20)) +
  ylab("Number of gestures") +
  xlab("Syllable position in word") 
ggsave('figs/position_gestures.png')

df_gestures %>%
  filter(word_len_syls == 1) %>%
  ggplot(aes(x = as_factor(syl_position + 1))) +
  geom_bar() +
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  ylab("Number of gestures") +
  xlab("Syllable position in word") +
  ggtitle("Word template") +
  facet_wrap(~ template) +
  ggtitle("One syllable words by word template")
ggsave('figs/position_gestures_1_syl.png')

df_gestures %>%
  filter(word_len_syls == 2) %>%
  ggplot(aes(x = as_factor(syl_position + 1))) +
  geom_bar() +
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  ylab("Number of gestures") +
  xlab("Syllable position in word") +
  ggtitle("Word template") +
  facet_wrap(~ template) +
  ggtitle("Two syllable words by word template")
ggsave('figs/position_gestures_2_syl.png')

df_gestures %>%
  filter(word_len_syls == 3) %>%
  ggplot(aes(x = as_factor(syl_position + 1))) +
  geom_bar() +
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  ylab("Number of gestures") +
  xlab("Syllable position in word") +
  ggtitle("Word template") +
  facet_wrap(~ template) +
  ggtitle("Three syllable words by word template")
ggsave('figs/position_gestures_3_syl.png')

df_gestures %>%
  filter(word_len_syls == 4) %>%
  ggplot(aes(x = as_factor(syl_position + 1))) +
  geom_bar() +
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  ylab("Number of gestures") +
  xlab("Syllable position in word") +
  ggtitle("Word template") +
  facet_wrap(~ template) +
  ggtitle("Four syllable words by word template")
ggsave('figs/position_gestures_4_syl.png')

df_gestures %>%
  filter(word_len_syls == 5) %>%
  ggplot(aes(x = as_factor(syl_position + 1))) +
  geom_bar() +
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  ylab("Number of gestures") +
  xlab("Syllable position in word") +
  ggtitle("Word template") +
  facet_wrap(~ template) +
  ggtitle("Five syllable words by word template")
ggsave('figs/position_gestures_5_syl.png')

df_gestures %>%
  filter(word_len_syls == 6) %>%
  ggplot(aes(x = as_factor(syl_position + 1))) +
  geom_bar() +
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  ylab("Number of gestures") +
  xlab("Syllable position in word") +
  ggtitle("Word template") +
  facet_wrap(~ template) +
  ggtitle("One syllable words by word template")
ggsave('figs/position_gestures_6_syl.png')

df %>%
  ggplot(aes(x = as_factor(gesture), y=maxf0)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20)) +
  ylab("maxf0") +
  xlab("Has gesture")
ggsave('figs/maxf0.png')

df %>%
  ggplot(aes(x = as_factor(position), y=maxf0)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20)) +
  ylab("maxf0") +
  xlab("Has gesture")
ggsave('figs/maxf0_position.png')


df %>%
  ggplot(aes(x = as_factor(gesture), y=meanf0)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20)) +
  ylab("meanf0") +
  xlab("Has gesture")
ggsave('figs/meanf0.png')

df %>%
  ggplot(aes(x = as_factor(gesture), y=minf0)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20)) +
  ylab("minf0") +
  xlab("Has gesture")
ggsave('figs/minf0.png')

df %>%
  ggplot(aes(x = as_factor(gesture), y=mean_intensity)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20)) +
  ylab("mean intensity") +
  xlab("Has gesture")
ggsave('figs/mean_intensity.png')

df %>%
  ggplot(aes(x = as_factor(gesture), y=duration)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20)) +
  ylab("duration") +
  xlab("Has gesture")
ggsave('figs/duration.png')

df %>%
  ggplot(aes(x = duration, y=maxf0)) +
  geom_point() +
  geom_smooth(method='lm') +
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20)) +
  ylab("Max f0") +
  xlab("Duration") + 
  facet_wrap(~ position)
ggsave('figs/duration_maxf0.png')



m1 <- glmer(gesture ~ closed_syl * position + maxf0 + mean_intensity + duration + (1|subject) + (1|word), family = 'binomial', data = df)
summary(m1)

m1_dur <- lmer(duration ~ closed_syl + position * maxf0 + mean_intensity + gesture + (1|word) + (1|subject), data = df)
summary(m1_dur)

# Get words that have both gestured and ungestured tokens
double_words <- df %>%
  group_by(word, syl_position) %>%
  summarize(no_gesture = sum(gesture == 0),
            yes_gesture = sum(gesture == 1)) %>%
  filter(no_gesture > 0 & yes_gesture > 0)

double_df <- inner_join(df, double_words, by=c('word', 'syl_position')) 

m2 <- lmer(duration ~ gesture + closed_syl + position + (1|syllable), data=double_df)
summary(m2)

double_df_agg <- double_df %>%
  mutate(syllable = str_replace_all(syllable, 't͡ʃ', 'tʃ')) %>%
  group_by(word, gesture, closed_syl, subject, position, syllable) %>%
  summarize(m_duration = mean(duration))

double_df_agg %>%
  ggplot(aes(x=as_factor(gesture), y=m_duration)) +
  geom_boxplot()

double_df_agg %>%
  ggplot(aes(x=as_factor(gesture), y=m_duration)) +
  geom_boxplot() +
  facet_wrap(~ word * syllable)

gest_t <- double_df_agg %>%
  filter(gesture == 1) %>% 
  group_by(word, syllable) %>%
  summarize(m_dur = mean(m_duration))

no_gest_t <- double_df_agg %>%
  filter(gesture == 0) %>% 
  group_by(word, syllable) %>%
  summarize(m_dur = mean(m_duration))

t.test(gest_t$m_dur, no_gest_t$m_dur, paired = TRUE)
