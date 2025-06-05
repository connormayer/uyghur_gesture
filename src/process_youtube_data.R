library(tidyverse)
library(lme4)
library(viridis)
library(ggsignif)
library(ggpubr)
library(lmerTest)

df <- read_csv("data/full_syllable_data.csv", na=c("", "NA", "--undefined--")) %>%
  mutate(position = ifelse(is_initial, 'initial',
                           ifelse(is_final, 'final',
                           'medial')),
         template = str_replace_all(word, '[aeiouüöé]', 'V'),
         template = str_replace_all(template, '[^V\\.]', 'C'),
         template = str_replace_all(template, '[CV]*C(\\W|$)', 'H\\1'),
         template = str_replace_all(template, '[CV]*V(\\W|$)', 'L\\1'),
         other_heavy = preceding_heavy | following_heavy,
         closed_syl = fct_recode(as_factor(closed_syl), "light" = "FALSE", "heavy" = "TRUE"),
         position = fct_relevel(position, "initial", "medial", "final")) %>%
  group_by(subject) %>%
  mutate(maxf0 = scale(maxf0),
         minf0 = scale(minf0),
         meanf0 = scale(meanf0),
         mean_intensity = scale(mean_intensity),
         duration = scale(duration)) %>%
  ungroup()

df[df$word_len_syls == 1,]$position <- 'final'

df_gestures <- df %>% 
  group_by(closed_syl) %>%
  summarize(foo = sum(gesture) / n())

# gesture = fct_recode(as_factor(gesture), "no gesture" = '0', "gesture" = "1"),
# closed_syl = fct_recode(as_factor(closed_syl), "light" = "FALSE", "heavy" = "TRUE")

# df_gestures %>% 
#   ggplot(aes(x = other_heavy)) + 
#   geom_bar() +
#   theme_minimal() +
#   theme(axis.text = element_text(size=20),
#         axis.title = element_text(size=20),
#         legend.text = element_text(size=12),
#         legend.title = element_text(size=20)) +
#   ylab("Number of gestures") +
#   xlab("Other heavy syllable in word?") +
#   facet_wrap(~ closed_syl) + 
# ggsave('figs/other_heavy_gestures.png')

df %>% 
  ggplot(aes(x = subject, group=gesture, fill=gesture)) + 
  geom_bar() +
  theme_minimal() +
  theme(axis.text = element_text(size=25),
        axis.title = element_text(size=25),
        legend.text = element_text(size=25),
        legend.title = element_blank()) +
  ylab("Number of syllables") +
  xlab("Speaker") +
  scale_fill_viridis(discrete=TRUE)
ggsave('figs/subject_gestures.png')

# df %>%
#   ggplot(aes(x = closed_syl)) +
#   geom_bar(aes(group=gesture, fill=gesture)) +
#   theme_minimal() +
#   theme(axis.text = element_text(size=25),
#         axis.title = element_text(size=25),
#         legend.text = element_text(size=25),
#         legend.title = element_blank()) +
#   ylab("Number of syllables") + 
#   xlab("Syllable weight") +
#   scale_fill_viridis(discrete=TRUE) +
#   ylim(NA, 3000) + 
#   geom_bracket(xmin="light", 
#                xmax="heavy",
#                label = "***", 
#                y.position = 2000, 
#                size = 1,
#                label.size = 20, inherit.aes=FALSE)
#   ggsave('figs/heavy_gestures.png')
#   
df_gestures %>%
  ggplot(aes(x = closed_syl, y=foo)) +
  geom_bar(stat='identity') +
  theme_minimal() +
  theme(axis.text = element_text(size=25),
        axis.title = element_text(size=25),
        legend.text = element_text(size=25),
        legend.title = element_blank()) +
  ylab("Proportion of syllables with gestures") + 
  xlab("Syllable weight") +
  scale_fill_viridis(discrete=TRUE) +
  geom_bracket(xmin="light", 
               xmax="heavy",
               label = "*", 
               y.position = 0.25, 
               size = 1,
               label.size = 20, inherit.aes=FALSE,
               tip.length = 0.07) +
  ylim(NA, 0.35)
ggsave('figs/heavy_gestures.png')

df_pos <- df %>% 
  filter(word_len_syls > 1) %>%
  group_by(position) %>%
  summarize(foo = sum(gesture) / n())

df_pos %>%
  ggplot(aes(x = position, y = foo)) +
  geom_bar(stat='identity') +
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20)) +
  ylab("Proportion of syllables with gestures") +
  xlab("Syllable position in word") 
ggsave('figs/position_gestures.png')

df_gestures <- df %>% 
  group_by(position, word_len_syls, syl_position, template) %>%
  summarize(foo = sum(gesture) / n())

df_gestures %>%
  filter(word_len_syls == 1) %>%
  ggplot(aes(x = as_factor(syl_position + 1), y=foo)) +
  geom_bar(stat='identity') +
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
  ggplot(aes(x = as_factor(syl_position + 1), y=foo)) +
  geom_bar(stat='identity') +
  theme_minimal() +
  theme(axis.text = element_text(size=25),
        axis.title = element_text(size=25),
        legend.text = element_text(size=12),
        legend.title = element_text(size=25),
        title = element_text(size=25),
        panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        strip.text = element_text(size=25)) +
  ylab("Proportion of syllables with gestures") +
  xlab("Syllable position in word") +
  ggtitle("Word template") +
  facet_wrap(~ template) +
  ggtitle("Two syllable words by syllable weight")
ggsave('figs/position_gestures_2_syl.png')

df_gestures %>%
  filter(word_len_syls == 3) %>%
  ggplot(aes(x = as_factor(syl_position + 1), y=foo)) +
  geom_bar(stat='identity') +
  theme_minimal() +
  theme(axis.text = element_text(size=25),
        axis.title = element_text(size=25),
        legend.text = element_text(size=12),
        legend.title = element_text(size=25),
        title = element_text(size=25),
        panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        strip.text = element_text(size=25)) +
  ylab("Proportion of syllables with gestures") +
  xlab("Syllable position in word") +
  ggtitle("Word template") +
  facet_wrap(~ template) +
  ggtitle("Three syllable words by syllable weight")
ggsave('figs/position_gestures_3_syl.png')

df_gestures %>%
  filter(word_len_syls == 4) %>%
  ggplot(aes(x = as_factor(syl_position + 1), y=foo)) +
  geom_bar(stat='identity') +
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
  ggplot(aes(x = as_factor(syl_position + 1), y=foo)) +
  geom_bar(stat='identity') +
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
  mutate(gesture = fct_recode(as_factor(gesture), "no gesture" = '0', "gesture" = "1")) %>%
  ggplot(aes(x = gesture, y=maxf0, fill=gesture)) +
  geom_boxplot(outlier.shape=NA) +
  theme_minimal() +
  theme(axis.text = element_text(size=25),
        axis.title.y = element_text(size=25),
        axis.title.x = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=25)) +
  scale_fill_discrete(guide="none") +
  ylab("Max syllable f0 (Z-score)") +
  ylim(NA, 3)
ggsave('figs/maxf0.png')

# df %>%
#   ggplot(aes(x = as_factor(position), y=maxf0)) +
#   geom_boxplot() +
#   theme_minimal() +
#   theme(axis.text = element_text(size=20),
#         axis.title = element_text(size=20),
#         legend.text = element_text(size=12),
#         legend.title = element_text(size=20)) +
#   ylab("maxf0") +
#   xlab("Has gesture")
# ggsave('figs/maxf0_position.png')
# 
# 
# df %>%
#   ggplot(aes(x = as_factor(gesture), y=meanf0)) +
#   geom_boxplot() +
#   theme_minimal() +
#   theme(axis.text = element_text(size=20),
#         axis.title = element_text(size=20),
#         legend.text = element_text(size=12),
#         legend.title = element_text(size=20)) +
#   ylab("meanf0") +
#   xlab("Has gesture")
# ggsave('figs/meanf0.png')

# df %>%
#   ggplot(aes(x = as_factor(gesture), y=minf0)) +
#   geom_boxplot() +
#   theme_minimal() +
#   theme(axis.text = element_text(size=20),
#         axis.title = element_text(size=20),
#         legend.text = element_text(size=12),
#         legend.title = element_text(size=20)) +
#   ylab("minf0") +
#   xlab("Has gesture")
# ggsave('figs/minf0.png')

df %>%
  mutate(gesture = fct_recode(as_factor(gesture), "no gesture" = '0', "gesture" = "1")) %>%
  ggplot(aes(x = gesture, y=mean_intensity, fill=gesture)) +
  geom_boxplot(outlier.shape=NA) +
  theme_minimal() +
  theme(axis.text = element_text(size=25),
        axis.title.y = element_text(size=25),
        axis.title.x = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=25)) +
  scale_fill_discrete(guide="none") +
  ylab("Mean syllable intensity (Z-score)") +
  ylim(-3, 3)
ggsave('figs/mean_intensity.png')

df %>%
  mutate(gesture = fct_recode(as_factor(gesture), "no gesture" = '0', "gesture" = "1")) %>%
  ggplot(aes(x = gesture, y=duration, fill=gesture)) +
  geom_boxplot(outlier.shape=NA) +
  theme_minimal() +
  theme(axis.text = element_text(size=25),
        axis.title.y = element_text(size=25),
        axis.title.x = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=25)) +
  scale_fill_discrete(guide="none") +
  ylab("Syllable duration (Z-score)") +
  geom_bracket(xmin="no gesture", 
               xmax="gesture",
               label = "*", 
               y.position = 2.5, 
               size = 1,
               label.size = 15, inherit.aes=FALSE,
               tip.length = 0.03) +
  ylim(NA, 3)
ggsave('figs/duration.png')

df %>%
  ggplot(aes(x = closed_syl, y=duration, fill=closed_syl)) +
  geom_boxplot(outlier.shape=NA) +
  theme_minimal() +
  theme(axis.text = element_text(size=25),
        axis.title.y = element_text(size=25),
        axis.title.x = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=25)) +
  scale_fill_discrete(guide="none") +
  ylab("Syllable duration (Z-score)") +
  geom_bracket(xmin="light", 
               xmax="heavy",
               label = "***", 
               y.position = 2.5, 
               size = 1,
               label.size = 15, inherit.aes=FALSE,
               tip.length = 0.03) +
  ylim(NA, 3)
ggsave('figs/duration.png')

df %>%
  ggplot(aes(x = position, y=duration, fill=position)) +
  geom_boxplot(outlier.shape=NA) +
  theme_minimal() +
  theme(axis.text = element_text(size=25),
        axis.title.y = element_text(size=25),
        axis.title.x = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=25)) +
  scale_fill_discrete(guide="none") +
  ylab("Syllable duration (Z-score)") +
  geom_bracket(xmin=c('initial', 'initial', 'medial'), 
               xmax=c('medial', 'final', 'final'),
               label = c('**', '**', '**'), 
               y.position = c(2.5, 3.75, 3), 
               size = 1,
               label.size = 15, inherit.aes=FALSE,
               tip.length = 0.03) +
  ylim(NA, 4)
ggsave('figs/duration.png')

df %>%
  ggplot(aes(x = duration, y=maxf0)) +
  geom_point() +
  geom_smooth(method='lm') +
  theme_minimal() +
  theme(axis.text = element_text(size=25),
        axis.title = element_text(size=25),
        legend.text = element_text(size=12),
        legend.title = element_text(size=25),
        panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        strip.text = element_text(size=25)) +
  ylab("Max syllable f0 (Z-score)") +
  xlab("Syllable duration (Z-score)") + 
  facet_wrap(~ position)
ggsave('figs/duration_maxf0.jpg')

df %>%
  mutate(gesture = fct_recode(as_factor(gesture), "no gesture" = '0', "gesture" = "1")) %>%
  ggplot(aes(x = gesture, y=duration, fill=gesture)) +
  geom_boxplot(outlier.shape=NA) +
  scale_fill_discrete(guide="none") +
  theme_minimal() +
  theme(axis.text = element_text(size=25),
        axis.title.y = element_text(size=25),
        axis.title.x = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=25),
        panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        strip.text = element_text(size=25)) +
  ylab("Syllable duration (Z-score)") +
  facet_wrap(~ closed_syl + position, nrow = 2) +
  ylim(NA, 2)
ggsave('figs/duration_maxf0.jpg')


# df %>%
#   ggplot(aes(x = duration, y=meanf0)) +
#   geom_point() +
#   geom_smooth(method='lm') +
#   theme_minimal() +
#   theme(axis.text = element_text(size=20),
#         axis.title = element_text(size=20),
#         legend.text = element_text(size=12),
#         legend.title = element_text(size=20)) +
#   ylab("Mean f0") +
#   xlab("Duration") + 
#   facet_wrap(~ position)
# ggsave('figs/duration_meanf0.jpg')
# 
# df %>%
#   ggplot(aes(x = duration, y=minf0)) +
#   geom_point() +
#   geom_smooth(method='lm') +
#   theme_minimal() +
#   theme(axis.text = element_text(size=20),
#         axis.title = element_text(size=20),
#         legend.text = element_text(size=12),
#         legend.title = element_text(size=20)) +
#   ylab("Min f0") +
#   xlab("Duration") + 
#   facet_wrap(~ position)
# ggsave('figs/duration_minf0.jpg')

df2 <- df
df2[df2$word_len_syls == 1,]$position <- 'final'

df3 <- df %>% filter(word_len_syls > 1)

m1 <- glmer(gesture ~ closed_syl * position + maxf0 + mean_intensity + duration + (1|subject) + (1|syllable), family = 'binomial', data = df2)
summary(m1)

m1_dur <- lmer(duration ~ closed_syl + position * maxf0 + mean_intensity + gesture * position + (1|syllable), data = df2)
summary(m1_dur)

m1 <- glmer(gesture ~ closed_syl * position + maxf0 + mean_intensity + duration + (1|subject) + (1|syllable), family = 'binomial', data = df3)
summary(m1)

m1_dur <- lmer(duration ~ closed_syl + position * maxf0 + mean_intensity + gesture * position + (1|subject) + (1|syllable), data = df3)
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
  group_by(word, gesture, closed_syl, subject, syl_position, syllable) %>%
  summarize(m_duration = mean(duration))

double_df_agg %>%
  mutate(gesture = as_factor(gesture)) %>%
  mutate(gesture = fct_recode(gesture, "no gesture" = '0', "gesture" = "1")) %>%
  ggplot(aes(x=closed_syl, y=m_duration, fill = gesture)) +
  geom_boxplot(outlier.shape=NA) +
  #scale_fill_discrete(guide="none") +
  theme_minimal() +
  theme(axis.text = element_text(size=25),
        axis.title.y = element_text(size=25),
        axis.title.x = element_blank(),
        legend.text = element_text(size=25),
        legend.title = element_blank(),
        panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        strip.text = element_text(size=25)) +
  ylim(NA, 2.75) + 
  ylab("Syllable duration (Z-score)")

double_df_agg %>%
  mutate(gesture = as_factor(gesture)) %>%
  mutate(gesture = fct_recode(gesture, "no gesture" = '0', "gesture" = "1")) %>%
  ggplot(aes(x=gesture, y=m_duration)) +
  geom_boxplot() +
  facet_wrap(~ word * syllable, scales='free')

gest_t <- double_df_agg %>%
  filter(gesture == 1) %>% 
  group_by(word, syllable) %>%
  summarize(m_dur = mean(m_duration))

no_gest_t <- double_df_agg %>%
  filter(gesture == 0) %>% 
  group_by(word, syllable) %>%
  summarize(m_dur = mean(m_duration))

t.test(gest_t$m_dur, no_gest_t$m_dur, paired = TRUE)

df %>% 
  ungroup() %>%
  filter(gesture == 1) %>%
  select(word) %>%
  write_csv('data/gesture_words.csv')

df %>% 
  ungroup() %>%
  select(word) %>%
  write_csv('data/all_words.csv')


