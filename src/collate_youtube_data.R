library(tidyverse)

syllable_data <- read_csv('data/syllable_data.csv') %>%
  mutate(start_time = round(start_time, digits=2),
         end_time = round(end_time, digits=2))

pitchpro_a <- read_tsv('data/pitchpro_gulnisa.tsv')
pitchpro_b <- read_tsv('data/pitchpro_mustafa.tsv')
pitchpro_c <- read_tsv('data/pitchpro_elise.tsv')

gesture_data <- read_csv('data/uyghur_gestures_annotated.csv') %>%
  mutate(Subject = ifelse(Subject == 'Gulnisa', 'A',
                          ifelse(Subject == 'Mustafa', 'B', 'C')),
         t1_ph = round(t1_ph, digits=2),
         t2_ph = round(t2_ph, digits=2))

syllable_a <- syllable_data %>%
  filter(subject == 'A')

syllable_b <- syllable_data %>%
  filter(subject == 'B')

syllable_c <- syllable_data %>%
  filter(subject == 'C')

subj_a <- cbind(syllable_a, pitchpro_a) %>%
  select(-rowLabel)

subj_b <- cbind(syllable_b, pitchpro_b) %>%
  select(-rowLabel)

subj_c <- cbind(syllable_c, pitchpro_c) %>%
  select(-rowLabel)

full_data <- rbind(subj_a, subj_b, subj_c)

for (gest_i in 1:nrow(gesture_data)) {
  found <- FALSE
  for (data_i in 1:nrow(full_data)) {
    gest_row <- gesture_data[gest_i,]
    data_row <- full_data[data_i,]
    if (gest_row$t1_ph >= data_row$start_time && gest_row$t2_ph <= data_row$end_time && gest_row$Subject == data_row$subject) {
      full_data[data_i,]$gesture <- 1
      found <- TRUE
      break
    }
  }
  if (!found) {
    print(gest_row)
  }
}

full_data %>% write_csv('data/full_syllable_data.csv')
