# load packages ----
library(tidyverse)

# load data ----
spotify_data <- read_csv("data/tracks_features.csv")
billboard_data <- read_csv("data/hot-100-current.csv")

billboard_join <- read_csv("data/billboard_join.csv")
spotify_join <- read_csv("data/spotify_join.csv")
manually_collected <- read_csv("data/manually_collected.csv")

testing_data <- read_csv("data/universal_top_spotify_songs.csv")

# clean spotify data ----
spotify_data %>%
  skimr::skim()

unique(spotify_data$time_signature)
unique(spotify_data$mode)

spotify_data <- spotify_data %>%
  filter(year >= 1960) %>%
  mutate(mode = factor(mode, levels = c(0, 1)),
         time_signature = factor(time_signature),
         duration_sec = duration_ms / 1000) %>%
  select(-c(duration_ms, id, album, album_id, artist_ids, track_number, disc_number, release_date)) %>%
  distinct(name, artists, .keep_all = TRUE)

manually_collected <- manually_collected %>%
  mutate(mode = factor(mode, levels = c(0, 1)),
         time_signature = factor(time_signature))

# separate process for winners ----
winners <- spotify_join %>%
  left_join(spotify_data %>%
              mutate(artist = str_match(artists, "\\[(?:\\s*)((['\"])(.*?)\\2)")[,4]) %>%
              select(-artists) %>%
              rbind(manually_collected), by = join_by(performer == artist, title == name)) %>%
  distinct(performer, title, .keep_all = TRUE) %>%
  left_join(billboard_join %>% left_join(billboard_data %>%
                                       filter(title %in% billboard_join$title, performer %in% billboard_join$performer) %>%
                                       mutate(chart_quarter = lubridate::quarter(chart_week)) %>%
                                       group_by(performer, title) %>%
                                       summarize(weeks_on_chart = max(wks_on_chart),
                                                 peak_position = min(peak_pos),
                                                 started_charting = min(year(chart_week)),
                                                 charted_q1 = any(chart_quarter == 1),
                                                 charted_q2 = any(chart_quarter == 2),
                                                 charted_q3 = any(chart_quarter == 3),
                                                 charted_q4 = any(chart_quarter == 4),
                                                 .groups = "drop"), by = join_by(performer == performer, title == title)),
            by = join_by(winner_id == winner_id)) %>%
  rename(won_grammy = won.x, artist = performer.x, song = title.x) %>%
  select(-c(winner_id, year.x, year.y, performer.y, title.y, year, won.y))
  
# combine information ----
clean_artist <- function(artist_vec) {
  pattern <- "(,|:|&|\\bwith\\b|\\band\\b|\\bfeat.\\b|\\bfeaturing\\b|\\().*"
  
  cleaned <- sub("é", "e", artist_vec, ignore.case = TRUE)
  cleaned <- sub("¥\\$[: ,]", "", cleaned)
  cleaned <- sub(pattern, "", cleaned, ignore.case = TRUE)
  
  cleaned <- trimws(cleaned)
  
  return(cleaned)
}

clean_song <- function(song_vec) {
  pattern <- "(\\?|-|\\bfeat.\\b|\\bfeaturing\\b|\\().*"
  
  cleaned <- sub(pattern, "", song_vec, ignore.case = TRUE)
  cleaned <- gsub('[\'"’‘★*]', "", cleaned)
  
  cleaned <- trimws(cleaned)
  
  return(cleaned)
}

cleaned_spotify_data <- spotify_data %>%
  mutate(artist = str_match(artists, "\\[(?:\\s*)((['\"])(.*?)\\2)")[,4]) %>%
  select(-artists) %>%
  rbind(manually_collected) %>%
  mutate(join_artist = tolower(clean_artist(artist)), join_name = tolower(clean_song(name))) %>%
  distinct(join_name, join_artist, .keep_all = TRUE)

cleaned_billboard_data <- billboard_data %>%
  filter(year(chart_week) >= 1960) %>%
  mutate(chart_quarter = lubridate::quarter(chart_week)) %>%
  group_by(performer, title) %>%
  summarize(weeks_on_chart = max(wks_on_chart),
            peak_position = min(peak_pos),
            started_charting = min(year(chart_week)),
            charted_q1 = any(chart_quarter == 1),
            charted_q2 = any(chart_quarter == 2),
            charted_q3 = any(chart_quarter == 3),
            charted_q4 = any(chart_quarter == 4),
            .groups = "drop") %>%
  mutate(join_artist = tolower(clean_artist(performer)), join_name = tolower(clean_song(title)))

combined_data <- cleaned_spotify_data %>%
  left_join(cleaned_billboard_data, by = join_by(join_artist == join_artist, join_name == join_name)) %>%
  mutate(won_grammy = "No") %>%
  rename(song = name) %>%
  select(-c(join_artist, join_name, performer, title, year))

modeling_data <- winners %>%
  rbind(combined_data) %>%
  filter(!is.na(started_charting)) %>%
  mutate(won_grammy = factor(won_grammy)) %>%
  distinct(tolower(artist), tolower(song), .keep_all = TRUE) %>%
  select(-c('tolower(artist)', 'tolower(song)'))

# this filters out Michelle - The Beatles, which one but never charted
# if we expand to having non-charting songs in the future, we can resolve this

# write out modeling data ----
write.csv(modeling_data, file = "data/modeling_data.csv", row.names = FALSE)

# cleaning testing data ----
testing_data <- testing_data %>%
  mutate(join_artist = tolower(clean_artist(artists)), join_name = tolower(clean_song(name)),
         mode = factor(mode, levels = c(0, 1)),
         time_signature = factor(time_signature),
         duration_sec = duration_ms / 1000) %>%
  select(-c(spotify_id, daily_rank, daily_movement, weekly_movement, country,
            snapshot_date, popularity, album_name, album_release_date, duration_ms)) %>%
  left_join(cleaned_billboard_data, by = join_by(join_artist == join_artist, join_name == join_name)) %>%
  distinct(join_name, join_artist, .keep_all = TRUE) %>%
  rename(song = name, artist = artists) %>% filter(!is.na(charted_q1)) %>%
  select(-c(join_artist, join_name, performer, title))

# write out testing data ----
write.csv(testing_data, file = "data/testing_data.csv", row.names = FALSE)
