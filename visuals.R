# load packages ----
library(tidyverse)
library(ggthemes)
library(gtExtras)

# load data ----
data <- read_csv("data/modeling_data.csv")
predictions <- read_csv("data/top_prediction.csv")

# primary histograms ----
data %>%
  ggplot(aes(x = peak_position)) +
  geom_histogram(color = "black", fill = "plum", binwidth = 5) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  labs(title = "Histogram of ...", subtitle = "...",
       x = "Peak Position", y = "Count",
       caption = "Source: Spotify, Billboard Top 100")

# description tables ----
audio_features <- tibble(feature = c(histogram_vars), description =
                           c("This feature describes how suitable a track is for dancing based on musical elements like tempo, rhythm stability, beat strength, and overall regularity. A higher danceability score indicates a more danceable track.",
                             "Representing the intensity and activity level of a track, this feature is often correlated with fast, loud, and noisy tracks. Tracks with high energy scores tend to be more upbeat and energetic.",
                             "Representing the overall loudness of a track in decibels (dB), this feature can be useful for audio normalization or volume adjustment purposes.",
                             "Speechiness identifies tracks that contain spoken words, like podcasts or rap music. Higher values indicate more speech-like sounds, distinguishing them from purely instrumental tracks.",
                             "This feature is a confidence measure of whether a track is acoustic. Tracks with higher acousticness scores are more likely to be acoustic, with little or no electronic elements.",
                             "As the name suggests, this feature predicts whether a track contains no vocals. A higher instrumentalness score indicates a higher likelihood that the track is purely instrumental.",
                             "Liveness detects the presence of an audience in the recording. A higher value suggests the track was performed live, offering a sense of authenticity and connection.",
                             "Also known as the “positiveness” score, valence describes the musical positiveness conveyed by a track. Tracks with higher valence scores tend to sound more positive, cheerful, and euphoric, while lower scores suggest more negative, sad, or depressing emotions.",
                             "The overall estimated tempo of a track, measured in beats per minute (BPM), can be crucial for applications like automatic DJ mixing or beat-synchronized visualizations."))

selection <- audio_features %>%
  filter(feature == "danceability") %>%
  select(description)

header_title <- str_to_title(selection$feature)

selection %>%
  gt() %>%
  tab_header(title = header_title) %>%
  tab_style(locations = cells_title(), cell_fill("plum")) %>%
  tab_style(locations = cells_title(), cell_text(color = "black", weight = "bold", size = "20px")) %>%
  tab_options(table.width = "350px", column_labels.hidden = TRUE, table.font.size = "16px")

histogram_vars <- colnames(data %>% select(-c(artist, song, won_grammy, explicit, key, mode, time_signature, tempo, loudness,
                                              duration_sec, started_charting, weeks_on_chart, peak_position, starts_with("charted"))))

# song feature chart ----
testing_data %>%
  filter(song == "Espresso") %>%
  select(histogram_vars) %>%
  pivot_longer(cols = -c(), names_to = "feature", values_to = "value") %>%
  ggplot(aes(x = reorder(feature, -value), y = value)) +
  geom_col(color = "black", fill = "cornflowerblue", alpha = 0.9) +
  theme_fivethirtyeight() +
  theme(axis.title.y = element_text(), plot.title = element_text(size = 24), plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 12)) +
  labs(title = "The Most Important Features of Espresso by Sabrina Carpenter",
       subtitle = "To be determined",
       y = "Value", caption = "\nSource: Spotify Track Data via Kaggle")

# billboard history table ----
testing_data %>%
  filter(song == "Espresso") %>%
  select(weeks_on_chart, peak_position, charted_q1, charted_q2, charted_q3, charted_q4) %>%
  mutate(q1 = if_else(charted_q1 == TRUE, "✔",  "❌"), 
         q2 = if_else(charted_q2 == TRUE, "✔",  "❌"),
         q3 = if_else(charted_q3 == TRUE, "✔",  "❌"),
         q4 = if_else(charted_q4 == TRUE, "✔",  "❌"), .keep = "unused") %>% 
  head(1) %>%
  gt() %>%
  tab_header(title = "Billboard Chart Summary") %>%
  cols_label(weeks_on_chart = "Weeks", peak_position = "Peak", q1 = "Q1", q2 = "Q2", q3 = "Q3", q4 = "Q4") %>%
  gt_color_box(columns = c(weeks_on_chart), domain = c(0, 92), palette = c("lightgrey", "purple")) %>%
  gt_color_box(columns = c(peak_position), domain = c(1, 100), palette = c("purple", "lightgrey")) %>%
  fmt_markdown(columns = q1:q4)

# donut charts ----
to_percentile <- ecdf(predictions$Grammy_Prob)

predictions <- predictions %>%
  mutate(percentile = to_percentile(Grammy_Prob) * 100)

predictions %>%
  filter(song == "Espresso") %>%
  ggplot(aes(x = 2, y = percentile, fill = percentile)) +
  geom_bar(stat = "identity", width = 1) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_gradientn(colors = c("red2", "yellow2", "green2"),
                       values = c(0, 0.1, 1),
                       limits = c(1, 100)) +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  theme_void() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#F0F0F0", linewidth = 0),
        plot.background = element_rect(fill = "#F0F0F0", linewidth = 0),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(0, 0, 0, 0),
        panel.spacing = unit(0, "null"))
