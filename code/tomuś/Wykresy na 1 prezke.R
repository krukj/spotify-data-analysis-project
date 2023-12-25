library(lubridate)
library(tidyr)
library(extrafont)
library(plotly)


tomek <- read.csv("tomek_extended.csv")

tomek %>%
  rename(timeDate = ts,
         track = master_metadata_track_name,
         artist = master_metadata_album_artist_name,
         album = master_metadata_album_album_name) %>%
  separate(timeDate, into = c("date", "time"), sep = "T") %>%
  mutate(date = ymd(date),
         time = hms(time)) %>%
  select(date, time, ms_played, track, artist, album,
         reason_start, reason_end, shuffle, skipped, offline, offline_timestamp,
         danceability, energy, key, loudness, mode, speechiness, acousticness,
         instrumentalness, liveness, valence, tempo) -> tomek

################################################################################
# Fav artist

tomek %>%
  filter(date >= ymd("2023-03-20"),
         !is.na(artist)) %>%
  group_by(artist) %>%
  summarise(minutesPlayed = round(sum(ms_played) / (1000 * 60))) %>%
  arrange(minutesPlayed) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(artist, minutesPlayed), y = minutesPlayed,
             text = paste("Artist:", artist, "<br>Total minutes played:", minutesPlayed))) +
  geom_bar(stat = "identity", fill = "#1DB954") +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "artist",
       y = "minutes played",
       title = "Favourite artists from last 9 months") +
  theme(
    text = element_text(family = "Roboto"),
    plot.title = element_text(size = 22, hjust = 0.5, color = '#1DB954'),
    panel.background = element_rect(fill = "#191414"),
    plot.background = element_rect(fill = "#191414"), 
    axis.text.y = element_text(size = 14, color = '#1DB954'),
    axis.text.x = element_text(size = 14, color = '#1DB954'),
    axis.title = element_text(size = 18, color = '#1DB954'),
    panel.border = element_rect(color = "#3b3939", fill = NA, size = 0.5),
    panel.grid.major = element_line(color = "#3b3939", size = 0.1),
    panel.grid.minor = element_line(color = "#3b3939", size = 0.1)
  ) -> pArtists
ggplotly(pArtists, tooltip = "text")


################################################################################
# Density
tomek %>%
  filter(artist %in% c("Frédéric Chopin", "Taco Hemingway", "rów babicze")) %>%
  ggplot(aes(x = artist, y = tempo)) +
  geom_violin(fill = "#1DB954", color = "#1DB954", size = 0.5) +
  scale_y_continuous(limits = c(50, 210), expand = c(0, 0)) +
  coord_flip() +
  labs(x = "artist",
       y = "tempo",
       title = "Distribution of listened songs tempo for certain artists") +
  theme(
    text = element_text(family = "Gotham"),
    plot.title = element_text(size = 22, hjust = 0.5, color = '#1DB954'),
    panel.background = element_rect(fill = "#191414"),
    plot.background = element_rect(fill = "#191414"), 
    axis.text.y = element_text(size = 14, color = '#1DB954'),
    axis.text.x = element_text(size = 14, color = '#1DB954'),
    axis.title = element_text(size = 18, color = '#1DB954'),
    panel.border = element_rect(color = "#3b3939", fill = NA, size = 0.5),
    panel.grid.major = element_line(color = "#3b3939", size = 0.1),
    panel.grid.minor = element_line(color = "#3b3939", size = 0.1)
  ) -> p
ggplotly(p)
renderPlotly(p)  
  