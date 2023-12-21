library(ggplot2)
library(gt)


# data prep

julka_ext$ts <- as.POSIXct(all_extended$ts, format = "%Y-%m-%dT%H:%M:%SZ")
julka_ext$username <- "Julka"
julka_ext <- julka_ext %>% 
  select(-ip_addr_decrypted, -user_agent_decrypted)
colnames(julka_ext)[which(names(julka_ext) == "ts")] <- "time"
colnames(julka_ext)[which(names(julka_ext) ==
                        "master_metadata_track_name")] <- "track_name"
colnames(julka_ext)[which(names(julka_ext) == 
                        "master_metadata_album_album_name")] <- "album_name"
colnames(julka_ext)[which(names(julka_ext) ==
                        "master_metadata_album_artist_name")] <- "artist_name"



# top 10 songs + album + times played

top_10_songs <- julka_ext %>% 
  # filter time ...
  group_by(track_name, artist_name, album_name) %>%
  summarise(
    minutes_listened = round(sum(ms_played) / (1000 * 60)),
    songs_played = n()
  ) %>%
  arrange(desc(songs_played)) %>%
  na.omit() %>%
  head(10)

# favourite artist + times played + minutes listened

favourite_artist <- julka_ext %>% 
  # filter time ...
  group_by(artist_name) %>% 
  summarise(
    minutes_listened = sum(ms_played) / (1000 * 60),
    songs_played = n()
  ) %>%
  arrange(desc(songs_played)) %>%
  na.omit() %>%
  head(1)


# listening repartition over day


data_with_hour <- julka_ext
data_with_hour$time <- strptime(data_with_hour$time, 
                                format ="%Y-%m-%d %H:%M:%S")
data_with_hour$hour <- format(data_with_hour$time, "%H")


listening_repartition <- data_with_hour %>% 
  # filter time ...
  group_by(hour) %>% 
  summarise(minutes_listened = sum(ms_played) / (1000 * 60)) %>% 
  mutate(total_time = sum(minutes_listened),
         repartition = (minutes_listened / total_time) * 100)

# listening_repart_plot

listening_repart_plot <- ggplot(listening_repartition,
                                aes(x = hour, y = repartition)) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), 
                     limits = c(0, 7.5)) +
  labs(title = "Listening repartition over day") +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(b = 10))
  )

listening_repart_plot
ggplotly(listening_repart_plot)

listening_repart_plotly <- plot_ly(
  listening_repartition,
  x = ~hour,
  y = ~repartition,
  type = 'bar',
  hoverinfo = 'text',
  text = ~paste("..")
) %>%
  layout(
    title = "Listening repartition over day",
    yaxis = list(title = "Repartition")
  )

listening_repart_plotly


# top_10_songs_table 

top_10_songs <- as.data.frame(top_10_songs)

top_10_songs %>%
  gt() %>%
  tab_header(title = "Top 10 songs")
