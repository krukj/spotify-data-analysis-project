library(spotifyr)

Sys.setenv(SPOTIFY_CLIENT_ID = 'aa337ae22ca74c18b67667dfa9e50403')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'dce7d9eed7e247a8bcbb0fb18d8fcae3')

access_token <- get_spotify_access_token()

julka_data %>%
  distinct(artist_name, .keep_all = TRUE) %>%
  filter(!is.na(artist_name)) %>%
  mutate(genre = NA,
         image = NA,
         followers = NA) -> artists_ext


for(i in 5947:nrow(artists_ext)) {
  track_data <- get_track(id = gsub("spotify:track:", "", artists_ext$spotify_track_uri[i]),
                          authorization = access_token)
  
  artists <- track_data$artists
  artist_id <- artists$id[1]
  artist_data <- get_artist(artist_id, access_token)
                          
  genre <- ifelse(!is.null(artist_data$genres[1][[1]]), artist_data$genres[1][[1]], NA)
  image <- artist_data$images$url[1]
  followers <- artist_data$followers[[2]]
  
  artists_ext$genre[i] <- genre
  artists_ext$image[i] <- ifelse(!is.null(image), image, NA)
  artists_ext$followers[i] <- followers
  
  if (i %% 100 == 0) {
    Sys.sleep(10)
    cat(i, " ")
  }
}

tomek_data_ext <- left_join(tomek_data,
                            artists_ext[, c(
                                            "image",
                                            "genre",
                                            "followers",
                                            "artist_name"
                                            )],
                            by = "artist_name")
                                        
                                              
tomek_data_ext <- tomek_data_ext[, !names(tomek_data_ext) %in% c("X.1", "X", "platform", "ip_addr_decrypted",
                                                                 "conn_country", "user_agent_decrypted")]
tomek_data_ext$username <- "Tomek"
write.csv(tomek_data_ext, "tomek_data_extended.csv", row.names = FALSE)


julka_data_ext <- left_join(julka_data,
                            artists_ext[, c(
                                            "image",
                                            "genre",
                                            "followers",
                                            "artist_name"
                                            )],
                            by = "artist_name")

julka_data_ext <- julka_data_ext[, !names(julka_data_ext) %in% c("X")]
write.csv(julka_data_ext, "julka_data_extended.csv", row.names = FALSE)
