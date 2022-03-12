# ///////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#
#                       COLLECTING METAL TRACKS' FEATURES                      #
#                                                                              #
#                  Collect name, id, artist's name, and features               #
#             of progressive and generic metal tracks from Spotify             #
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\//////////////////////////////////////#
#                      **** Code author: Simon Gorin ****                      #

# Ensures that the project root that contains the R script indicated is set up correctly
here::i_am("data/collect_prog_and_metal_artists.R")

#==============================================================================#
####------------------------- 1 Loading libraries --------------------------####
#==============================================================================#

library(spotifyr)     # awesome package to get Spotify data
library(tidyverse)    # collections of package for data science
library(keyring)      # to deal with stored passwords/credentials
library(here)         # to work with relative path

# Source a custom adaptation of the 'get_track_audio_analysis' to access the
# confidence of the reliability of different musical features
source(here("functions", "get_track_audio_analysis_confidence.R"))

# You need to indicate your Spotify client ID and secret. For more information
# on how to do that when using the spotifyr package, see here:
# https://github.com/charlie86/spotifyr#authentication

Sys.setenv(SPOTIFY_CLIENT_ID = "XXX")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "XXX")


#==============================================================================#
####--------------------- 2 Creating lists of artists ----------------------####
#==============================================================================#

# After a quick search on the web, I found three interesting playlists containing
# generic metal songs. The code chunk below retrieves all the songs from these
# playlists and collect the name and id of all artists in these songs. This is
# done using the 'get_playlist_tracks' function from the 'spotifyr' package.

metal_mix_by_spotify <- get_playlist_tracks('37i9dQZF1EQpgT26jgbgRI',
                                            fields = c("track.album.artists"))

kickass_metal_by_spotify <- get_playlist_tracks('37i9dQZF1DWTcqUzwhNmKv',
                                                fields = c("track.album.artists"))


metal_essentials_by_spotify <- get_playlist_tracks('37i9dQZF1DWWOaP4H0w5b0',
                                                   fields = c("track.album.artists"))

generic_metal_artists <-
    bind_rows(metal_mix_by_spotify,
              kickass_metal_by_spotify,
              metal_essentials_by_spotify) %>%
    unnest(track.album.artists) %>%
    select(name, id) %>%
    distinct()

# Same as in the chunk above but with progressive metal playlists...

progressive_metal_by_spotify <-
    get_playlist_tracks('37i9dQZF1DX5wgKYQVRARv',
                        fields = c("track.album.artists"))

progressive_metal_by_century_media <-
    get_playlist_tracks('62sl2B97a8xD7B99pNdJwc',
                        fields = c("track.album.artists"))

sound_of_progressive_metal_by_spotify <-
    get_playlist_tracks('74RFwHMUEVXkqK2KeeIMPl',
                        fields = c("track.album.artists"))

prog_metal_artists <-
    bind_rows(progressive_metal_by_spotify,
              progressive_metal_by_century_media,
              sound_of_progressive_metal_by_spotify) %>%
    unnest(track.album.artists) %>%
    select(name, id) %>%
    distinct()

# The code chunk below filters the two lists by excluding from the
# lists any artists they have in common.

generic_metal_artists_final <-
    generic_metal_artists %>%
    filter(!(id %in% unique(prog_metal_artists$id)))

prog_metal_artists_final <-
    prog_metal_artists %>%
    filter(!(id %in% unique(generic_metal_artists$id)))


#==============================================================================#
####---------------- 3 Retrieving songs from all the artists ---------------####
#==============================================================================#

# The vector below contains a list of keywords used to filter out album/songs. The
# purpose of the list is to reduce the presence of duplicate and "non-musical"
# tracks. For instance, "live" songs/albums are probably duplicates as it is
# likely that a "studio" version also exists.

song_album_vector_filter <- c("intro", "outro", "interlude", "commentary", "remaster",
                              "edition", "compilation", "live", "version", "remix",
                              "acoustic", "- live", "\\(live", "\\[live", "unplugged",
                              "deluxe", "reissue", "anniversary", "bonus", "cover", "redux")

# Collapse all the keyword in a single string. The list is probably not optimal
# but I think this is doing a fair job for now...

song_album_filter <- glue::glue_collapse(song_album_vector_filter, sep = "|")

# For each artist in 'generic_metal_artists_final', all artist's albums are
# retrieved using the 'get_artist_albums' function from the 'spotifyr' package.
# To reduce the possibility of having duplicates, any album with a word matching
# one of those in 'song_album_filter' described in the first section are ignored.

generic_metal_albums <-
    map_df(generic_metal_artists_final$id, ~ get_artist_albums(id = .x, include_groups = c("album"))) %>%
    filter(!str_detect(str_to_lower(name), song_album_filter)) %>%
    unnest(artists, names_sep = "_") %>%
    # The code below ensures that if several instances of the same album exist
    # only the first one is considered
    group_by(artists_name, name) %>%
    slice(1) %>%
    ungroup()

# The code chunk below retrieves each song from each album referenced in
# 'generic_metal_albums' and then retrieves the features of each song.
# PLEASE BE AWARE THAT THIS STEP MAY TAKE SOME TIME

generic_metal_tracks <-
    map_df(generic_metal_albums$id, ~ get_album_tracks(id = .x)) %>%
    filter(!str_detect(str_to_lower(name), song_album_filter)) %>%
    unnest(artists, names_sep = "_") %>%
    distinct(id, .keep_all = TRUE) %>%
    group_by(str_to_lower(name), str_to_lower(artists_name)) %>%
    slice(1) %>%
    ungroup() %>%
    select(id, name, artists_name)

generic_metal_tracks_features <-
    generic_metal_tracks %>%
    # Features can be retrieved for 100 tracks at once.
    # Data are then split into set of 100 tracks to speed up the process
    mutate(n_group_features = rep(c(1:((n() %/% 100) + 1)), each = 100)[1:n()]) %>%
    select(id, n_group_features) %>%
    group_split(n_group_features) %>%
    map_df(., ~ get_track_audio_features(.x$id)) %>%
    left_join(generic_metal_tracks, by = "id") %>%
    filter(acousticness < 1,  # Removing acoustic tracks (according to Spotify 1 is acoustic),
           liveness <= 0.8,   # Removing live tracks (according to Spotify above 0.8 provides strong likelihood that a track is live)
           energy > 0,        # Removing tracks with 0 energy (potential silent songs)
           tempo > 0) %>%     # Removing tracks with tempo equals 0 (potential silent/transition songs)
    bind_cols(map_dfr(.$analysis_url, get_track_audio_analysis_confidence)) %>%
    select(-c(liveness, acousticness, type, uri, track_href))

# Retrieves each album of the artists in 'prog_metal_artists_final'

prog_metal_albums <-
    map_df(prog_metal_artists_final$id, ~ get_artist_albums(id = .x, include_groups = c("album"))) %>%
    filter(!str_detect(str_to_lower(name), song_album_filter)) %>%
    unnest(artists, names_sep = "_") %>%
    # The code below ensures that if several instances of the same album exist
    # only the first one is considered
    group_by(artists_name, name) %>%
    slice(1) %>%
    ungroup()

# The code chunk below retrieves each song from each album referenced in
# 'prog_metal_albums' and then retrieves the features of each song.
# PLEASE BE AWARE THAT THIS STEP MAY TAKE SOME TIME

prog_metal_tracks <-
    map_df(prog_metal_albums$id, ~ get_album_tracks(id = .x)) %>%
    filter(!str_detect(str_to_lower(name), song_album_filter)) %>%
    unnest(artists, names_sep = "_") %>%
    distinct(id, .keep_all = TRUE) %>%
    group_by(str_to_lower(name), str_to_lower(artists_name)) %>%
    slice(1) %>%
    ungroup() %>%
    select(id, name, artists_name)

prog_metal_tracks_features <-
    prog_metal_tracks %>%
    # Features can be retrieved for 100 tracks at once.
    # Data are then split into set of 100 tracks to speed up the process
    mutate(n_group_features = rep(c(1:((n() %/% 100) + 1)), each = 100)[1:n()]) %>%
    select(id, n_group_features) %>%
    group_split(n_group_features) %>%
    map_df(., ~ get_track_audio_features(.x$id)) %>%
    left_join(prog_metal_tracks, by = "id") %>%
    filter(acousticness < 1,  # Removing acoustic tracks (according to Spotify 1 is acoustic),
           liveness <= 0.8,   # Removing live tracks (according to Spotify above 0.8 provides strong likelihood that a track is live)
           energy > 0,        # Removing tracks with 0 energy (potential silent songs)
           tempo > 0) %>%     # Removing tracks with tempo equals 0 (potential silent/transition songs)
    bind_cols(map_dfr(.$analysis_url, get_track_audio_analysis_confidence)) %>%
    select(-c(liveness, acousticness, type, uri, track_href))

# The databases created are now saved and tracks with missing features are removed before saving
write_csv(drop_na(generic_metal_tracks_features), here("data", "generic_metal_tracks_features.csv"))
write_csv(drop_na(prog_metal_tracks_features), here("data", "prog_metal_tracks_features.csv"))
