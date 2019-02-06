---
title: "01. Spotify - Data Mining"
author: "Delvin So"
date: '2018-06-22'
output: 
  html_document:
    keep_md: true
    theme: journal
    toc: true
    toc_float : true
    toc_depth : 2
editor_options: 
  chunk_output_type: inline
---


```r
knitr::opts_chunk$set(echo = TRUE,
                      tidy = FALSE,
                      cache = TRUE,
                      # set this to false for this notebook b/c don't want to continually query API
                      eval = FALSE)

suppressMessages(library(tidyverse))
```

```
## Warning: package 'dplyr' was built under R version 3.5.1
```

```r
suppressMessages(library(spotifyr))
suppressMessages(library(xml2))
suppressMessages(library(readr))
suppressMessages(library(httr))
suppressMessages(library(naniar))

# To make in-line chunks executable
pl <- read_csv("data/pl.csv", col_types = cols())
playlist_info2 <- read_csv("data/raw/pl_info.csv", col_types = cols())
playlist_audio_features2 <- read_csv("data/raw/pl_audio_feats.csv", col_types = cols())
artist_features2 <- read_csv("data/raw/pl_artist_feats.csv", col_types = cols())
```

## Problem Statement

The purpose of this project is to gain insight into

1) Key predictors that are influential in a playlist's success
2) Using the identified predictors, generate a playlist that is successful (metric measured in the # of followers)

## Connecting with the Spotify API

In order to query Spotify's API, we need to set our client ID and secret. This can be done [here](https://developer.spotify.com/documentation/general/guides/authorization-guide/).



```r
Sys.setenv(SPOTIFY_CLIENT_ID = 'xxxxxxxxxxx')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'xxxxxxxxx')
access_token <- get_spotify_access_token()
```

## The Mining

The API currently does not offer any access to searching playlists with the most followers - however, we do have access to individual user's playlists. As Spotify's [own account](https://open.spotify.com/user/spotify) has numerous, highly featured playlists, we can use these for our goal of gaining insight into what features make a playlist successful. It's likely that Spotify features these playlists not only because they contain popular songs, but because there are intrinsic characteristics that make them popular. 

It might be worth noting that there are several playlists named 'This is ..' which feature a specific artists' songs. Throughout the data gathering process, we will be using the spotifyr package - a vignette can be found [here](link) and making several modifications of our own.

### Retrieving the Playlists

The first step is to retrieve Spotify's featured playlists - of importance is the uri or the unique resoruce identifier that indicates which playlist we're looking at. In addition to the uri, the function also returns other properties such as the playlist image, the number of tracks and so forth.


```r
# 06/18/2018
pl <- get_user_playlists("spotify")
```

```r
glimpse(pl)
```

```
## Observations: 1,603
## Variables: 6
## $ playlist_name       <chr> "Today's Top Hits", "RapCaviar", "mint", "...
## $ playlist_uri        <chr> "37i9dQZF1DXcBWIGoYBM5M", "37i9dQZF1DX0XUs...
## $ playlist_tracks_url <chr> "https://api.spotify.com/v1/users/spotify/...
## $ playlist_num_tracks <int> 50, 57, 52, 45, 63, 63, 65, 108, 85, 162, ...
## $ snapshot_id         <chr> "UdKUxr5Oa4a6Z2+omLmwqJ7+QiVnlojojcJM+Bj0T...
## $ playlist_img        <chr> "https://i.scdn.co/image/0cbd7328059081212...
```

```r
# write_csv(pl, "data/raw/pl_info.csv")
```


There are 1603 playlists in total, the next step is to retrieve the number of followers for each playlist. As of v.1.1.0, spotifyr does not retrieve this metric so we will have to do it ourselves.

NOTE: Edit this into get_user_playlists(). Would need to go deeper into parse_playlist_list_to_df()..

### Retrieve Playlist Information and Followers

```r
# we use map instead of map_df because it allows for NA's to return into an individual tibble within the list, otherwise the function will break out upon hitting an error
playlist_info <- map(pl$playlist_uri, ~ tryCatch(get_playlists_info("spotify", .x),
                                        error = function(e){NA}))

# collapsing the tibbles within the list into one dataframe and cleaning
playlist_info2 <- bind_rows(playlist_info) %>% 
  na.omit()
```

```r
head(playlist_info2)
```

```
## # A tibble: 6 x 3
##   playlist_name    playlist_uri           playlist_followers
##   <chr>            <chr>                               <int>
## 1 Today's Top Hits 37i9dQZF1DXcBWIGoYBM5M           20284811
## 2 RapCaviar        37i9dQZF1DX0XUsuxWHRQd            9741110
## 3 mint             37i9dQZF1DX4dyzvuaRJ0n            5015179
## 4 Are & Be         37i9dQZF1DX4SBhb3fqCJd            4355050
## 5 Rock This        37i9dQZF1DXcF6B6QPhFDv            4245616
## 6 Hot Country      37i9dQZF1DX1lVhptIYRda            4810160
```

```r
glimpse(playlist_info2)
```

```
## Observations: 1,548
## Variables: 3
## $ playlist_name      <chr> "Today's Top Hits", "RapCaviar", "mint", "A...
## $ playlist_uri       <chr> "37i9dQZF1DXcBWIGoYBM5M", "37i9dQZF1DX0XUsu...
## $ playlist_followers <int> 20284811, 9741110, 5015179, 4355050, 424561...
```

```r
# write_csv(playlist_info2, "data/raw/pl_info.csv")
```
There were 55 playlists where follower information could not be found. 

### Retrieving Playlist Audio Features

This portion of the data gathering is definitely the most exhaustive one, taking a few hours to run to completion. For each playlist, we're going to retrieve

* the track name
* the track uri
* thet track's popularity
* the artist name
* the artist album
* when it was added
* 14 features related to the music's characteristics (eg. energy, key, acousticness)

A full description of the audio features can be found [here](https://developer.spotify.com/documentation/web-api/reference/tracks/get-audio-features/).

```r
playlist_audio_features <- map(pl$playlist_uri, ~
                                 tryCatch(get_playlist_audio_features("spotify", .x), 
                                          error = function(e){NA}))

# collapse list of tibbles and cleaning
playlist_audio_features2 <- bind_rows(playlist_audio_features) %>% 
  # removing unnecessary variables - could dig into function and edit it myself
  select(-c(snapshot_id, playlist_tracks_url,
            playlist_img, album_img, track_preview_url, track_open_spotify_url)) %>%
  # is it necessary to omit here?
  na.omit() #95 053 -> 94 546
```

```r
head(playlist_audio_features2)
```

```
## # A tibble: 6 x 23
##   playlist_name  playlist_uri   playlist_num_tr… track_name     track_uri 
##   <chr>          <chr>                     <int> <chr>          <chr>     
## 1 Today's Top H… 37i9dQZF1DXcB…               50 Girls Like Yo… 6FRLCMO5T…
## 2 Today's Top H… 37i9dQZF1DXcB…               50 Jackie Chan    4kWO6O1BU…
## 3 Today's Top H… 37i9dQZF1DXcB…               50 Lucid Dreams   32iYwQ4OY…
## 4 Today's Top H… 37i9dQZF1DXcB…               50 Youngblood     55S2PQgSM…
## 5 Today's Top H… 37i9dQZF1DXcB…               50 Back To You -… 4hQ6UGyWQ…
## 6 Today's Top H… 37i9dQZF1DXcB…               50 lovely (with … 0u2P5u6lv…
## # ... with 18 more variables: artist_name <chr>, album_name <chr>,
## #   track_added_at <dttm>, track_popularity <int>, danceability <dbl>,
## #   energy <dbl>, key <chr>, loudness <dbl>, mode <chr>,
## #   speechiness <dbl>, acousticness <dbl>, instrumentalness <dbl>,
## #   liveness <dbl>, valence <dbl>, tempo <dbl>, duration_ms <dbl>,
## #   time_signature <int>, key_mode <chr>
```

```r
glimpse(playlist_audio_features2)
```

```
## Observations: 94,546
## Variables: 23
## $ playlist_name       <chr> "Today's Top Hits", "Today's Top Hits", "T...
## $ playlist_uri        <chr> "37i9dQZF1DXcBWIGoYBM5M", "37i9dQZF1DXcBWI...
## $ playlist_num_tracks <int> 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50...
## $ track_name          <chr> "Girls Like You (feat. Cardi B)", "Jackie ...
## $ track_uri           <chr> "6FRLCMO5TUHTexlWo8ym1W", "4kWO6O1BUXcZmax...
## $ artist_name         <chr> "Maroon 5", "Tiësto", "Juice WRLD", "5 Sec...
## $ album_name          <chr> "Girls Like You (feat. Cardi B)", "Jackie ...
## $ track_added_at      <dttm> 2018-06-18 15:58:34, 2018-06-18 15:58:34,...
## $ track_popularity    <int> 92, 89, 78, 95, 92, 92, 86, 93, 94, 73, 10...
## $ danceability        <dbl> 0.851, 0.747, 0.437, 0.596, 0.601, 0.351, ...
## $ energy              <dbl> 0.541, 0.834, 0.481, 0.854, 0.724, 0.296, ...
## $ key                 <chr> "C", "D#", "B", "G", "F#", "E", "C#", "B",...
## $ loudness            <dbl> -6.825, -2.867, -9.681, -5.114, -4.856, -1...
## $ mode                <chr> "major", "minor", "minor", "minor", "major...
## $ speechiness         <dbl> 0.0505, 0.0450, 0.2370, 0.4630, 0.0486, 0....
## $ acousticness        <dbl> 0.5680, 0.3740, 0.3770, 0.0169, 0.0945, 0....
## $ instrumentalness    <dbl> 0.00e+00, 0.00e+00, 0.00e+00, 0.00e+00, 1....
## $ liveness            <dbl> 0.1300, 0.0586, 0.3300, 0.1240, 0.1200, 0....
## $ valence             <dbl> 0.448, 0.687, 0.216, 0.152, 0.508, 0.120, ...
## $ tempo               <dbl> 124.959, 128.005, 83.606, 120.274, 102.061...
## $ duration_ms         <dbl> 235545, 215760, 239947, 203418, 207905, 20...
## $ time_signature      <int> 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, ...
## $ key_mode            <chr> "C major", "D# minor", "B minor", "G minor...
```

```r
# write_csv(playlist_audio_features2, "data/pl_audio_feats.csv")
```

We now have the track information for 94546 songs over 1603 playlists. Now onto the last bit of information - artist features.

### Retrieving Artist Features

For each artist, we want to retrieve features such as the number of followers they have, their genre, and so forth. Again, spotifyr v1.1.0 does not offer this capability so we had to get a little bit creative and write our own function. 

First, we store the artist names, each one will be individually queried using the Spotify's search endpoint.


```r
artist_names <- unique(playlist_audio_features2$artist_name) %>% sort()
```
There are 21802 artists in Spotify's featured playlists. 

For this section, I used a for loop that would store query each artist's name, return the results and bind it to an existing dataframe. Normally, we would avoid this and do it the 'R' way using vectors, but I couldn't find any other way to store the results as the function ran. This way, the results would be stored in an object and even if the function failed (which it did, several times in fact and which is why I wrote this function in the first place), the results would be stored and we could simply continue querying and adding from the artist which failed. 


```r
 for (i in artist_names){
      
        # TODO: checkpoints, maybe check if artist_features already exists and if artist_name is in it, then skip
        temp <- tryCatch(get_artists_ds(artist_name = i,
                                  return_closest_artist = TRUE),
                         error = function(e){NA})
        
        print(paste("Artist:", i))
        
        if((is.na(temp))){ temp <- rep(NA, 5) } # number of columns in artist_features
        
        # create the dataframe of features if it doesn't exist using the first results
        # else attach the results to the existing df.
        if(exists("artist_features")){ 
          artist_features <- rbind(artist_features, temp)
        }
        else{
          artist_features <- temp
        }
    }
    rm(i, temp)
    
        
# for some reason there are duplicates even though we applied unique
artist_features2 <- artist_features %>%
  arrange(desc(artist_name, artist_followers)) %>%
  filter(!duplicated(artist_name))
```


```r
head(artist_features2)
```

```
## # A tibble: 6 x 5
##   artist_name       artist_uri   artist_followers artist_pop artist_genres
##   <chr>             <chr>                   <int>      <int> <chr>        
## 1 황푸하 Hwang Puha 2R1gQPNVl0C…              347         26 <NA>         
## 2 홍대요정프로젝트 (Hongda… 28zDx1f06DS…                0          0 <NA>         
## 3 허니비            1FwrRQKHqsl…               39         26 <NA>         
## 4 하현곤 팩토리     5dDTRkIan1O…                0         10 <NA>         
## 5 트리스            60xLryjzNHX…                6         12 <NA>         
## 6 크르르 Krr        2wCEpu2tMAi…              100         15 k-indie
```

```r
glimpse(artist_features2)
```

```
## Observations: 21,217
## Variables: 5
## $ artist_name      <chr> "황푸하 Hwang Puha", "홍대요정프로젝트 (Hongdae-elf Proj...
## $ artist_uri       <chr> "2R1gQPNVl0CauipSDNEbl5", "28zDx1f06DSEe3nwfc...
## $ artist_followers <int> 347, 0, 39, 0, 6, 100, 73, 20, 1856, 30, 82, ...
## $ artist_pop       <int> 26, 0, 26, 10, 12, 15, 18, 21, 23, 19, 27, 16...
## $ artist_genres    <chr> NA, NA, NA, NA, NA, "k-indie", NA, NA, NA, NA...
```

```r
# write_csv(artist_features2, "data/raw/pl_artist_feats.csv")
```

There are 21217 artists in Spotify's featured playlists. 


Onto the [EDA](link)!
