library(tidyverse)
library(tidytuesdayR)
library(spotifyr)

musicals <- tidytuesdayR::tt_load(2020, week = 18)$grosses

# Get a quick sample
tops <- musicals %>% group_by(show) %>% 
            summarise(total = sum(weekly_gross)) %>%
            mutate(logtotal = log10(total)) %>% 
            filter(logtotal>7.6) %>% select(show)

# Initialize album information
tops[,c('album_id', 'album_name', 'total_tracks')] <- NA

# Search spotify for albums by looking up '[show] original cast'
# then extract the id, name, and total tracks for later
for(i in seq_along(tops$show)){
  if(!is.na(tops$album_id[i])) next # Useful if you run this multiple times
  
  showstring <- paste(tops$show[i], "original cast")
  retrieved <- search_spotify(showstring, type=c('album'))[1,]
  
  if(is_empty(retrieved)) next # Skip if nothing found
  
  tops$album_id[i] <- retrieved[['id']]
  tops$album_name[i] <- retrieved[['name']]
  tops$total_tracks[i] <- retrieved[['total_tracks']]
}
print(paste(sum(is.na(tops$album_id)) %>% as.character(), "shows didn't have soundtracks on spotify"))
# Filter any not on spotify
tops <- filter(tops, !is.na(album_name)) 

# Initialize a buffer to hold all our track info
alltracks <- tibble()
for(showid in tops$album_id){
  # Get tracks for one show
  showtracks <- 
    get_album_tracks(showid, limit=50) %>% 
    select(trackname=name, track_id=id, track_number, duration_ms)
  
  showtracks$album_id <- showid
  # Get the audio features for each track on the album
  # (takes a few minutes, many queries)
  features <- tibble()
  for(trackid in showtracks$track_id){
    features <- rbind(features, get_track_audio_features(trackid) %>% select(track_id=id, 1:11))
  }
  alltracks <- rbind(alltracks, left_join(showtracks, features, by='track_id'))
}
alltracks <- left_join(alltracks, select(tops, show, album_id), by='album_id')

# Get summary information
ost_summary <- alltracks %>% 
                  group_by(show,album_id) %>% 
                  summarize_at(vars(danceability:tempo), mean, na.rm=T)
colnames(ost_summary) <- c('show', 'album_id', paste0('avg_', colnames(ost_summary[3:13])))

# Write files so we don't have to query that info again
write.csv(alltracks, "trackinformation.csv", row.names = F)
write.csv(tops, "topshows.csv", row.names = F)
write.csv(ost_summary, "ost_summary.csv", row.names = F)

