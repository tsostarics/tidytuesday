library(tidyverse)
library(tidytuesdayR)
library(hrbrthemes)
library(ggrepel)
tuesdata <- tidytuesdayR::tt_load(2020, week = 18)

# Read in the spotify data
alltracks <- read_csv('trackinformation.csv')
topshows  <- read_csv('topshows.csv')
ost_summary <- read_csv('ost_summary.csv')

# Extract the subset of musicals we're looking at
musicals <- tuesdata$grosses
musicals <- filter(musicals, show %in% topshows$show)
musicals$year_month <- musicals$week_ending %>% substr(1,7)

# Merge in cpi data so we can adjust for inflation (given by month)
cpidata  <- tuesdata$cpi %>% mutate(year_month = substr(year_month, 1, 7))
musicals <- left_join(musicals, cpidata, by='year_month')
musicals <- 
  musicals %>% 
  mutate(weekly_gross_overall_adj = weekly_gross_overall/cpi*100,
         weekly_gross_adj = weekly_gross/cpi*100, 
         avg_ticket_price_adj = avg_ticket_price/cpi*100)

# Get the release year for each show along with
# its running sum over the course of its lifespan
musicals <- 
  musicals %>% 
  group_by(show) %>% 
  mutate(releaseyear = first(unique(week_ending)),
         releaseyear = as.numeric(substr(releaseyear,1,4)),
         cumulgross = cumsum(weekly_gross_adj),
         nweek = seq_along(week_ending))

# Rough 3 year Cats adjustment, which premiered in 1982 not 1985
musicals[musicals$show=='Cats','nweek'] <- musicals[musicals$show=='Cats','nweek'] + 156
musicals[musicals$show=='Cats','releaseyear'] <- 1982

# Merge in the spotify data
musicals <- left_join(musicals, select(ost_summary, -album_id), by='show')

# Rank shows by their total gross amount
rankbygross <- 
  musicals %>% 
  group_by(show) %>% 
  summarize(rank = max(cumulgross)) %>% 
  arrange(desc(rank))
rankbygross$rank <- 1:nrow(rankbygross)
musicals <- left_join(musicals, rankbygross, by='show')

# If we just want to get the top 10 musicals
top10 <- filter(musicals, rank<11)

# Plot the top ten musicals' earnings over time
# (adjusted for inflation)
top10 %>% 
  ggplot(aes(x=nweek, y=cumulgross, color=show)) + 
  geom_line(size=1.5) +
  theme_minimal()+
  theme(legend.position='bottom')


# Plot multiple variables with positive correlations
# these are all z-score transformed
scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
musicals %>% 
  group_by(show, releaseyear, avg_danceability, avg_energy, avg_valence, avg_tempo) %>% 
  summarize() %>% 
  ungroup() %>% 
  mutate_at(vars(starts_with("avg_")), scale2) %>% 
  rename(`Average Danceability`=avg_danceability,
         `Average Energy`=avg_energy,
         `Average Tempo`=avg_tempo,
         `Average Valence`=avg_valence) %>% 
  pivot_longer(cols=starts_with("Average"), 
               names_to = 'feature', 
               values_to = 'value') %>% 
  ggplot(aes(x=releaseyear, y=value)) + 
  geom_point(size=2, color=ft_cols$yellow) +
  geom_smooth(method=lm, color=ft_cols$peach) + 
  facet_wrap(~feature) + 
  theme_ft_rc()

t10sum <- top10 %>% group_by(show, releaseyear, avg_danceability, avg_tempo) %>% summarize()
# Plot just the danceability data
allsum <- musicals %>% group_by(show, releaseyear, avg_danceability, avg_tempo) %>% summarize()

danceshows <- c("Hamilton", "Cats",
                "Mamma Mia!", "Mean Girls",
                "The Phantom of the Opera")
allsum %>% 
  ggplot(aes(x=releaseyear, y=avg_danceability)) + 
  geom_label_repel(data=filter(allsum,show %in% danceshows),
                   aes(
                     x                = releaseyear, 
                     y                = avg_danceability, 
                     label            = show
                   ), 
                   min.segment.length = .001, 
                   segment.color      = ft_cols$slate, 
                   segment.size       = 1,
                   nudge_y            = .04, 
                   nudge_x            = -4.5,  
                   fill               = ft_cols$slate, 
                   color              = ft_cols$white,
                   alpha = .8)+
  geom_smooth(method = lm, 
              color  = ft_cols$yellow,
              alpha  = .2) +
  geom_point(size=2, color=ft_cols$yellow) +
  theme_ft_rc() +
  xlab("Year show premiered")+
  ylab("Average danceability of soundtrack")+
  ggtitle("Musical Soundtracks are Becoming more Danceable")+
  labs(caption=
"Danceability describes how suitable a song is for dancing based on a combination of musical elements including tempo, 
rhythm stability, beat strength, and overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable.
(Data from Playbill and Spotify)
@tsostaricsling")+
  theme(plot.caption = element_text(hjust = 0)) -> dplot
  ggsave(dplot, filename = "danceability.png", width=8, height=6, dpi = 300, type = "cairo")
  

temposhows <- c("Chicago", "Hamilton", "Cats",
                "Jersey Boys","Dear Evan Hansen",
                "Wicked")
# Plot just the tempo data
allsum %>% 
  ggplot(aes(x=releaseyear, y=avg_tempo)) + 
  geom_smooth(method = lm, 
              color  = ft_cols$yellow,
              alpha  = .2) +
  geom_label_repel(data=filter(allsum,show %in% temposhows),
                   aes(
                     x                = releaseyear, 
                     y                = avg_tempo, 
                     label            = show
                   ), 
                   min.segment.length = .001, 
                   segment.color      = ft_cols$slate, 
                   segment.size       = 1,
                   nudge_y            = -3, 
                   nudge_x            = -2,  
                   fill               = ft_cols$slate, 
                   color              = ft_cols$white,
                   alpha = .8)+
  geom_point(size=2, color=ft_cols$yellow) +
  theme_ft_rc() +
  xlab("Year show premiered")+
  ylab("Average tempo  of soundtrack")+
  ggtitle("Musical Soundtracks are Getting Faster")


