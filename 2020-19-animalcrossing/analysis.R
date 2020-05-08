library(tidyverse)
library(gsubfn)
library(hrbrthemes)
library(fishualize)
library(rvest)


# Scrapes villager names from nookipedia.com
# not run because I already saved the output
get_nookipedia_tables <- function(vils, verbose=F){
  site <- 'https://nookipedia.com/wiki/'
  
  # Create a buffer list to hold the scraped data
  # (using a list lets us avoid costly copies on each iteration)
  acclist <- list_along(vils$name)
  
  for(i in seq_along(vils$name)){
    # Check special cases
    vilname <-  switch(vils$name[i],
                       Carmen = 'Carmen_(rabbit)',
                       Flora  = 'Flora_(villager)',
                       Snooty = 'Snooty_(villager)',
                       June   = 'June_(villager)',
                       Renée = 'Ren%C3%A9e',
                       vils$name[i])
    # Convert spaces to _ for url purposes
    vilname <- gsub(' ', '_', vilname)
    if(verbose)print(vilname)
    # Extract table from html by extracting the 1 table with a Language column
    # then add it to our list of tibbles
    tblnodes <- paste0(site,vilname) %>% read_html() %>% html_nodes('table')
    acclist[[i]] <- 
      tblnodes[suppressWarnings(str_which(tblnodes,'Language'))] %>% 
      html_table() %>% .[[1]] %>% 
      tibble(id = vils$id[i],
             translit = str_extract(Name,'[A-Za-z0-9][^[ァ-ヾぁ-ん호]] ?.*$'), 
             native = str_extract(Name, '^[0-9]?[^[A-z-0-9]]+')) %>% 
      select(id, language=Language, translit, native, meaning=Meaning) %>% 
      rbind(tibble(id = vils$id[i], language='English', translit=vils$name[i], native=NA, meaning=''))
  }
  # Bind the list of tibbles, as per:
  # https://stackoverflow.com/questions/2851327/convert-a-list-of-data-frames-into-one-data-frame
  return(data.table::rbindlist(acclist))
}

# Checks the annotations to see if it's from the English name
fromENG <- function(x){
  if(is.na(x)) return(FALSE)
  return(str_detect(str_to_lower(x), "english"))
}

# Checks the annotations to see if it's from the Japanese name
fromJPN <- function(x){
  if(is.na(x)) return(FALSE)
  return(str_detect(str_to_lower(x), "japanese"))
}


# This just reads in the multi language data I scraped (and saved)
villagers <- readxl::read_xlsx('villagers_multilang_expanded.xlsx')

villagers <- 
  villagers %>% 
  filter(!str_detect(language,'\\('), 
         !language%in%c('Chinese','English'))

# This is getting the metrics I was ultimately interested in (loan names)
vil_names <- left_join(villagers,
                       filter(villagers, language=='Japanese') %>% 
                         select(JPNname=translit, name),by='name') %>% 
              select(id, name, JPNname, translit,language,meaning) %>% 
              filter(language!='Japanese') %>% 
              group_by(id, language) %>% 
              mutate(JPNname = gsub('nb [0-9]]','', JPNname),
                     translit= ifelse(is.na(translit), str_extract(meaning,"^(T-Bone)|(O'Hare)|^[[:alpha:]]+$"), translit),
                     EngLoan = F,
                     JpnLoan = F,
                     EngLoan = fromENG(meaning),
                     JpnLoan = fromJPN(meaning),
                     EngDist = adist(name, translit,costs = c('i'=2,'d'=1,'s'=2)),
                     JpnDist = adist(JPNname, translit,costs = c('i'=2,'d'=1,'s'=2)),
                     EngLoan = ifelse(is.na(EngDist), EngLoan, EngDist<3 | EngLoan),
                     JpnLoan = ifelse(is.na(JpnDist), JpnLoan, JpnDist<3 | JpnLoan),
                     LoanStatus= ifelse(is.na(translit), NA, "Unique to Language"),
                     LoanStatus= ifelse(EngLoan&JpnLoan, "From Both", LoanStatus),
                     LoanStatus= ifelse(EngLoan&!JpnLoan, "From English", LoanStatus),
                     LoanStatus= ifelse(JpnLoan&!EngLoan, "From Japanese", LoanStatus))

# Not all 391 villagers were scraped successfully, so I adjust to make
# calculations out of the samples I was able to get 
# (size was generally within 330-390 for all but Russian and Chinese)
vil_sample <- vil_names %>% filter(!is.na(translit), !is.na(LoanStatus)) %>% group_by(language) %>% summarize(vils = n())
vil_counts <- vil_names %>% filter(!is.na(translit), !is.na(LoanStatus)) %>% group_by(language, LoanStatus) %>% summarize(rawcount = n())
vil_counts <- left_join(vil_counts, vil_sample, by='language') %>% mutate(percent = rawcount/vils)

# Where are borrowings coming from? The main plot
vil_counts %>% filter(language!='Russian') %>% 
  ggplot(aes(y=percent, x=language, fill=forcats::fct_rev(LoanStatus))) + 
  geom_bar(stat='identity', color=NA) + 
  ylab("Percent of villagers")+
  xlab("Language")+
  ggtitle("Translations or Transliterations: How Original are Villager Names Across Languages?",
          subtitle = "Cross-linguistic analysis shows differences in naming approaches")+
  labs(caption="While many villagers receive original names for each language, some are derivative transliterations of the English or Japanese source name.
Comparing across languages helps us see how many names are derivative of both English and Japanese (ENG: Chief, JPN: Chiifu, FRE: Chef), 
how many are derived exclusively from English (ENG: Bea, JPN: Beeguru, FRE: Béa) or Japanese (ENG: Judy, JPN: Misuzu, GER: Misuzu), 
and how many are original names for the target language (ENG: Tybait, JPN: Harimao, GER: Arne).
Data from nookipedia.com
@tsostaricsling")+
  theme_ft_rc()+
  scale_y_continuous(labels = scales::percent)+
  labs(fill="Where is the name from?")+
  scale_fill_manual(values=c(ft_cols$slate,'#e6452e','#eed58c','#f48124'))+
  theme(plot.background = element_rect(fill='#303030'),
        panel.background = element_rect(fill=NA),
        panel.grid.major = element_line('#4b5957'),
        panel.grid.minor = element_line('#4b5957'),
        panel.border = element_rect('#4b5957',fill=NA),
        axis.text = element_text(color='#f7f7f7'),
        axis.title = element_text(color='#eed58c'),
        strip.text = element_text(color='#eed58c'),
        plot.title = element_text(color='#eed58c'),
        plot.subtitle = element_text(color='#f7f7f7'),
        plot.caption = element_text(color='#f7f7f7'),
        legend.title = element_text(color='#f7f7f7'),
        legend.text = element_text(color='#f7f7f7'),
        axis.text.y.right = element_text(margin=2))
