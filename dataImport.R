require(dplyr)


## Import required data and add to list
locations <- read.csv('./.data/sitelocations_decdegree.csv', header = T, stringsAsFactors=F) %>%
  filter(SITEID != "") %>% mutate(longitude = as.character(lon),
                                  latitude = as.character(lat)) %>% rename(SiteID = SITEID)

wesa       <- read.csv('./.data/wesa.clean.csv') %>%
  filter(RecordID != 'RecordID' & !is.na(counts.cleaned)) %>%
  select(RecordID, ID, SiteID, counts.cleaned ) %>%
  group_by(RecordID) %>% 
  dplyr::summarize(max.count = max(counts.cleaned, na.rm=T), 
                   avg.count = mean(counts.cleaned, na.rm=T),
                   sd.count = sd(counts.cleaned, na.rm = T),
                   sum.count = sum(counts.cleaned, na.rm = T), 
                   n.counts = n() )%>%
  mutate(SiteID = substr(RecordID, 1, 4), 
         Year = substr(RecordID, 5, 8), 
         Month = substr(RecordID, 9,9),
         Day = substr(RecordID, 10, 11),
         Month.name = ifelse(Month =='7', 'July', 'August')) %>%
  left_join(locations, by = 'SiteID')


all.surveys.info <- wesa %>% select(RecordID, SiteID, Year, Month, Day) %>%
  mutate (  
    Year = substr(RecordID, 5, 8), 
    Month = substr(RecordID, 9,9),
    Day = substr(RecordID, 10, 11),
    Date = as.Date(paste(Year, "-" , Month, "-", Day,sep =""), format = "%Y-%m-%d"))


falc    <- read.csv( './.data/falc.clean.csv') %>%
  select(RecordID, SiteID, ID, Count) %>%
    mutate (  
      Year = substr(RecordID, 5, 8), 
      Month = substr(RecordID, 9,9),
      Day = substr(RecordID, 10, 11),
      Date = as.Date(paste(Year, "-" , Month, "-", Day,sep =""), format = "%Y-%m-%d")) %>%
    group_by(RecordID, SiteID, Date, Year, Month) %>%
    dplyr::summarize(
      TotalCounts = n(),
      numberCounted = mean(Count, na.rm=T),
      observed = 1
    ) %>%
    right_join(all.surveys.info, by=c('RecordID', 'SiteID', 'Date', 'Year', 'Month') ) %>%
    dplyr::mutate(observed.y.n = ifelse(is.na(observed), 0, 1),
                  count.falc = ifelse(is.na(numberCounted), 0, numberCounted),
                  Month.name = ifelse(Month =='7', 'July', 'August') ) %>%
  left_join(locations, by = 'SiteID')




data_ <- list(falc= falc,wesa= wesa,locations= locations)