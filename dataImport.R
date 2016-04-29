require(dplyr)


## Import required data and add to list
locations <- read.csv('./.data/sitelocations_decdegree.csv', header = T, stringsAsFactors=F) %>%
  filter(SITEID != "") %>% mutate(longitude = as.character(lon),
                                  latitude = as.character(lat)) %>% rename(SiteID = SITEID)

site.info  <- read.csv( './.data/site.info.clean.csv', stringsAsFactor = F)




wesa       <- read.csv('./.data/wesa.clean.csv', stringsAsFactor = F) %>%
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


# Compile zero counts -----------------------------------------------------
records <- unique(wesa$RecordID)
zero.counts <- 
  site.info %>%
  filter(!(RecordID %in% records)) %>%
  select(-X, -SiteName) %>%
  left_join(locations, by = 'SiteID')  %>% #RecordID, SiteID, Year, Month, Day, SiteName, OnlineName, size) 
  mutate(Year = substr(RecordID, 5, 8), 
         Month = substr(RecordID, 9,9),
         Day = substr(RecordID, 10, 11),
         Month.name = ifelse(Month =='7', 'July', 'August'))

zero.counts$max.count <- 0
zero.counts$avg.count <- 0
zero.counts$sd.count <- NA
zero.counts$sum.count <- 0
zero.counts$n.counts <- NA
name.list <- c(names(wesa))
zero.counts <- zero.counts %>% select(which(names(.) %in% name.list))
wesa <- rbind(wesa, zero.counts) 



all.surveys.info <- wesa %>% select(RecordID, SiteID, Year, Month, Day) %>%
  mutate (  
    Year = substr(RecordID, 5, 8), 
    Month = substr(RecordID, 9,9),
    Day = substr(RecordID, 10, 11),
    Date = as.Date(paste(Year, "-" , Month, "-", Day,sep =""), format = "%Y-%m-%d"))


falc    <- read.csv( './.data/falc.clean.csv', stringsAsFactor = F) %>%
  select(RecordID, SiteID, ID, Count) %>%
    mutate (  
      Count = as.numeric(Count),
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
