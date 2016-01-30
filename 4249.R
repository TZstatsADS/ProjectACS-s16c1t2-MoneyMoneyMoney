library(data.table)
library(ggplot2)
library(dplyr)

# Select variables: state, income, occupation, race, language spoken at home
var <- c("ST", "PINCP", "OCCP", "RAC3P", "LANP")
dA <- fread("~/Documents/cycle1-2/data/csv_pus/ss13pusa.csv", select = var)
dB <- fread("~/Documents/cycle1-2/data/csv_pus/ss13pusb.csv", select = var)
rm(dA, dB)
data <- rbind(dA, dB)
setwd("~/Documents/cycle1-2/data")
save(data, file="4249Data.RData")
#%>% filter(LANP==708, RAC3P==005) %>%
china <- data %>% select(ST, PINCP, OCCP)

# Add a new column to test whether a person is computer related job or not
computerCode <- c(0110, 1005, 1006, 1010, 1050, 1105, 1106, 1107, 1400, 5800,
                  5900, 7010, 7900)
techOrNot <- function(x) {
  ifelse(x %in% computerCode, 1, 0)
}
withComputer <- mutate(china, computerCheck = techOrNot(OCCP))
withComputer <- group_by(withComputer, ST)
computerFraction <- summarize(withComputer, value=mean(computerCheck))
state_list =list("1"="alabama", "2"="alaska", "4"="arizona", "5" = "arkansas", 
                 "6" = "california", "8" = "colorado", "9" = "connecticut", 
                 "10" = "delaware", "11" = "district of columbia", "12" = "florida",
                 "13" = "georgia", "15" = "hawaii",  "16" = "idaho",   
                 "17" = "illinois", "18" = "indiana", "19" = "iowa", "20" = "kansas",  
                 "21" = "kentucky", "22" = "louisiana",  "23" = "maine",   
                 "24" = "maryland", "25" = "massachusetts", "26" = "michigan", 
                 "27" = "minnesota",  "28" = "mississippi", "29" = "missouri", 
                 "30" = "montana", "31" = "nebraska", "32" = "nevada",  
                 "33" = "new hampshire", "34" = "new jersey", "35" = "new mexico", 
                 "36" = "new york", "37" = "north carolina", "38" = "north dakota", 
                 "39" = "ohio",    "40" = "oklahoma", "41" = "oregon",  "42" = "pennsylvania",
                 "44" = "rhode island", "45" = "south carolina", "46" = "south dakota", 
                 "47" = "tennessee",  "48" = "texas",   "49" = "utah",    "50" = "vermont", 
                 "51" = "virginia", "53" = "washington", "54" = "west virginia", "55" = "wisconsin",  
                 "56" = "wyoming", "72" = "puerto rico")
regionTransfer <- function(x){return ( state_list[[as.character(x)]]) }
plotData <- computerFraction %>% mutate(region = vapply(ST, regionTransfer, "")) %>%
  select(region, value)
state_choropleth(plotData)
