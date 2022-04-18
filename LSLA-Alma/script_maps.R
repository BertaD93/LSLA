library(data.table)
library(ggplot2)
library(lubridate)
library(sf) 
library(readxl)

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

####  Load data

# Load deals
domestic_deals <- fread("https://raw.githubusercontent.com/a-velazquez/LSLA/main/raw/domestic/deals.csv")
transnational_deals <- fread("https://raw.githubusercontent.com/a-velazquez/LSLA/main/raw/transnational/deals.csv")


# Load investors
domestic_investors <- fread("https://raw.githubusercontent.com/a-velazquez/LSLA/main/raw/domestic/investors.csv")
transnational_investors <- fread("https://raw.githubusercontent.com/a-velazquez/LSLA/main/raw/transnational/investors.csv")

# Load locations
domestic_locations <- fread("https://raw.githubusercontent.com/a-velazquez/LSLA/main/raw/domestic/locations.csv")
transnational_locations <- fread("https://raw.githubusercontent.com/a-velazquez/LSLA/main/raw/transnational/locations.csv")

#### Summarize key information

## Transnational Deals

# Deal size
summary(transnational_deals$`Deal size`)
# Negotiation status
unique(transnational_deals$`Current negotiation status`)
# Implementation status
unique(transnational_deals$`Current implementation status`)
unique(transnational_deals$`Intention of investment`)
unique(transnational_deals$`Target country`)
summary(transnational_deals$`Fully updated`)
length(unique(transnational_deals$`Deal ID`))


summary(domestic_deals$`Deal size`)

unique(domestic_deals$`Current negotiation status`)
unique(domestic_deals$`Current implementation status`)
unique(domestic_deals$`Intention of investment`)
unique(domestic_deals$`Target country`)
length(unique(domestic_deals$`Deal ID`))

clean_deals <- function(df, original, add_items=c()){
  # Make deal id integer
  df[,`:=`(`Deal ID`, as.integer(`Deal ID`))]
  
  # Subset only to concluded deals
  # df<-df[grepl("Contract signed",`Negotiation status`, fixed = TRUE)]
  
  # Create "date" column using size under contract information
  # df[,`:=`("date"=gsub("[^0-9]","", `Negotiation status`))]
  df[,`:=`("date"=sub("#.*", "",
                      `Size under contract (leased or purchased area, in ha)`))]
  
  df[,`:=`("year"=substr(date,1,4))]
  df_tm <- df[year != ""]
  
  df_tm[, `:=`("year"=as.Date(paste0(df_tm$year, "-12-31"), "%Y-%m-%d"))]
  
  by_vec <- c(c("year", "Deal scope"), add_items)
  
  df_tm<-df_tm[,.N, by=by_vec]
  
  if(original){
    return(df)
  }
  
  else{
    return(df_tm)
  }
}


transnational_deals <- clean_deals(transnational_deals, TRUE)
domestic_deals <- clean_deals(domestic_deals, TRUE)

Ndeals_domestic <- clean_deals(domestic_deals, FALSE)
Ndeals_transnational <- clean_deals(transnational_deals, FALSE)

countryDeals_domestic <- clean_deals(domestic_deals, FALSE, 
                                     add_items = c("Target country"))
countryDeals_transnational <- clean_deals(transnational_deals, FALSE, 
                                          add_items = c("Target country"))

negtiationDeals_transnational <- clean_deals(domestic_deals, FALSE, 
                                             add_items = c("Current negotiation status"))
nrgotiationDeals_domestic <- clean_deals(transnational_deals, FALSE, 
                                         add_items = c("Current negotiation status"))

implemDeals_transnational <- clean_deals(domestic_deals, FALSE, 
                                         add_items = c("Current implementation status"))
implemDeals_domestic <- clean_deals(transnational_deals, FALSE, 
                                    add_items = c("Current implementation status"))

allDeals_domestic <- clean_deals(domestic_deals, FALSE, 
                                 add_items = c("Target country",
                                               "Current negotiation status",
                                               "Current implementation status"))
allDeals_transnational <- clean_deals(transnational_deals, FALSE, 
                                      add_items = c("Target country",
                                                    "Current negotiation status",
                                                    "Current implementation status"))

domestic_locations <- merge(domestic_locations, unique(domestic_deals[,.(`Deal ID`,`Target country`)]), by="Deal ID")
domestic_locations[, c("Lat", "Long") := tstrsplit(Point, ",", fixed=TRUE)]
domestic_locations <- domestic_locations[!is.na(Lat) & !is.na(Long)]

br_domestic_deals <- st_as_sf(domestic_locations[`Target country`=="Brazil"], coords = c("Long", "Lat"), crs=4674)
pr_domestic_deals <- st_as_sf(domestic_locations[`Target country`=="Peru"], coords = c("Long", "Lat"), crs=4326)


transnational_locations <- merge(transnational_locations, unique(transnational_deals[,.(`Deal ID`,`Target country`)]), by="Deal ID")
transnational_locations[, c("Lat", "Long") := tstrsplit(Point, ",", fixed=TRUE)]

br_transnational_deals <- st_as_sf(transnational_locations[`Target country`=="Brazil"], coords = c("Long", "Lat"), crs=4674)
pr_transnational_deals <- st_as_sf(transnational_locations[`Target country`=="Peru"], coords = c("Long", "Lat"),crs=4326)



br_states <- st_read("C:/Users/Tita/Downloads/BR_UF_2021.shp")
pr_states <- st_read("C:/Users/Tita/Downloads/per_admbnda_adm1_ign_20200714.shp")



inf_br <- as.data.table(read_excel("C:/Users/Tita/Downloads/infrastructure_BR.xlsx"))
setnames(inf_br, c("Type of land registration system in the economy:"), c("land_registration_system"))
inf_br[Location=="Federal District", Location := "Distrito Federal"]


inf_pr <- as.data.table(read_excel("C:/Users/Tita/Downloads/infrastructure_PR.xlsx"))
setnames(inf_pr, c("Type of land registration system in the economy:"), c("land_registration_system"))

ldr_br <- as.data.table(read_excel("C:/Users/Tita/Downloads/ldr_BR.xlsx"))
setnames(ldr_br, c("Land dispute resolution index (0–8)"), c("land_dispute_resolution_index"))
ldr_br[Location=="Federal District", Location := "Distrito Federal"]
ldr_pr <- as.data.table(read_excel("C:/Users/Tita/Downloads/ldr_PR.xlsx"))
setnames(ldr_pr, c("Land dispute resolution index (0–8)"), c("land_dispute_resolution_index"))


br_states <- merge(br_states, inf_br[,.(Location, land_registration_system)], by.x="NM_UF", by.y="Location")
br_states <- merge(br_states, ldr_br[,.(Location, land_dispute_resolution_index)], by.x="NM_UF", by.y="Location")


br_states <- st_set_crs(br_states, "+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")
br_states <- st_transform(br_states, crs=4674)


gr <- ggplot() +
  geom_sf(data=br_states,aes(fill=land_dispute_resolution_index)) +
  geom_sf_label(data=br_states,aes(label = NM_UF)) +
  geom_sf(data=br_domestic_deals)

inp <- ("GitHub/LSLA-Alma/")
ggsave(paste(inp, "map_1.png", sep="/"), plot=gr, width = 10, height = 7, dpi=600)


gr2<- ggplot(pr_states) +
  geom_sf() +
  geom_sf_label(aes(label = ADM1_ES))

ggsave(paste(inp, "map_2.png", sep="/"), plot=gr2, width = 10, height = 7, dpi=600)
