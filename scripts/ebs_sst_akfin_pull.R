# EBS temp for Mariela & Lia pcod example
# Created by Matt Callahan
# Edited by Lia Domke, to adjust to my needs
# 11-6-25

library(tidyverse)
library(lubridate)
library(httr)
library(RJDBC)
library(keyring)
# library(akfinupload) # just for the java driver

# connect to akfin
con <- dbConnect(odbc::odbc(), "akfin", 
                 UID=key_list("akfin_oracle_db")$username, 
                 PWD = keyring::key_get("akfin_oracle_db", keyring::key_list("akfin_oracle_db")$username))

# if you want to perform queries in oracle you can also use r using dbplyr
# example code:
# pandalus_bio<-tbl(con, in_schema("GAP_PRODUCTS", "AKFIN_BIOMASS_V")) %>%
#   rename_with(tolower) %>%
#   filter(species_code==66020 & area_id %in% gap_regions )%>%
#   collect()


# pull subregions from web service
# Since these are automated for the shiny app and ESRs this is super straightforward
subregion_daily <- httr::content(httr::GET('https://apex.psmfc.org/akfin/data_marts/akmp/ecosystem_sub_crw_avg_sst?ecosystem_sub=Southeastern%20Bering%20Sea,Northern%20Bering%20Sea&start_date=19850101&end_date=20250818'), type = "application/json") %>% 
  bind_rows() %>% 
  mutate(date=as_date(READ_DATE),
         year=year(date)) %>% 
  dplyr::select(year, date,region=ECOSYSTEM_SUB, meansst=MEANSST)

# calculate monthly means
subregion_monthly <- subregion_daily %>%
  mutate(month=month(date)) %>%
  group_by(year, month, region) %>%
  summarise(meansst=mean(meansst))

# Query ecosystem as a whole
# I have to do this through sql since we don't have a web service with pre-aggregated data
# jdbcDriver <- RJDBC::JDBC(driverClass="oracle.jdbc.OracleDriver",
#                           classPath=system.file("driver", "ojdbc8.jar", package = "akfinupload") )
#server<-"jdbc:oracle:thin:@akfin"

# set up the query for ecosystem temp data
query <- "select extract(year from read_date) year, read_date, ecosystem region, round(avg(temp),2) meansst
from afsc.erddap_crw_sst a
left join AFSC.erddap_crw_sst_spatial_lookup b on a.crw_id = b.id
where ecosystem='Eastern Bering Sea'
and depth >= -200
and depth <=-10
group by extract(year from read_date), read_date, ecosystem"

# pull regional daily temps across the sea - this will take like 10 minutes
region_daily <-dbGetQuery(con, query)

### see what tables I can access
# accessible_tables <- dbGetQuery(con, "SELECT BASISFISH_CATCH")
# print(accessible_tables)
# 
# df <- dbGetQuery(con, "SELECT * FROM akr.v_cas_tac")
# head(df)
# 

# disconnect from akfin
dbDisconnect(con)

region_daily2 <- region_daily %>%
  rename_with(tolower) %>%
  mutate(date=as_date(read_date)) %>%
  dplyr::select(year, date, region, meansst)

region_monthly <- region_daily2 %>%
  mutate(month=month(date)) %>%
  group_by(year, month, region) %>%
  summarise(meansst=mean(meansst))


# combine and save - this will overright old datasets
bind_rows(subregion_daily, region_daily) %>%
  arrange(date, region) %>%
  write.csv("data/ebs_daily_sst.csv", row.names = FALSE)

bind_rows(subregion_monthly, region_monthly) %>%
  arrange(year, month, region) %>%
  write.csv("data/ebs_monthly_sst.csv", row.names = FALSE)



####  BELOW is leftover code from matt callahn and I didn't run or compare with any of them ####
# now for a comparison of oi and crw for the region
oiquery <- "select extract(year from read_date) year, read_date, ecosystem_sub region, round(avg(temp),2) meansst
from afsc.erddap_oi_sst a
left join AFSC.erddap_oi_sst_spatial_lookup b on a.oi_id = b.id
where ecosystem_sub in ('Northern Bering Sea', 'Southeastern Bering Sea')
and depth >= -200
and depth <=-10
group by extract(year from read_date), read_date, ecosystem_sub"

oi_daily <-dbGetQuery(con, oiquery)

oi_daily <- oi_daily %>%
  rename_with(tolower) %>%
  mutate(date=as_date(read_date)) %>%
  dplyr::select(year, date, region, meansst)


sst_ts <- oi_daily %>%
  left_join(subregion_daily, by=c("year","date","region"), suffix=c("_oi","_crw")) 


ggplot()+
  geom_point(data=sst_ts%>% filter(region=="Northern Bering Sea"), aes(x=meansst_crw, y=meansst_oi), color="blue", size=0.5)+
  geom_point(data=sst_ts%>% filter(region=="Southeastern Bering Sea"), aes(x=meansst_crw, y=meansst_oi), color="red", size=0.5)


ggplot()+
  geom_line(data=sst_ts%>% filter(region=="Southeastern Bering Sea" & year > 2019), aes(x=yday(date), y=meansst_oi), color="blue", size=0.5)+
  geom_line(data=sst_ts%>% filter(region=="Southeastern Bering Sea" & year > 2019), aes(x=yday(date), y=meansst_crw), color="red", size=0.5)+
  facet_wrap(~year)
ggsave("sebs_oi_crw_2020_2025.png", width=8, height=6, units="in", dpi=300)

fit_nbs <- lm(meansst_oi ~ meansst_crw, data=sst_ts %>% filter(region=="Northern Bering Sea"))
summary(fit_nbs)
  
fit_sebs <- lm(meansst_oi ~ meansst_crw, data=sst_ts %>% filter(region=="Southeastern Bering Sea"))
summary(fit_sebs)
