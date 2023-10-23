##DPC_TIDY_1.1 - Joe Simeone
##Purpose: Take Delaware Population (DPC) raw data and format it for age adjustment and analysis. 
##Includes exploratory viz with ggplot/ plotly 
##Includes Knitr table with annual county estimates 2019 - 2023

rm(list=ls()) ##Gives clean enviornment

##Load packages for cleaning
library("tidyverse")     
library("readxl") #Loads package to read xlsx and sheets
library("writexl") #Exports packages 
library("janitor")

##packages for visualization
library("plotly")
library("knitr")
library("kableExtra")

setwd("/Users/joesimeone/Desktop/R Master/dpc")
##A function that deletes first blank row across different sheets
import_dpc <- function(sheetname){read_excel("DPC_Population Estimates_2022.xlsx",
                                             sheet = sheetname) %>% na.omit}

##Imports New Castle County Data
ncc_all <-import_dpc(sheetname = "New Castle-All")
ncc_white <-import_dpc(sheetname = "New Castle-Wh")
ncc_oth <-import_dpc(sheetname = "New Castle-Oth")
ncc_hisp <-import_dpc(sheetname = "New Castle-Hisp")
ncc_black <-import_dpc(sheetname = "New Castle-Bl")


##Imports Wilmington data
wilm_all <- import_dpc(sheetname = "Wilm-All")
wilm_white <- import_dpc(sheetname = "Wilm-Wh")
wilm_oth <- import_dpc(sheetname = "Wilm-Oth")
wilm_hisp <- import_dpc(sheetname = "Wilm-Hisp")
wilm_black <- import_dpc(sheetname = "Wilm-Bl")


#Imports Kent County Data
kent_all <- import_dpc(sheetname = "Kent-All")
kent_white <- import_dpc(sheetname = "Kent-Wh")
kent_oth <- import_dpc(sheetname = "Kent-Oth")
kent_hisp <- import_dpc(sheetname = "Kent-Hisp")
kent_black <- import_dpc(sheetname = "Kent-Bl")



#Imports Sussex County Data
sus_all <- import_dpc(sheetname = "Sussex-All")
sus_white <- import_dpc(sheetname = "Sussex-Wh")
sus_oth <- import_dpc(sheetname = "Sussex-Oth")
sus_hisp <- import_dpc(sheetname = "Sussex-Hisp")
sus_black<- import_dpc(sheetname = "Sussex-Bl")



##Basically, we want data formatted at the lowest level of stratification so that we can merge w/ SUDORS as needed.
##Proceeds in a few steps: 1). Pivot county level sheets long and lose meaningless values, then bind different county, 
##                               race objects together. Results in three variable data_frames (age/ gender/ race, year, population estimate).
##                           2). Apply a cleaning protocol to bound data with clean_dpc_data function.
##                           3). Bind together into master file for Delaware.

##Create function to pivot county level objects
piv_dpc_dat <- function(dat){
  dat %>% pivot_longer(cols = '2010':'2050', names_to = 'year', values_to = 'AgeGendCount')}

##Pivot NCC
ncc_wh_long <- piv_dpc_dat(dat=ncc_white) %>% na.omit()
ncc_oth_long <- piv_dpc_dat(dat=ncc_oth) %>% na.omit()
ncc_hisp_long <- piv_dpc_dat(dat=ncc_hisp) %>% na.omit()
ncc_black_long <- piv_dpc_dat(dat=ncc_black) %>% na.omit()

ncc_bind <- bind_rows(ncc_wh_long, ncc_oth_long, ncc_hisp_long, ncc_black_long)  ##Bind statement. Long data


##Pivot Wilm
wilm_wh_long <- piv_dpc_dat(dat=wilm_white)
wilm_oth_long <- piv_dpc_dat(dat=wilm_oth)
wilm_hisp_long <- piv_dpc_dat(dat=wilm_hisp)
wilm_black_long <- piv_dpc_dat(dat=wilm_black)

wilm_bind <- bind_rows(wilm_wh_long, wilm_oth_long, wilm_hisp_long, wilm_black_long)


##Pivot Kent
kent_wh_long <- piv_dpc_dat(dat=kent_white)
kent_oth_long <- piv_dpc_dat(dat=kent_oth)
kent_hisp_long <- piv_dpc_dat(dat=kent_hisp)
kent_black_long <- piv_dpc_dat(dat=kent_black)

kent_bind <- bind_rows(kent_wh_long, kent_oth_long, kent_hisp_long, kent_black_long)


##Pivot Sussex
sus_wh_long <- piv_dpc_dat(dat=sus_white)
sus_oth_long <- piv_dpc_dat(dat=sus_oth)
sus_hisp_long <- piv_dpc_dat(dat=sus_hisp)
sus_black_long <- piv_dpc_dat(dat=sus_black)

sus_bind <- bind_rows(sus_wh_long, sus_oth_long, sus_hisp_long, sus_black_long)









##Function call to clean county level data
clean_dpc_data <- function(dat, county, dum){ dat %>% rename(population_est= AgeGendCount) %>% 
    mutate(agegend_chr = as.character(`Age-Gender`)) %>% subset(agegend_chr!="Total") %>% 
    
    ##Create Character Variables
    mutate(age = parse_number(agegend_chr),
           race = str_extract(agegend_chr, "['W' 'B' 'H' 'O']+"),
           sex = str_extract(agegend_chr, "['M' 'F']+"),
           county := {{ county }}) %>% 
    
    ##Create Dummy Variables
    mutate(county_dummy := as.integer({{ dum }}),
           race_dummy =  case_when(race == 'W' ~ 0,
                                   race == 'B' ~ 1,
                                   race == 'H' ~ 5,
                                   race == 'O' ~ 7),
           sex_dummy =   if_else(sex == "M", 0, 1)) %>% 
    
    ## Create Age Groupings - Age Adjustment
    mutate(age_group_rt = case_when(
      age <= 4 ~ 0,
      age >= 5 & age <= 14 ~ 1,
      age >= 15 & age <= 24 ~ 2,
      age >= 25 & age <= 34 ~ 3,
      age >= 35 & age <= 44 ~ 4,
      age >= 45 & age <= 54 ~ 5,
      age >= 55 & age <= 64 ~ 6,
      age >= 65 & age <= 74 ~ 7,
      age >= 75 & age <= 84 ~ 8,
      age >= 85 ~ 9),
      
      ## Create Age Groupings - ten year age groups
      age_group_tenyr = case_when(
        age < 5 ~ 0,
        age >= 5 & age <= 14 ~ 1,
        age >= 15 & age <= 24 ~ 2,
        age >= 25 & age <= 34 ~ 3,
        age >= 45 & age <= 54 ~ 4,
        age >= 55 & age <= 64 ~ 5,
        age >= 65 ~ 6)) %>% 
    
    ## Discards initial age gender variable for cleaner formats
    select(!c(`Age-Gender`, 'agegend_chr'))}


##Creates objects for each clean county dataset
ncc_clean <- clean_dpc_data(dat=ncc_bind, county="NCC", dum = 1)
kent_clean <- clean_dpc_data(dat=kent_bind, county="K", dum = 2)
sus_clean <- clean_dpc_data(dat=sus_bind, county="SUS", dum = 3)
wilm_clean <- clean_dpc_data(dat=wilm_bind, county="WILM", dum = 4)


##Combined cleaned DE DATA
DEBIND1 <- rbind(ncc_clean, kent_clean, sus_clean)



## Coded out, but lets us write a file to export using clean data as needed.
#writexl::write_xlsx(DEBIND1, "/Users/joesimeone/Desktop/R Master/DPC_Tidy_2022_EST.xlsx")

#######################EXPLORATORY VISUAL SAMPLE################################
##Function to derive county (or other) strats for a single year
##    Lets you apply grouping variable as needed to summarize data by year and relevant stratification
get_ann_chart_by_strat <- function(YrStrat, by_var, expr, StratVar) {
  
  DEBIND1 %>% 
    filter(year==YrStrat) %>%                                                    ##Filters year
    group_by(across({{ by_var }})) %>%                                           ##Group Values
    summarise("sum_{{expr}}" := sum({{ expr }})) %>%                             ##Sums population estimates by group 
    ggplot(aes(county, sum_population_est, fill= {{StratVar}})) +                ##Makes basic graph to eyeball estimates
    geom_col(position = "dodge") + scale_y_continuous(labels = scales::comma)}.  ##Gives clustered bars and formats y axis


sex_co_23 <- get_ann_chart_by_strat(2023, c(county, sex), population_est, sex)
race_co_23 <- get_ann_chart_by_strat(2023, c(county, race), population_est, race)
age_co_23<- get_ann_chart_by_strat(2023, c(county,  age_group_tenyr), population_est,  age_group_tenyr)

ggplotly(race_co_23)


#######################KNITR Table Sample################################
year_snatch <- c("2018", "2019", "2021", "2022", "2023")
filter(dat, name %in% target) 


co_est_tbl <- DEBIND1 %>% group_by(year, county) %>% 
  summarise(sum_county = sum(population_est)) %>% 
  filter(year %in% year_snatch)

T <- knitr::kable(co_est_tbl, "html", caption = "Delaware Population, By County, 2018-2023") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "responsive", position = "left")) %>%
  pack_rows(index = c("2022" = 12, "2023" = 6)) %>% footnote(general = c("<i>All data owned by DFS & analyzed by DPH Staff</i>", "<i>Data stratified as it became available by month</i>"),
                                                             escape = FALSE)

print(T)




