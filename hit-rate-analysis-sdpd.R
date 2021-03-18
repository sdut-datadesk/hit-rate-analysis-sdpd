# Libraries
library(readr)
library(dplyr)
library(stringr)
library(stringi)
library(tidyr)

# Set working directory
setwd("~/Desktop/hit-rate-analysis-sdpd")

###################################################################
######################## IMPORT STOPS #############################
###################################################################

# Import stop data
library(readr)
stops <- read_csv("ripa_stops_datasd.csv", 
                  col_types = cols(highway_exit = col_character(), 
                                   stop_id = col_character(),
                                   time_stop = col_character(),
                                   land_mark = col_character(), officer_assignment_key = col_character(), 
                                   school_name = col_character()))

# Create column for eventual merge
stops$id <- paste0(stops$stop_id, "_", stops$pid)

# Rearrange
library(dplyr)
stops <- stops %>%
  select(stop_id, pid, id, everything())

# How many unique stop_individuals are there?
n_distinct(stops$id)
# 427,350
## Means there are duplicates in the data

# Find dupes
dups <- table(stops$id) %>% as.data.frame()
dups <- dups %>% filter(Freq >1)
# There are 15 stops_people (30 rows) that appear to have all of the same info
## Except for age. The majority were entered by officers with 1 years experience.

# Remove duplicates from df
## New row count == 427335
stops = subset(stops, !(id %in% dups$Var1))

# Check unique stop_individuals again
n_distinct(stops$id)
# 427,335 which matches row count

# Simplify column names
names(stops)
names(stops) <- c("stop_id", "pid", "id", "ori", "agency", "exp_years", 
                  "date", "time", "dur", "is_serv", "assign_key", 
                  "assign_words", "inters", "block", "ldmk", 
                  "street", "hw_exit", "is_school", "school_name", 
                  "city", "beat", "beat_name", "is_student", "lim_eng", 
                  "age", "gender_words", "is_gendnc", "gender_code", 
                  "gendnc_code", "lgbt")

# Remove dups
remove(dups)

###################################################################
######################## IMPORT RACE ##############################
###################################################################

# Import ethnicity data
race <- read_csv("ripa_race_datasd.csv", 
                 col_types = cols(stop_id = col_character()))

# Create column for merge
race$id <- paste0(race$stop_id, "_", race$pid)

# Rearrange
race <- race %>%
  select(id, everything())

# Remove original stop_id and pid columns to avoid dups in merge
race <- race %>% 
  select(-stop_id, -pid)

# Check for dups / why row count doesn't == stops df
n_distinct(race$id)
# 427,350
## This means duplicate rows

# Multiple races can be assigned to one person and they're all in multiple rows
## Aggregate rows based on the id and collapse bases into one cell
race = aggregate(race~id, data = race, paste, collapse="|")
## New row count == 427,350

###################################################################
###################### IMPORT DISABILITY ##########################
###################################################################

# Import disability data
dis <- read_csv("ripa_disability_datasd.csv", 
                col_types = cols(stop_id = col_character()))

# Create column for merge
dis$id <- paste0(dis$stop_id, "_", dis$pid)

# Rearrange
dis <- dis %>%
  select(id, everything())

# Remove original stop_id and pid columns to avoid dups in merge
dis <- dis %>% 
  select(-stop_id, -pid)

# Check for dups / why row count doesn't == stops df
n_distinct(dis$id)
# 427,350
## This means duplicate rows

# There can be multiple disabilities listed for each person
## Aggregate rows based on the id and collapse bases into one cell
dis <- aggregate(disability~id, data = dis, paste, collapse="|")
## Row count == 427,350

###################################################################
######################## IMPORT REASON ############################
###################################################################

# Import stop reasons data
reason <- read_csv("ripa_stop_reason_datasd.csv", 
                   col_types = cols(reason_for_stopcode = col_character(), 
                                    stop_id = col_character()))

# Create column for merge
reason$id <- paste0(reason$stop_id, "_", reason$pid)

# Rearrange
reason <- reason %>%
  select(id, everything())

# Remove original stop_id and pid columns to avoid dups in merge
reason <- reason %>% 
  select(-stop_id, -pid)

# Check for dups / why row count doesn't == stops df
n_distinct(reason$id)
# 427,350
## This means duplicate rows

# There are duplicates due to multiple reason_for_stop_detail entries/diffs
## Aggregate rows based on the id and collapse bases into one cell
reason2 <- reason %>% 
  group_by(id) %>%
  summarise(reason_for_stop_detail = paste(reason_for_stop_detail, collapse = "|"))

# Upon inspection, no other columns contain multiple (different) entries
## Keep only disctinct rows for each id to merge with reason2
reason3 <- reason[!duplicated(reason$id),]

# Remove reason_for_stop_detail from reason3 for merge
reason3 <- reason3 %>% 
  select(-reason_for_stop_detail)

# Left_join reason2 and reason3
reason_final <- left_join(reason2, reason3, by = "id")

# Rearrange
reason_final <- reason_final %>%
  select(id, reason_for_stop, reason_for_stopcode, 
         reason_for_stop_code_text, reason_for_stop_detail, 
         reason_for_stop_explanation)

remove(reason, reason2, reason3)

# Simplify column names
names(reason_final)
names(reason_final) <- c("id", "reason_words", "reasonid", "reason_text", 
                         "reason_detail", "reason_exp")

###################################################################
###################### IMPORT SEARCH BASIS ########################
###################################################################

# Import search basis
search_basis <- read_csv("ripa_search_basis_datasd.csv", 
                         col_types = cols(stop_id = col_character()))

# Create column for merge
search_basis$id <- paste0(search_basis$stop_id, "_", search_basis$pid)

# Rearrange
search_basis <- search_basis %>%
  select(id, everything())

# Remove original stop_id and pid columns to avoid dups in merge
search_basis <- search_basis %>% 
  select(-stop_id, -pid)

# Check for dups / why row count doesn't == stops df
n_distinct(search_basis$id)
# 427,350
## This means duplicate rows

# There can be multiple search bases listed for each person
## Aggregate rows based on the id and collapse bases into one cell
search_basis2 <- search_basis %>% 
  group_by(id) %>%
  summarise(basis_for_search = paste(basis_for_search, collapse = "|"))

# Upon inspection, no other columns contain multiple (different) entries
## Keep only disctinct rows for each id to merge with search_basis2
search_basis3 <- search_basis[!duplicated(search_basis$id),]

# Remove orig basis_for_search column
search_basis3 <- search_basis3 %>% 
  select(-basis_for_search)

# Left_join
search_basis_final <- left_join(search_basis2, search_basis3, by = "id")

remove(search_basis, search_basis2, search_basis3)

# Simplify column names
names(search_basis_final)
names(search_basis_final) <- c("id", "search_basis", "search_basis_exp")

###################################################################
###################### IMPORT SEIZE BASIS #########################
###################################################################

# Import seize basis
seize_basis <- read_csv("ripa_prop_seize_basis_datasd.csv", 
                        col_types = cols(stop_id = col_character()))

# Create column for merge
seize_basis$id <- paste0(seize_basis$stop_id, "_", seize_basis$pid)

# Rearrange
seize_basis <- seize_basis %>%
  select(id, everything())

# Remove original stop_id and pid columns to avoid dups in merge
seize_basis <- seize_basis %>% 
  select(-stop_id, -pid)

# Check for dups / why row count doesn't == stops df
n_distinct(seize_basis$id)
# 427,350
## This means duplicate rows

# There can be multiple seize bases listed for each person
## Aggregate rows based on the id and collapse bases into one cell
seize_basis <- seize_basis %>% 
  group_by(id) %>%
  summarise(basisforpropertyseizure = paste(basisforpropertyseizure, collapse = "|"))

# Simplify column names
names(seize_basis) <- c("id", "seiz_basis")

###################################################################
####################### IMPORT PROP TYPE ##########################
###################################################################

# Import property seized type
prop_type <- read_csv("ripa_prop_seize_type_datasd.csv", 
                      col_types = cols(stop_id = col_character()))

# Create column for merge
prop_type$id <- paste0(prop_type$stop_id, "_", prop_type$pid)

# Rearrange
prop_type <- prop_type %>%
  select(id, everything())

# Remove original stop_id and pid columns to avoid dups in merge
prop_type <- prop_type %>% 
  select(-stop_id, -pid)

# Check for dups / why row count doesn't == stops df
n_distinct(prop_type$id)
# 427,350
## This means duplicate rows

# There can be multiple property types listed for each person
## Aggregate rows based on the id and collapse bases into one cell
prop_type <- prop_type %>% 
  group_by(id) %>%
  summarise(type_of_property_seized = paste(type_of_property_seized, collapse = "|"))

# Simplify column names
names(prop_type) <- c("id", "prop_type")

###################################################################
####################### IMPORT CONTRABAND #########################
###################################################################

# Import contraband / evidence
cont <- read_csv("ripa_contraband_evid_datasd.csv", 
                 col_types = cols(stop_id = col_character()))

# Create column for merge
cont$id <- paste0(cont$stop_id, "_", cont$pid)

# Rearrange
cont <- cont %>%
  select(id, everything())

# Remove original stop_id and pid columns to avoid dups in merge
cont <- cont %>% 
  select(-stop_id, -pid)

# Check for dups / why row count doesn't == stops df
n_distinct(cont$id)
# 427,350
## This means duplicate rows

# There can be multiple contrabands listed for each person
## Aggregate rows based on the id and collapse bases into one cell
cont <- cont %>% 
  group_by(id) %>%
  summarise(contraband = paste(contraband, collapse = "|"))

# Simplify column names
names(cont) <- c("id", "cont")

###################################################################
######################### IMPORT ACTIONS ##########################
###################################################################

# Import actions taken
actions <- read_csv("ripa_actions_taken_datasd.csv", 
                    col_types = cols(consented = col_character(), 
                                     stop_id = col_character()))

# Create column for merge
actions$id <- paste0(actions$stop_id, "_", actions$pid)

# Rearrange
actions <- actions %>%
  select(id, everything())

# Remove original stop_id and pid columns to avoid dups in merge
actions <- actions %>% 
  select(-stop_id, -pid)

# Check for dups / why row count doesn't == stops df
n_distinct(actions$id)
# 427,350
## This means duplicate rows

# There can be multiple actions taken for each person
## Aggregate rows based on the id and collapse bases into one cell
actions2 <- actions %>% 
  group_by(id) %>%
  summarise(actions = paste(action, collapse = "|"))

# There's Y or N for consent of each action 
## Aggregate rows based on the id and collapse bases into one cell
actions3 <- actions %>%
  group_by(id) %>%
  summarise(consented = paste(consented, collapse = "|"))

# Left_join actions2 and actions3
actions_final <- left_join(actions2, actions3, by = "id")

remove(actions, actions2, actions3)

# Simplify column names
names(actions_final) <- c("id", "actions", "act_consent")

###################################################################
########################### MERGE #################################
###################################################################

# Merge
library(plyr)
master <- join_all(list(stops, race, dis, reason_final, search_basis_final, 
                        seize_basis, prop_type, cont, actions_final), 
                   by = "id", 
                   type = "left")
## Row count should == 427,335 (stops row count)
## 44 columns total

# Unload plyr, to avoid masking with dplyr
detach("package:plyr", unload=TRUE)

# Remove originals to clean environment
remove(actions_final, cont, dis, prop_type, race, reason_final, search_basis_final, seize_basis, stops)

###################################################################
########################### CLEAN #################################
###################################################################

# Remove leading and trailing whitespace
library(stringr)
master <- master %>% 
  mutate_if(is.character, str_trim)

# Remove all types of whitespace inside strings
master <- master %>% 
  mutate_if(is.character, str_squish)

# Remove floating commas
master$reason_exp <- gsub(" , ", ", ", master$reason_exp)
master$search_basis_exp <- gsub(" , ", ", ", master$search_basis_exp)

## Count number of characters in time string
library(stringi)
master$count <- nchar(stri_escape_unicode(master$time))

table(master$count)
#      8     19 
#   427312   23 

# Some times have incorrect dates attached
## gsub out the dates
master$time <- gsub("1900-01-01 ", "", master$time)
master$time <- gsub("1899-12-30 ", "", master$time)

# Create second time column that's in time format
library(chron)
master$time2 <- times(master$time)

# Unload chron, to avoid masking with lubridate
detach("package:chron", unload=TRUE)

# Remove count column
master <- master %>% 
  select(-count)

###################################################################
####################### CLEAN RACE ################################
###################################################################

# Create simplified column for race, written out
master <- master %>% 
  mutate(race_simp = str_replace_all(race, "White", "white") %>% 
           str_replace_all("Pacific Islander", "pi") %>% 
           str_replace_all("Native American", "nam") %>% 
           str_replace_all("Middle Eastern or South Asian", "me_sa") %>% 
           str_replace_all("Hispanic/Latino/a", "hisp") %>% 
           str_replace_all("Black/African American", "black") %>% 
           str_replace_all("Asian", "asian"))

# Create new race category
## Hispanic + other race == "hisp"
## More than one race (but not hisp) == "mixed"
master <- master %>% 
  mutate(race_condensed = case_when(str_detect(race_simp, "hisp") ~ "hisp", # if contains "hisp", add to hispanic category
                                    str_detect(race_simp, "\\|") ~ "mixed", # if contains "|", create mixed category
                                    TRUE ~ race_simp)) # if neither above is true, paste original from race_words

# Remove original race column
master <- master %>% 
  select(-race)

# Create race column descriptions for easy calculations
master$asian <- ifelse(grepl("asian", master$race_condensed), 1, 0)
master$black <- ifelse(grepl("black", master$race_condensed), 1, 0)
master$hisp <- ifelse(grepl("hisp", master$race_condensed), 1, 0)
master$me_sa <- ifelse(grepl("me_sa", master$race_condensed), 1, 0)
master$mixed <- ifelse(grepl("mixed", master$race_condensed), 1, 0)
master$nam <- ifelse(grepl("nam", master$race_condensed), 1, 0)
master$pi <- ifelse(grepl("pi", master$race_condensed), 1, 0)
master$white <- ifelse(grepl("white", master$race_condensed), 1, 0)

###################################################################
###################### CLEAN STOP REASON ##########################
###################################################################

# Create new column for reason of stop simplified
## Only one reason is listed in this column, no multiple entries
master <- master %>% 
  mutate(reason_simp = str_replace_all(reason_words, "Determine whether the student violated school policy", "rs_school") %>% 
           str_replace_all("Possible conduct warranting discipline under Education Code sections 48900, 48900.2, 48900.3, 48900.4 and 48900.7", "rs_ed") %>%
           str_replace_all("Consensual Encounter resulting in a search", "rs_consent") %>%
           str_replace_all("Investigation to determine whether the person was truant", "rs_truant") %>%
           str_replace_all("Knowledge of outstanding arrest warrant/wanted person", "rs_warrant") %>%
           str_replace_all("Known to be on Parole / Probation / PRCS / Mandatory Supervision", "rs_parole") %>% 
           str_replace_all("Reasonable Suspicion", "rs_susp") %>% 
           str_replace_all("Traffic Violation", "rs_traff"))

# Create final "grouped" reason id column
master$reason_condensed <- ifelse(master$reason_simp == "rs_traff", "TRAFFIC",
                                  ifelse(master$reason_simp == "rs_susp", "SUSP",
                                         ifelse(master$reason_simp == "rs_school", "OTHER",
                                                ifelse(master$reason_simp == "rs_ed", "OTHER",
                                                       ifelse(master$reason_simp == "rs_consent", "OTHER",
                                                              ifelse(master$reason_simp == "rs_truant", "OTHER",
                                                                     ifelse(master$reason_simp == "rs_warrant", "OTHER",
                                                                            ifelse(master$reason_simp == "rs_parole", "OTHER", "CHECK"))))))))

###################################################################
###################### CLEAN SEARCH BASIS #########################
###################################################################

# Change character string of NA in search_basis to real NA
master$search_basis[master$search_basis == "NA"] = NA

# Create is_searched column check
master$is_searched <- ifelse(!is.na(master$search_basis),1,0)

# Create new column for search basis type, simplified
master <- master %>% 
  mutate(search_basis_simp = str_replace_all(search_basis, "Suspected violation of school policy", "sch_school") %>% 
           str_replace_all("Vehicle inventory", "sch_inventory") %>%
           str_replace_all("Exigent circumstances/emergency", "sch_emerg") %>%
           str_replace_all("Incident to arrest", "sch_arrest") %>%
           str_replace_all("Consent given", "sch_consent") %>%
           str_replace_all("Officer Safety/safety of others", "sch_safety") %>% 
           str_replace_all("Search Warrant", "sch_warrant") %>% 
           str_replace_all("Condition of parole / probation/ PRCS / mandatory supervision", "sch_parole") %>% 
           str_replace_all("Suspected weapons", "sch_susp_weapons") %>% 
           str_replace_all("Visible contraband", "sch_vis_cont") %>% 
           str_replace_all("Odor of contraband", "sch_od_cont") %>% 
           str_replace_all("Canine detection", "sch_k9") %>% 
           str_replace_all("Evidence of crime", "sch_crime"))

# Create columns for search basis descriptions
master$sch_consent <- ifelse(grepl("sch_consent", master$search_basis_simp), 1, 0)
master$sch_safety <- ifelse(grepl("sch_safety", master$search_basis_simp), 1, 0)
master$sch_warrant <- ifelse(grepl("sch_warrant", master$search_basis_simp), 1, 0)
master$sch_parole <- ifelse(grepl("sch_parole", master$search_basis_simp), 1, 0)
master$sch_susp_weapons <- ifelse(grepl("sch_susp_weapons", master$search_basis_simp), 1, 0)
master$sch_vis_cont <- ifelse(grepl("sch_vis_cont", master$search_basis_simp), 1, 0)
master$sch_od_cont <- ifelse(grepl("sch_od_cont", master$search_basis_simp), 1, 0)
master$sch_k9 <- ifelse(grepl("sch_k9", master$search_basis_simp), 1, 0)
master$sch_crime <- ifelse(grepl("sch_crime", master$search_basis_simp), 1, 0)
master$sch_arrest <- ifelse(grepl("sch_arrest", master$search_basis_simp), 1, 0)
master$sch_emerg <- ifelse(grepl("sch_emerg", master$search_basis_simp), 1, 0)
master$sch_inventory <- ifelse(grepl("sch_inventory", master$search_basis_simp), 1, 0)
master$sch_school <- ifelse(grepl("sch_school", master$search_basis_simp), 1, 0)

###################################################################
######################## DISCRETION ###############################
###################################################################

# RIPA categorized search basis into higher discretion and lower discretion
## Experts suggested (in interviews) that the U-T categorize search bases 
## into "discretionary" and "non-discretionary"

# Create discretionary column
## Since there can be more than one "search basis", 
## create check if any of these reasons were selected
master$discretionary <- ifelse(master$sch_consent == 1, 1,
                               ifelse(master$sch_safety == 1, 1,
                                      ifelse(master$sch_parole == 1, 1,
                                             ifelse(master$sch_susp_weapons == 1, 1,
                                                    ifelse(master$sch_vis_cont == 1, 1,
                                                           ifelse(master$sch_od_cont == 1, 1,
                                                                  ifelse(master$sch_k9 == 1, 1,
                                                                         ifelse(master$sch_crime == 1, 1,
                                                                                ifelse(master$sch_emerg == 1, 1,
                                                                                       ifelse(master$sch_school == 1, 1, 0))))))))))

# Create non_discretionary column
## Since there can be more than one "search basis",
## create check if any of these reasons were selected
master$non_discretionary <- ifelse(master$sch_warrant == 1, 1,
                                   ifelse(master$sch_arrest == 1, 1,
                                          ifelse(master$sch_inventory == 1, 1, 0)))

# There are some that are both discretionary and non_discretionary, due to multiple search bases
## Remove 1's from discretionary if there's also a non_discretionary reason for the search
### Since non_discretionary searches will always take place, regardless of other circumstances
master$discretionary <- ifelse(master$discretionary == 1 & master$non_discretionary == 1, 0, 
                               master$discretionary)

###################################################################
###################### CLEAN CONTRABAND ###########################
###################################################################

# Create new column for contraband type found during search, simplified
master <- master %>% 
  mutate(cont_simp = str_replace_all(cont, "Other Contraband or evidence", "cont_other") %>% 
           str_replace_all("Cell phone\\(s\\) or electronic device\\(s\\)", "cont_cell") %>%
           str_replace_all("Suspected Stolen property", "cont_stolen_prop") %>%
           str_replace_all("Drug Paraphernalia", "cont_para") %>%
           str_replace_all("Money", "cont_money") %>%
           str_replace_all("Alcohol", "cont_alcohol") %>% 
           str_replace_all("Drugs/narcotics", "cont_drugs") %>% 
           str_replace_all("Weapon\\(s\\) other than a firearm", "cont_weapons") %>% 
           str_replace_all("Ammunition", "cont_ammu") %>% 
           str_replace_all("Firearm\\(s\\)", "cont_firearm") %>% 
           str_replace_all("None", "cont_none"))

# Create columns for contraband descriptions
master$cont_other <- ifelse(grepl("cont_other", master$cont_simp), 1, 0)
master$cont_cell <- ifelse(grepl("cont_cell", master$cont_simp), 1, 0)
master$cont_stolen_prop <- ifelse(grepl("cont_stolen_prop", master$cont_simp), 1, 0)
master$cont_para <- ifelse(grepl("cont_para", master$cont_simp), 1, 0)
master$cont_money <- ifelse(grepl("cont_money", master$cont_simp), 1, 0)
master$cont_alcohol <- ifelse(grepl("cont_alcohol", master$cont_simp), 1, 0)
master$cont_drugs <- ifelse(grepl("cont_drugs", master$cont_simp), 1, 0)
master$cont_weapons <- ifelse(grepl("cont_weapons", master$cont_simp), 1, 0)
master$cont_ammu <- ifelse(grepl("cont_ammu", master$cont_simp), 1, 0)
master$cont_firearm <- ifelse(grepl("cont_firearm", master$cont_simp), 1, 0)
master$cont_none <- ifelse(grepl("cont_none", master$cont_simp), 1, 0)

# Create column that aggregates drugs, weapons and ammunition
master$drugs_weapons <- ifelse(master$cont_drugs == 1, 1, 
                               ifelse(master$cont_weapons == 1, 1,
                                      ifelse(master$cont_firearm == 1, 1,
                                             ifelse(master$cont_ammu == 1, 1, 0))))

###################################################################
######################### CLEAN ACTIONS ###########################
###################################################################

# Create new column for actions, simplified
master <- master %>% 
  mutate(act_simp = str_replace_all(actions, "None", "act_none") %>% 
           str_replace_all("Admission or written statement obtained from student", "act_student") %>%
           str_replace_all("Vehicle impounded", "act_vi") %>%
           str_replace_all("Property was seized", "act_prop_seiz") %>%
           str_replace_all("Search of property was conducted", "act_sch_prop") %>%
           str_replace_all("Asked for consent to search property", "act_req_sch_prop") %>% 
           str_replace_all("Search of person was conducted", "act_sch_pers") %>% 
           str_replace_all("Asked for consent to search person", "act_req_sch_pers") %>% 
           str_replace_all("Person photographed", "act_photo") %>%
           str_replace_all("Physical or Vehicle contact", "act_physical") %>%
           str_replace_all("Chemical spray used", "act_chem") %>%
           str_replace_all("Baton or other impact weapon used", "act_baton") %>%
           str_replace_all("Canine bit or held person", "act_k9_bit") %>%
           str_replace_all("Impact projectile discharged or used", "act_ip") %>%
           str_replace_all("Electronic control device used", "act_elect") %>%
           str_replace_all("Firearm discharged or used", "act_fad") %>%
           str_replace_all("Firearm pointed at person", "act_fp") %>%
           str_replace_all("Canine removed from vehicle or used to search", "act_k9_rem") %>%
           str_replace_all("Patrol car detention", "act_car_det") %>%
           str_replace_all("Handcuffed or flex cuffed", "act_hc") %>%
           str_replace_all("Curbside detention", "act_curb") %>%
           str_replace_all("Field sobriety test conducted", "act_sober") %>%
           str_replace_all("Person removed from vehicle by physical contact", "act_rem_cont") %>%
           str_replace_all("Person removed from vehicle by order", "act_rem_order"))

# Create separate column for action detained category
master$act_detained <- ifelse(grepl("act_car_det", master$act_simp), 1, 
                              ifelse(grepl("act_hc", master$act_simp), 1,
                                     ifelse(grepl("act_curb", master$act_simp), 1, 0)))

# Create separate column for action force category
master$act_force <- ifelse(grepl("act_chem", master$act_simp), 1, 
                           ifelse(grepl("act_baton", master$act_simp), 1,
                                  ifelse(grepl("act_k9_bit", master$act_simp), 1,
                                         ifelse(grepl("act_ip", master$act_simp), 1, 
                                                ifelse(grepl("act_elect", master$act_simp), 1,
                                                       ifelse(grepl("act_fad", master$act_simp), 1,
                                                              ifelse(grepl("act_fp", master$act_simp), 1,
                                                                     ifelse(grepl("act_rem_cont", master$act_simp), 1,
                                                                            ifelse(grepl("act_physical", master$act_simp), 1,0)))))))))

# Create separate column for action none category
master$act_none <- ifelse(grepl("act_none", master$act_simp), 1, 0)

# Create separate column for action other category
master$act_other <- ifelse(grepl("act_k9_rem", master$act_simp), 1, 
                           ifelse(grepl("act_stud", master$act_simp), 1,
                                  ifelse(grepl("act_photo", master$act_simp), 1,
                                                ifelse(grepl("act_sober", master$act_simp), 1,
                                                       ifelse(grepl("act_rem_order", master$act_simp), 1, 0)))))

# Create separate column for action search requested category
master$act_req_search <- ifelse(grepl("act_req_sch_prop", master$act_simp), 1, 
                                ifelse(grepl("act_req_sch_pers", master$act_simp), 1, 0))

# Create separate column for action search conducted category
master$act_search <- ifelse(grepl("act_sch_prop", master$act_simp), 1, 
                            ifelse(grepl("act_sch_pers", master$act_simp), 1, 0))

# Create separate column for action seize category
master$act_seize <- ifelse(grepl("act_vi", master$act_simp), 1, 
                           ifelse(grepl("act_prop_seize", master$act_simp), 1, 0))

###################################################################
########################## ANALYSIS ###############################
###################################################################

# How many stops total?
n_distinct(master$stop_id)
# 372120

# How many stops resulted in searches?
master %>% 
  filter(is_searched == 1) %>% 
  group_by(stop_id) %>% 
  summarise(total = n())
# 81,910
## This means 22 percent of stops resulted in a search of a person or property

# Create table of just searches
searches <- master %>% 
  filter(is_searched == 1)
## Row count == 88,768 people were searched in the 81,910 stops

###################################################################

# How many people were stopped because it was officer-initiated?
sum(master$is_serv)
# 48,356 were stopped because of a service call
## 378,979 people were stopped when initiated by an officer

# How many stops were officer-initiated?
master %>% 
  filter(master$is_serv == 0) %>%
  group_by(stop_id) %>% 
  summarise(total = n())
# 329,183 were not service calls out of 372,120 total stops
## 88.5 percent of stops were initiated by an officer, 
## or were not in response to a call for service, radio call or dispatch. 

###################################################################

# Calculate proportion of race of all people stopped
master %>% 
  count(race_condensed) %>% 
  arrange(desc(n))

# race_condensed  count
#          white 180117
#           hisp 125629
#          black  84605
#          asian  20259
#          me_sa  11120
#             pi   3312
#          mixed   1423
#            nam    870

# Calculate percentages
master %>% 
  count(race_condensed) %>% 
  mutate((prop = n / sum(n))*100) %>% 
  arrange(desc(`(prop = n/sum(n)) * 100`))

# race_condensed  count               percent
#          white 180117              42.1488996
#           hisp 125629              29.3982473
#          black  84605              19.7982847
#          asian  20259               4.7407771
#          me_sa  11120               2.6021739
#             pi   3312               0.7750360
#          mixed   1423               0.3329940
#            nam    870               0.2035873

###################################################################
# Reason for stop analysis

# Calculate proportion of race for each reason for stop
reasons <- master %>% 
  group_by(reason_condensed) %>% 
  count(race_condensed)

# Spread
library(tidyr)
reasons <- reasons %>%
  spread(key = reason_condensed, value = n, fill = 0)

# Add percentages
reasons %>%
  mutate(per_of_other = round((OTHER / sum(OTHER))*100,1),
         per_of_susp = round((SUSP / sum(SUSP))*100,1),
         per_of_traffic = round((TRAFFIC / sum(TRAFFIC))*100,1)) %>% 
  arrange(desc(per_of_susp))

# race_condensed OTHER   SUSP TRAFFIC per_of_other per_of_susp per_of_traffic
# white           9050 104121   66946         39.4        46.6           37  
# hisp            7094  56606   61929         30.9        25.3           34.2
# black           5367  50536   28702         23.3        22.6           15.9
# asian            861   6166   13232          3.7         2.8            7.3
# me_sa            290   3207    7623          1.3         1.4            4.2
# pi               204   1531    1577          0.9         0.7            0.9
# mixed             63    636     724          0.3         0.3            0.4
# nam               66    619     185          0.3         0.3            0.1
# TOTAL          22995 223422  180918

# Add percentages by race, as opposed to by the total
reasons %>% 
  mutate(other_per = round((OTHER / (OTHER + SUSP + TRAFFIC))*100,1),
         susp_per = round((SUSP / (OTHER + SUSP + TRAFFIC))*100,1),
         traffic_per = round((TRAFFIC / (OTHER + SUSP + TRAFFIC))*100,1),
         total_race = OTHER + SUSP + TRAFFIC) %>%
  arrange(desc(susp_per))

# race_condensed OTHER   SUSP TRAFFIC other_per susp_per traffic_per total_race
# nam               66    619     185       7.6     71.1        21.3        870
# black           5367  50536   28702       6.3     59.7        33.9      84605
# white           9050 104121   66946       5       57.8        37.2     180117
# pi               204   1531    1577       6.2     46.2        47.6       3312
# hisp            7094  56606   61929       5.6     45.1        49.3     125629
# mixed             63    636     724       4.4     44.7        50.9       1423
# asian            861   6166   13232       4.2     30.4        65.3      20259
# me_sa            290   3207    7623       2.6     28.8        68.6      11120

###################################################################
# Pedestrian vs Traffic Stops

# Create is_traffic stop column
master$is_traffic <- ifelse(master$reason_condensed == "TRAFFIC", 1, 0)

sum(master$is_traffic)
# 180,918 were involved in traffic stops

# Calculate proportion of race stopped in traffic vs ped stops
trVped <- master %>% 
  group_by(is_traffic) %>% 
  count(race_condensed)

# Spread
trVped <- trVped %>%
  spread(key = is_traffic, value = n, fill = 0)

# Rename columns
names(trVped) <- c("race_condensed", "pedestrian", "traffic")

# Add percentages
trVped %>%
  mutate(per_of_pedestrian = round((pedestrian / sum(pedestrian))*100,1),
         per_of_traffic = round((traffic / sum(traffic))*100,1)) %>% 
  arrange(desc(per_of_pedestrian))

# race_condensed pedestrian traffic per_of_pedestrian per_of_traffic
# white              113171   66946              45.9           37  
# hisp                63700   61929              25.9           34.2
# black               55903   28702              22.7           15.9
# asian                7027   13232               2.9            7.3
# me_sa                3497    7623               1.4            4.2
# pi                   1735    1577               0.7            0.9
# mixed                 699     724               0.3            0.4
# nam                   685     185               0.3            0.1
# TOTAL              246417  180918

# Add percentages by race, as opposed to by the total
trVped %>%
  mutate(pedestrian_per = round((pedestrian / (pedestrian + traffic))*100,1),
         traffic_per = round((traffic / (pedestrian + traffic))*100,1),
         total_race = pedestrian + traffic) %>% 
  arrange(desc(pedestrian_per))

# race_condensed pedestrian traffic pedestrian_per traffic_per total_race
# nam                   685     185           78.7        21.3        870
# black               55903   28702           66.1        33.9      84605
# white              113171   66946           62.8        37.2     180117
# pi                   1735    1577           52.4        47.6       3312
# hisp                63700   61929           50.7        49.3     125629
# mixed                 699     724           49.1        50.9       1423
# asian                7027   13232           34.7        65.3      20259
# me_sa                3497    7623           31.4        68.6      11120

###################################################################
# How many people were searched?
sum(master$is_searched)
# 88,768
## They searched about 20.77 percent of people

###################################################################
# Searched vs Not Searched

# Calculate proportion of race searched and not searched
search_race <- master %>% 
  group_by(is_searched) %>% 
  count(race_condensed)

# Spread
search_race <- search_race %>%
  spread(key = is_searched, value = n, fill = 0)

# Rename columns
names(search_race) <- c("race_condensed", "no_search", "searched")

# Add percentages
search_race %>%
  mutate(per_of_no_search = round((no_search / sum(no_search))*100,1),
         per_of_searched = round((searched / sum(searched))*100,1)) %>%
  arrange(desc(per_of_searched))

# race_condensed no_search searched per_of_no_search per_of_searched
# white             145164    34953             42.9            39.4
# hisp               97949    27680             28.9            31.2
# black              63386    21219             18.7            23.9
# asian              17629     2630              5.2             3  
# me_sa              10011     1109              3               1.2
# pi                  2596      716              0.8             0.8
# mixed               1169      254              0.3             0.3
# nam                  663      207              0.2             0.2

# Add percentages by race, as opposed to by the total
search_race %>%
  mutate(no_search_per = round((no_search / (no_search + searched))*100,1),
         searched_per = round((searched / (no_search + searched))*100,1),
         total_race = no_search + searched) %>%
  arrange(desc(searched_per))

# race_condensed no_search searched no_search_per searched_per total_race
# black              63386    21219          74.9         25.1      84605
# nam                  663      207          76.2         23.8        870
# hisp               97949    27680          78           22       125629
# pi                  2596      716          78.4         21.6       3312
# white             145164    34953          80.6         19.4     180117
# mixed               1169      254          82.2         17.8       1423
# asian              17629     2630          87           13        20259
# me_sa              10011     1109          90           10        11120

# GLM TESTS
# If black
glmTest1 <- glm(is_searched ~ black,
                data = master,
                family = "binomial")
summary(glmTest1)
exp(coef(glmTest1))

# (Intercept)     black 
#  0.2454712   1.3637385
## Someone is 1.4 times more likely to be searched if they are black

# If native american
glmTest2 <- glm(is_searched ~ nam,
                data = master,
                family = "binomial")
summary(glmTest2)
exp(coef(glmTest2))

#(Intercept)  nam 
# 0.2620892   1.1912630
## Someone is 1.2 times more likely to be searched if they are Native American

# If hisp
glmTest3 <- glm(is_searched ~ hisp,
                data = master,
                family = "binomial")
summary(glmTest3)
exp(coef(glmTest3))

#(Intercept)   hispanic 
# 0.2538796   1.1131105
## Someone is 1.1 times more likely to be searched if they are hispanic

###################################################################
# Discretionary vs non-discretionary

# Calculate proportion of race searched when there's discretion or not
disc_race <- searches %>% 
  group_by(discretionary) %>% 
  count(race_condensed)

# Spread
disc_race <- disc_race %>%
  spread(key = discretionary, value = n, fill = 0)

# Rename columns
names(disc_race) <- c("race_condensed", "non_disc", "disc")

# Add percentages
disc_race %>%
  mutate(per_of_non_disc = round((non_disc / sum(non_disc))*100,1),
         per_of_disc = round((disc / sum(disc))*100,1)) %>% 
  arrange(desc(per_of_disc))

# race_condensed non_disc  disc per_of_non_disc per_of_disc
# white             20872 14081            42          36.1
# hisp              14933 12747            30          32.7
# black             10999 10220            22.1        26.2
# asian              1488  1142             3           2.9
# me_sa               743   366             1.5         0.9
# pi                  419   297             0.8         0.8
# mixed               166    88             0.3         0.2
# nam                 123    84             0.2         0.2
# TOTAL             49743 39025

# Add percentages by race, as opposed to by the total
disc_race %>%
  mutate(non_disc_per = round((non_disc / (non_disc + disc))*100,1),
         disc_per = round((disc / (non_disc + disc))*100,1),
         total_race = non_disc + disc) %>% 
  arrange(desc(disc_per))

# race_condensed non_disc  disc non_disc_per disc_per total_race
# black             10999 10220         51.8     48.2      21219
# hisp              14933 12747         53.9     46.1      27680
# asian              1488  1142         56.6     43.4       2630
# pi                  419   297         58.5     41.5        716
# nam                 123    84         59.4     40.6        207
# white             20872 14081         59.7     40.3      34953
# mixed               166    88         65.4     34.6        254
# me_sa               743   366         67       33         1109

###################################################################
# Drugs, weapons or ammunition in contraband

# Calculate proportion of race when drugs, weapons or ammunition was found
## Of the people who were searched
dw_race <- searches %>% 
  group_by(drugs_weapons) %>% 
  count(race_condensed)

# Spread
dw_race <- dw_race %>%
  spread(key = drugs_weapons, value = n, fill = 0)

# Rename columns
names(dw_race) <- c("race_condensed", "none", "drugs_weapons")

# Add percentages
dw_race %>%
  mutate(per_of_none = round((none / sum(none))*100,1),
         per_of_drugs_weapons = round((drugs_weapons / sum(drugs_weapons))*100,1)) %>% 
  arrange(desc(per_of_drugs_weapons))

# race_condensed  none drugs_weapons per_of_none per_of_drugs_weapons
# white          29696          5257        39.4                 39.1
# hisp           23591          4089        31.3                 30.4
# black          17782          3437        23.6                 25.6
# asian           2282           348         3                    2.6
# me_sa            978           131         1.3                  1  
# pi               611           105         0.8                  0.8
# mixed            218            36         0.3                  0.3
# nam              175            32         0.2                  0.2
# TOTAL          75333         13435

# Add percentages by race, as opposed to by the total
dw_race %>%
  mutate(none_per = round((none / (none + drugs_weapons))*100,1),
         drugs_weapons_per = round((drugs_weapons / (none + drugs_weapons))*100,1),
         total_race = none + drugs_weapons) %>% 
  arrange(desc(drugs_weapons_per))

# race_condensed  none drugs_weapons none_per drugs_weapons_per total_race
# black          17782          3437     83.8              16.2      21219
# nam              175            32     84.5              15.5        207
# white          29696          5257     85                15        34953
# hisp           23591          4089     85.2              14.8      27680
# pi               611           105     85.3              14.7        716
# mixed            218            36     85.8              14.2        254
# asian           2282           348     86.8              13.2       2630
# me_sa            978           131     88.2              11.8       1109

###################################################################
# Calculate search yield rates
## Proportion of individuals subject to a search where contraband / evidence was found
### search yield rate formula = 
### (number of searched people with contraband / total number searched) * 100

# Overall syr
searches %>% 
  summarise(syr = mean(cont_none))
# 0.7536838 ~ 76 percent of people searched had no contraband or evidence

# syr by race
syr <- searches %>% 
  group_by(cont_none) %>% 
  count(race_condensed)

# Spread
syr <- syr %>%
  spread(key = cont_none, value = n, fill = 0)

# Rename columns
## Unlike other 1/0 check columns, if cont_cont == 1, there was no contraband found
names(syr) <- c("race_condensed", "cont", "no_cont")

# Add percentages to calculate hit rate
syr %>%
  mutate(hit_rate = round((cont / (sum(cont) + sum(no_cont)))*100,1)) %>% 
  arrange(desc(hit_rate))

# race_condensed  cont no_cont hit_rate
# white           8543   26410      9.6
# hisp            6572   21108      7.4
# black           5685   15534      6.4
# asian            580    2050      0.7
# me_sa            219     890      0.2
# pi               160     556      0.2
# mixed             53     201      0.1
# nam               53     154      0.1
# TOTAL          21865   66903

# Add percentages by race, as opposed to by the total
syr %>%
  mutate(cont_per = round((cont / (cont + no_cont))*100,1),
         no_cont_per = round((no_cont / (cont + no_cont))*100,1),
         total_race = cont + no_cont) %>% 
  arrange(desc(cont_per))

# race_condensed  cont no_cont cont_per no_cont_per total_race
# black           5685   15534     26.8        73.2      21219
# nam               53     154     25.6        74.4        207
# white           8543   26410     24.4        75.6      34953
# hisp            6572   21108     23.7        76.3      27680
# pi               160     556     22.3        77.7        716
# asian            580    2050     22.1        77.9       2630
# mixed             53     201     20.9        79.1        254
# me_sa            219     890     19.7        80.3       1109

###################################################################
# SYRs by discretion or non-discretion searches

# syr by race
syr_disc <- searches %>% 
  group_by(cont_none, discretionary) %>% 
  count(race_condensed)

# Spread
syr_disc <- syr_disc %>%
  spread(key = cont_none, value = n, fill = 0)

# Rearrange
syr_disc <- syr_disc %>%
  select(race_condensed, everything())

# Rename columns
## Unlike other 1/0 check columns, if cont_cont == 1, there was no contraband found
names(syr_disc) <- c("race_condensed", "disc", "cont", "no_cont")

# Spread again to pull out discretionary or not
syr_disc <- pivot_wider(data = syr_disc, 
                        id_cols = race_condensed, 
                        names_from = disc, 
                        values_from = c("cont", "no_cont"))

# Rename columns
names(syr_disc) <- c("race", "cont_non_disc", "cont_disc", "no_cont_non_disc", "no_cont_disc")

# Add percentages
syr_disc %>%
  mutate(per_of_cont_non_disc = round((cont_non_disc / (sum(cont_non_disc) + sum(no_cont_non_disc)))*100,1),
         per_of_cont_disc = round((cont_disc / (sum(cont_disc) + sum(no_cont_disc)))*100,1),
         per_of_no_cont_non_disc = round((no_cont_non_disc / (sum(no_cont_non_disc) + sum(cont_non_disc)))*100,1),
         per_of_no_cont_disc = round((no_cont_disc / (sum(no_cont_disc) + sum(cont_disc)))*100,1)) %>% 
  arrange(desc(per_of_cont_disc)) %>%
  select(-cont_non_disc, -cont_disc, -no_cont_non_disc, -no_cont_disc)

# race  per_of_cont_non_disc per_of_cont_disc per_of_no_cont_non_disc per_of_no_cont_disc
# white                  9.1             10.3                    32.9                25.8
# hisp                   7                7.9                    23                  24.7
# black                  5.8              7.2                    16.3                19  
# asian                  0.6              0.7                     2.4                 2.3
# me_sa                  0.3              0.2                     1.2                 0.7
# pi                     0.2              0.2                     0.7                 0.6
# mixed                  0.1              0.1                     0.3                 0.2
# nam                    0.1              0.1                     0.2                 0.2

# Add percentages by race, as opposed to by the total
syr_disc %>%
  mutate(cont_non_disc_per = round((cont_non_disc / (cont_non_disc + no_cont_non_disc))*100,1),
         cont_disc_per = round((cont_disc / (cont_disc + no_cont_disc))*100,1),
         no_cont_non_disc_per = round((no_cont_non_disc / (cont_non_disc + no_cont_non_disc))*100,1),
         no_cont_disc_per = round((no_cont_disc / (cont_disc + no_cont_disc))*100,1),
         total_race = cont_non_disc + cont_disc + no_cont_non_disc + no_cont_disc) %>% 
  arrange(desc(cont_disc_per)) %>%
  select(-cont_non_disc, -cont_disc, -no_cont_non_disc, -no_cont_disc)

# race  cont_non_disc_per cont_disc_per no_cont_non_disc_per no_cont_disc_per total_race
# white              21.6          28.6                 78.4             71.4      34953
# black              26.3          27.3                 73.7             72.7      21219
# pi                 20.3          25.3                 79.7             74.7        716
# hisp               23.3          24.2                 76.7             75.8      27680
# nam                26.8          23.8                 73.2             76.2        207
# asian              21.4          22.9                 78.6             77.1       2630
# mixed              19.9          22.7                 80.1             77.3        254
# me_sa              18.8          21.6                 81.2             78.4       1109

###################################################################
# SYRs by just discretion and contraband

# Create discretionary df
discretions <- master %>% 
  filter(discretionary == 1)

# race breakdown of people invovled in discretionary searches
discretions %>% 
  count(race_condensed) %>% 
  mutate((prop = n / sum(n))*100) %>% 
  arrange(desc(`(prop = n/sum(n)) * 100`))

# race_condensed   COUNT            PERCENT
#         white  14081              36.0819987
#           hisp 12747              32.6636771
#          black 10220              26.1883408
#          asian  1142               2.9263293
#          me_sa   366               0.9378603
#             pi   297               0.7610506
#          mixed    88               0.2254965
#            nam    84               0.2152466

# Calculate hit rate by races  
syr_disc2 <- discretions %>% 
  group_by(cont_none) %>% 
  count(race_condensed)

# Spread
syr_disc2 <- syr_disc2 %>%
  spread(key = cont_none, value = n, fill = 0)

# Rename columns
## Unlike other 1/0 check columns, if cont_cont == 1, there was no contraband found
names(syr_disc2) <- c("race_condensed", "cont", "no_cont")

# Add percentages
syr_disc2 %>%
  mutate(per_of_cont = round((cont / (sum(cont) + sum(no_cont)))*100,1),
         per_of_no_cont = round((no_cont / (sum(cont) + sum(no_cont)))*100,1)) %>% 
  arrange(desc(per_of_cont))

# race_condensed  cont no_cont per_of_cont per_of_no_cont
# white           4027   10054        10.3           25.8
# hisp            3091    9656         7.9           24.7
# black           2795    7425         7.2           19  
# asian            262     880         0.7            2.3
# me_sa             79     287         0.2            0.7
# pi                75     222         0.2            0.6
# mixed             20      68         0.1            0.2
# nam               20      64         0.1            0.2

# Add percentages by race, as opposed to by the total
syr_disc2 %>%
  mutate(cont_per = round((cont / (cont + no_cont))*100,1),
         no_cont_per = round((no_cont / (cont + no_cont))*100,1),
         total_race = cont + no_cont) %>% 
  arrange(desc(cont_per))

# race_condensed  cont no_cont cont_per no_cont_per total_race
# white           4027   10054     28.6        71.4      14081
# black           2795    7425     27.3        72.7      10220
# pi                75     222     25.3        74.7        297
# hisp            3091    9656     24.2        75.8      12747
# nam               20      64     23.8        76.2         84
# asian            262     880     22.9        77.1       1142
# mixed             20      68     22.7        77.3         88
# me_sa             79     287     21.6        78.4        366

###################################################################
# SYRs by consent and contraband

# Create consent df
## Filtering to searches where consent is the only reason
consent <- master %>% 
  filter(search_basis == "Consent given")

# Race breakdown of people invovled in consent searches
consent %>% 
  count(race_condensed) %>% 
  mutate((prop = n / sum(n))*100) %>% 
  arrange(desc(`(prop = n/sum(n)) * 100`))

# race_condensed    n (prop = n/sum(n)) * 100
#           hisp 2197              41.1577370
#          white 1713              32.0906707
#          black 1103              20.6631697
#          asian  190               3.5593855
#          me_sa   66               1.2364181
#             pi   38               0.7118771
#          mixed   17               0.3184713
#            nam   14               0.2622705

# Calculate syr among consent searches
syr_consent <- consent %>% 
  group_by(cont_none) %>% 
  count(race_condensed)

# Spread
syr_consent <- syr_consent %>%
  spread(key = cont_none, value = n, fill = 0)

# Rename columns
## Unlike other 1/0 check columns, if cont_cont == 1, there was no contraband found
names(syr_consent) <- c("race_condensed", "cont", "no_cont")

# Add percentages
syr_consent %>%
  mutate(per_of_cont = round((cont / (sum(cont) + sum(no_cont)))*100,1),
         per_of_no_cont = round((no_cont / (sum(cont) + sum(no_cont)))*100,1)) %>% 
  arrange(desc(per_of_cont))

# race_condensed  cont no_cont per_of_cont per_of_no_cont
# hisp             368    1829         6.9           34.3
# white            331    1382         6.2           25.9
# black            197     906         3.7           17  
# asian             44     146         0.8            2.7
# me_sa             12      54         0.2            1  
# pi                 9      29         0.2            0.5
# mixed              2      15         0              0.3
# nam                2      12         0              0.2

# Add percentages by race, as opposed to by the total
syr_consent %>%
  mutate(cont_per = round((cont / (cont + no_cont))*100,1),
         no_cont_per = round((no_cont / (cont + no_cont))*100,1),
         total_race = cont + no_cont) %>% 
  arrange(desc(cont_per))

# race_condensed  cont no_cont cont_per no_cont_per total_race
# pi                 9      29     23.7        76.3         38
# asian             44     146     23.2        76.8        190
# white            331    1382     19.3        80.7       1713
# me_sa             12      54     18.2        81.8         66
# black            197     906     17.9        82.1       1103
# hisp             368    1829     16.8        83.2       2197
# nam                2      12     14.3        85.7         14
# mixed              2      15     11.8        88.2         17

###################################################################
# Actions taken by race
# Calculate proportion of race by force being used
force_race <- master %>% 
  group_by(act_force) %>% 
  count(race_condensed)

# Spread
force_race <- force_race %>%
  spread(key = act_force, value = n, fill = 0)

# Rename columns
names(force_race) <- c("race_condensed", "no_force", "force")

# Add percentages
force_race %>%
  mutate(per_of_no_force = round((no_force / sum(no_force))*100,1),
         per_of_force = round((force / sum(force))*100,1)) %>% 
  arrange(desc(per_of_force))

# race_condensed no_force force per_of_no_force per_of_force
# white            177220  2897            42.3         34.7
# hisp             122794  2835            29.3         33.9
# black             82555  2050            19.7         24.5
# asian             19938   321             4.8          3.8
# me_sa             10984   136             2.6          1.6
# pi                 3246    66             0.8          0.8
# mixed              1390    33             0.3          0.4
# nam                 849    21             0.2          0.3
# TOTAL            418976  8359

# Add percentages by race, as opposed to by the total
force_race %>%
  mutate(no_force_per = round((no_force / (no_force + force))*100,1),
         force_per = round((force / (no_force + force))*100,1),
         total_race = no_force + force) %>% 
  arrange(desc(force_per))

# race_condensed no_force force no_force_per force_per total_race
# black             82555  2050         97.6       2.4      84605
# nam                 849    21         97.6       2.4        870
# hisp             122794  2835         97.7       2.3     125629
# mixed              1390    33         97.7       2.3       1423
# pi                 3246    66         98         2         3312
# asian             19938   321         98.4       1.6      20259
# white            177220  2897         98.4       1.6     180117
# me_sa             10984   136         98.8       1.2      11120

# GLM TESTS
# If black
glmTest4 <- glm(act_force ~ black,
                data = master,
                family = "binomial")
summary(glmTest4)
exp(coef(glmTest4))

# (Intercept)       black 
# 0.01875329  1.32413736
## Someone is 1.3 times more likely to have officers use force if they are black

# If hisp
glmTest5 <- glm(act_force ~ hisp,
                data = master,
                family = "binomial")
summary(glmTest5)
exp(coef(glmTest5))

# (Intercept)        hisp 
# 0.01865069  1.23788670 
## Someone is 1.2 times more likely to have officers use force if they are hisp

###################################################################
# Officer drawing weapon by race

# How many individuals had firearms discharged at them
master$is_fired <- ifelse(grepl("act_fad", master$act_simp), 1, 0)
sum(master$is_fired)
# Officers fired at 6 people

# Race of those fired at
master %>% 
  filter(is_fired == 1) %>%
  group_by(is_fired) %>% 
  count(race_condensed) %>% 
  arrange(desc(n))

# race_condensed     n
# hisp               3
# black              2
# white              1

# Create tag for all types of "forceful actions"
## master$is_fired already created above
# master$is_fired <- ifelse(grepl("act_fad", master$act_simp), 1, 0)
master$is_pointed <- ifelse(grepl("act_fp", master$act_simp), 1, 0)
master$is_chem <- ifelse(grepl("act_chem", master$act_simp), 1, 0)
master$is_baton <- ifelse(grepl("act_baton", master$act_simp), 1, 0)
master$is_k9 <- ifelse(grepl("act_k9_bit", master$act_simp), 1, 0)
master$is_ip <- ifelse(grepl("act_ip", master$act_simp), 1, 0)
master$is_elect <- ifelse(grepl("act_elect", master$act_simp), 1, 0)
master$is_rem <- ifelse(grepl("act_rem_cont", master$act_simp), 1, 0)
master$is_physical <- ifelse(grepl("act_physical", master$act_simp), 1, 0)

# Total forceful actions
sum(master$is_pointed) + sum(master$is_chem) + sum(master$is_baton) + sum(master$is_k9) + sum(master$is_ip) + sum(master$is_elect) + sum(master$is_fired) + sum(master$is_rem) + sum(master$is_physical)
# Forceful tactics were applied 9,002 times on 8,359 people
sum(master$is_pointed)
# police pointed firearms at 1,722 people
sum(master$act_force)
# Forceful tactics were used on 8,359 people

# Calculate proportion of race by weapons pointed
pointed_race <- master %>% 
  group_by(is_pointed) %>% 
  count(race_condensed)

# Spread
pointed_race <- pointed_race %>%
  spread(key = is_pointed, value = n, fill = 0)

# Rename columns
names(pointed_race) <- c("race_condensed", "no_point", "point")

# Add percentages
pointed_race %>%
  mutate(per_of_no_point = round((no_point / sum(no_point))*100,1),
         per_of_point = round((point / sum(point))*100,1)) %>% 
  arrange(desc(per_of_point))

# race_condensed no_point point per_of_no_point per_of_point
# hisp             124918   711            29.4         41.3
# white            179649   468            42.2         27.2
# black             84159   446            19.8         25.9
# asian             20206    53             4.7          3.1
# me_sa             11097    23             2.6          1.3
# pi                 3300    12             0.8          0.7
# mixed              1418     5             0.3          0.3
# nam                 866     4             0.2          0.2
# TOTAL            425613  1722

# Add percentages by race, as opposed to by the total
pointed_race %>%
  mutate(no_point_per = round((no_point / (no_point + point))*100,1),
         point_per = round((point / (no_point + point))*100,1),
         total_race = no_point + point) %>% 
  arrange(desc(point_per))

# race_condensed no_point point no_point_per point_per total_race
# hisp             124918   711         99.4       0.6     125629
# black             84159   446         99.5       0.5      84605
# nam                 866     4         99.5       0.5        870
# mixed              1418     5         99.6       0.4       1423
# pi                 3300    12         99.6       0.4       3312
# asian             20206    53         99.7       0.3      20259
# white            179649   468         99.7       0.3     180117
# me_sa             11097    23         99.8       0.2      11120

# GLM TESTS
# If hispanic
glmTest6 <- glm(is_pointed ~ hisp,
                data = master,
                family = "binomial")
summary(glmTest6)
exp(coef(glmTest6))

# (Intercept)        hisp 
# 0.003362211 1.692854487
## Someone is 1.7 times more likely to have police point firearms if they are Hispanic

# If black
glmTest7 <- glm(is_pointed ~ black,
                data = master,
                family = "binomial")
summary(glmTest7)
exp(coef(glmTest7))

# (Intercept)       black 
# 0.00373696  1.41812928
## Someone is 1.4 times more likely to have officers point firearms if they are black

###################################################################
# OTHER ACTIONS ANALYSIS

# Calculate proportion of race by detained
detained <- master %>% 
  group_by(act_detained) %>% 
  count(race_condensed)

# Spread
detained <- detained %>%
  spread(key = act_detained, value = n, fill = 0)

# Rename columns
names(detained) <- c("race_condensed", "no_det", "det")

# Add percentages
detained %>%
  mutate(per_of_no_det = round((no_det / sum(no_det))*100,1),
         per_of_det = round((det / sum(det))*100,1)) %>% 
  arrange(desc(per_of_det))

# race_condensed no_det   det per_of_no_det per_of_det
# white          120257 59860          43         40.6
# hisp            81047 44582          29         30.2
# black           50024 34581          17.9       23.5
# asian           15780  4479           5.6        3  
# me_sa            9161  1959           3.3        1.3
# pi               2198  1114           0.8        0.8
# mixed             977   446           0.3        0.3
# nam               510   360           0.2        0.2

# Add percentages by race, as opposed to by the total
detained %>%
  mutate(no_det_per = round((no_det / (no_det + det))*100,1),
         det_per = round((det / (no_det + det))*100,1),
         total_race = no_det + det) %>% 
  arrange(desc(det_per))

# race_condensed no_det   det no_det_per det_per total_race
# nam               510   360       58.6    41.4        870
# black           50024 34581       59.1    40.9      84605
# hisp            81047 44582       64.5    35.5     125629
# pi               2198  1114       66.4    33.6       3312
# white          120257 59860       66.8    33.2     180117
# mixed             977   446       68.7    31.3       1423
# asian           15780  4479       77.9    22.1      20259
# me_sa            9161  1959       82.4    17.6      11120

# If black
glmTest8 <- glm(act_detained ~ black,
                data = master,
                family = "binomial")
summary(glmTest8)
exp(coef(glmTest8))

# (Intercept)       black 
# 0.4905841   1.4091125
## Someone is 1.4 times more likely to be detained if they are black

# If nam
glmTest9 <- glm(act_detained ~ nam,
                data = master,
                family = "binomial")
summary(glmTest9)
exp(coef(glmTest9))

# (Intercept)         nam 
# 0.5261197   1.3416763
## Someone is 1.3 times more likely to be detained if they are native american

