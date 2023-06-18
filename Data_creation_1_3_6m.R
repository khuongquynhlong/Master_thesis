library(tidyverse)
library(magrittr)

options(scipen = 999)


df <- readRDS("statin_final_gop_the.rds")

dim(df)
names(df)

head(df)

#---------- Apply washout periods (Change tr_ep_day )
#-------------------------------------------------------------------------------

# Create treatment episode to determine time to the nearest statin treatment
df <- df %>% group_by(ma_the) %>%
  mutate(tr_episode = cumsum(statin_yn),
         date_in_lag1 = lag(date_in, n = 1))

df %<>% mutate(date_fr_pre = as.numeric(date_in - date_in_lag1))

# First day of 1st episode equal to 0
df$date_fr_pre[is.na(df$date_fr_pre)] <- 0

# Restart the 1st day of next episode
df$date_fr_pre[df$statin_yn == 1] <- 0

# Create number of days for each treatment episode (from the 1st date of this episode)
df %<>% group_by(ma_the, tr_episode) %>%
  mutate(tr_ep_day = cumsum(date_fr_pre)) %>% ungroup()

# ----- Had previously documented stroke (ischemic or hemorrhagic) or TIA
df %<>% mutate(his_stroke = ifelse(xhn == 1 | tia ==1 | dotquy_tm == 1, 1, 0))

# create variable to show cumulative number of stroke 
df %<>% group_by(ma_the) %>% 
  mutate(his_stroke_cum = cumsum(his_stroke)) %>% 
  ungroup()

# Create date first achieve 3 months of no using statin and has history of stroke
# Change tr_ep_day according to washout period (e.g., 30, 90, 180)

wash_df <- df %>% filter(tr_ep_day > 90 & his_stroke_cum > 0) %>%
  group_by(ma_the) %>%
  summarise(date_wash = min(date_in)) %>% ungroup()


# Merge with full dataset 
df %<>% left_join(wash_df, by = "ma_the")

rm(wash_df)

# Create variable for eligibility regarding washout period and history of stroke
df %<>% mutate(include = ifelse(date_in >= date_wash, 1, 0))

dim(df)
df$ma_the %>% unique() %>% length()


#----- Select who eligible for inclusion criteria

df %<>% filter(include == 1)

dim(df)
df$ma_the %>% unique() %>% length()

#---------- Exclusion criteria
#-------------------------------------------------------------------------------
# •	History of coronary heart diseases
# •	Peripheral vascular disease (PVD)
# •	Atrial fibrillation
# •	Prosthetic heart valves
# •	Sinus node dysfunction
# •	Severe renal dysfunction
# •	Active liver disease or hepatic dysfunction
# •	Taking anticoagulants drugs

df %<>% rowwise() %>% 
  mutate(exc_num = sum(machvanh, mmnb, rungnhi, vantim, nutxoang, than_roiloan,
                       gan_roiloan, viemgan, dongmau, na.rm = T))
df %<>% filter(exc_num == 0)


dim(df)
df$ma_the %>% unique() %>% length()

#----- Exclude those have stroke at baseline (i.e., had history of stroke but need to be recovered)
df %<>% mutate(his_stroke_rev = 1 - his_stroke) %>%
  group_by(ma_the) %>%
  mutate(his_stroke_rev_cum = cumsum(his_stroke_rev)) %>% ungroup()

df %<>% filter(his_stroke_rev_cum > 0)

dim(df)
df$ma_the %>% unique() %>% length()

#----- Exclude visits with more than 1 events for ICH
# Exclude those with more than 1 events
df %<>% group_by(ma_the) %>%
  mutate(xhn_cum = cumsum(xhn)) %>% 
  filter(xhn_cum <= 1) %>% 
  ungroup()

# Take the 1st date of experience event
df %<>% group_by(ma_the) %>%
  mutate(xhn_cum2 = cumsum(xhn_cum)) %>%
  filter(xhn_cum2 <= 1) %>% 
  ungroup()

dim(df)
df$ma_the %>% unique() %>% length()

#---------- Create variables
#===============================================================================
#----- ID
df %<>% group_by(ma_the) %>% mutate(id = cur_group_id()) %>% ungroup()

df$cons <- 1

#----- Visit and visit time
df %<>% group_by(id) %>%
  mutate(visit_n = cumsum(cons),
         date_b = min(date_in),
         visit_day = as.numeric(date_in - date_b),
         visit_month = visit_day/30.5) %>% ungroup()

#----- Rename variables

df %<>% rename(
  age = tuoi_chuan,
  sex = gioi_tinh,
  anticoagulant = dongmau,
  hyper_med = ha_thuoc,
  ICH = xhn,
  TIA = tia,
  hepatitis = viemgan,
  diabetes = dtd,
  obesity = beophi,
  chro_pulmonary = phoiman,
  heart_failure = suytim,
  hypertension = tha,
  sinus_node = nutxoang,
  cancer = ungthu,
  hypothyroidism = suygiap,
  stroke_ische = dotquy_tm,
  CHD = machvanh,
  COPD = copd,
  heart_valve = vantim,
  hepatic_dys = gan_roiloan,
  osteoporosis = loangxuong,
  other_heart = benhtim,
  depression = tramcam,
  PVD = mmnb,
  atrial_fibri = rungnhi,
  renal_dys= than_roiloan,
  aneurysmis = phinhdm
)

df %<>% select(ma_the, id, visit_n, date_in, date_b, visit_day, visit_month,
               age, sex, atorvastatin_sl, fluvastatin_sl, lovastatin_sl, pravastatin_sl,
               rosuvastatin_sl, simvastatin_sl, statin_yn, ICH, TIA, stroke_ische,
               CHD, heart_failure, PVD, atrial_fibri, heart_valve, sinus_node, 
               renal_dys, anticoagulant, hepatic_dys, hepatitis, hypertension,
               diabetes, obesity, other_heart, chro_pulmonary, COPD, cancer,
               hypothyroidism, osteoporosis, depression, aneurysmis, hyper_med, 
               corticoids, nsaids, aspirin, insulin, antidepress)

df %<>% mutate(
  hyper = ifelse(hypertension == 1| hyper_med == 1, 1, 0),
  depress = ifelse(depression == 1| antidepress == 1, 1, 0)
)


#----------- Create baseline variables
df2 <- df %>%
  select(id, date_in, statin_yn, ICH, TIA, stroke_ische, age,
         CHD, heart_failure, PVD, atrial_fibri, heart_valve, sinus_node, 
         renal_dys, anticoagulant, hepatic_dys, hepatitis, hypertension,
         diabetes, obesity, other_heart, chro_pulmonary, COPD, cancer,
         hypothyroidism, osteoporosis, depression, aneurysmis, hyper_med, 
         corticoids, nsaids, aspirin, insulin, antidepress, hyper, depress) %>%
  group_by(id) %>%
  filter(date_in == min(date_in)) %>% ungroup() %>%
  select(-date_in)



names(df2)[-1] <- paste0(names(df2)[-1], "_b")

df %<>% left_join(df2, by = "id")

# Save intermediate file
saveRDS(df, "statin_clean_3m.rds")
# saveRDS(df, "statin_clean_1m.rds")
# saveRDS(df, "statin_clean_6m.rds")


