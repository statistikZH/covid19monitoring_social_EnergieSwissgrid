# Daten werden rückwirkend korrigiert. Hier wird das Ausmass der Korrekturen geprüft. 

# import dat 
dat_gitrepos <- read.csv("./Social_SwissgridEnergie.csv", header=T, sep=",", stringsAsFactors=FALSE, encoding="UTF-8")
dat_backup <- read.csv(url("https://raw.githubusercontent.com/statistikZH/covid19monitoring_social_EnergieSwissgrid/master/Social_SwissgridEnergie.csv"), header=T, sep=",", stringsAsFactors=FALSE, encoding="UTF-8")

# cbinda
dat <- dat_gitrepos %>%
  filter(date %in% dat_backup$date) %>% 
  bind_cols(dat_backup)  %>%
  select(date, date_old = date1, value, value_old = value1) %>%
  mutate(
    diff_abs = value-value_old,
    diff_perc = value/value_old*100
    ) 

# check
View(dat %>% arrange(diff_abs))
View(dat %>% arrange(desc(diff_abs)))
max(dat$diff_perc)
min(dat$diff_perc)