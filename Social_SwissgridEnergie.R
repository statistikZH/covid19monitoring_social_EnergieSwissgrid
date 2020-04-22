# Mobility_EWZ_Energie

# Import libraries
library(dplyr) # Version = 0.8.3


# Number formatting
options(scipen = 1000000)
options(digits = 6)

# copy/paste xlsx zu csv: 
# 1. Navigiere zu Tab: Zeitreihen0h15
# 2. Kopiere ganze 
# - **Spalte A** "Zeitstempfel 
# - **Spalte B** "Summe endverbrauchte Energie Regelblock Schweiz\nTotal energy consumed by end users in the Swiss controlblock"
# 3. lösche die ersten zwei Zeilen im csv
# 4. Lösche trailing blank line
# => Resultierender Datensatz: dat_2019.csv & dat_2020.csv
  
dat_2019 <- read.csv("./dat_2019.csv", header=T, sep="\t", stringsAsFactors=FALSE, encoding="UTF-8")
dat_2020 <- read.csv("./dat_2020.csv", header=T, sep="\t", stringsAsFactors=FALSE, encoding="UTF-8")

#check
sum(dat_2019$kWh) # 55987487561 => stimmt mit xlsx überein
sum(dat_2020$kWh) # 15081893217 => stimmt mit xlsx überein

# rbind
dat <- rbind(dat_2019, dat_2020)
names(dat) <- c("date", "value")
    
#prepare dat
dat_prep <- dat %>%
  mutate(
    date_h= as.POSIXct(date, format="%d.%m.%Y %H:%M"),
    date_day= format(date_h, format="%d.%m.%Y")
    ) %>% 
  group_by(date_day) %>%
  summarise(value=sum(as.numeric(gsub(",", ".", value)))) %>% 
  transmute(
        'date' := as.POSIXct(.data$date_day, format="%d.%m.%Y"),
        'value' :=  .data$value, #.data$value/1000000, #check
        'topic' := "Soziales",
        'variable_short' := "sum_endverbrauchteenergie_ch",
        'variable_long' := "Summe endverbrauchte Energie Regelblock Schweiz",
        'location' := "CH",
        'unit' := "GWh",
        'source' := "Swissgrid",
        'update' := "t\u00e4glich",
        'public' := "ja",
        'description' := "?"
    ) %>%
  filter(!(is.na(value))) %>%
  filter(date != "2019-01-01") %>% 
  filter(date != "2020-04-01") %>% 
  arrange(date)
  
write.table(dat_prep, "./Social_SwissgridEnergie.csv", sep=",", fileEncoding="UTF-8", row.names = F)
