# Mobility_EWZ_Energie

# Import libraries
library(dplyr) # Version = 0.8.3


# Number formatting
options(scipen = 1000000)
options(digits = 6)

# get and transform data function
getData <- function(url_dat)
{ 
    #library(readxl)
    # https://stackoverflow.com/questions/53635818/convert-datetime-from-excel-to-r/53648681
    # import dat
    # path <- "P:/KK_Temp/EnergieUebersichtCH-2020.xlsx" #%
    # 
    # sheets_name <- excel_sheets(path)
    # dat <- read_excel(path, sheet = "Zeitreihen0h15", col_types = "guess")
    # 
    # dat2 <- dat %>% 
    #   select(date = "...1", 
    #          value = "Summe endverbrauchte Energie Regelblock Schweiz\nTotal energy consumed by end users in the Swiss controlblock") %>%
    #   slice(-1) 
    #  
    # View(dat2)
    
    # copy paste to xlsx to csv as read_excel does not work without much wrangling. 
    # resulting csv : dat.csv
    dat <- read.csv("./dat.csv", header=T, sep="\t", stringsAsFactors=FALSE, encoding="UTF-8")
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
            'value' := .data$value/1000000,
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
      arrange(date)
    
    # return
    return(dat_prep)
}

# main
dat_prep <- getData()
write.table(dat_prep, "./Social_SwissgridEnergie.csv", sep=",", fileEncoding="UTF-8", row.names = F)
