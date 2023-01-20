
# run everytime -----------------------------------------------------------
setwd("C:/Users/kzyatitsky/R_work/Raw_data")
.libPaths("C:/Users/kzyatitsky/R/win-library/4.1")

library(tidyverse)
library(lubridate)
library(plotly)
library(tidytable)
library(clock)
library(zoo)
library(runner)
library(data.table)
library(plotly)
library(stringr)
# bby data ----------------------------------------------------------------


dat <- data.frame(Digits_Lower = 1:5,
                  Digits_Upper = 6:10,
                  random = 20:24)
dat

cb <- data.frame(Digits = c("Digits_Lower", "Digits_Upper"),
                 x = 1:2, 
                 y = 3:4)


# for loop ----------------------------------------------------------------

dat.loop <- dat
#try to add new columns with paste0
for(i in seq_len(nrow(cb)))
{
  dat.loop[paste0("disp", sep = '.', cb$Digits[i])] <- 
    (dat.loop[, cb$Digits[i]]- cb$y[i]) * cb$x[i]
}


# edit question -----------------------------------------------------------

dat <- data.frame(Digits_Lower = 1:5,
                  Digits_Upper = 6:10,
                  Digits_CTRL = 11:15,
                  Digits_50 = 16:20,
                  random = 20:24)
dat

cb <- data.frame(Digits = c("Digits_Lower", "Digits_Upper", "Digits_CTRL", "Digits_50"),
                 x = c(1,2,5,7), 
                 y = c(3,4,6, 8))
cb


results2 <- dat %>% 
  crossing(cb) %>% 
  rowwise() %>%
  mutate(disp = (get(`Digits`)-y) *x ) %>%
  ungroup() %>% 
  pivot_wider(names_from = Digits,
              values_from = disp,
              names_prefix = "disp_")



results3 <- results2 %>% 
  group_by(random) %>% 
  fill(starts_with("disp"), .direction = c("downup")) %>% 
  ungroup() %>% 
  select(-c(x,y)) %>% 
  unique()

results3 %>% 
  deparse() %>% 
  clipr:: write_clip()
reprex()


# try with tidytable ------------------------------------------------------

results2 <- dat %>% 
  crossing.(cb) %>% 
  mutate_rowwise.(disp = (get(`Digits`)-y) *x ) %>%
  pivot_wider.(names_from = Digits,
              values_from = disp,
              names_prefix = "disp_")
#can probably adjust order if get picky


results3 <- results2 %>% 
  #a little worried fill won't work with more than 3, i guess could test it out
  #maybe use .replace_NA
  fill.(starts_with("disp"), .direction = c("downup"), .by = 'random') %>% 
  select.(-c(x,y)) %>% 
  distinct.()

results3 %>% 
  deparse() %>% 
  clipr:: write_clip()
reprex()

# try if fill doesn't work out for large dataset --------------------------

  
results3 <- results2 %>% 
  mutate(disp_Digits_Lower = replace(is.na(disp_Digits_Lower), duplicated(Digits_Lower) | 
                                       duplicated(Digits_Lower, fromLast = TRUE), NA))
#potentially if need new solution 
# mutate(across(starts_with("disp"), ~case_when(
# 
# match(duplicated(Digits_Lower))
  


# now see if works on larger dataset --------------------------------------


# files -------------------------------------------------------------------

data.dat <- read.table("eng_data_20200128_0945.dat", header = T, sep = ',', skip = 1,
                       na.strings = 'NAN')

calibration <- read.csv('Calibration_values_Zion.csv', header = T, sep = ',')

#test 15 to 10 min 
data.dat <- read.table("CR6_ZIONPC_cell_eng_data_20211130.csv", header = T, sep = ',', skip = 1,
                       na.strings = 'NAN')

data.dat <- read.table("eng_data_20200427_1330.dat", header = T, sep = ',', skip = 1,
                       na.strings = 'NAN')


#comment out after, for nahuku
# setwd("C:/Users/kzyatitsky/R_work/Raw_data/HAVO_Nahuku")
# data.dat <- read.table("eng_data_20211105_0900.csv", header = T, sep = ',', skip = 1,
#                        na.strings = 'NAN' )


# data wrangling ----------------------------------------------------------

calibration2 <- calibration %>% 
  #delete first three rows calibration file which are Site_name, Serial_Number, Model 
  select(-c(1:3)) %>% 
  mutate(base.value = (Install_Zero - Cal_Zero) * 
           L_gauge_factor + (First_Temp - Temp) * 
           (((Install_Zero * TempCor_M) + 
               (TempCor_B)) * L_gauge_factor)) %>% 
  mutate(poly.value = -((Poly_GF_A*(Install_Zero^2))+(Poly_GF_B*Install_Zero)))

df.dat <- data.dat[-c(1:2),]
df.date <- df.dat %>% 
  # select(-c(1:2)) %>% 
  mutate(
    TIMESTAMP = parse_date_time(TIMESTAMP, orders = c("mdy HM", "ymd HMS"),
                           #double check this is right date format 
      tz = "US/Mountain"), #later put input$time 
    datetime_utc = date_set_zone(TIMESTAMP, "UTC"), 
    # date_time = as_naive_time(TIMESTAMP)
  ) %>% 
  select(datetime_utc, everything())
 
# df.dat$TIMESTAMP <- as.POSIXct(df.dat$TIMESTAMP)
# df.dat$TIMESTAMP <- as_naive_time(df.dat$TIMESTAMP)


# test out code -----------------------------------------------------------


df.24 <- df.date %>% 
  # arrange(date_time, .by_group = T) %>% 
  arrange(TIMESTAMP) %>% 
  mutate(across(where(is.character), as.numeric)) 
 
 #merge datasets by extending rows of df.dat by rows of calibration 2
results2 <- df.24 %>% 
  crossing.(calibration2) %>% 
  mutate_rowwise.(disp = (get(`Digits`)- Cal_Zero) * L_gauge_factor 
    + (get(`TT`) - Temp) * (((get(`Digits`) * TempCor_M) + 
                               (TempCor_B)) * L_gauge_factor)) %>% 
  mutate_rowwise.(poly = (Poly_GF_A * (get(`Digits`)^2)) +
                    (Poly_GF_B * get(`Digits`)) + poly.value) %>%
  mutate.(norm = disp - base.value) %>% 
  
  # check out runner --------------------------------------------------------

  # mutate.(ra.RUN = mean_run(x = norm, k = 24 *3600, idx = TIMESTAMP, na_pad = F),
  #       .by = "Disp") %>%   
  mutate.(ra = rollapply(norm, 1:n() - findInterval(TIMESTAMP - 24 * 3600, 
                                                      TIMESTAMP), mean, na.rm = T, align = 'right', partial=TRUE, fill = 0),
          .by = "Disp") %>% 
  mutate.(ra_poly = rollapply(poly, 1:n() - findInterval(TIMESTAMP - 24 * 3600,
                                                    TIMESTAMP), mean, na.rm = T, align = 'right', partial=TRUE, fill = 0),
          .by = "Disp") %>%
  pivot_wider.(names_from = c(Digits),
              values_from = c(disp, norm, poly, ra, ra_poly)) #, ra.RUN, ra.poly
              
calibration3 <- calibration2[ , !(names(calibration2) %in% "Digits")]

#clean data
results3 <- results2 %>% 
  fill.(starts_with("disp") | starts_with("norm") | starts_with("poly") | starts_with("ra") , .direction = c("downup"), .by = "TIMESTAMP") %>% # | starts_with("ra.RUN")
  select.(!(names(calibration3))) %>% 
  distinct.() %>% 
  mutate.(T_ra = rollapply(across.(ends_with("_C")), 1:n() - findInterval(TIMESTAMP - 24 * 3600, 
                                                          TIMESTAMP), 
                         mean, na.rm = T, align = 'right', partial=TRUE, fill = 0)) %>% 
  rename_at(vars(matches("^ra")), ~ str_remove(., "Digits_"))
  
  


#POSIX is in seconds so this is why this works   
# results3$TIMESTAMP - 24 * 3600
findInterval(results3$TIMESTAMP - 24 * 3600, 
         results3$TIMESTAMP)


# now graph ---------------------------------------------------------------

#reshape to long format 
longDF <- results3 %>% 
  pivot_longer.(starts_with("ra"), names_to = 'variables', values_to = "values") %>% 
  select.(c("TIMESTAMP", "variables", "values", "T_ra")) %>% 
  #need to add in NAs so don't have 6 traces
  mutate.(T_ra = ifelse(duplicated(T_ra), NA, T_ra))


plot_so2 <- plot_ly(data = longDF, width = 1000, height = 550,
                    x = ~TIMESTAMP, y = ~values,
                    color = ~variables,
                    type = "scatter", mode = "lines+markers") %>%
  add_trace(y = ~T_ra,
            name = "Temperature",
            line = list(color = "#00AFBB"),
            showlegend = TRUE,
            yaxis = "y2") %>%
  layout(title = list(text = 'My plot title', y = 0.95),
         xaxis = list(title = 'Time'),
         yaxis = list(title = 'Displacement (mm)'),
         legend = list(orientation = 'v', x = 1.05),
         yaxis2 = list(overlaying = "y",
                       side = "right", title = "Temperature (deg C)",
                       range = range(na.omit(longDF$T_ra))))
plot_so2
  