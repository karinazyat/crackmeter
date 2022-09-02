# install.packages("tidyverse")
# install.packages('leaflet')
#install.packages('zoo')
# install.packages('reshape2')
# install.packages('Rtools')
#install.packages("DT")
# install.packages('foreach')


library(tidyverse)
library(lubridate)
library(scales)
library(zoo)
library(reshape2)
library(plotly)
library(foreach)
library(magrittr)
#library(leaflet)

setwd("C:/Users/kzyatitsky/R_work/Raw_data")
.libPaths("C:/Users/kzyatitsky/R/win-library/4.1")
#.libPaths("C:/Users/kzyatitsky/R/win-library/4.1")

#got rid of rows
# df1 <- read_csv("eng_data_20200401_1515.csv")

#keep first few rows, set second row as header
# df.rows <- read_csv("eng_data_20200427_1330.csv", skip = 1)


#remove first 2 rows (this will only work if same pattern for every one)
# df.remove_rows <- df.rows[-c(1:2),]

#read in .dat, skip first row and set second row as header, separate by commas
data.dat <- read.table("eng_data_20200128_0945.dat", header = T, sep = ',', skip = 1,
                       na.strings = 'NAN')
data.1107 <- read.csv("eng_data_20191107_1945.dat", header = T, sep = ',', skip = 1,
                      na.strings = 'NAN')
data.1108 <- read.csv("eng_data_20191108_2030.dat", header = T, sep = ',', skip = 1,
                      na.strings = 'NAN')
data.1110 <- read.csv("eng_data_20191110_1700.dat", header = T, sep = ',', skip = 1,
                      na.strings = 'NAN')
data.1122 <- read.csv("eng_data_20191122_1200.dat", header = T, sep = ',', skip = 1,
                      na.strings = 'NAN')
data.1212 <- read.csv("eng_data_20191212_1945.dat", header = T, sep = ',', skip = 1,
                      na.strings = 'NAN')
data.1208 <- read.csv("eng_data_20191208_1000.dat", header = T, sep = ',', skip = 1,
                      na.strings = 'NAN')
data.1219 <- read.csv("eng_data_20191219_1200.dat", header = T, sep = ',', skip = 1,
                      na.strings = 'NAN')
data.0106 <- read.csv("eng_data_20200106_1530.dat", header = T, sep = ',', skip = 1,
                      na.strings = 'NAN')
data.0401 <- read.csv("eng_data_20200401_1515.dat", header = T, sep = ',',
                      na.strings = 'NAN')
data.0427 <- read.csv("eng_data_20200427_1330.dat", header = T, sep = ',', skip = 1,
                      na.strings = 'NAN')
data.1107_1715 <- read.csv("eng_data_20191107_1715.dat", header = T, sep = ',', skip = 1,
                           na.strings = 'NAN')
data.all <- read.csv("CR6_ZIONPC_cell_eng_data_20211130.dat", header = T, sep = ',', skip = 1,
                     blank.lines.skip = T)
data.test <- read.csv("checkR.csv")
data.check <- read.csv("CR6_ZIONPC_cell_eng_data_20211130.csv", header = T, sep = ',', skip = 1)
data.check2 <- read.csv('data_check.csv', header = T, sep = ',', skip = 1)
calibration <- read.csv('Calibration_values_Zion.csv', header = T, sep = ',')

data.thurston <- read.csv("eng_data_20191216_1010.dat", header = T, sep = ',', skip = 1)
data.thurston_check <- read.csv('eng_data_20200427_0940.csv', header = T, sep = ',', skip = 1)
test.thurston <- read.csv('Thurston_VW_data_20220601.csv', header = T, sep = ',', skip = 1)


# calibration  ------------------------------------------------------------

calibration2 <- calibration %>% 
  mutate(base.value = (Install_Zero - Cal_Zero) * 
           L_gauge_factor + (First_Temp - Temp) * 
           (((Install_Zero * TempCor_M) + 
               (TempCor_B)) * L_gauge_factor)) %>% 
  mutate(poly.value = -((Poly_GF_A*(Install_Zero^2))+(Poly_GF_B*Install_Zero)))


  mutate(raT.24 = rollapply(T109_C, 96, mean, na.rm = T, align = 'right', partial=TRUE, fill = NA)) %>%
 
  mutate(disp.lower = (Digits_Lower -Cal_Zero) * L_gauge_factor + 
           (TT_Lower - Temp) *
           (((Digits_Lower * TempCor_M) + (-TempCor_B)) * L_gauge_factor)) %>%
    
  mutate(norm.dispL = disp.lower- lower) %>%
  mutate(raL.24 = rollapply(norm.dispL, 96, mean, na.rm = T,align = 'right', partial=TRUE, fill = NA)) %>%
  mutate(disp.upper = (Digits_Upper -2584) * 0.02828 + (TT_Upper - 22.8) *
           (((Digits_Upper * 0.000384) + (-0.3482)) * 0.02828)) %>%
  mutate(norm.dispU = disp.upper- upper) %>%
  mutate(raU.24 = rollapply(norm.dispU, 96, mean, na.rm = T, align = 'right', partial=TRUE, fill = NA)) %>%
  mutate(disp.control = (Digits_CTRL -2520) * 0.02869 + (TT_Upper - 22.8) *
           (((Digits_CTRL * 0.000384) + (-0.3482)) * 0.02869)) %>%
  mutate(norm.dispC = disp.control- control) %>%
  mutate(raC.24 = rollapply(norm.dispC, 96, mean, na.rm = T, align = 'right', partial=TRUE, fill = NA)) %>% 
  mutate(poly.noTL = (0.000000077403*(Digits_Lower^2))+(0.02746*Digits_Lower)+poly.lower) %>% 
  mutate(raL.noT = rollapply(poly.noTL, 96, mean, na.rm = T,align = 'right', partial=TRUE, fill = NA)) %>%
  mutate(poly.noTU = (0.000000088762*(Digits_Upper^2))+(0.02727*Digits_Upper)+poly.upper) %>% 
  mutate(raU.noT = rollapply(poly.noTU, 96, mean, na.rm = T,align = 'right', partial=TRUE, fill = NA)) %>%
  mutate(poly.noTC = (0.00000010095*(Digits_CTRL^2))+(0.02765*Digits_CTRL)+poly.control) %>% 
  mutate(raC.noT = rollapply(poly.noTC, 96, mean, na.rm = T,align = 'right', partial=TRUE, fill = NA)) 



data.bound <- bind_rows(data.1107, data.1108)
data.unique <- unique(data.bound)
data.merged <- data.unique %>% 
class(data.dat$TIMESTAMP)

txt_input <- c(data.0427, data.dat)

N <- 2
data.remove <- tail(data.dat, -N)

f1 <- function(dat, n) if(n <= 0) dat else tail(dat, -n)
data.remove <- f1(data.dat, 0)
data.remove <- f1(data.dat, 2)
data.remove <- data.dat[cumsum(complete.cases(data.dat)) != 0, ]

setNew<-data.dat[data.dat$RECORD != ""]
class(data.dat$Disp_Lower)

#temporary solution if all files have RECORD in this same format 
#would work for file-- but would it work for all files
setNew <- subset(data.0401, RECORD!="" & RECORD!= "RN")

#for some reason I like this solution better
#but would only start when record is 0 
remove.rows <- data.0401[which.max(data.0401$RECORD == "0") : nrow(data.0401), ]

#for loop goes through, subsets each row,
#filters out columns that don't have data, 
#adds the rows to the newdf

temp <- list.files(pattern="*.dat")
dfs <- lapply(temp, read.table(skip = 1,
                               header = T, sep = ',', row.names = 2))

lapply(list.files(pattern = ".dat"), read.csv(temp, skip = 1,
                                              header = T, sep = ',', row.names = 2))

for (i in temp){
  
  tmp <- get(i)    # load the actual table
  read.table(i, skip = 1,
             header = T, sep = ',', row.names = 2)
  tmp <- tmp[-c(1:2),] # remove first column
  tmp$TIMESTAMP <- as.POSIXct(tmp$TIMESTAMP)
  assign(i, tmp)   # re-assign table to original table name
}


for (i in 1:nrow(txt_input)) {
  
  c <- txt_input[i,]
  #print(c)
  #gets a 1 row df with all columns that aren't NA
  c1 <- c[-c(1:2),]
  

  c1$TIMESTAMP <- as.POSIXct(c1$TIMESTAMP)
  
  #if the subset wasn't just filled with NA's/had a length ==11, add to newdf 
  # # Error in names(x) <- value : 
  # 'names' attribute [11] must be the same length as the vector [1]
  #solved bc before i was just excluding lines of length 0, now I'm taking only the rows with data entries in exactly 11 columns
    
   #newdf <- bind_rows(newdf,c1)
  }

#remove first row


#write function to remove NaN values 
is.nan.data.frame <- function(x)
do.call(cbind, lapply(x, is.nan))



#remove NaN values -- not sure any are actually here 
df.remove_rows[is.nan(df.remove_rows)] <- 0

df.dat <- data.dat[-c(1:2),]
df.1107 <- data.1107[-c(1:2),]
df.1108 <- data.1108[-c(1:2),]
df.0106 <-data.0106[-c(1:2),]
df.0427 <- data.0427[-c(1:2),]
df.1208 <- data.1208[-c(1:2),]
df.1110 <- data.1110[-c(1:2),]
df.1122 <- data.1122[-c(1:2),]
df.1212 <- data.1212[-c(1:2),]
df.1219 <- data.1219[-c(1:2),]
df.1107_1715 <- data.1107_1715[-c(1:2),]
df.all <- data.all[-c(1:2),]
df.check <- data.check[-c(1:2),]
df.check2 <- data.check2[-c(1:2),]
data.thurston2 <- data.thurston[-c(1:2),]
data.thurston_check2 <- data.thurston_check[-c(1:2),]
test.thurston2 <- test.thurston[-c(1:2),]



data.bound <- bind_rows(df.1107, df.dat, df.1108, df.0106, df.0427, df.1208,
                        df.1110, df.1122, df.1212, df.1219, df.1107_1715)
data.unique <- unique(data.bound)
df.dates <- df.all

df.1107_1715$TT_CTRL[5]

is.na(df.1107_1715$TT_CTRL)
is.infinite(df.24ra$TT_CTRL)
class(df.24ra$TT_CTRL[1])
class(df.1212$TIMESTAMP)
df.1212$TIMESTAMP <- as_datetime(df.1212$TIMESTAMP)

#this removes time and just has date
# df.remove_rows$TIMESTAMP <- as.Date
df.date <- df.check 

test.thurston2$TIMESTAMP <- as.POSIXct(test.thurston2$TIMESTAMP)
df.dat$TIMESTAMP <- as.POSIXct(df.dat$TIMESTAMP)

data.thurston2$TIMESTAMP <- as.POSIXct(parse_date_time(data.thurston2$TIMESTAMP,"mdy HM"))
df.date$TIMESTAMP <- as.character(df.date$TIMESTAMP)
names(df.date)[names(df.date) == 'X'] <- 'raL.24'
names(df.date)[names(df.date) == 'X.1'] <- 'raL.noT'
names(df.date)[names(df.date) == 'X.2'] <- 'raU.24'
names(df.date)[names(df.date) == 'X.3'] <- 'raU.noT'
names(df.date)[names(df.date) == 'X.5'] <- 'raC.24'
names(df.date)[names(df.date) == 'X.6'] <- 'raC.noT'
names(df.date)[names(df.date) == 'X.10'] <- 'raT.24'

df.compare <- df.date %>% 
  select(TIMESTAMP, raT.24,
         Norm.Disp.L, raL.24, no.temp, raL.noT, 
         Norm.Disp.U, raU.24, no.temp.3, raU.noT, 
         Norm.Disp.CTRL, raC.24, no.temp.6, raC.noT)

df.date_che <- df.date %>% 
  mutate(check = ifelse(grepl("^[:digit:][:punct:]",df.date$TIMESTAMP),
                        "yes",
                        "no"))

df.date_check <- df.date %>% 
  mutate(TIMESTAMP = ifelse(grepl("^[:digit:][:punct:]",TIMESTAMP),
                            as.POSIXct(parse_date_time(TIMESTAMP,"mdy HM")),
                            as.POSIXct(TIMESTAMP)))  
    
# data.bound$TIMESTAMP <- as.POSIXct(parse_date_time(data.bound$TIMESTAMP,"mdy HM"))
# }
# else{

grepl(df.date$TIMESTAMP, "^[:digit:][:punct:]") 
grepl("^[:digit:][:punct:]", df.date$TIMESTAMP) 
    

data.unique$TIMESTAMP <- as.POSIXct(data.unique$TIMESTAMP)
df.date$TIMESTAMP <- as.POSIXct(df.date$TIMESTAMP)
df.date$TIMESTAMP <- as.Date(df.date$TIMESTAMP)
df.dates$TIMESTAMP <- as.POSIXct.default(df.dates$TIMESTAMP)
df.dates <- df.dates %>% 
  mutate(TIMESTAMP = mdy_hms(TIMESTAMP))
df.dates$TIMESTAMP <- as.numeric(as.character(df.dates$TIMESTAMP)) 
df.test <- as.POSIXct(as.numeric(as.character(df.dates$TIMESTAMP)),origin="1970-01-01")

df.check$TIMESTAMP <- as.numeric(as.character(df.check$TIMESTAMP))
# class(df.dates$TIMESTAMP)
# class(df.10$BattV)

#get ride of infinite values, come back to this  
# df.finite <- df.dates 
# df.finite$TT_CTRL[!is.finite(df.finite$TT_CTRL)] <- 0
#   mutate(across(where(is.character), as.numeric)) 
# do.call(data.frame, lapply(df.finite, function(x) replace(x, !is.finite(x), 0)))

data.unique[is.na(data.unique)] <- 0

df.24ra <- df.date %>% 
  arrange(TIMESTAMP) %>% 
  mutate(across(where(is.character), as.numeric)) %>% 
  #mutate_if(is.na, ~replace(., is.na(.), 0))
 # mutate_if(is.character, ~replace(., is.na(.), 0))
  #mutate(across(where(is.nan), 0)) %>% 
 # mutate_all(~replace(., is.na(.), 0))
  #mutate(across(where(is.nan), 0)) %>% 
  #might have to make some of these user inputs later 
  #or option for default? 
  mutate(raT.24 = rollapply(T109_C, 96, mean, na.rm = T,align = 'right', partial=TRUE, fill = 0)) %>%
  mutate(disp.lower = (Digits_Lower -2631) * 0.02828 + (TT_Lower - 22.8) *
           (((Digits_Lower * 0.000384) + (-0.3482)) * 0.02828)) %>%
  mutate(norm.dispL = disp.lower- disp.lower[1]) %>%
  mutate(raL.24 = rollapply(norm.dispL, 96, mean, na.rm = T, align = 'right', partial=TRUE, fill = 0)) %>%
  mutate(disp.upper = (Digits_Upper -2584) * 0.02828 + (TT_Upper - 22.8) *
           (((Digits_Upper * 0.000384) + (-0.3482)) * 0.02828)) %>%
  mutate(norm.dispU = disp.upper- disp.upper[1]) %>%
  mutate(raU.24 = rollapply(norm.dispU, 96, mean, na.rm = T,align = 'right', partial=TRUE, fill = 0)) %>%
  mutate(disp.control = (Digits_CTRL -2520) * 0.02869 + (TT_CTRL - 22.8) *
           (((Digits_CTRL * 0.000384) + (-0.3482)) * 0.02869)) %>%
  mutate(norm.dispC = disp.control- disp.control[1]) %>%
  mutate(raC.24 = rollapply(norm.dispC, 96, mean, na.rm = T, align = 'right', partial=TRUE, fill = 0)) 

plot_ly(df.24ra) %>% 
  add_trace(x = ~TIMESTAMP, y = ~raT.24, type = "scatter", 
            mode = "markers", marker = list(color = "#00AFBB"), yaxis = "y2", name = "Temperature") %>%
  add_trace(x = ~TIMESTAMP, y = ~raL.24,
            type = "scatter", mode = "markers", marker = list(color = '#00FF00'),
            name = "Lower Displacement", textposition = "top center") %>%
  add_trace(x = ~TIMESTAMP, y = ~raU.24, type = "scatter", 
            mode = "markers", marker = list(color = "#E7B800"), name = "Upper Displacement") %>%
  add_trace(x = ~TIMESTAMP, y = ~raC.24, type = "scatter", 
            mode = "markers", marker = list(color = '#FF0033'), name = "Control") %>%
  layout(
    legend = list(orientation = 'v'),
    yaxis2 = list(side = "right", title = "Temperature (deg C)"),
    title = "Pine Creek Housing Crackmeter Displacement (24 hour rolling average data)",
    xaxis = list(title="Time"),
    yaxis = list(overlaying = "y2", title="Displacement (mm)")
  )
#%>% 
  # mutate(min = ifelse(!is.nan(raC.24),
  #                     pmin(raC.24, raL.24, raU.24),
  #                     pmin(raL.24, raU.24))) %>% 
  # mutate(max = ifelse(!is.nan(raC.24),
  #                     pmax(raC.24, raL.24, raU.24),
  #                     pmax(raL.24, raU.24))) %>% 
  select(c(1,25,28, 31, 34)) #:36

  
#

# calibration mutate and graph --------------------------------------------
  
  library(data.table) #not sure if i need this 
  
  df.dat2 <- df.dat %>%
    arrange(TIMESTAMP) %>% 
    mutate(across(where(is.character), as.numeric)) 
  df.dat3 <- df.dat2 %>% #if don't make new df won't have TT_Lower as numeric
    mutate(disp = across(contains("Digits"), disp)) 
  
  
   disp <- function(x){
     
        x <- df.dat[ , grepl( "Digits" , names(df.dat) ) ]
        
         for (i in 1:ncol(x)) {

      }
        }


# with stack overflow/ nina help ------------------------------------------

# need to install.packages('tidytable')  
# dplyr attempts ----------------------------------------------------------

    
   test.data2 <- map_df(test.data, my_func)
   
   data.dat <- test.data %>%
     mutate(disp = across(contains("Digits"), my_func)) 
    
    
    
    
    for (j in cols) set(dt, j = j, value = -dt[[j]])
   
   dt[ , (out_cols) := lapply(.SD, "*", -1), .SDcols = cols]
   dt
    
    data.dat <- dt %>%
      mutate(disp = across(contains("Digits"), my_func)) 
    
   x <- dt[ , grepl( "digits" , names(dt) ) ] #grepl isn't starts with just contains

    #my code
    test.data2 <- as.data.table(test.data)
    test.calibration2<- as.data.table(test.calibration)
    
    test.dataN <- test.data2[rep(1:.N,nrow(test.calibration2))]
    test.calibrationN <- test.calibration2[rep(1:.N,each=nrow(test.data2))]
    #[, 2:1]
  
    #now using calibration2
    df.loop <- df.dat %>% 
      arrange(TIMESTAMP) %>% 
      mutate(across(where(is.character), as.numeric))
    
    calibration3 <- data.frame(x = c('x', 'y'),
                               Digits = c("Digits_Lower", "Digits_Upper"),
                               L_gauge_factor = 1:2,
                               Cal_Zero = 3:4)
    
    test.data <- data.frame(Digits_Lower = 1:5,
                            Digits_Upper = 6:10,
                            Digits_CTRL = 11:15,
                            TT_Lower = 1:5,
                            TT_Upper = 6:10,
                            random = 20:24)
    
    #dummy data for stack overflow
    
    dat <- data.frame(replicate(5, sample(0:10, 5, rep = T)))
    dat <- setNames(dat, c("Digits_Lower", "Digits_Upper", "TT_Lower", "TT_Upper", "Random"))
    
        dat <- data.frame(Digits_Lower = c(1:5),
                         Digits_Upper = c(6:10),
                         TT_Lower = 2:6,
                         TT_Upper = 4:8,
                         random = 20:24)
        
        cb <- data.frame(Digits = c("Digits_Lower", "Digits_Upper"),
                          TT = c("TT_Lower", "TT_Upper"),
                          x = 1:2, 
                          y = 3:4)
  
        pmap(calibration3, function(x) {
          calibration3() #not quite sure how to do this, must be way to iterate through rows and store them that's not loop 
        }  ) 

    
     #using set
    for (j in cols_to_change) { 
      set(dt, ,out_cols, value = dt[[j]]*2) 
    }
    
    lapply(df, FUN= my_func)
    library(data.table)
    #create data table data
    dt <- data.table(digits_lower = 1:5, digits_upper = 1:5, temp = 1:5)
    cb <- data.table(col1= 1:2)
    df <- data.table(q= 1:5, y = 1:5)
    #identify columns to change containing 'digits'
 
    
    df.loop <- test.data %>% 
      mutate(disp = across(contains("Digits")),
      #, disp), #disp = function #and then could replace $ with . and remove
                    .fns = function(calibration3){
                      for(i in seq_len(nrow(calibration3)))
                      {
                      (test.data[, calibration3$Digits[i]]- calibration3$Cal_Zero[i]) * calibration3$L_gauge_factor[i]
                      }
                      })
    
    cols_to_change <- stringr::str_subset(names(df.loop), "Digits")
    #new column names
    out_cols <- paste("disp", cols_to_change, sep =".") #eventually want to only keep string after 'digits'
    #using .SD
    df.loop[, (out_cols) := lapply(.SD, my_func), .SDcols = cols_to_change]
  
  # need to make sure have as many columns operating by in data as rows in calibration values 
    df.loop <- as.data.table(test.data)
    calibration.dt <- as.data.table(calibration2)
    
    #function definition: 
    my_func <- function(calibration.dt, x){
      for(i in seq_len(nrow(calibration.dt)))
      {
        df.loop[, calibration.dt$Digits[i]] <- (x[, calibration.dt$Digits[i]]- calibration.dt$Cal_Zero[i]) * calibration.dt$L_gauge_factor[i]
      }
    }

   
    
    #how to add pesky columns 
    df.loop <- test.data
    
    df.loop <- df.dat %>% 
      arrange(TIMESTAMP) %>% 
      mutate(across(where(is.character), as.numeric)) 
      
    for(i in seq_len(nrow(calibration2)))
    {
      df.loop[paste0("disp", sep = '.', calibration2$Digits[i])] <- (df.loop[, calibration2$Digits[i]]- calibration2$Cal_Zero[i]) * calibration2$L_gauge_factor[i]  + 
        (df.loop[, calibration2$TT[i]] - calibration2$Temp[i]) * (((df.loop[, calibration2$Digits[i]] * calibration2$TempCor_M[i]) + 
                                                         (calibration2$TempCor_B[i])) * calibration2$L_gauge_factor[i])
    }
    
    #this code works, although not sure how it would work with TT
    for(i in seq_len(nrow(calibration2)))
    {
      df.loop[, calibration2$Digits[i]] <- (test.data[, calibration2$Digits[i]]- calibration2$Cal_Zero[i]) * calibration2$L_gauge_factor[i]
    }
    
    df.loop <- test.data
    #try to add new columns with paste0
    for(i in seq_len(nrow(calibration2)))
    {
      df.loop[paste0("disp", sep = '.', calibration2$Digits[i])] <- (test.data[, calibration2$Digits[i]]- calibration2$Cal_Zero[i]) * calibration2$L_gauge_factor[i]
    }
    
    #assign values to calibration 
    cal_func <- function(x){
      
    }

    #second function 
    
    
    

    
    
    (x[,i] -calibration2$Cal_Zero[,j]) * calibration2$L_gauge_factor[,j] + 
      (df.dat2$TT_Lower - calibration2$Temp[,j]) * (((x[,i] * calibration2$TempCor_M[,j]) + 
                                                       (calibration2$TempCor_B[,j])) * calibration2$L_gauge_factor[,j])
    
    (Digits_Lower -2631) * 0.02828 + (TT_Lower - 22.8) *
      (((Digits_Lower * 0.000384) + (-0.3482)) * 0.02828)
    
    df1 %>% 
      mutate(across(.cols = c(grass, moss, rock),
                    .fns = function(x){
                      x * (df2 %>% 
                             filter(Cover == cur_column()) %>% 
                             pull(value))
                    }))
    
    test.calibration <- data.frame(col1 = c("Digits_Lower", "Digits_Upper"),
                            col2 = c(2,4),
                            col3 = c(1,3))
    

    
    df.digits <- test.data %>% 
     
      # mutate(disp = across(contains("digits")),
                           #, disp), #disp = function
      mutate(across(.cols = c(digits_lower, digits_upper),
                    .fns = function(x){
                      x * (test.calibration %>% 
                             filter(col1 == cur_column()) %>% 
                             pull(col2))
                    }))

    
    names() %>% 
    lapply(
      function(.col) lapply(
        mylist[[.col]], # need to specify only columns that start with Digits
        function(.fct) sprintf("%s.%s = %s(%s)", .col, .fct, .fct, .col))) %>% 
    unlist() %>% 
    paste(collapse = ", ") %>% 
    sprintf("as.data.table(df.dat)[, .(%s), by = Digits]", .) %>% 
    parse(text = .) %>% 
    eval() 
  
function()

  df.24ra <- df.dat %>% 
    arrange(TIMESTAMP) %>% 
    mutate(across(where(is.character), as.numeric)) %>% 
    mutate(raT.24 = rollapply(T109_C, 96, mean, na.rm = T,align = 'right', partial=TRUE, fill = 0)) %>%
    
    mutate(disp.lower = (Digits_Lower -calibration2$Cal_Zero[1]) * calibration2$L_gauge_factor[1] + 
             (TT_Lower - calibration2$Temp[1]) * (((Digits_Lower * calibration2$TempCor_M[1]) + 
             (calibration2$TempCor_B[1])) * calibration2$L_gauge_factor[1])) %>%
    mutate(norm.dispL = disp.lower- calibration2$base.value[1]) %>%
    mutate(raL.24 = rollapply(norm.dispL, 96, mean, na.rm = T, align = 'right', partial=TRUE, fill = 0)) %>%
    
    mutate(disp.upper = (Digits_Upper -2584) * 0.02828 + (TT_Upper - 22.8) *
             (((Digits_Upper * 0.000384) + (-0.3482)) * 0.02828)) %>%
    mutate(norm.dispU = disp.upper- disp.upper[1]) %>%
    mutate(raU.24 = rollapply(norm.dispU, 96, mean, na.rm = T,align = 'right', partial=TRUE, fill = 0)) %>%
    mutate(disp.control = (Digits_CTRL -2520) * 0.02869 + (TT_CTRL - 22.8) *
             (((Digits_CTRL * 0.000384) + (-0.3482)) * 0.02869)) %>%
    mutate(norm.dispC = disp.control- disp.control[1]) %>%
    mutate(raC.24 = rollapply(norm.dispC, 96, mean, na.rm = T, align = 'right', partial=TRUE, fill = 0)) 
  
  plot_ly(df.24ra) %>% 
    add_trace(x = ~TIMESTAMP, y = ~raT.24, type = "scatter", 
              mode = "markers", marker = list(color = "#00AFBB"), yaxis = "y2", name = "Temperature") %>%
    add_trace(x = ~TIMESTAMP, y = ~raL.24,
              type = "scatter", mode = "markers", marker = list(color = '#00FF00'),
              name = "Lower Displacement", textposition = "top center") %>%
    add_trace(x = ~TIMESTAMP, y = ~raU.24, type = "scatter", 
              mode = "markers", marker = list(color = "#E7B800"), name = "Upper Displacement") %>%
    add_trace(x = ~TIMESTAMP, y = ~raC.24, type = "scatter", 
              mode = "markers", marker = list(color = '#FF0033'), name = "Control") %>%
    layout(
      legend = list(orientation = 'v'),
      yaxis2 = list(side = "right", title = "Temperature (deg C)"),
      title = "Pine Creek Housing Crackmeter Displacement (24 hour rolling average data)",
      xaxis = list(title="Time"),
      yaxis = list(overlaying = "y2", title="Displacement (mm)")
    )
  

# unclear graph attempt, prob delete --------------------------------------

  
  
df.graph <- df.24ra %>% 
  #combi mutate() and across() supersedes mutate_[at|if | all]
 # mutate(across(where(is.character), as.numeric)) %>% 
  select(TIMESTAMP, raT.24, raL.24, raU.24, raC.24) %>% 
  gather(key = 'variable', value = 'value', -TIMESTAMP) # %>% 
# head(100)
#   #head(10) %>% 

# ylim.prim <- c(-1, 1)
# ylim.sec <- c(0, 15)
# ylim.prim <- c(min(df.24ra$raC.24 | df.24ra$raC.24 | df.24ra$raU.24), 
#                max(df.24ra$raC.24 | df.24ra$raC.24 | df.24ra$raU.24))   # displacement
# 
# ylim.prim <- c(min(df.24ra$min), max(df.24ra$max))
# ylim.sec <- c(min(df.24ra$raT.24), max(df.24ra$raT.24)) 
# b <- diff(ylim.prim)/diff(ylim.sec)
# a <- ylim.prim[1] - b*ylim.sec[1]


# df.24ra %>% 
#  #ggplot(aes(x = TIMESTAMP, y = value)) +
#   ggplot() +   # temperature
#   geom_point(aes(x= TIMESTAMP, y= a + raT.24*b, color = "#E7B800"), size = 1) +
#   geom_point(aes(x= TIMESTAMP, y= raL.24, color = '#00FF00'), size = 1) +
#   geom_point(aes(x= TIMESTAMP, y= raU.24, color = "#6600CC"), size = 1) +
#   geom_point(aes(x= TIMESTAMP, y= raC.24, color = "#FF0000"), size = 1) +
#  # scale_color_manual(values = c( '#00FF00', "#E7B800", "#00AFBB", '#FF0033')) +
#   scale_x_datetime(labels = date_format('%b %Y')) +
#   scale_y_continuous(
# 
#     # Features of the first axis
#     name = "Displacement (mm)",
# 
#     # Add a second axis and specify its features
#     sec.axis = sec_axis(~ (. - a)/b, name="Temperature (deg C)")
#   ) +
#   theme_minimal()

ylim.prim <- c(min(min(df.24ra$raC.24), min(df.24ra$raL.24), min(df.24ra$raU.24)), 
               max(max(df.24ra$raC.24), max(df.24ra$raL.24), max(df.24ra$raU.24)))   # displacement
ylim.prim <- c(min(df.24ra$raC.24 | df.24ra$raL.24 | df.24ra$raU.24))
ylim.sec <- c(min(df.24ra$raT.24), max(df.24ra$raT.24)) 
ylim.prim <- c(-1, 5)
ylim.sec <- c(0, 16)





names(df.24ra) <- c("Date", "Temperature", "Lower Displacement", "Upper Displacement",
                    "Control", "Min", "Max")


df.24ra.melt <- melt(df.24ra, id.vars = 'Date', variable.name = 'Category',
                     value.name = 'Displacement')

ylim.prim <- c(min(df.24ra$Min), max(df.24ra$Max))
ylim.sec <- c(min(df.24ra$Temperature), max(df.24ra$Temperature)) 

TEMP <- df.24ra$Temperature #needed for coherent normalisation

fit = lm(b ~ . + 0, 
         tibble::tribble(
           ~a, ~s,  ~b,
           1,  (ylim.sec[1] - mean(TEMP))/sd(TEMP),  ylim.prim[1],
           1,  (ylim.sec[2] - mean(TEMP))/sd(TEMP), ylim.prim[2]))

a <- fit$coefficients['a']
s <- fit$coefficients['s']


df.24ra %>% 
  #ggplot(aes(x = TIMESTAMP, y = value)) +
  ggplot() +   # temperature
  #geom_point(aes(x= TIMESTAMP, y= a + raT.24*b, color = "#E7B800"), size = 1) +
  geom_point(aes(x= Date, y = (a + ((Temperature - mean(TEMP))/sd(TEMP)) * s), 
             color = "#E7B800"), size = 1) +
  geom_point(aes(x= Date, y= `Lower Displacement`, color = '#00FF00'), size = 1) +
  geom_point(aes(x= Date, y= `Upper Displacement`, color = "#00AFBB"), size = 1) +
  geom_point(aes(x= Date, y= Control, color = '#FF0033'), size = 1) +
 scale_color_hue(labels = c("Upper Displacement", "Lower Displacement", "Rock Temperature", 
                            "Control Displacement")) +
  # scale_color_manual(values = c('raL.24' ='#00FF00', 'raT.24' = "#E7B800", 
  #                               'raU.24'= "#00AFBB", "raC.24" = '#FF0033')) +
  scale_x_datetime(name = NULL, labels = date_format('%b %Y')) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Displacement (mm)",
    limits = ylim.prim,
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ (. - a)/s * sd(TEMP) + mean(TEMP), name="Temperature (deg C)")
  ) +
  ggtitle("Pine Creek Housing Crackmeter Displacement (24 hour rolling average data)") +
  guides(color=guide_legend("Legend")) +
  #labs(fill = "Legend") +
  theme_minimal()


# export  -----------------------------------------------------------------

write.table(data.thurston2,"thurston_1010_take2.csv", row.names = FALSE, sep = ',')
