library(data.table)
library(dplyr)


# Read data and keep only severity (lesividad) and date columns
  library(readxl)

  accidents<-data.frame()

  # Data info: https://datos.madrid.es/portal/site/egob/menuitem.c05c1f754a33a9fbe4b2e4b284f1a5a0/?vgnextoid=7c2843010d9c3610VgnVCM2000001f4a900aRCRD&vgnextchannel=374512b9ace9f310VgnVCM100000171f5a0aRCRD&vgnextfmt=default
  urls <- c("https://datos.madrid.es/egob/catalogo/300228-8-accidentes-trafico-detalle.xlsx", #2010
            "https://datos.madrid.es/egob/catalogo/300228-7-accidentes-trafico-detalle.xlsx", #2011
            "https://datos.madrid.es/egob/catalogo/300228-6-accidentes-trafico-detalle.xlsx", #2012
            "https://datos.madrid.es/egob/catalogo/300228-5-accidentes-trafico-detalle.xlsx", #2013
            "https://datos.madrid.es/egob/catalogo/300228-4-accidentes-trafico-detalle.xlsx", #2014
            "https://datos.madrid.es/egob/catalogo/300228-3-accidentes-trafico-detalle.xlsx", #2015
            "https://datos.madrid.es/egob/catalogo/300228-2-accidentes-trafico-detalle.xlsx", #2016
            "https://datos.madrid.es/egob/catalogo/300228-1-accidentes-trafico-detalle.xlsx", #2017
            "https://datos.madrid.es/egob/catalogo/300228-0-accidentes-trafico-detalle.xlsx") #2018

  for(i in 1:length(urls)){
    print(i)
  
    destfile <- tempfile(fileext = ".xlsx")
    curl::curl_download(urls[i], destfile)
    temp <- read_excel(destfile)
    
    temp <- temp %>%
      select(FECHA, LESIVIDAD)
    
    accidents <- rbind(accidents, temp)
  }

  rm(list= c("temp", "destfile", "urls", "i"))

  
# Convert severity (lesividad) and date columns into tratable data
  
  accidents$LESIVIDAD <- factor(accidents$LESIVIDAD)
  levels(accidents$LESIVIDAD)[levels(accidents$LESIVIDAD)=="HG"] <- "Seriously_Injured"
  levels(accidents$LESIVIDAD)[levels(accidents$LESIVIDAD)=="HL"] <- "Slightly_Injured" 
  levels(accidents$LESIVIDAD)[levels(accidents$LESIVIDAD)=="MT"] <- "Dead"   
  levels(accidents$LESIVIDAD)[levels(accidents$LESIVIDAD)=="IL"] <- "Unscathed"   
  levels(accidents$LESIVIDAD)[levels(accidents$LESIVIDAD)=="NO ASIGNADA"] <- "Unknown"     

  accidents$FECHA <- as.Date(accidents$FECHA)
  

# Count data and group it, one row for date
  library(tidyr)  
  count_accidents <- accidents %>% 
                      group_by(FECHA, LESIVIDAD) %>%
                      summarise(count = n()) %>%
                      spread(value = count, key = LESIVIDAD, fill=0) %>% 
                      as.data.frame()
  
  #rm(accidents)  

# Trend lines
  trend_Seriously_Injured <- loess(Seriously_Injured ~ as.numeric(FECHA), data = count_accidents)
  trend_Slightly_Injured <- loess(Slightly_Injured ~ as.numeric(FECHA), data = count_accidents)
  trend_Dead <- loess(Dead ~ as.numeric(FECHA), data = count_accidents)
  trend_Unscathed <- loess(Unscathed ~ as.numeric(FECHA), data = count_accidents)
  
  
  
# Plot data
  library(plotly)  
  
  # plot data
  plot_ly(count_accidents) %>% 
    
    add_trace(x = ~FECHA, y = ~Seriously_Injured, type="scatter", 
              mode = "markers", name = "Seriously Injured", 
              legendgroup = "Seriously Injured", marker = list(color = "#ffa233"))  %>% 
    add_trace(x = ~FECHA, y = ~Slightly_Injured, type="scatter", 
              mode = "markers", name = "Slightly Injured", 
              legendgroup = "Slightly Injured", marker = list(color = "#fafdbd"))  %>% 
    add_trace(x = ~FECHA, y = ~Dead, type="scatter", 
              mode = "markers", name = "Dead", 
              legendgroup = "Dead", marker = list(color = "#ffa3a3"))  %>% 
    add_trace(x = ~FECHA, y = ~Unscathed, type="scatter", 
              mode = "markers", name = "Unscathed", 
              legendgroup = "Unscathed", marker = list(color = "#b3fcc1"))  %>% 
    

    add_trace(x = as.Date(trend_Seriously_Injured$x, origin = "1970-01-01"), y = fitted(trend_Seriously_Injured),
              type="scatter", mode = "lines", line = list(color = '#FF5733 '), 
              name = "Trend Seriously Injured", legendgroup = "Seriously Injured", 
              hoverinfo = 'none', showlegend = FALSE) %>%
    add_trace(x = as.Date(trend_Slightly_Injured$x, origin = "1970-01-01"), y = fitted(trend_Slightly_Injured),
              type="scatter", mode = "lines", line = list(color = '#fff000 '), 
              name = "Trend Slightly Injured", legendgroup = "Slightly Injured", 
              hoverinfo = 'none', showlegend = FALSE) %>%
    add_trace(x = as.Date(trend_Dead$x, origin = "1970-01-01"), y = fitted(trend_Dead),
              type="scatter", mode = "lines", line = list(color = '#ff0f0f '), 
              name = "Trend Dead", legendgroup = "Dead", 
              hoverinfo = 'none', showlegend = FALSE) %>%
    add_trace(x = as.Date(trend_Unscathed$x, origin = "1970-01-01"), y = fitted(trend_Unscathed),
              type="scatter", mode = "lines", line = list(color = '#0fff3d '), 
              name = "Trend Unscathed", legendgroup = "Unscathed", 
              hoverinfo = 'none', showlegend = FALSE) %>%
      
    layout(
      xaxis = list(title = "Date"),
      yaxis = list(title = "Number of Accidents"),
      title = "Traffic accidents in Madrid (Spain)"
    )
  

  
  library(zoo)
  

  