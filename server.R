library(shiny)
require(ggplot2)
library(leaflet)
#read in data set
usageD <- read.csv("water-use-intotal-clean.csv")
supplyD <- read.csv("water-supply-intotal-clean.csv")
supplyC <- read.csv("water-supply-composition-clean.csv")
usageC <- read.csv("water-usage-composition-clean.csv")
liv_per <- read.csv("living-usage-per-clean.csv")
location <- read.csv("provincelocation.csv")
avgD <- read.csv("water-use-sup-ave.csv")

#pre-process data set
usageD$water_use <- as.numeric(usageD$water_use)
supplyD$water_supply <- as.numeric(supplyD$water_supply)

supplyC$percentage <- as.numeric(supplyC$percentage)
usageC$percentage <- as.numeric(usageC$percentage)

liv_per$living_usage_per <-as.numeric(liv_per$living_usage_per)

shinyServer(function(input, output){
  # set text hint for 
  # set text hint for region's detailed information plot
  output$caption <- reactiveText(function(){
    if (input$method == "Water supply composition" || input$method == "Water usage composition"){
      paste("Average water resource data from 2012 to 2016")
    } else if (input$method == "Water usage in total" || input$method == "Water supply in total"){
      paste("Unit: hundred million cubic meters")
    }
  })
  
  # set text hint for overview plot
  output$caption2 <- reactiveText(function(){
    if (input$choice == "Living water usage per capita"){
      paste("Unit: cubic meters(avg data from 2012 to 2016")
    } else {
      paste("Unit: hundred million cubic meters")
    }
  })
  
  # plot the overview of the data set
  output$plot2 <- reactivePlot(function(){
    #plot the living water usage per capita in overview tab
    if (input$choice == "Living water usage per capita"){
      p <- ggplot(data = liv_per, aes(x = province, y = living_usage_per)) + geom_bar(stat = "identity", aes(color = province))
      print(p)
    }
    #plot the water supply in total in overview tab
    if (input$choice == "Water supply in total"){
      p <- ggplot(data = subset(avgD, data.type = water_supply), aes(x = province, y = data)) + geom_bar(stat = "identity", aes(color = province))
      print(p)
    }
    #plot the water usage in total in overview tab
    if (input$choice == "Water usage in total"){
      p <- ggplot(data = subset(avgD, data.type = water_use), aes(x = province, y = data)) + geom_bar(stat = "identity", aes(color = province))
      print(p)
    }
  })
  
  
  output$plot <- reactivePlot(function(){
    shiny <- supplyD
    # water supply in total plot output control
    
    if (input$variable == "Beijing" && input$method == "Water supply in total"){
      shiny <- subset(supplyD, province == "Beijing" )
    }
    if (input$variable == "Tianjing" && input$method == "Water supply in total"){
      shiny <- subset(supplyD, province == "Tianjing")
    }
    if (input$variable == "Heibei" && input$method == "Water supply in total"){
      shiny <- subset(supplyD, province == "Heibei")
    }
    if (input$variable == "Shanxi(1)" && input$method == "Water supply in total"){
      shiny <- subset(supplyD, province == "Shanxi(1)")
    }
    if (input$variable == "Neimeng" && input$method == "Water supply in total"){
      shiny <- subset(supplyD, province == "Neimeng")
    }
    if (input$variable == "Liaoning" && input$method == "Water supply in total"){
      shiny <- subset(supplyD, province == "Liaoning")
    }
    if (input$variable == "Jilin" && input$method == "Water supply in total"){
      shiny <- subset(supplyD, province == "Jilin")
    }
    if (input$variable == "Heilongjiang" && input$method == "Water supply in total"){
      shiny <- subset(supplyD, province == "Heilongjiang")
    }
    if (input$variable == "Shanghai" && input$method == "Water supply in total"){
      shiny <- subset(supplyD, province == "Shanghai")
    }
    if (input$variable == "Jiangsu" && input$method == "Water supply in total"){
      shiny <- subset(supplyD, province == "Jiangsu")
    }
    if (input$variable == "Zhejiang" && input$method == "Water supply in total"){
      shiny <- subset(supplyD, province == "Zhejiang")
    }
    if (input$variable == "Anhui" && input$method == "Water supply in total"){
      shiny <- subset(supplyD, province == "Anhui")
    }
    if (input$variable == "Fujian" && input$method == "Water supply in total"){
      shiny <- subset(supplyD, province == "Fujian")
    }
    if (input$variable == "Jiangxi" && input$method == "Water supply in total"){
      shiny <- subset(supplyD, province == "Jiangxi")
    }
    if (input$variable == "Shandong" && input$method == "Water supply in total"){
      shiny <- subset(supplyD, province == "Shandong")
    }
    if (input$variable == "Henan" && input$method == "Water supply in total"){
      shiny <- subset(supplyD, province == "Henan")
    }
    if (input$variable == "Hubei" && input$method == "Water supply in total"){
      shiny <- subset(supplyD, province == "Hubei")
    }
    if (input$variable == "Hunan" && input$method == "Water supply in total"){
      shiny <- subset(supplyD, province == "Hunan")
    }
    if (input$variable == "Guangdong" && input$method == "Water supply in total"){
      shiny <- subset(supplyD, province == "Guangdong")
    }
    if (input$variable == "Guangxi" && input$method == "Water supply in total"){
      shiny <- subset(supplyD, province == "Guangxi")
    }
    if (input$variable == "Hainan" && input$method == "Water supply in total"){
      shiny <- subset(supplyD, province == "Hainan")
    }
    if (input$variable == "Chongqing" && input$method == "Water supply in total"){
      shiny <- subset(supplyD, province == "Chongqing")
    }
    if (input$variable == "Sichuan" && input$method == "Water supply in total"){
      shiny <- subset(supplyD, province == "Sichuan")
    }
    if (input$variable == "Guizhou" && input$method == "Water supply in total"){
      shiny <- subset(supplyD, province == "Guizhou")
    }
    if (input$variable == "Yunnan" && input$method == "Water supply in total"){
      shiny <- subset(supplyD, province == "Yunnan")
    }
    if (input$variable == "Xizang" && input$method == "Water supply in total"){
      shiny <- subset(supplyD, province == "Xizang")
    }
    if (input$variable == "Shanxi(3)" && input$method == "Water supply in total"){
      shiny <- subset(supplyD, province == "Shanxi(3)")
    }
    if (input$variable == "Gansu" && input$method == "Water supply in total"){
      shiny <- subset(supplyD, province == "Gansu")
    }
    if (input$variable == "Qinghai" && input$method == "Water supply in total"){
      shiny <- subset(supplyD, province == "Qinghai")
    }
    if (input$variable == "Ningxia" && input$method == "Water supply in total"){
      shiny <- subset(supplyD, province == "Ningxia")
    }
    if (input$variable == "Xinjiang" && input$method == "Water supply in total"){
      shiny <- subset(supplyD, province == "Xinjiang")
    }
    
    
    # water usage in total plot output control
    
    if (input$variable == "Beijing" && input$method == "Water usage in total"){
      shiny <- subset(usageD, province == "Beijing" )
    }
    if (input$variable == "Tianjing" && input$method == "Water usage in total"){
      shiny <- subset(usageD, province == "Tianjing")
    }
    if (input$variable == "Heibei" && input$method == "Water usage in total"){
      shiny <- subset(usageD, province == "Heibei")
    }
    if (input$variable == "Shanxi(1)" && input$method == "Water usage in total"){
      shiny <- subset(usageD, province == "Shanxi(1)")
    }
    if (input$variable == "Neimeng" && input$method == "Water usage in total"){
      shiny <- subset(usageD, province == "Neimeng")
    }
    if (input$variable == "Liaoning" && input$method == "Water usage in total"){
      shiny <- subset(usageD, province == "Liaoning")
    }
    if (input$variable == "Jilin" && input$method == "Water usage in total"){
      shiny <- subset(usageD, province == "Jilin")
    }
    if (input$variable == "Heilongjiang" && input$method == "Water usage in total"){
      shiny <- subset(usageD, province == "Heilongjiang")
    }
    if (input$variable == "Shanghai" && input$method == "Water usage in total"){
      shiny <- subset(usageD, province == "Shanghai")
    }
    if (input$variable == "Jiangsu" && input$method == "Water usage in total"){
      shiny <- subset(usageD, province == "Jiangsu")
    }
    if (input$variable == "Zhejiang" && input$method == "Water usage in total"){
      shiny <- subset(usageD, province == "Zhejiang")
    }
    if (input$variable == "Anhui" && input$method == "Water usage in total"){
      shiny <- subset(usageD, province == "Anhui")
    }
    if (input$variable == "Fujian" && input$method == "Water usage in total"){
      shiny <- subset(usageD, province == "Fujian")
    }
    if (input$variable == "Jiangxi" && input$method == "Water usage in total"){
      shiny <- subset(usageD, province == "Jiangxi")
    }
    if (input$variable == "Shandong" && input$method == "Water usage in total"){
      shiny <- subset(usageD, province == "Shandong")
    }
    if (input$variable == "Henan" && input$method == "Water usage in total"){
      shiny <- subset(usageD, province == "Henan")
    }
    if (input$variable == "Hubei" && input$method == "Water usage in total"){
      shiny <- subset(usageD, province == "Hubei")
    }
    if (input$variable == "Hunan" && input$method == "Water usage in total"){
      shiny <- subset(usageD, province == "Hunan")
    }
    if (input$variable == "Guangdong" && input$method == "Water usage in total"){
      shiny <- subset(usageD, province == "Guangdong")
    }
    if (input$variable == "Guangxi" && input$method == "Water usage in total"){
      shiny <- subset(usageD, province == "Guangxi")
    }
    if (input$variable == "Hainan" && input$method == "Water usage in total"){
      shiny <- subset(usageD, province == "Hainan")
    }
    if (input$variable == "Chongqing" && input$method == "Water usage in total"){
      shiny <- subset(usageD, province == "Chongqing")
    }
    if (input$variable == "Sichuan" && input$method == "Water usage in total"){
      shiny <- subset(usageD, province == "Sichuan")
    }
    if (input$variable == "Guizhou" && input$method == "Water usage in total"){
      shiny <- subset(usageD, province == "Guizhou")
    }
    if (input$variable == "Yunnan" && input$method == "Water usage in total"){
      shiny <- subset(usageD, province == "Yunnan")
    }
    if (input$variable == "Xizang" && input$method == "Water usage in total"){
      shiny <- subset(usageD, province == "Xizang")
    }
    if (input$variable == "Shanxi(3)" && input$method == "Water usage in total"){
      shiny <- subset(usageD, province == "Shanxi(3)")
    }
    if (input$variable == "Gansu" && input$method == "Water usage in total"){
      shiny <- subset(usageD, province == "Gansu")
    }
    if (input$variable == "Qinghai" && input$method == "Water usage in total"){
      shiny <- subset(usageD, province == "Qinghai")
    }
    if (input$variable == "Ningxia" && input$method == "Water usage in total"){
      shiny <- subset(usageD, province == "Ningxia")
    }
    if (input$variable == "Xinjiang" && input$method == "Water usage in total"){
      shiny <- subset(usageD, province == "Xinjiang")
    }
    
    
    
    
    # water supply composition in plot output control
    
    if (input$variable == "Beijing" && input$method == "Water supply composition"){
      shiny <- subset(supplyC, province == "Beijing" )
    }
    if (input$variable == "Tianjing" && input$method == "Water supply composition"){
      shiny <- subset(supplyC, province == "Tianjing")
    }
    if (input$variable == "Heibei" && input$method == "Water supply composition"){
      shiny <- subset(supplyC, province == "Heibei")
    }
    if (input$variable == "Shanxi(1)" && input$method == "Water supply composition"){
      shiny <- subset(supplyC, province == "Shanxi(1)")
    }
    if (input$variable == "Neimeng" && input$method == "Water supply composition"){
      shiny <- subset(supplyC, province == "Neimeng")
    }
    if (input$variable == "Liaoning" && input$method == "Water supply composition"){
      shiny <- subset(supplyC, province == "Liaoning")
    }
    if (input$variable == "Jilin" && input$method == "Water supply composition"){
      shiny <- subset(supplyC, province == "Jilin")
    }
    if (input$variable == "Heilongjiang" && input$method == "Water supply composition"){
      shiny <- subset(supplyC, province == "Heilongjiang")
    }
    if (input$variable == "Shanghai" && input$method == "Water supply composition"){
      shiny <- subset(supplyC, province == "Shanghai")
    }
    if (input$variable == "Jiangsu" && input$method == "Water supply composition"){
      shiny <- subset(supplyC, province == "Jiangsu")
    }
    if (input$variable == "Zhejiang" && input$method == "Water supply composition"){
      shiny <- subset(supplyC, province == "Zhejiang")
    }
    if (input$variable == "Anhui" && input$method == "Water supply composition"){
      shiny <- subset(supplyC, province == "Anhui")
    }
    if (input$variable == "Fujian" && input$method == "Water supply composition"){
      shiny <- subset(supplyC, province == "Fujian")
    }
    if (input$variable == "Jiangxi" && input$method == "Water supply composition"){
      shiny <- subset(supplyC, province == "Jiangxi")
    }
    if (input$variable == "Shandong" && input$method == "Water supply composition"){
      shiny <- subset(supplyC, province == "Shandong")
    }
    if (input$variable == "Henan" && input$method == "Water supply composition"){
      shiny <- subset(supplyC, province == "Henan")
    }
    if (input$variable == "Hubei" && input$method == "Water supply composition"){
      shiny <- subset(supplyC, province == "Hubei")
    }
    if (input$variable == "Hunan" && input$method == "Water supply composition"){
      shiny <- subset(supplyC, province == "Hunan")
    }
    if (input$variable == "Guangdong" && input$method == "Water supply composition"){
      shiny <- subset(supplyC, province == "Guangdong")
    }
    if (input$variable == "Guangxi" && input$method == "Water supply composition"){
      shiny <- subset(supplyC, province == "Guangxi")
    }
    if (input$variable == "Hainan" && input$method == "Water supply composition"){
      shiny <- subset(supplyC, province == "Hainan")
    }
    if (input$variable == "Chongqing" && input$method == "Water supply composition"){
      shiny <- subset(supplyC, province == "Chongqing")
    }
    if (input$variable == "Sichuan" && input$method == "Water supply composition"){
      shiny <- subset(supplyC, province == "Sichuan")
    }
    if (input$variable == "Guizhou" && input$method == "Water supply composition"){
      shiny <- subset(supplyC, province == "Guizhou")
    }
    if (input$variable == "Yunnan" && input$method == "Water supply composition"){
      shiny <- subset(supplyC, province == "Yunnan")
    }
    if (input$variable == "Xizang" && input$method == "Water supply composition"){
      shiny <- subset(supplyC, province == "Xizang")
    }
    if (input$variable == "Shanxi(3)" && input$method == "Water supply composition"){
      shiny <- subset(supplyC, province == "Shanxi(3)")
    }
    if (input$variable == "Gansu" && input$method == "Water supply composition"){
      shiny <- subset(supplyC, province == "Gansu")
    }
    if (input$variable == "Qinghai" && input$method == "Water supply composition"){
      shiny <- subset(supplyC, province == "Qinghai")
    }
    if (input$variable == "Ningxia" && input$method == "Water supply composition"){
      shiny <- subset(supplyC, province == "Ningxia")
    }
    if (input$variable == "Xinjiang" && input$method == "Water supply composition"){
      shiny <- subset(supplyC, province == "Xinjiang")
    }
    
    
    
    
    # water usage composition in plot output control
    
    if (input$variable == "Beijing" && input$method == "Water usage composition"){
      shiny <- subset(usageC, province == "Beijing" )
    }
    if (input$variable == "Tianjing" && input$method == "Water usage composition"){
      shiny <- subset(usageC, province == "Tianjing")
    }
    if (input$variable == "Heibei" && input$method == "Water usage composition"){
      shiny <- subset(usageC, province == "Heibei")
    }
    if (input$variable == "Shanxi(1)" && input$method == "Water usage composition"){
      shiny <- subset(usageC, province == "Shanxi(1)")
    }
    if (input$variable == "Neimeng" && input$method == "Water usage composition"){
      shiny <- subset(usageC, province == "Neimeng")
    }
    if (input$variable == "Liaoning" && input$method == "Water usage composition"){
      shiny <- subset(usageC, province == "Liaoning")
    }
    if (input$variable == "Jilin" && input$method == "Water usage composition"){
      shiny <- subset(usageC, province == "Jilin")
    }
    if (input$variable == "Heilongjiang" && input$method == "Water usage composition"){
      shiny <- subset(usageC, province == "Heilongjiang")
    }
    if (input$variable == "Shanghai" && input$method == "Water usage composition"){
      shiny <- subset(usageC, province == "Shanghai")
    }
    if (input$variable == "Jiangsu" && input$method == "Water usage composition"){
      shiny <- subset(usageC, province == "Jiangsu")
    }
    if (input$variable == "Zhejiang" && input$method == "Water usage composition"){
      shiny <- subset(usageC, province == "Zhejiang")
    }
    if (input$variable == "Anhui" && input$method == "Water usage composition"){
      shiny <- subset(usageC, province == "Anhui")
    }
    if (input$variable == "Fujian" && input$method == "Water usage composition"){
      shiny <- subset(usageC, province == "Fujian")
    }
    if (input$variable == "Jiangxi" && input$method == "Water usage composition"){
      shiny <- subset(usageC, province == "Jiangxi")
    }
    if (input$variable == "Shandong" && input$method == "Water usage composition"){
      shiny <- subset(usageC, province == "Shandong")
    }
    if (input$variable == "Henan" && input$method == "Water usage composition"){
      shiny <- subset(usageC, province == "Henan")
    }
    if (input$variable == "Hubei" && input$method == "Water usage composition"){
      shiny <- subset(usageC, province == "Hubei")
    }
    if (input$variable == "Hunan" && input$method == "Water usage composition"){
      shiny <- subset(usageC, province == "Hunan")
    }
    if (input$variable == "Guangdong" && input$method == "Water usage composition"){
      shiny <- subset(usageC, province == "Guangdong")
    }
    if (input$variable == "Guangxi" && input$method == "Water usage composition"){
      shiny <- subset(usageC, province == "Guangxi")
    }
    if (input$variable == "Hainan" && input$method == "Water usage composition"){
      shiny <- subset(usageC, province == "Hainan")
    }
    if (input$variable == "Chongqing" && input$method == "Water usage composition"){
      shiny <- subset(usageC, province == "Chongqing")
    }
    if (input$variable == "Sichuan" && input$method == "Water usage composition"){
      shiny <- subset(usageC, province == "Sichuan")
    }
    if (input$variable == "Guizhou" && input$method == "Water usage compositiion"){
      shiny <- subset(usageC, province == "Guizhou")
    }
    if (input$variable == "Yunnan" && input$method == "Water usage composition"){
      shiny <- subset(usageC, province == "Yunnan")
    }
    if (input$variable == "Xizang" && input$method == "Water usage composition"){
      shiny <- subset(usageC, province == "Xizang")
    }
    if (input$variable == "Shanxi(3)" && input$method == "Water usage composition"){
      shiny <- subset(usageC, province == "Shanxi(3)")
    }
    if (input$variable == "Gansu" && input$method == "Water usage composition"){
      shiny <- subset(usageC, province == "Gansu")
    }
    if (input$variable == "Qinghai" && input$method == "Water usage composition"){
      shiny <- subset(usageC, province == "Qinghai")
    }
    if (input$variable == "Ningxia" && input$method == "Water usage composition"){
      shiny <- subset(usageC, province == "Ningxia")
    }
    if (input$variable == "Xinjiang" && input$method == "Water usage composition"){
      shiny <- subset(usageC, province == "Xinjiang")
    }
    
    
    if (input$method == "Water supply in total"){
      p <- ggplot(data = shiny, aes(x = year, y = water_supply)) + geom_point()
      p <- p + geom_smooth(method = "lm", color = "red", size = 0.6)
      print(p)
    }
    
    if (input$method == "Water usage in total"){
      p <- ggplot(data = shiny, aes(x = year, y = water_use)) + geom_point()
      p <- p + geom_smooth(method = "lm", color = "red", size = 0.6)
      print(p)
    }
    
    if (input$method == "Water supply composition"){
      p <- ggplot(data = shiny, aes(x = '', y = percentage, fill = source_type)) + geom_bar(stat = "identity", width = 1)
      p <- p + coord_polar(theta = "y")
      print(p)
    }
    
    if (input$method == "Water usage composition"){
      p <- ggplot(data = shiny, aes(x = '', y = percentage, fill = source_type)) + geom_bar(stat = "identity", width = 1)
      p <- p + coord_polar(theta = "y")
      print(p)
    }
  })
  
  # make the map for the "About" tab to show the location of each regions
  output$map2 <- renderLeaflet({
    leaflet(data = location) %>%
      addTiles() %>%
      addCircleMarkers(~longtitude, ~ latitude, label = ~as.character(Province))
  })
  
  # make leaflet map for the "Map" tab to show the water usage per capita for each region in color ramp.
  output$map <- renderLeaflet({
    cPal <- colorNumeric(palette = c("red", "yellow"), domain = liv_per$living_usage_per)
    location$living_usage_per <- liv_per$living_usage_per
    label_map <- paste("Province:", location$Province, "<br/>",
                    "Usage per capita:", location$living_usage_per)
    leaflet(data = location) %>%
      addTiles() %>%
      #map and add text label in the map
      addCircleMarkers(~longtitude, ~ latitude, popup = label_map, label = ~as.character(Province), 
                       fillColor = ~cPal(location$living_usage_per),
                       fillOpacity = 0.8) %>%
      addLegend("bottomleft", pal = cPal, values = ~living_usage_per, title = "Per capita: m^3",
                opacity = 1)
  })
})