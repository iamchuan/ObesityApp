library(shiny)
library(maps)
library(mapproj)
library(leaflet)
library(shinydashboard)
library(plotly)
library(DT)
source("helpers.R")

us.map <- readOGR("Data/cb_2016_us_county_500k/cb_2016_us_county_500k.shp",
                  layer = "cb_2016_us_county_500k", 
                  stringsAsFactors = FALSE)

us.map <- us.map[us.map$GEOID %in% health_fips$FIPS,]
health_fips <- health_fips %>% 
  rename(GEOID = FIPS) %>% 
  group_by(GEOID) %>% 
  filter(row_number() < 2)

# Merge spatial df with downloade data.
leafmap <- merge(us.map, 
                 health_fips, 
                 by=c("GEOID"))

pal <- colorQuantile("YlOrRd", NULL, n = 20)

shinyServer(
  function(input, output) {
    
    output$map <- renderLeaflet({
      # Format popup data for leaflet map.
      popup_dat <- paste0("<strong>County: </strong>", 
                          leafmap$NAME, 
                          "<br><strong>Value: </strong>", 
                          leafmap@data[, input$var])
      leaflet(leafmap) %>% 
        addTiles() %>%
        addPolygons(fillColor = pal(leafmap@data[, input$var]), 
                    fillOpacity = 0.8, 
                    color = "#BDBDC3", 
                    weight = 1,
                    popup = popup_dat)
    })
    
    
    output$trendPlot <- renderPlotly({
      args1 <- switch(input$var1,
                      "Percent Adult Obesity, 2010" = "PCT_OBESE_ADULTS10",
                      "Percent Adult Diabetic, 2010" = "PCT_DIABETES_ADULTS10",
                      "Poverty Rate, 2010" = "POVRATE10", 
                      "Median household income, 2010" = "MEDHHINC10", 
                      "Population, low access to store (%), 2010" ="PCT_LACCESS_POP10",
                      "Grocery stores/1,000 pop, 2012" = "GROCPTH12", 
                      "Convenience stores/1,000 pop, 2012" = "CONVSPTH12", 
                      "Fast-food restaurants/1,000 pop, 2012" = "FFRPTH12", 
                      "Full-service restaurants/1,000 pop, 2012" = "FSRPTH12", 
                      "Household food insecurity (%), 2010-12" = "FOODINSEC_10_12", 
                      "Farms with direct sales, 2007" = "DIRSALES_FARMS07", 
                      "Farmers' markets/1,000 pop, 2013" = "FMRKTPTH13", 
                      "Vegetable farms, 2007" = "VEG_FARMS07", 
                      "High schoolers physically active (%), 2009" = "PCT_HSPA09",
                      "Recreation & fitness facilities/1,000 pop, 2012" = "RECFACPTH12", 
                      "% Population 65 years or older, 2010" = "PCT_65OLDER10", 
                      "% Population under age 18, 2010" = "PCT_18YOUNGER10"
      )
      
      args2 <- switch(input$var2,
                      "Percent Adult Obesity, 2010" = "PCT_OBESE_ADULTS10",
                      "Percent Adult Diabetic, 2010" = "PCT_DIABETES_ADULTS10",
                      "Percent Poverty, 2010" = "POVRATE10", 
                      "Median household income, 2010" = "MEDHHINC10", 
                      "Population, low access to store (%), 2010" ="PCT_LACCESS_POP10",
                      "Grocery stores/1,000 pop, 2012" = "GROCPTH12", 
                      "Convenience stores/1,000 pop, 2012" = "CONVSPTH12", 
                      "Fast-food restaurants/1,000 pop, 2012" = "FFRPTH12", 
                      "Full-service restaurants/1,000 pop, 2012" = "FSRPTH12", 
                      "Household food insecurity (%), 2010-12" = "FOODINSEC_10_12", 
                      "Farms with direct sales, 2007" = "DIRSALES_FARMS07", 
                      "Farmers' markets/1,000 pop, 2013" = "FMRKTPTH13", 
                      "Vegetable farms, 2007" = "VEG_FARMS07", 
                      "High schoolers physically active (%), 2009" = "PCT_HSPA09",
                      "Recreation & fitness facilities/1,000 pop, 2012" = "RECFACPTH12", 
                      "% Population 65 years or older, 2010" = "PCT_65OLDER10", 
                      "% Population under age 18, 2010" = "PCT_18YOUNGER10"
      )
      
      # Create a convenience data.frame which can be used for charting
      plot.df <- data.frame(fulldf[,args1],
                            fulldf[,args2],
                            fulldf$County,
                            fulldf$State,
                            fulldf$PCT_OBESE_ADULTS10)
      
      # Add column names
      colnames(plot.df) <- c("x", "y", "County", "State", "Obese")
      
      p <- plot_ly(plot.df, x = ~x, y = ~y, 
                   text = paste(plot.df$County, ",", plot.df$State, "Adult % Obese:", plot.df$Obese),
                   mode = "markers", 
                   color = ~Obese,
                   colors = )
      
      layout(p,title = paste(input$var2, "vs ", input$var1),
             xaxis = list(title = input$var1),
             yaxis = list(title = input$var2))
    })
    
    output$tabdb <- DT::renderDataTable(tabledb, options = list(
      scrollX = TRUE))
    
    output$coefs <- DT::renderDataTable(coef, options = list(
      pageLength = 15))
    
    output$rendvifs <- renderDataTable(printvifs)    
    
    output$predtable <- renderUI({
      g <- predfunc(GROC = input$GROC, Conv = input$Conv, Full = input$Full, FF = input$FF, LACCESS = 
                      input$LACCESS, MEDHHIN = input$MEDHHIN, RECFAC = input$RECFAC,
                    PCT18 = input$PCT18, FOODINS = input$FOODINS, FARMRT = input$FARMRT, 
                    VEGFARM = input$VEGFARM, DIABETE = input$DIABETE, HSACT = input$HSACT,
                    POVRT = input$POVRT, PCT65 = input$PCT65)
      str1 = paste("Predicted obesity rate is ", round(g[1], 1), "%")
      str2 = paste("95% confidence interval (CI) is ", round(g[2], 1), "%", "to", round(g[3], 1), "%")
      HTML(paste(str1, str2, sep = '<br/>'))
    })
  }
)
