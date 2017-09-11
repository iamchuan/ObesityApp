library(shiny)
library(shinydashboard)
library(plotly)
source("helpers.R")
library(DT)

dashboardPage(
    dashboardHeader(title = "Obesity Prediction App", titleWidth = 400), 
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Map", tabName = "mappanel", icon = icon("map")),
        menuItem("Trends", tabName = "trends", icon = icon("line-chart")),
        menuItem("Predictions", tabName = "predict", icon = icon("chevron-right")),
        menuItem("Data", tabName = "data", icon = icon("database")),
        menuItem("About", tabName = "about", icon = icon("info"))
      )),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "mappanel",
                fluidRow(
                column(width = 12,
                       box(
                         selectInput("var", # choose the residents
                                     label = "Choose a variable to display",
                                     choices = c("Adult Obesity Rate (county), 2009"="PCT_OBESE_ADULTS09",
                                                 "Adult Obesity Rate (county), 2010"="PCT_OBESE_ADULTS10"),
                                     selected = "Adult Obesity Rate (county), 2010")
                         )
                ),
                leafletOutput("map", width = "100%")
                )),
        
      tabItem(tabName = "trends",
              fluidRow(
                column(width = 12,
                       box(
                         # helpText("Create demographic maps with
                         #          information from the USDA's Food Environment Atlas."),
                         selectInput("var1", 
                                     label = "Choose X variable",
                                     choices = c("Percent Adult Obesity, 2010",
                                                 "Percent Adult Diabetic, 2010",
                                                 "Percent Poverty, 2010",
                                                 "Median household income, 2010",
                                                 "Population, low access to store (%), 2010",
                                                 "Grocery stores/1,000 pop, 2012",
                                                 "Convenience stores/1,000 pop, 2012",
                                                 "Fast-food restaurants/1,000 pop, 2012",
                                                 "Full-service restaurants/1,000 pop, 2012",
                                                 "Household food insecurity (%), 2010-12",
                                                 "Farms with direct sales, 2007",
                                                 "Farmers' markets/1,000 pop, 2013",
                                                 "Vegetable farms, 2007",
                                                 "High schoolers physically active (%), 2009",
                                                 "Recreation & fitness facilities/1,000 pop, 2012",
                                                 "% Population 65 years or older, 2010",
                                                 "% Population under age 18, 2010"),
                                     selected = "Median household income, 2010")
                       ),
                       box(selectInput("var2", 
                                       label = "Choose Y variable",
                                       choices = c("Percent Adult Obesity, 2010",
                                                   "Percent Adult Diabetic, 2010",
                                                   "Percent Poverty, 2010",
                                                   "Median household income, 2010",
                                                   "Population, low access to store (%), 2010",
                                                   "Grocery stores/1,000 pop, 2012",
                                                   "Convenience stores/1,000 pop, 2012",
                                                   "Fast-food restaurants/1,000 pop, 2012",
                                                   "Full-service restaurants/1,000 pop, 2012",
                                                   "Household food insecurity (%), 2010-12",
                                                   "Farms with direct sales, 2007",
                                                   "Farmers' markets/1,000 pop, 2013",
                                                   "Vegetable farms, 2007",
                                                   "High schoolers physically active (%), 2009",
                                                   "Recreation & fitness facilities/1,000 pop, 2012",
                                                   "% Population 65 years or older, 2010",
                                                   "% Population under age 18, 2010"),
                                       selected = "Percent Obese 2009")
                       )
                ),
                box(width = 12, plotlyOutput("trendPlot", height = 550))
              )
      ),

      tabItem(tabName = "predict",
              fluidRow(

                h2("Obesity prediction"),
                h3(htmlOutput("predtable")),
                column(width = 12,
                       box(
                         sliderInput("GROC", label = p("Grocery stores/1000 people, 2012"),
                                     min = rlow(data$GROCPTH12),
                                     max = rhigh(data$GROCPTH12), value =
                                       valmean(data$GROCPTH12)),
                         sliderInput("Conv", label = p("Conveniences stores/1000 people, 2012"),
                                     min = rlow(data$CONVSPTH12),
                                     max = rhigh(data$CONVSPTH12), value =
                                       valmean(data$CONVSPTH12)),
                         sliderInput("FF", label = p("Fast-food stores/1000 people, 2012"),
                                     min = rlow(data$FFRPTH12),
                                     max = rhigh(data$FFRPTH12), value =
                                       valmean(data$FFRPTH12)),
                         sliderInput("Full", label = p("Full-service restaurants/1,000 pop, 2012"),
                                     min = rlow(data$FSRPTH12),
                                     max = rhigh(data$FSRPTH12), value =
                                       valmean(data$FSRPTH12)),
                         sliderInput("LACCESS", label = p("Population, low access to store
                                                          (%), 2010"),
                                     min = rlow(data$PCT_LACCESS_POP10),
                                     max = rhigh(data$PCT_LACCESS_POP10), value =
                                       valmean(data$PCT_LACCESS_POP10)),
                         sliderInput("MEDHHIN", label = p("Median household income, 2010"),
                                     min = rlow(data$MEDHHINC10),
                                     max = rhigh(data$MEDHHINC10), value =
                                       valmean(data$MEDHHINC10)),
                         sliderInput("PCT18", label = p("% Population under age 18, 2010"),
                                     min = rlow(data$PCT_18YOUNGER10),
                                     max = rhigh(data$PCT_18YOUNGER10),
                                     value = valmean(data$PCT_18YOUNGER10)),
                         sliderInput("RECFAC", label = p("Recreation & fitness facilities/
                                                         1,000 pop, 2012"),
                                     min = rlow(data$RECFACPTH12),
                                     max = rhigh(data$RECFACPTH12), value =
                                       valmean(data$RECFACPTH12))
                         ),

                       box(
                         sliderInput("FOODINS", label = p("Household food insecurity (%,
                                                          three-year average), 2010-12*"),
                                     min = rlow(data$FOODINSEC_10_12),
                                     max = rhigh(data$FOODINSEC_10_12), value =
                                       valmean(data$FOODINSEC_10_12)),
                         sliderInput("FARMRT",label = p("Farmers' markets/1,000 pop, 2013"),
                                     min = rlow(data$FMRKTPTH13),
                                     max = rhigh(data$FMRKTPTH13), value =
                                       valmean(data$FMRKTPTH13)),
                         sliderInput("VEGFARM", label = p("Vegetable farms, 2007"),
                                     min = rlow(data$VEG_FARMS07),
                                     max = rhigh(data$VEG_FARMS07), value =
                                       valmean(data$VEG_FARMS07)),
                         sliderInput("DIABETE", label = p("Adult diabetes rate, 2010"),
                                     min = rlow(data$PCT_DIABETES_ADULTS10),
                                     max = rhigh(data$PCT_DIABETES_ADULTS10), value =
                                       valmean(data$PCT_DIABETES_ADULTS10)),
                         sliderInput("HSACT", label = p("High schoolers physically active (%), 2009"),
                                     min = rlow(data$PCT_HSPA09),
                                     max = rhigh(data$PCT_HSPA09), value =
                                       valmean(data$PCT_HSPA09),
                                     step =  0.1),
                         sliderInput("POVRT", label = p("Poverty rate, 2010"),
                                     min = rlow(data$POVRATE10),
                                     max = rhigh(data$POVRATE10), value =
                                       valmean(data$POVRATE10)),
                         sliderInput("PCT65", label = p("% Population 65 years or older, 2010"),
                                     min = rlow(data$PCT_65OLDER10),
                                     max = rhigh(data$PCT_65OLDER10), value =
                                       valmean(data$PCT_65OLDER10))
                       )
                       ),
                h2("Summary:"),
                p("In this prediction, multiple linear regression model was applied to
                  17 variables to predict obesity rates. Results of stepwise regression
                  showed that at least 10 variables are significant. Basic diagnostics indicate
                  model assumptions were not violated. 76% of the data was complete cases, while
                  the rest had at least one NA. Only the complete cases were used in prediction."),
                
                h3("Results"),
                DT::dataTableOutput("coefs"),
                p(sigcodes),
                p(sumreg),
                
                h2("Diagnostics: VIF"),
                dataTableOutput("rendvifs")
                )
              ),

      tabItem(tabName = "data",
              DT::dataTableOutput("tabdb")
              ),
    
      tabItem(tabName = "about",
              fluidRow(
                column(10, offset = 1,
                       
                       h4("Data Source:"),
                       p(a("Food Environment Atlas", 
                           href="https://www.ers.usda.gov/data-products/food-environment-atlas/data-access-and-documentation-downloads.aspx"),
                         
                         "The current version of the Food Environment Atlas has 211 variables, 
                         including new indicators on store availability; 
                         restaurant availability and expenditures; participants in the SNAP Program, 
                         the National School Lunch Program (NSLP), School Breakfast Program (SBP), 
                         Summer Food Service Program (SFSP), Child and Adult Care Food Program (CACFP),
                         and the WIC Program; adult obesity rate for 2013; recreation and fitness facilities; 
                         and persistent-child-poverty counties"),
                       
                       p("The prevalence of obesity and overweight has increased dramatically in the United States since the mid-1970s, 
                         and nearly two of three adult Americans are either overweight or obese. 
                         Although high costs—in health, social, and economic terms—are known to be associated with obesity, 
                         how the U.S. population reached this point is less well understood."),
                       
                       p('This Shiny App gives users the capability to understand the obesity trends and predict obesity rate. 
                         The App would be useful for organizations of health in disease control and medical research.'),
                       
                       h4("Tools:"),
                       p("Built using Shiny by RStudio"),
                       p("Code available on", a("GitHub", href="https://github.com/iamchuan?tab=repositories")),
                       
                       p("Author: Chuan Hong", a("LinkedIn", href="https://www.linkedin.com/in/iamchuan/"))
                )
            )
      )
    )
))