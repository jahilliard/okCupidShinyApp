library(shiny)
library(shinydashboard)

dashboardPage(skin = "purple",
  dashboardHeader(title = "The Bartenders"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Gender vs. Income", tabName = "genderIncome", icon = icon("dashboard")),
      menuItem("Age vs. Gender", tabName = "ageSex", icon = icon("th-large")),
      menuItem("Gender Breakdown", tabName = "genderUni", icon = icon("user")),
      menuItem("Drug Comparison", tabName = "drugs", icon = icon("pencil")),
      menuItem("Word Clouds", tabName = "text_mining", icon=icon("cloud")),
      menuItem("Sentiment Analysis", tabName = "sentiment", icon = icon("text-width")),
      menuItem("Heatmap", tabName = "heatmap", icon = icon("asterisk")),
      menuItem("MDS", tabName = "mds", icon = icon("chevron-down"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "genderIncome",
              fluidRow(
                
                box(width = 12,
                  checkboxInput(inputId = "side",
                                label = strong("Show as Side-by-Side or Stacked"),
                                value = FALSE),
                   plotOutput(outputId = "genderIncome", height = "300px")#,
              )
      )),
      
      # Second tab content
      tabItem(tabName = "ageSex",
              fluidRow(
                box(width = 12,
                  checkboxInput(inputId = "densities",
                                label = strong("Display Male/Female Density Curves"),
                                value = FALSE),
                  plotOutput(outputId = "ageSex", height = "300px")
                )
              )
      ),
#veer
      tabItem(tabName = "genderUni",
              fluidRow(
                box(width = 12,
                  checkboxInput(inputId = "genderBreakdown",
                                label = strong("Show Gender Breakdown"),
                                value = FALSE),
                  
                  plotOutput(outputId = "genderUni", height = "300px")
                )
              )
            ),


      tabItem(tabName = "drugs",
              fluidRow(
                box(
                  width = 12,
                  plotOutput(outputId = "drugsHeavy", height = "500px", width = "1000px")
                )
              )
      ),
      tabItem(tabName = "text_mining",
              fluidRow(
                box(
                  h3("Common Positive Words Used in Essays"),
                  plotOutput(outputId = "cloudP")
                ),
                box(
                  h3("Common Negative Words Used in Essays"),
                  plotOutput(outputId = "cloudN")
                )
              )
      ),
      tabItem(tabName = "sentiment",
        fluidRow(
          box(width = 12, 
            plotOutput(outputId = "sentplot")
          ),
          box(
            width = 12,
            plotOutput(outputId = "hatelove")
          )
        )
      ),
      tabItem(tabName = "heatmap",
              fluidRow(
                box(
                  width = 12,
                  plotOutput(outputId = "heatmap")
                ),
                box(
                  sliderInput(inputId = "bw1", label = h3("Age Bandwidth"), min = 1, max = 10, 
                  value = 2)
                ),
                box(
                  sliderInput(inputId = "bw2", label = h3("Height Bandwidth"), min = 1, max = 10,
                              value = 1)
                ),
                box(
                  radioButtons(
                    "radio", label = h4("Choose a Subset"), 
                    choices = list("Heavy Drinkers" = TRUE, "Non-Heavy Drinkers" = FALSE),
                    selected = TRUE
                  )
                )
              )
      ),
      tabItem(tabName = "mds",
        fluidRow(
          box(
            actionButton("button", label = "Generate Sample!")
          ),
          box(
            width = 12, 
            plotOutput(outputId = "mds_plot")
          ),
          box(width = 12, 
            radioButtons(
              "radio2", label = h4("Choose a Subset"), 
              choices = list("Heavy Drinkers" = TRUE, "Non-Heavy Drinkers" = FALSE),
              selected = TRUE
            )
          )
        )
      )

    )
  )
)