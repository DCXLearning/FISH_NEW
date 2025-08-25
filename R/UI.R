# ui <- fluidPage(
#   titlePanel("តារាងសរុបការចាប់ត្រីប្រចាំឆ្នាំ"),
# 
#   actionButton("updateFileButton", "📥 ទាញយករបាយការណ៍ Excel (រួមសមុទ្រ + អប្បរាគមន៍)"),
#   br(), br(),
#   tabsetPanel(
#     tabPanel("🐟 តារាងត្រីសមុទ្រ",
#              h4("សរុបត្រីសមុទ្រប្រចាំឆ្នាំ"),
#              DT::dataTableOutput("catch_table"),
#              h4("ស្រះ បែ ស៊ង"),
#              DT::dataTableOutput("aquaculture_pond_table")),
# 
#     tabPanel("🦐 ផលវារីវប្បកម្ម",
#              h4("សរុបទិន្នន័យអប្បរាគមន៍ប្រចាំឆ្នាំ"),
#              DT::dataTableOutput("aquaculture_table"),
#              h4("ផលនេសាទសមុទ្រ"),
#              DT::dataTableOutput("fishing_products_table"),
#              h4("ផលក្នុងវាលស្រែ"),
#              DT::dataTableOutput("fishing_products_rice_field_table"),
#              h4("ផលក្នុងដែននេសាទ"),
#              DT::dataTableOutput("fishing_products_fishery_domain_table"),
#              h4("ផលឧបករណ៍ដាយ"),
#              DT::dataTableOutput("fishing_products_dai_table")),
# 
#     tabPanel("🐟 បទល្មើស",
#              h4("បទល្មើស"),
#              DT::dataTableOutput("law_enforcement_table")),
#   )
# )


library(shiny)
library(shinyjs)
library(DT) # Assuming you use DT for tables

ui <- fluidPage(
  shinyjs::useShinyjs(),

  # --- CSS Styling from the sample code ---
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Arial', sans-serif;
        overflow-x: hidden;
        margin: 0;
        padding: 0;
      }
      .fixed-header {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        background-color: #e44d26;
        color: white;
        padding: 10px 20px;
        z-index: 1050;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
        display: flex;
        align-items: center;
        box-sizing: border-box;
        height: 100px;
        flex-wrap: wrap;
      }
      .header-top-row {
          display: flex;
          align-items: center;
          width: 100%;
          margin-bottom: 5px;
      }
      .header-logo {
        height: 30px;
        margin-right: 15px;
      }
      .header-title {
        font-size: 20px;
        font-weight: bold;
        margin-right: auto;
      }
      .sidebar {
        position: fixed;
        width: 260px;
        height: 100%;
        background-color: #2c3e50;
        padding: 20px;
        color: white;
        box-shadow: 2px 0 5px rgba(0,0,0,0.1);
        overflow-y: auto;
        top: 0;
        left: 0;
        transition: transform 0.3s ease-in-out;
        z-index: 2000;
        box-sizing: border-box;
        padding-top: 60px;
      }
      .sidebar.hidden {
        transform: translateX(-260px);
        z-index: 1000;
      }
      .main-content {
        margin-left: 260px;
        padding: 20px;
        background-color: #f8f9fa;
        transition: margin-left 0.3s ease-in-out;
        min-height: 100vh;
        box-sizing: border-box;
        padding-top: calc(100px + 20px);
      }
      .main-content.full-width {
        margin-left: 0;
      }
      #closeSidebarBtn {
        position: absolute;
        top: 15px;
        right: 15px;
        z-index: 2001;
        background-color: transparent;
        color: white;
        border: none;
        font-size: 28px;
        padding: 0;
        cursor: pointer;
        transition: color 0.3s ease-in-out;
        outline: none !important;
        box-shadow: none !important;
      }
      #closeSidebarBtn:hover {
        color: #18bc9c;
      }
      #openSidebarBtn {
        background-color: transparent;
        color: white;
        border: none;
        font-size: 28px;
        padding: 0;
        cursor: pointer;
        transition: color 0.3s ease-in-out;
        outline: none !important;
        box-shadow: none !important;
        margin-left: 10px;
      }
      #openSidebarBtn:hover {
        color: #18bc9c;
      }
      .fixed-header .tabbable {
        width: 100%;
        background-color: transparent;
      }
      .fixed-header .nav-tabs {
        border-bottom: none;
        background-color: #c0392b;
      }
      .fixed-header .nav-tabs > li > a {
        color: white;
        border-radius: 0;
        border: none;
        padding: 8px 15px;
        margin-right: 0;
      }
      .fixed-header .nav-tabs > li.active > a,
      .fixed-header .nav-tabs > li.active > a:hover,
      .fixed-header .nav-tabs > li.active > a:focus {
        color: #fff;
        background-color: #a02012;
        border: none;
        border-bottom: 3px solid #18bc9c;
      }
      .fixed-header .nav-tabs > li > a:hover,
      .fixed-header .nav-tabs > li > a:focus {
        background-color: #d14836;
        border-color: transparent;
      }
      .tab-content {
        display: none;
      }
      .sidebar h3 { color: #ecf0f1; margin-bottom: 25px; }
      .sidebar h4 { color: #18bc9c; margin-top: 30px; border-bottom: 1px solid #4a627d; padding-bottom: 5px;}
      .sidebar label { color: #bdc3c7; font-weight: bold; margin-bottom: 5px; display: block; }
      .sidebar select, .sidebar input[type='checkbox'] {
        width: 100%; margin-bottom: 15px; padding: 8px; border-radius: 5px;
        background-color: #34495e; color: white; border: 1px solid #4a627d;
        box-sizing: border-box;
      }
      .sidebar input[type='checkbox'] { width: auto; margin-left: 0; }
      .sidebar .selectize-input, .sidebar .selectize-dropdown {
        background-color: #34495e !important; color: white !important; border-color: #4a627d !important;
      }
      .sidebar .radio-inline { color: #bdc3c7; }
      h4 { color: #34495e; margin-top: 20px; margin-bottom: 15px; }
      .dataTables_wrapper .dataTables_filter input {
        border: 1px solid #ced4da; border-radius: .25rem; padding: .375rem .75rem; width: auto;
      }
      .btn-success { background-color: #18bc9c; border-color: #18bc9c; color: white; font-weight: bold; }
      .btn-success:hover { background-color: #15a88c; border-color: #15a88c; }
      .action-button:focus,
      .action-button:active {
        outline: none !important;
        box-shadow: none !important;
      }
      .btn-primary {
        background-color: #3498db;
        border-color: #3498db;
        color: white;
        font-weight: bold;
      }
      .btn-primary:hover {
        background-color: #2980b9;
        border-color: #2980b9;
      }
    "))
  ),

  # --- Fixed Header/Navbar at the very top ---
  div(class = "fixed-header",
      div(class = "header-top-row",
          actionButton("openSidebarBtn", "", icon = icon("bars")),
          # To add your custom logo and title, uncomment these lines and place the image in the 'www' folder
          # tags$img(src = "Picture1.jpg", class = "header-logo"),
          span(" ", class = "header-title")
      ),
      tabsetPanel(
        id = "mainTabs",
        tabPanel("T06_ប្រភេទត្រីសមុទ្រ"),
        tabPanel("T10_ស្រះ បែ ស៊ង"),
        tabPanel("T07_ផលវារីវប្បកម្ម"),
        tabPanel("T05_ផលនេសាទសមុទ្រ"),
        tabPanel("T04_ផលក្នុងវាលស្រែ"),
        tabPanel("T03_ផលក្នុងដែននេសាទ"),
        tabPanel("T02_ផលឧបករណ៍ដាយ"),
        tabPanel("T08_បទល្មើស")
      )
  ),

  # --- Collapsible Sidebar with Filters and Download Buttons ---
  div(id = "sidebarPanel", class = "sidebar",
      actionButton("closeSidebarBtn", "", icon = icon("times")),
      h3("Filters"),
      selectInput("year", "Select Year", choices = year_choices), # Replace with your year_choices
      selectInput("quarter", "Select Quarter", choices = quarter_choices),
      selectInput("month", "Select Month", choices = month_choices), # Replace with your province_choices
      selectInput("week", "Select Week", choices = week_choices),
      selectInput("province", "Select Province", choices = province_choices),
      
      checkboxInput("show_all", "Show All Rows", value = FALSE),

      tags$hr(),
      h4("Download Reports"),
      actionButton("updateFileButton", "Download Combined Excel", class = "btn-success"),
      br(), br(),

  ),

  # --- Main Content Area ---
  div(
    id = "mainContent",
    class = "main-content",
    
    conditionalPanel(
      condition = "input.mainTabs == 'T06_ប្រភេទត្រីសមុទ្រ'",
      h4("Marine Fish Catch Summary (តារាងត្រីសមុទ្រ)"),
      DTOutput("catch_table")
    ),
    
    conditionalPanel(
      condition = "input.mainTabs == 'T10_ស្រះ បែ ស៊ង'",
      h4("Aquaculture Ponds & Cages (ស្រះ បែ ស៊ង)"),
      DTOutput("aquaculture_pond_table")
    ),
    
    conditionalPanel(
      condition = "input.mainTabs == 'T07_ផលវារីវប្បកម្ម'",
      h4("Aquaculture Products (ផលវារីវប្បកម្ម)"),
      DTOutput("aquaculture_table")
    ),
    
    conditionalPanel(
      condition = "input.mainTabs == 'T05_ផលនេសាទសមុទ្រ'",
      h4("Marine Fishing Products (ផលនេសាទសមុទ្រ)"),
      DTOutput("fishing_products_table")
    ),
    
    conditionalPanel(
      condition = "input.mainTabs == 'T04_ផលក្នុងវាលស្រែ'",
      h4("Rice Field Catch (ផលក្នុងវាលស្រែ)"),
      DTOutput("fishing_products_rice_field_table")
    ),
    
    conditionalPanel(
      condition = "input.mainTabs == 'T03_ផលក្នុងដែននេសាទ'",
      h4("Fishery Domain Catch (ផលក្នុងដែននេសាទ)"),
      DTOutput("fishing_products_fishery_domain_table")
    ),
    
    conditionalPanel(
      condition = "input.mainTabs == 'T02_ផលឧបករណ៍ដាយ'",
      h4("Dai Fishing Catch (ផលឧបករណ៍ដាយ)"),
      DTOutput("fishing_products_dai_table")
    ),
    
    conditionalPanel(
      condition = "input.mainTabs == 'T08_បទល្មើស'",
      h4("Law Enforcement Incidents (បទល្មើស)"),
      DTOutput("law_enforcement_table")
    )
  )
  
)
