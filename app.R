# MSNA Mali 2023 dashboard data collection

library(tidyverse)
library(sf)
library(leaflet)
library(fresh)

admin1_shape <- read_sf("shapefiles/mli_admbnda_adm1_1m_gov_20211220.shp")

admin2_shape <- read_sf("shapefiles/mli_admbnda_adm2_1m_gov_20211220.shp")

# Interviews --------------------------------------------------------------

interviews_region <- sample.int(1000, nrow(admin1_shape))

interviews_cercle <- sample.int(1000, nrow(admin2_shape))

admin1_shape$n_interviews <- interviews_region

admin2_shape$n_interviews <- interviews_cercle

bins_admin1 <- c(0, 10, 20, 50, 100, 200, 500, 1000)
pal_admin1 <- colorBin("Blues", domain = admin1_shape$n_interviews, bins = bins_admin1)

labels_admin1 <- sprintf(
  "<strong>%s</strong><br/>%g Enquêtes",
  admin1_shape$ADM1_FR, admin1_shape$n_interviews
) %>% lapply(htmltools::HTML)

bins_admin2 <- c(0, 10, 20, 50, 100, 200, 500, 1000)
pal_admin2 <- colorBin("Blues", domain = admin2_shape$n_interviews, bins = bins_admin2)

labels_admin2 <- sprintf(
  "<strong>%s</strong><br/>%g Enquêtes",
  admin2_shape$ADM2_FR, admin2_shape$n_interviews
) %>% lapply(htmltools::HTML)

# Summary stats -----------------------------------------------------------

n_overall <- 1243
n_overall_total <- 10012

n_overview_summary <- paste(paste(n_overall, n_overall_total, sep = " / "), 
                            paste0(" (",paste0(round(n_overall/n_overall_total*100, digits = 2), "%"), sep = ""), ")", sep = "")

n_pdi <- 392
n_pdi_total <- 1109

n_pdi_summary <- paste(paste(n_pdi, n_pdi_total, sep = " / "), 
                       paste0(" (",paste0(round(n_pdi/n_pdi_total*100, digits = 2), "%"), sep = ""), ")", sep = "")

n_nonpdi <- 2028
n_nonpdi_total <- 9031

n_nonpdi_summary <- paste(paste(n_nonpdi, n_nonpdi_total, sep = " / "), 
                          paste0(" (",paste0(round(n_nonpdi/n_nonpdi_total*100, digits = 2), "%"), sep = ""), ")", sep = "")

# Dates  ------------------------------------------------------------------


collection_dates <- seq(as.Date('01/06/2023',
                                format = "%d/%m/%Y"), 
                        as.Date('01/08/2023',
                                format = "%d/%m/%Y"), by = 'days')

collection_n_interviews <- sample.int(100, length(collection_dates))

fake_df <- data.frame(day = collection_dates,
                      n_interviews = collection_n_interviews)

fake_df %>% 
  slice_tail(n = 10) %>% 
  ggplot(aes(x = day, y = n_interviews)) +
  geom_col()

# App ---------------------------------------------------------------------

library(shinydashboard)

ui <- dashboardPage(
  
  skin = "black",
  dashboardHeader(title = "Mali MSNA 2023",
                  titleWidth = 200),
  dashboardSidebar(
    collapsed = TRUE,
    sidebarMenu(
      menuItem("Apercu", tabName = "apercu", icon = icon("earth-africa")),
      menuItem("Communautés hôtes", tabName = "nonpdi", icon = icon("house")),
      menuItem("PDI", tabName = "pdi", icon = icon("people-group"))
    )),
  dashboardBody(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    use_googlefont("Roboto Condensed"),
    use_theme(create_theme(
      theme = "default",
      bs_vars_font(
        family_sans_serif = "Roboto Condensed"
      )
    )),
    
    tabItems(
      # First tab content
      tabItem(tabName = "apercu",
              
              fluidRow(
                valueBox(value = "Aperçu",
                         subtitle = p("Ce dashboard a pour but de donner à tous les partenaires impliqués un aperçu de la collecte de données en cours pour le MSNA 2023. Le dashboard est structuré de la manière suivante : Sur la page actuelle, vous trouverez un aperçu global de la collecte de données. Les cartes sont interactives. C'est-à-dire si vous les survolez avec votre curseur, le nombre d'enquêtes réalisées s'affiche. En cliquant sur le bouton à droite du titre, vous trouvez des chiffres sur les différents groupes de population.", br(), "Si vous avez des questions ou des suggestions, vous pouvez contacter l'adresse suivante :", a("colin.walder@impact-initiatives.org", href="mailto:colin.walder@impact-initiatives.org", style = "color:lightgrey;")),
                         icon = NULL,
                         width = 12,
                         color = "light-blue")
              ),
              
              fluidRow(
                valueBox(h4("Enquêtes totales"), h3(n_overview_summary), 
                         color = "light-blue"),
                valueBox(h4("Enquêtes PDI"), h3(n_pdi_summary), 
                         color = "light-blue"),
                valueBox(h4("Enquêtes Non-PDI"), h3(n_nonpdi_summary), 
                         color = "light-blue")
              ),
              
              fluidRow(
                box(leafletOutput("admin1_plot", height = 500),
                    status = "primary",
                    title = strong("Couverture Niveau Région"),
                    width = 6,
                    solidHeader = TRUE),
                
                box(leafletOutput("admin2_plot", height = 500), 
                    status = "info",
                    title = strong("Couverture Niveau Cercle"),
                    width = 6,
                    solidHeader = TRUE)), 
              
              fluidRow(
                box(plotOutput("dates_collection"),
                    status = "primary",
                    width = 12,
                    title = strong("Nombre d'enquêtes effectuées les derniers 10 jours"))
              )
              
      ),
      
      # Second tab content
      tabItem(tabName = "nonpdi",
              
              fluidRow(
                
                box(leafletOutput("admin1_plot_nonpdi", height = 500),
                    status = "primary",
                    title = strong("Couverture Niveau Région"),
                    width = 6,
                    solidHeader = TRUE),
                
                box(leafletOutput("admin2_plot_nonpdi", height = 500), 
                    status = "info",
                    title = strong("Couverture Niveau Cercle"),
                    width = 6,
                    solidHeader = TRUE)
                
                
              ), 
              
              fluidRow(
                box(plotOutput("dates_collection_nonpdi"),
                    status = "primary",
                    width = 12,
                    title = strong("Nombre d'enquêtes effectuées les derniers 10 jours"))
              )
              
      ),
      
      # Third tab content
      tabItem(tabName = "pdi",
              
              fluidRow(
                
                box(leafletOutput("admin1_plot_pdi", height = 500),
                    status = "primary",
                    title = strong("Couverture Niveau Région"),
                    width = 6,
                    solidHeader = TRUE),
                
                box(leafletOutput("admin2_plot_pdi", height = 500), 
                    status = "info",
                    title = strong("Couverture Niveau Cercle"),
                    width = 6,
                    solidHeader = TRUE)
                
                
              ),
              
              fluidRow(
                box(plotOutput("dates_collection_pdi"),
                    status = "primary",
                    width = 12,
                    title = strong("Nombre d'enquêtes effectuées les derniers 10 jours"))
              )
      )
    )
  )
  
)

server <- function(input, output) {
  
  # main tab
  
  output$admin1_plot <- renderLeaflet({
    
    leaflet(admin1_shape) %>% 
      setView(lng = -3,
              lat = 16.77,
              zoom = 5) %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      addPolygons(
        fillColor = ~pal_admin1(n_interviews),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "white",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels_admin1,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"))
    
  })
  
  output$admin2_plot <- renderLeaflet({
    
    leaflet(admin2_shape) %>% 
      setView(lng = -3,
              lat = 16.77,
              zoom = 5) %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      addPolygons(
        fillColor = ~pal_admin2(n_interviews),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "white",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels_admin2,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"))
    
  })
  
  output$dates_collection <- renderPlot({
    
    fake_df %>% 
      slice_tail(n = 10) %>% 
      ggplot(aes(x = day, y = n_interviews, label = n_interviews)) +
      geom_col(fill = blues9[9]) +
      geom_label() +
      labs(x = "",
           y = "") +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 15),
            axis.title.y = element_text(size = 15))
    
  })
  
  # non-pdi tab
  
  output$admin1_plot_nonpdi <- renderLeaflet({
    
    leaflet(admin1_shape) %>% 
      setView(lng = -3,
              lat = 16.77,
              zoom = 5) %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      addPolygons(
        fillColor = ~pal_admin1(n_interviews),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "white",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels_admin1,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"))
    
  })
  
  output$admin2_plot_nonpdi <- renderLeaflet({
    
    leaflet(admin2_shape) %>% 
      setView(lng = -3,
              lat = 16.77,
              zoom = 5) %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      addPolygons(
        fillColor = ~pal_admin2(n_interviews),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "white",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels_admin2,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"))
    
  })
  
  output$dates_collection_nonpdi <- renderPlot({
    
    fake_df %>% 
      slice_tail(n = 10) %>% 
      ggplot(aes(x = day, y = n_interviews, label = n_interviews)) +
      geom_col(fill = blues9[9]) +
      geom_label() +
      labs(x = "",
           y = "") +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 15),
            axis.title.y = element_text(size = 15))
    
  })
  
  # pdi tab
  
  output$admin1_plot_pdi <- renderLeaflet({
    
    leaflet(admin1_shape) %>% 
      setView(lng = -3,
              lat = 16.77,
              zoom = 5) %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      addPolygons(
        fillColor = ~pal_admin1(n_interviews),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "white",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels_admin1,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"))
    
  })
  
  output$admin2_plot_pdi <- renderLeaflet({
    
    leaflet(admin2_shape) %>% 
      setView(lng = -3,
              lat = 16.77,
              zoom = 5) %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      addPolygons(
        fillColor = ~pal_admin2(n_interviews),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "white",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels_admin2,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"))
    
  })
  
  output$dates_collection_pdi <- renderPlot({
    
    fake_df %>% 
      slice_tail(n = 10) %>% 
      ggplot(aes(x = day, y = n_interviews, label = n_interviews)) +
      geom_col(fill = blues9[9]) +
      geom_label() +
      labs(x = "",
           y = "") +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 15),
            axis.title.y = element_text(size = 15))
    
  })
  
}

shinyApp(ui, server)


