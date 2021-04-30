# Libraries ---------------------------------------------------------------------------------------
## Uncomment line below to install necessary packages before launching the app.
#install.packages(c("shiny", "shinydashboard", "tidyverse", "ggplot2", "leaflet", "broom", "kableExtra", "janitor"))

library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(leaflet)
library(broom)
library(kableExtra)
library(janitor)

# Data --------------------------------------------------------------------------------------------
division_data <- read_csv("Data/Division_data.csv")
divisions <- readRDS("Data/map.RDS")
table_data <- readRDS("Data/table_data.RDS")
demo_data <- readRDS("Data/demo_data.RDS")
val_data <- readRDS("Data/val_data.RDS")
model_data <- readRDS("Data/model_data.RDS")

# Data Preparation --------------------------------------------------------------------------------
combined_data <- division_data %>%
    left_join(divisions@data, by = c("DivisionNm" = "Elect_div"))

divisions@data <- divisions@data %>%
    left_join(division_data, by = c("Elect_div" = "DivisionNm"))

full_model <- lm(data = model_data, `Liberal Vote` ~ StateAb +
                     `Couple without children` +
                     `One parent family` +
                     `Median weekly household income` +
                     `Medain monthly mortgage repayments` +
                     `Homes rented` +
                     `Other tenure type` +
                     `Proportion born overseas` +
                     `Recent Migrant` +
                     `Employed as a manager` +
                     `Employed as a professional` +
                     `Employed as a technician or trade worker` +
                     `Community and Personal Service Workers` +
                     `Employed as a cleric or administrative worker` +
                     `Employed as a machinery operator or driver` +
                     `Employed as a labourer` +
                     `Proportion with Year 12 or equivalent qualification` +
                     `Proportion with Cert III or higher qualification` +
                     `Voter Age` +
                     `Proportion engaged in work, training or education`)

estimates <- tidy(full_model) %>%
    dplyr::select(term, estimate) %>%
    pivot_wider(names_from = term, values_from = estimate) %>%
    clean_names()

# UI ----------------------------------------------------------------------------------------------

ui <- dashboardPage(
    dashboardHeader(title = "Predictors of Australian Election Results", titleWidth = "500px"),
    dashboardSidebar(disable = TRUE),
    dashboardBody(box(title = "2019 Australian Federal Election Results", width = 12, solidHeader = TRUE, status = "primary",
                      fluidRow(box(fluidRow(
                                      valueBoxOutput("elect_lib", width = 6),
                                      valueBoxOutput("elect_lab", width = 6)),
                                      br(),
                                      plotOutput("elect_hist", height = "800px"),
                                      br(),
                                      box(title = "Data Sources", background = "red", width = 12,
                                          p("Australian Bureau of Statistics. 2082.0 -", em("Discover Your Commonwealth Electoral Division, Australia, 2019."), "(2019). https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/2082.02019?OpenDocument"),
                                          p("Australian Electoral Commission (2018).", em("Federal electoral boundary GIS data for free download."), "Australian Electoral Commission. https://www.aec.gov.au/Electorates/gis/gis_datadownload.htm"),
                                          p("Australian Electoral Commission. (2019).", em("House of representatives downloads - AEC Tally Room."), "https://results.aec.gov.au/24310/Website/HouseDownloadsMenu-24310-Csv.htm")),
                                    width = 3),
                                column(box(title = "Info", background = "blue", width = 12, p("The 2019 Australian federal election was one of the most unexpected in recent memory, which Liberal Party presentative Scott Morrison defying the predictions of the media and pollsters to become Australia's next Prime Minister. This visualisation dashboard will explore in three parts why this was the case and what factors contributed to his victory. The first panel displays the distribution of election results in Australia for 2019 on a two-party-preferred basis. On the left the results for the Liberal Party and Labor Party are shown overall and by state, as can be seen the main states contributing to his victory were Queensland and Western Australia. The map in the middle is interactive. Zooming in and clicking on one of Australia's 151 electorates will add its details to the table on the right. Additionally, this table can be updated by using the dropdown list in the top right-hand corner. To remove an electorate simply click on its name and press backspace. From this map it is apparent that the Liberal party performed better in regional areas compared to metropoliton centres.")),
                                       box(leafletOutput("elect_map", height = "1000px"),
                                           width = 12),
                                           width = 6),
                                box(selectizeInput("elect", "Choose Electorate", choices = unique(combined_data$DivisionNm), multiple = TRUE, selected = "La Trobe"),
                                    tableOutput("elect_table"),
                                    width = 3))
                      ),
                  box(title = "Distribution of Australian Demographic Statistics", width = 12, solidHeader = TRUE, status = "primary",
                      fluidRow(box(selectInput("demos", label = "Choose Demographic Variable", choices = sort(colnames(demo_data))[c(-3,-20, -21)]),
                                   box(
                                       valueBoxOutput("demo_nat", width = 12),
                                       plotOutput("nat_dist", height = "200px"),
                                       width = 12, height = "353px"),
                                   box(
                                       valueBoxOutput("demo_lib", width = 12),
                                       plotOutput("lib_dist", height = "200px"),
                                       width = 12, height = "353px"),
                                   box(
                                       valueBoxOutput("demo_lab", width = 12),
                                       plotOutput("lab_dist", height = "200px"),
                                       width = 12, height = "353px"),
                                   width = 3),
                                    column(box(title = "Info", background = "blue", width = 12, p("One possible explanation for the difference in results may be due to the distribution of various demographic statistics throughout Australia. The second panel displays this distribution across the country's 151 electorates. Click the 'View' button to explore. The different varaiables can be selected for display by using the dropdown list in the top left-hand corner. This selection will update the content on the left which contains the distribution and average for the chosen statistic nationally and also for Liberal and Labor seats respectively. The geographical distribution of this variables concentration will also be updated and shown in the central map which can also be interacted with. These visualisations convey the differences between seats that votes Liberal and those that voted Labor. Taking the default option 'Couple with childrem' for example, it appears that seats that supported Scott Morrison had fewer families consisting of couples with children compared to the national average while those that votes Labor had more. Similar findings can be explored for all other demographic variables. These results can then also be compared to the two party preferred map above."), actionButton("view", "View")),
                                           box(leafletOutput("demo_map", height = "1000px"),
                                            width = 12),
                                           width = 9))),
                  box(title = "Model Predicted Outcome of Australian Election",  width = 12, solidHeader = TRUE, status = "primary",
                        fluidRow(
                            box(
                                fluidRow(valueBoxOutput("mod_lib", width = 6),
                                         valueBoxOutput("mod_lab", width = 6)),
                                         br(),
                                         sliderInput("age", "Average Voter Age", min = round(min(model_data$`Voter Age`), digits = 0), max = round(max(model_data$`Voter Age`), digits = 0), value = mean(model_data$`Voter Age`)),
                                         sliderInput("income", "Median Weekly Household Income", min = round(min(model_data$`Median weekly household income`), digits = 0), max = round(max(model_data$`Median weekly household income`), digits = 0), value = mean(model_data$`Median weekly household income`)),
                                         sliderInput("overseas", "Proportion Born Overseas", min = round(min(model_data$`Proportion born overseas`), digits = 2), max = round(max(model_data$`Proportion born overseas`), digits = 2), value = mean(model_data$`Proportion born overseas`)),
                                         sliderInput("nochildren", "Proportion of Families without Children", min = round(min(model_data$`Couple without children`), digits = 2), max = round(max(model_data$`Couple without children`), digits = 2), value = mean(model_data$`Couple without children`)),
                                         sliderInput("noparent", "Proportion of Single Parent Families", min = round(min(model_data$`One parent family`), digits = 2), max = round(max(model_data$`One parent family`), digits = 2), value = mean(model_data$`One parent family`)),
                                         sliderInput("year12", "Proportion with Year 12 Qualification (equivalent)", min = round(min(model_data$`Proportion with Year 12 or equivalent qualification`), digits = 2), max = round(max(model_data$`Proportion with Year 12 or equivalent qualification`), digits = 2), value = mean(model_data$`Proportion with Year 12 or equivalent qualification`)),
                                         sliderInput("cert3", "Proportion with Cert III Qualification (equivalent)", min = round(min(model_data$`Proportion with Cert III or higher qualification`), digits = 2), max = round(max(model_data$`Proportion with Cert III or higher qualification`), digits = 2), value = mean(model_data$`Proportion with Cert III or higher qualification`)),
                                         sliderInput("engaged", "Proportion Engaged in Work, Training or Education", min = round(min(model_data$`Proportion engaged in work, training or education`), digits = 2), max = round(max(model_data$`Proportion engaged in work, training or education`), digits = 2), value = mean(model_data$`Proportion engaged in work, training or education`)),
                                         sliderInput("rented", "Proportion of Homes Rented", min = round(min(model_data$`Homes rented`), digits = 2), max = round(max(model_data$`Homes rented`), digits = 2), value = mean(model_data$`Homes rented`)),
                                         sliderInput("mortgage", "Median Monthly Mortgage Repayment", min = round(min(model_data$`Medain monthly mortgage repayments`), digits = 0), max = round(max(model_data$`Medain monthly mortgage repayments`), digits = 0), value = mean(model_data$`Medain monthly mortgage repayments`)),
                                         actionButton("reset", "Reset Values"),width = 3),
                                    column(box(title = "Info", background = "blue", width = 12, p("With the findings from above in mind it appears that the demographics of an electorate and the way it votes are associated. Thus, the third and final panel displays the projected output for the election based on the demographic variables available. Click the 'View' button to explore. This projection is based on a multiple linear model - that is, the percentage votes for Scott Morrison are calculated by adding and subtracting weights for these various factors. The sliders on the left hand side contain some of these variables from the model that were found to have a significant impact in predicting the election outcome of an electorate. These values can all be adjusted by dragging the sliders to the left or the right. Doing so will recalculate the projection based on the chosen values, updating both the boxes containing the seats won and the map in the centre of the screen. This allows forecasts to be made to determine how the election would have swung if these variables were different. Taking the first slider, 'Voter Age', for example, if the average age of Australian voters were decreased by only 1 year the model predicts that Labor would instead win 86 to 65. This indicates a strong preference amognst younger voters for the Labor party. Similar findings can be explored for all other provided variables To return all sliders to their original values, simply click the 'Reset Values' button in the bottom left-hand corner of the screen."), actionButton("view2", "View")),
                                           box(leafletOutput("model_plot", height = "1000px"),
                                               width = 12),
                                           width = 9))),
                  tags$head(tags$style(HTML('.content-wrapper { overflow: auto; }'))) # Fixes issue with background ending before content
                  )
)

# Server ------------------------------------------------------------------------------------------

server <- function(input, output, session) {

    # 1st Panel -----------------------------------------------------------------------------------

        # Allow map to be updated by selectInput and Leaflet --------------------------------------

    observeEvent(input$elect, {
        updateSelectInput(session, "elect", "Choose Electorate", choices = unique(combined_data$DivisionNm), selected = input$elect)
    })

    current_selection <- reactiveVal(NULL)

    observeEvent(input$elect, {
        current_selection(input$elect)
    })


    observeEvent(input$elect_map_shape_click, {
        click <- input$elect_map_shape_click
        updateSelectInput(session, "elect", "Choose Electorate",
                          choices = unique(combined_data$DivisionNm),
                          selected = c(click$id, current_selection()))
    })

        # Create histogram of results faceted by each state ---------------------------------------

    output$elect_hist <- renderPlot({
        combined_data %>%
            ggplot(aes(y = as.factor(Preferred),fill = as.factor(Preferred))) +
            geom_bar() +
            stat_count(geom = "text", colour = "white", size = 4.5, aes(label = ..count.., x = ..count.. -1)) +
            facet_wrap(~StateAb, ncol = 1) +
            scale_fill_manual(values = c("#e15759", "#4e79a7")) +
            theme_bw() +
            labs(x = "", y = "") +
            theme(legend.position = "none",
                  axis.ticks = element_blank(),
                  axis.text.x = element_blank(),
                  panel.grid = element_blank(),
                  axis.text.y = element_text(size = 16)) +
            scale_x_continuous(expand = expansion(mult = c(0,.1)))
    })

        # Create choropleth map to display election results ---------------------------------------

    output$elect_map <- renderLeaflet({
        colours <- combined_data$Preferred[match(divisions$Elect_div, combined_data$DivisionNm)]
        cpal <- colorFactor(palette = c("#e15759", "#4e79a7"), colours)

        leaflet(divisions) %>%
            addTiles() %>%
            addPolygons(color = "white",
                        opacity = 1,
                        weight = 0.2,
                        smoothFactor = 0.2,
                        fillOpacity = 1,
                        fillColor = ~cpal(colours),
                        layerId = ~Elect_div,
                        label = paste0(divisions@data$Elect_div, " ", divisions@data$`Liberal/National Coalition Percentage`, "%")) %>%
            fitBounds(lat1 = -42, lng1 = 113,lat2 = -11, lng2 = 152)

    })

        # Create value boxes showing the counts of seats won for each party -----------------------

    output$elect_lib <- renderValueBox({
        valueBox(nrow(combined_data[which(combined_data$Preferred == "Liberal"),]), "Liberal", color = "blue", icon = icon("vote-yea"))
    })

    output$elect_lab <- renderValueBox({
        valueBox(nrow(combined_data[which(combined_data$Preferred == "Labor"),]), "Labor", color = "red", icon = icon("vote-yea"))
    })

        # Create table showing details for each electorate selected -------------------------------

    output$elect_table <- function(){
        table_data %>%
            filter(DivisionNm %in% input$elect) %>%
            pivot_longer(cols = State:`Employed as a Labourer`) %>%
            pivot_wider(names_from = DivisionNm) %>%
            rename(Variable = name) %>%
            kable(caption = "Electorate information") %>%
            kable_styling(bootstrap_options = c("striped", "hover"))
    }

    # 2nd Panel -----------------------------------------------------------------------------------

    output$demo_map <- renderLeaflet({

        req(input$view)

        # Create colour palette based on which demographic is chosen ------------------------------

        if(input$demos == "Couple with children"){
        colours <- demo_data$`Couple with children`[match(divisions$Elect_div, demo_data$DivisionNm)]
        cpal <- colorNumeric("YlOrRd", colours)
        }

        if(input$demos == "Couple without children"){
            colours <- demo_data$`Couple without children`[match(divisions$Elect_div, demo_data$DivisionNm)]
            cpal <- colorNumeric("YlOrRd", colours)
        }

        if(input$demos == "DivisionNm"){
            colours <- demo_data$DivisionNm[match(divisions$Elect_div, demo_data$DivisionNm)]
            cpal <- colorNumeric("YlOrRd", colours)
        }

        if(input$demos == "Employed as a cleric or administrative worker"){
            colours <- demo_data$`Employed as a cleric or administrative worker`[match(divisions$Elect_div, demo_data$DivisionNm)]
            cpal <- colorNumeric("YlOrRd", colours)
        }

        if(input$demos == "Employed as a labourer"){
            colours <- demo_data$`Employed as a labourer`[match(divisions$Elect_div, demo_data$DivisionNm)]
            cpal <- colorNumeric("YlOrRd", colours)
        }

        if(input$demos == "Employed as a machinery operator or driver"){
            colours <- demo_data$`Employed as a machinery operator or driver`[match(divisions$Elect_div, demo_data$DivisionNm)]
            cpal <- colorNumeric("YlOrRd", colours)
        }

        if(input$demos == "Employed as a manager"){
            colours <- demo_data$`Employed as a manager`[match(divisions$Elect_div, demo_data$DivisionNm)]
            cpal <- colorNumeric("YlOrRd", colours)
        }

        if(input$demos == "Employed as a professional"){
            colours <- demo_data$`Employed as a professional`[match(divisions$Elect_div, demo_data$DivisionNm)]
            cpal <- colorNumeric("YlOrRd", colours)
        }

        if(input$demos == "Employed as a sales worker"){
            colours <- demo_data$`Employed as a sales worker`[match(divisions$Elect_div, demo_data$DivisionNm)]
            cpal <- colorNumeric("YlOrRd", colours)
        }

        if(input$demos == "Employed as a technician or trade worker"){
            colours <- demo_data$`Employed as a technician or trade worker`[match(divisions$Elect_div, demo_data$DivisionNm)]
            cpal <- colorNumeric("YlOrRd", colours)
        }

        if(input$demos == "Employed in community and personal service"){
            colours <- demo_data$`Employed in community and personal service`[match(divisions$Elect_div, demo_data$DivisionNm)]
            cpal <- colorNumeric("YlOrRd", colours)
        }

        if(input$demos == "Homes owned with a mortgage"){
            colours <- demo_data$`Homes owned with a mortgage`[match(divisions$Elect_div, demo_data$DivisionNm)]
            cpal <- colorNumeric("YlOrRd", colours)
        }

        if(input$demos == "Homes rented"){
            colours <- demo_data$`Homes rented`[match(divisions$Elect_div, demo_data$DivisionNm)]
            cpal <- colorNumeric("YlOrRd", colours)
        }

        if(input$demos == "Homes with LOTE spoken"){
            colours <- demo_data$`Homes with LOTE spoken`[match(divisions$Elect_div, demo_data$DivisionNm)]
            cpal <- colorNumeric("YlOrRd", colours)
        }

        if(input$demos == "Medain monthly mortgage repayments"){
            colours <- demo_data$`Medain monthly mortgage repayments`[match(divisions$Elect_div, demo_data$DivisionNm)]
            cpal <- colorNumeric("YlOrRd", colours)
        }

        if(input$demos == "Median weekly household income" ){
            colours <- demo_data$`Median weekly household income`[match(divisions$Elect_div, demo_data$DivisionNm)]
            cpal <- colorNumeric("YlOrRd", colours)
        }

        if(input$demos == "Median weekly rent"){
            colours <- demo_data$`Median weekly rent`[match(divisions$Elect_div, demo_data$DivisionNm)]
            cpal <- colorNumeric("YlOrRd", colours)
        }

        if(input$demos == "One parent family"){
            colours <- demo_data$`One parent family`[match(divisions$Elect_div, demo_data$DivisionNm)]
            cpal <- colorNumeric("YlOrRd", colours)
        }

        if(input$demos == "Other family"){
            colours <- demo_data$`Other family`[match(divisions$Elect_div, demo_data$DivisionNm)]
            cpal <- colorNumeric("YlOrRd", colours)
        }

        if(input$demos == "Proportion Aboriginal/TSI"){
            colours <- demo_data$`Proportion Aboriginal/TSI`[match(divisions$Elect_div, demo_data$DivisionNm)]
            cpal <- colorNumeric("YlOrRd", colours)
        }

        if(input$demos == "Proportion born overseas"){
            colours <- demo_data$`Proportion born overseas`[match(divisions$Elect_div, demo_data$DivisionNm)]
            cpal <- colorNumeric("YlOrRd", colours)
        }

        if(input$demos == "Proportion engaged in work, training or education"){
            colours <- demo_data$`Proportion engaged in work, training or education`[match(divisions$Elect_div, demo_data$DivisionNm)]
            cpal <- colorNumeric("YlOrRd", colours)
        }

        if(input$demos == "Proportion with Cert III or higher qualification"){
            colours <- demo_data$`Proportion with Cert III or higher qualification`[match(divisions$Elect_div, demo_data$DivisionNm)]
            cpal <- colorNumeric("YlOrRd", colours)
        }

        if(input$demos == "Proportion with Year 12 or equivalent qualification"){
            colours <- demo_data$`Proportion with Year 12 or equivalent qualification`[match(divisions$Elect_div, demo_data$DivisionNm)]
            cpal <- colorNumeric("YlOrRd", colours)
        }

        if(input$demos == "Recent Migrant" ){
            colours <- demo_data$`Recent Migrant`[match(divisions$Elect_div, demo_data$DivisionNm)]
            cpal <- colorNumeric("YlOrRd", colours)
        }

        if(input$demos == "Voter Age"){
            colours <- demo_data$`Voter Age`[match(divisions$Elect_div, demo_data$DivisionNm)]
            cpal <- colorNumeric("YlOrRd", colours)
        }

        # Use created colour pallette to produce choropleth map for electorates -------------------

        leaflet(divisions) %>%
            addTiles() %>%
            addPolygons(color = "white",
                        opacity = 1,
                        weight = 0.2,
                        smoothFactor = 0.2,
                        fillOpacity = 1,
                        fillColor = ~cpal(colours),
                        layerId = ~Elect_div,
                        label = paste0(divisions@data$Elect_div, ": ", round(colours, digits = 2))) %>%
            fitBounds(lat1 = -42, lng1 = 113,lat2 = -11, lng2 = 152) %>%
            addLegend(position = "bottomleft",
                      title = paste(input$demos),
                      pal = cpal,
                      values = ~colours,
                      opacity = 1,
                      bins = 5)
    })

    # Create value boxes for averages and produce histograms of chosen demographic ----------------

    output$demo_nat <- renderValueBox({
        req(input$view)
        value <- val_data %>%
            filter(Preferred == "Nationally") %>%
            pull(input$demos)

        valueBox(value, "National Average", color = "green", icon = icon("id-card"))
    })

    output$nat_dist <- renderPlot({
        req(input$view)
        demo_data %>%
            ggplot(aes(x = get(input$demos), y= ..density..))+
            geom_density(fill = "#00a65a", colour = "white", alpha = 0.5) +
            geom_histogram(colour = "white", fill = "#00a65a") +
            theme_bw() +
            labs(y = "", x = paste(input$demos)) +
            theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  panel.grid = element_blank(),
                  panel.border = element_blank())
    })

    output$demo_lib <- renderValueBox({
        req(input$view)
        value <- val_data %>%
            filter(Preferred == "Liberal") %>%
            pull(input$demos)

        valueBox(value, "Liberal Electorate Average", color = "blue", icon = icon("id-card"))
    })

    output$lib_dist <- renderPlot({
        req(input$view)
        demo_data %>%
            filter(Preferred == "Liberal") %>%
            ggplot(aes(x = get(input$demos), y= ..density..))+
            geom_density(fill = "#4e79a7", colour = "white", alpha = 0.5) +
            geom_histogram(colour = "white", fill = "#4e79a7") +
            theme_bw() +
            labs(y = "", x = paste(input$demos)) +
            theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  panel.grid = element_blank(),
                  panel.border = element_blank())
    })

    output$demo_lab <- renderValueBox({
        req(input$view)
        value <- val_data %>%
            filter(Preferred == "Labor") %>%
            pull(input$demos)

        valueBox(value, "Labor Electorate Average", color = "red", icon = icon("id-card"))
    })

    output$lab_dist <- renderPlot({
        req(input$view)
        demo_data %>%
            filter(Preferred == "Labor") %>%
            ggplot(aes(x = get(input$demos), y= ..density..))+
            geom_density(fill = "#e15759", colour = "white", alpha = 0.5) +
            geom_histogram(colour = "white", fill = "#e15759") +
            theme_bw() +
            labs(y = "", x = paste(input$demos)) +
            theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  panel.grid = element_blank(),
                  panel.border = element_blank())
    })

    # 3rd Panel -----------------------------------------------------------------------------------

        # Create choropleth map based on model projected results ----------------------------------

    output$model_plot <- renderLeaflet({

        req(input$view2)

        # Mutate the fitted value in response to user sliderInput ---------------------------------

        model_plot <- augment(full_model) %>%
            mutate(.fitted = .fitted + estimates$voter_age*(input$age - mean(model_data$`Voter Age`)) +
                                       estimates$median_weekly_household_income*(input$income - mean(model_data$`Median weekly household income`)) +
                                       estimates$proportion_born_overseas*(input$overseas - mean(model_data$`Proportion born overseas`)) +
                                       estimates$couple_without_children*(input$nochildren - mean(model_data$`Couple without children`)) +
                                       estimates$one_parent_family*(input$noparent - mean(model_data$`One parent family`)) +
                                       estimates$proportion_with_year_12_or_equivalent_qualification*(input$year12 - mean(model_data$`Proportion with Year 12 or equivalent qualification`)) +
                                       estimates$proportion_with_cert_iii_or_higher_qualification*(input$cert3 - mean(model_data$`Proportion with Cert III or higher qualification`))+
                                       estimates$proportion_engaged_in_work_training_or_education*(input$engaged - mean(model_data$`Proportion engaged in work, training or education`)) +
                                       estimates$homes_rented*(input$rented - mean(model_data$`Homes rented`)) +
                                       estimates$medain_monthly_mortgage_repayments*(input$mortgage - mean(model_data$`Medain monthly mortgage repayments`))) %>%
            dplyr::select(1, .fitted) %>%
            bind_cols(combined_data) %>%
            mutate(Preferred = case_when(.fitted > 50 ~ "Liberal",
                                         .fitted < 50 ~ "Labor"))

        # Create colour palette for model projected outcome and create leaflet --------------------

        colours <- model_plot$Preferred[match(divisions$Elect_div, model_plot$DivisionNm)]
        cpal <- colorFactor(palette = c("#e15759", "#4e79a7"), colours)

        leaflet(divisions) %>%
            addTiles() %>%
            addPolygons(color = "white",
                        opacity = 1,
                        weight = 0.1,
                        smoothFactor = 0.2,
                        fillOpacity = 1,
                        fillColor = ~cpal(colours),
                        layerId = ~Elect_div,
                        label = ~Elect_div) %>%
            fitBounds(lat1 = -42, lng1 = 113,lat2 = -11, lng2 = 152)

    })

        # Create value boxes for projected seats won using the same method as above -------------------

    output$mod_lib <- renderValueBox({

        req(input$view2)

        model_plot <- augment(full_model) %>%
            mutate(.fitted = .fitted + estimates$voter_age*(input$age - mean(model_data$`Voter Age`)) +
                       estimates$median_weekly_household_income*(input$income - mean(model_data$`Median weekly household income`)) +
                       estimates$proportion_born_overseas*(input$overseas - mean(model_data$`Proportion born overseas`)) +
                       estimates$couple_without_children*(input$nochildren - mean(model_data$`Couple without children`)) +
                       estimates$one_parent_family*(input$noparent - mean(model_data$`One parent family`)) +
                       estimates$proportion_with_year_12_or_equivalent_qualification*(input$year12 - mean(model_data$`Proportion with Year 12 or equivalent qualification`)) +
                       estimates$proportion_with_cert_iii_or_higher_qualification*(input$cert3 - mean(model_data$`Proportion with Cert III or higher qualification`))+
                       estimates$proportion_engaged_in_work_training_or_education*(input$engaged - mean(model_data$`Proportion engaged in work, training or education`)) +
                       estimates$homes_rented*(input$rented - mean(model_data$`Homes rented`)) +
                       estimates$medain_monthly_mortgage_repayments*(input$mortgage - mean(model_data$`Medain monthly mortgage repayments`))) %>%
            dplyr::select(1, .fitted) %>%
            bind_cols(combined_data) %>%
            mutate(Preferred = case_when(.fitted > 50 ~ "Liberal",
                                         .fitted < 50 ~ "Labor"))

        value <- model_plot %>%
            filter(Preferred == "Liberal") %>%
            nrow()

        valueBox(value, "Liberal", color = "blue", icon = icon("vote-yea"))
    })

    output$mod_lab <- renderValueBox({

        req(input$view2)

        model_plot <- augment(full_model) %>%
            mutate(.fitted = .fitted + estimates$voter_age*(input$age - mean(model_data$`Voter Age`)) +
                       estimates$median_weekly_household_income*(input$income - mean(model_data$`Median weekly household income`)) +
                       estimates$proportion_born_overseas*(input$overseas - mean(model_data$`Proportion born overseas`)) +
                       estimates$couple_without_children*(input$nochildren - mean(model_data$`Couple without children`)) +
                       estimates$one_parent_family*(input$noparent - mean(model_data$`One parent family`)) +
                       estimates$proportion_with_year_12_or_equivalent_qualification*(input$year12 - mean(model_data$`Proportion with Year 12 or equivalent qualification`)) +
                       estimates$proportion_with_cert_iii_or_higher_qualification*(input$cert3 - mean(model_data$`Proportion with Cert III or higher qualification`))+
                       estimates$proportion_engaged_in_work_training_or_education*(input$engaged - mean(model_data$`Proportion engaged in work, training or education`)) +
                       estimates$homes_rented*(input$rented - mean(model_data$`Homes rented`)) +
                       estimates$medain_monthly_mortgage_repayments*(input$mortgage - mean(model_data$`Medain monthly mortgage repayments`))) %>%
            dplyr::select(1, .fitted) %>%
            bind_cols(combined_data) %>%
            mutate(Preferred = case_when(.fitted > 50 ~ "Liberal",
                                         .fitted < 50 ~ "Labor"))

        value <- model_plot %>%
            filter(Preferred == "Labor") %>%
            nrow()

        valueBox(value, "Labor", color = "red", icon = icon("vote-yea"))
    })

        # Have sliders reset to original value when button is clicked -----------------------------

    observeEvent(input$reset, {
        updateSliderInput(session, "age", "Average Voter Age", min = round(min(model_data$`Voter Age`), digits = 0), max = round(max(model_data$`Voter Age`), digits = 0), value = mean(model_data$`Voter Age`))
        updateSliderInput(session, "income", "Median Weekly Household Income", min = round(min(model_data$`Median weekly household income`), digits = 0), max = round(max(model_data$`Median weekly household income`), digits = 0), value = mean(model_data$`Median weekly household income`))
        updateSliderInput(session,"overseas", "Proportion Born Overseas", min = round(min(model_data$`Proportion born overseas`), digits = 2), max = round(max(model_data$`Proportion born overseas`), digits = 2), value = mean(model_data$`Proportion born overseas`))
        updateSliderInput(session, "nochildren", "Proportion of Families without Children", min = round(min(model_data$`Couple without children`), digits = 2), max = round(max(model_data$`Couple without children`), digits = 2), value = mean(model_data$`Couple without children`))
        updateSliderInput(session, "noparent", "Proportion of Single Parent Families", min = round(min(model_data$`One parent family`), digits = 2), max = round(max(model_data$`One parent family`), digits = 2), value = mean(model_data$`One parent family`))
        updateSliderInput(session, "year12", "Proportion with Year 12 Qualification (equivalent)", min = round(min(model_data$`Proportion with Year 12 or equivalent qualification`), digits = 2), max = round(max(model_data$`Proportion with Year 12 or equivalent qualification`), digits = 2), value = mean(model_data$`Proportion with Year 12 or equivalent qualification`))
        updateSliderInput(session, "cert3", "Proportion with Cert III Qualification (equivalent)", min = round(min(model_data$`Proportion with Cert III or higher qualification`), digits = 2), max = round(max(model_data$`Proportion with Cert III or higher qualification`), digits = 2), value = mean(model_data$`Proportion with Cert III or higher qualification`))
        updateSliderInput(session, "engaged", "Proportion Engaged in Work, Training or Education", min = round(min(model_data$`Proportion engaged in work, training or education`), digits = 2), max = round(max(model_data$`Proportion engaged in work, training or education`), digits = 2), value = mean(model_data$`Proportion engaged in work, training or education`))
        updateSliderInput(session, "rented", "Proportion of Homes Rented", min = round(min(model_data$`Homes rented`), digits = 2), max = round(max(model_data$`Homes rented`), digits = 2), value = mean(model_data$`Homes rented`))
        updateSliderInput(session, "mortgage", "Median Monthly Mortgage Repayment", min = round(min(model_data$`Medain monthly mortgage repayments`), digits = 0), max = round(max(model_data$`Medain monthly mortgage repayments`), digits = 0), value = mean(model_data$`Medain monthly mortgage repayments`))
    })
}


# Launch ------------------------------------------------------------------------------------------
shinyApp(ui, server)
