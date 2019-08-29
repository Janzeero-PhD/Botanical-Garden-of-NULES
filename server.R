library(tidyverse)
library(leaflet)
library(shiny)
library(shinyjs)
library(ECharts2Shiny)

### csv files
bot1 <- read_csv("bot1.csv")
bot2 <- read_csv("bot2.csv")
bot3 <- read_csv("bot3.csv")
bot4 <- read_csv("bot4.csv")

# one data.frame
botsad.df <- rbind(bot1, bot2, bot3, bot4)

# extract link to the photo
botsad.df$link <- str_extract(botsad.df$description, 'http.+Назва')
botsad.df$link_2 <- str_extract(botsad.df$link, 'http.+height')
botsad.df$link_3 <- str_replace(botsad.df$link_2, 'height', " ")
botsad.df$link_4 <- str_replace(botsad.df$link_3, '"', " ")

# extract tree species and diameter
botsad.df$s_d <- str_extract(botsad.df$description, 'Назва.+Опис')
botsad.df$species_1 <- str_replace_all(botsad.df$s_d, "<br>", " ")
botsad.df$species_2 <- str_extract(botsad.df$species_1, 'Назва.+Лат')
botsad.df$species_3 <- str_replace(botsad.df$species_2, 'Назва', ' ')
botsad.df$species_4 <- str_replace(botsad.df$species_3, 'Лат', ' ')
botsad.df$species_5 <- str_replace(botsad.df$species_4, ':', ' ')

botsad.df$DBH <- str_extract(botsad.df$s_d, "\\d+")

# extract ecosystem services
botsad.df$e_s <- str_extract(botsad.df$description, 'Приріст.+рік')

botsad.df$inc_1 <- str_replace_all(botsad.df$e_s, "<br>", " ") # live biomass
botsad.df$inc_2 <- str_replace(botsad.df$inc_1, "Деп.+", " ")
botsad.df$inc_3 <- str_extract_all(botsad.df$inc_2, '\\d.\\d+')
botsad.df$inc_4 <- str_replace(botsad.df$inc_3, ',', ".")
botsad.df$inc_4 <- as.numeric(botsad.df$inc_4)

botsad.df$C_1 <- str_replace_all(botsad.df$e_s, "<br>", " ") # carbon
botsad.df$C_2 <- str_extract(botsad.df$C_1, "Деп.+")
botsad.df$C_3 <- str_replace(botsad.df$C_2, "Аку.+", " ")
botsad.df$C_4 <- str_extract(botsad.df$C_3, "Деп.+кгС/рік")
botsad.df$C_5 <- str_extract_all(botsad.df$C_4, '\\d+.\\d+')
botsad.df$C_6 <- str_replace(botsad.df$C_5, ',', ".")
botsad.df$C_6 <- as.numeric(botsad.df$C_6)

botsad.df$e_1 <- str_replace_all(botsad.df$e_s, "<br>", " ") # energy
botsad.df$e_2 <- str_extract(botsad.df$e_1, "Аку.+")
botsad.df$e_3 <- str_replace(botsad.df$e_2, "Прод.+", " ")
botsad.df$e_4 <- str_extract(botsad.df$e_3, "Аку.+МДж")
botsad.df$e_5 <- str_extract_all(botsad.df$e_4, '\\d+.\\d+')
botsad.df$e_6 <- str_replace(botsad.df$e_5, ',', ".")
botsad.df$e_6 <- as.numeric(botsad.df$e_6)

botsad.df$o_1 <- str_replace_all(botsad.df$e_s, "<br>", " ") # oxygen
botsad.df$o_2 <- str_extract(botsad.df$o_1, "Прод.+")
botsad.df$o_3 <- str_extract_all(botsad.df$o_2, '\\d+.\\d+')
botsad.df$o_4 <- str_replace(botsad.df$o_3, ',', ".")
botsad.df$o_4 <- as.numeric(botsad.df$o_4)

botsad.df$Cm_1 <- str_replace_all(botsad.df$C_3, "Деп.+-", " ") # C money
botsad.df$Cm_2 <- str_extract_all(botsad.df$Cm_1, '\\d+.\\d+')
botsad.df$Cm_3 <- str_replace(botsad.df$Cm_2, ',', ".")
botsad.df$Cm_3 <- as.numeric(botsad.df$Cm_3)

botsad.df$em_1 <- str_replace_all(botsad.df$e_3, "Аку.+-", " ") # energy money
botsad.df$em_2 <- str_extract_all(botsad.df$em_1, '\\d+.\\d+')
botsad.df$em_3 <- str_replace(botsad.df$em_2, ',', ".")
botsad.df$em_3 <- as.numeric(botsad.df$em_3)

# description
botsad.df$des_1 <- str_replace_all(botsad.df$description, "<br>", " ")
botsad.df$des_2 <- str_extract(botsad.df$des_1, "Опис.+Сан")
botsad.df$des_3 <- str_replace(botsad.df$des_2, "Сан", " ")


# new data
botsad.final <- botsad.df %>%
  dplyr::select(c(1:3, 8, 14, 15, 20, 26, 32, 36, 39, 42, 45))

botsad.final$Name <- as.factor(botsad.final$Name)
botsad.final <- botsad.final %>% mutate(id = row_number())

# rename to UA
botsad.final <- botsad.final %>%
  rename("Приріст фітомаси (кг/рік)" = inc_4) %>%
  rename("Депонування вуглецю (кг С/рік)" = C_6) %>%
  rename("Акумулювання енергії (МДж/рік)" = e_6) %>%
  rename("Продукування кисню (кг/рік)" = o_4) %>%
  rename("Вартість вуглецю (грн/рік)" = Cm_3) %>%
  rename("Вартість енергії (грн/рік)" = em_3)

# icon URLs
addResourcePath('www', 'www')

shrub_path <- 'www/shrub.png'
shrub_ex_path <- 'www/shrub_ex.png'
tree_ex_path <- 'www/tree_ex.png'
spruce_ex_path <- 'www/spruce_ex.png'
tree_path <- 'www/tree.png'
pine_path <- 'www/spruce.png'

# unique levels of species
unique(botsad.final$species_5)
botsad.final$species <- botsad.final$species_5 %>%
  str_trim()
unique(botsad.final$species)

#botsad.species <- data.frame(Species = unique(botsad.final$species))
#unique(botsad.species$Species)
#write.xlsx(botsad.species, "botsad_species.xlsx")

botsad.species.new <- openxlsx::read.xlsx("botsad_species.xlsx")
botsad.species.new$Species <- as.factor(botsad.species.new$Species)
botsad.species.new$Rare <- as.factor(botsad.species.new$Rare)
botsad.species.new$Type <- as.factor(botsad.species.new$Type)

head(unique(botsad.species.new$Species))
head(unique(botsad.final$species))

# delete empty species
botsad.final <- botsad.final %>%
  filter(species_5 != "     ")

# ifelse species
botsad.final$Rare <- botsad.species.new$Rare[match(botsad.final$species, 
                                                   botsad.species.new$Species)]
botsad.final$Type <- botsad.species.new$Type[match(botsad.final$species, 
                                                   botsad.species.new$Species)]
#botsad.final <- botsad.final %>%
#select(-rare)

# ifelse diameters
botsad.final$DBH <- as.numeric(botsad.final$DBH)
unique(botsad.final$DBH)
botsad.final$Diameter <- ifelse(botsad.final$DBH <= 8, "тонке",
                                ifelse(botsad.final$DBH <= 20, "середнє",
                                       "велике"))
botsad.final %>%
  group_by(Diameter) %>%
  summarize(n = n())

# Y coordinate to the south
botsad.final <- botsad.final %>%
  mutate(Y_new = Y - 0.00007)

# subsets
botsad.exotic <- botsad.final %>%
  filter(Rare == "екзот")
botsad.local <- botsad.final %>%
  filter(Rare == "місцевий")

# botsad treemap
botsad.final %>%
  group_by(Type, Rare) %>%
  summarize(n = n())

#
treemap.df <- "[{name: 'Хвойні /дерева',
value: 676,
children: [
{
  name: 'Хвойні дерева екзоти',
  value: 283,
},
  {
  name: 'Хвойні дерева місцеві',
  value: 393
  }
  ]
  },
  {
  name: 'Листяні дерева',
  value: 1417,
  children: [
  {name : 'Листяні дерева екзоти',
  value: 415
  },
  {
  name:'Листяні дерева місцеві',
  value: 1002
  }
  ]
  },
  {
  name: 'Чагарники',
  value: 729
  },
  {
  name: 'Ліани',
  value: 34
  }
  ]"
  
  # pie chart
  sum(botsad.final$`Вартість вуглецю (грн/рік)`, na.rm = T) # 7473.9
  sum(botsad.final$`Вартість енергії (грн/рік)`, na.rm = T) # 21519.6
  
  botsad.pie <- c(enc2native(rep("Депонований вуглець", 7473.9)),
                  enc2native(rep("Акумульована енергія", 21519.6)))
  
  # set of icons
  set_icons <- icons(
    iconUrl = ifelse(botsad.final$Type == "хвойне" & botsad.final$Rare == "екзот",
                     spruce_ex_path, ifelse(
                       botsad.final$Type == "хвойне" & botsad.final$Rare == "місцевий",
                       pine_path, ifelse(
                         botsad.final$Type == "листяне" & botsad.final$Rare == "екзот",
                         tree_ex_path, ifelse(
                           botsad.final$Type == "чагарник" & botsad.final$Rare == "місцевий",
                           shrub_path, ifelse(
                             botsad.final$Type == "листяне" & botsad.final$Rare == "місцевий", 
                             tree_path, shrub_ex_path))))),
    iconWidth = 15, iconHeight = 15,
    iconAnchorX = 10, iconAnchorY = 10
  )

server <- function(input, output) {
  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  # Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles('Esri.WorldImagery') %>%
      addMarkers(data = botsad.final,
                 lng = ~ X, lat = ~ Y_new,
                 popup = paste0("<img src = ", botsad.final$link_4, " />",
                                "<br> <b>", botsad.final$species, "</b> </br?> "),
                 label = botsad.final$species,
                 layerId = botsad.final$id
                 ,
                 icon = set_icons
                 ) %>%
    addEasyButton(easyButton(
      icon = "fa-crosshairs", title = "Геолокація",
      onClick = JS("function(btn, map){ map.locate({setView: true}); }")))
  })
  
  # store the click
  observeEvent(input$map_marker_click, {
    data_of_click$clickedMarker <- input$map_marker_click
  })
  
  # Make a table with ecosystem services
  output$table <- renderTable({
    if (is.null(data_of_click$clickedMarker)) {
      return(NULL)
    }
    return(
      subset(botsad.final %>%
               dplyr::select(7:12, 14), 
             id == data_of_click$clickedMarker$id
      )
    ) 
  }, na = '-', bordered = T)
  
  # Make a text with description
  output$text <- renderText({
    if (is.null(data_of_click$clickedMarker)) {
      return(NULL)
    }
    return(
      paste0(
        botsad.final[botsad.final$id == data_of_click$clickedMarker$id,]$des_3
      )
    )
  })
  
  # Make a text with species name
  output$text_name <- renderText({
    if (is.null(data_of_click$clickedMarker)) {
      return(NULL)
    }
    return(
      paste0(
        botsad.final[botsad.final$id == data_of_click$clickedMarker$id,]$species
      )
    )
  })
  
  # treemap
  renderTreeMap(div_id = "test",
                data = treemap.df,
                show.tools = F,
                name = "Усі екземпляри")
  
  # piechart
  renderPieChart(div_id = "pie",
                 data = botsad.pie,
                 theme = 'shine',
                 show.tools = F,
                 show.label = F,
                 show.legend = F,
                 radius = "85%")
}
