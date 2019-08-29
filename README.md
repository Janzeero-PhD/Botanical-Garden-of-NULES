# Botanical-Garden-of-NULES
Here is an example of building Shiny dashboard with Leaflet map in R. This app has medium level of code; herewith, it will be quite easy to reproduce for everybody who is sufficiently familiar with R Shiny.
App represents Web-app of map with trees and shrubs of Botanical Garden of National University of Life and Environmental Sciences of Ukraine (Kyiv). All text is provided in Ukrainian (Cyrillic).
For reproducing this example, you should at first download from this folder respective files (bot1.csv, bot2.csv, bot3.csv, bot4.csv, botsad_species.xlsx and set of images).
All code related to creating main data.frame botsad.final, as well as supportive data.frames, is presented in server.R.
So running server.R and ui.R, you will get a dashboard which contains:
- slidebar with general information and text about tree/shrub species which appears after clicking on the tree icon;
- Leaflet map with ~ 2900 trees and shrubs within Botanical Garden; those trees have different icons related to live form (conifer, broadleaf or shrub) and provenance (local or exotic for Ukraine) and you can click on them, getting a photo and some data below;
- table appeared below the map shows different ecosystem services provided by the tree, with some financial value of that;
- finally, right part of dashboard summarizes general data: tree-map represents diversity of local tree and shrub collection, while pie-chart describes how much local ecosystem services cost.
