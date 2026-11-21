# Step 1: Load our packages:
library(tidycensus)
library(tidyverse)
library(sf)
library(RColorBrewer)
library(classInt)
library(ggspatial)
library(sfhotspot)
library(patchwork)
library(osrm)
library(biscale)
library(sfdep)
library(janitor)
library(spgwr)
library(jtools)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
# Step 2: Working directory
# The script uses paths relative to the repository root, so run it from
# this folder (or set the working directory to the repo root). No absolute
# paths are used.
# setwd() was removed for portability; see README.md for details.

#Read in shapefile
us_cty_phe <- st_read("US_CTY_PreventativeHealthExp.shp")

# Create the map
# Jenks natural breaks (use style = "jenks")
jenk_breaks <- classIntervals(us_cty_phe$PHlth_Exp, n = 5, style = "jenks")$brks
print(jenk_breaks)

ggplot() +
  geom_sf(data = us_cty_phe, 
          aes(fill = PHlth_Exp), 
          color = "black", 
          linewidth = 0.2, 
          na.value = "lightgray") +
  
  # Use a continuous scale with custom breaks and labels
  scale_fill_fermenter(
    palette = "Reds",
    direction = 1,
    guide = "legend",
    breaks = jenk_breaks,
    name = "Expenditures"
  ) +
  
  guides(fill = guide_legend(
    reverse = TRUE,
    title.position = "top",
    title.hjust = 0.5
  )) +
  
  # Title
  labs(title = "Expenditures on Preventative Health Program by U.S. County",
       caption = "Author: Tobechi Dimkpa \nSources: Final 2025 \nDate: April 20,2025") +
  
  # Layout adjustments
  theme_void() +
  theme(plot.title = element_text(size = 13,
                                  family  = "Avenir",
                                  vjust = 3,
                                  hjust = 0.10),
        plot.title.position = "plot",
        legend.title = element_text(size = 10),
        legend.position = c(0.95, 0.5),
        legend.key.size = unit(.4, 'cm')) +
  annotation_north_arrow(height = unit(.8, "cm"),
                         width = unit(.6, "cm"),
                         pad_x = unit(0.1, "in"), 
                         pad_y = unit(0.001, "in"),
                         style = north_arrow_minimal) +
  annotation_scale(style = "ticks",
                   pad_x = unit(0.4, "in"),
                   pad_y = unit(.15, "in"),
                   unit_category = "imperial",
                   width_hint = .2) -> p1
ggsave(p1, filename= "s1q1.jpg", width = 8, height = 7)
#q3: moran's i = 0.766, p =0.001

#Question 4
# Calculate Spatial Lag  of death rate
us_cty_phe <- us_cty_phe |>
  mutate(nb = st_knn(geometry, 3),          # Identify neighbors
         wt = st_weights(nb, 
                         allow_zero = TRUE, # Allow empty neighbor sets
                         style = "W"),      # Row standardized 
         phe_lag = st_lag(PHlth_Exp, nb, wt))  # Calculate spatial lag


# Descriptive statistics for spatial lag of death rate
summary(us_cty_phe$phe_lag)
hist(us_cty_phe$phe_lag)

# Map the lag death rate
ggplot() +
  geom_sf(data = us_cty_phe,
          aes(fill = phe_lag),
          color = NA) +
  scale_fill_viridis_c() +
  geom_sf(data = NYC,
          fill = NA,
          color = "black") +
  theme_void() -> lag

rate + lag


# Standardize rate and death_lag
us_cty_phe |>
  mutate(rate_std = (PHlth_Exp - mean(PHlth_Exp, na.rm = TRUE)) / sd(PHlth_Exp, na.rm = TRUE),
         phe_lag_std = (phe_lag - mean(phe_lag, na.rm = TRUE)) / sd(phe_lag, na.rm = TRUE)) -> us_cty_phe


# Global Morans I
moran_I <- global_moran_perm(us_cty_phe$PHlth_Exp, 
                             us_cty_phe$nb, 
                             us_cty_phe$wt, 
                             alternative = "two.sided",
                             nsim = 999)

print(moran_I)
plot(moran_I)


# Local Moran's I 
us_cty_phe <- us_cty_phe |>
  mutate(local_moran_I = local_moran(PHlth_Exp, nb, wt, nsim = 999, alternative = "two.sided")) |> 
  unnest(local_moran_I)

ggplot() +
  geom_sf(data  = us_cty_phe, 
          aes(fill = mean)) +
  scale_fill_manual(values = c("High-High" = "#FF0000", 
                               "Low-Low" = "#0000FF",   
                               "Low-High" = "#66CCFF",   
                               "High-Low" = "#FF99CC",  
                               "Not significant" = "grey90"))

# Scatter plot 
ggplot(us_cty_phe, 
       aes(x = rate_std, y = phe_lag_std, color = mean)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "grey20", linetype = "dashed") + 
  geom_vline(xintercept = 0, color = "grey20", linetype = "dashed") +  
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = .5) +
  theme_minimal()


# Classify significant clusters
us_cty_phe |> 
  mutate(cluster_type = case_when(
    mean  == "Low-Low" & p_ii <= 0.05 ~ "Low-Low",
    mean  == "High-Low" & p_ii <= 0.05 ~ "High-Low",
    mean  == "Low-High" & p_ii <= 0.05 ~ "Low-High",
    mean  == "High-High" & p_ii <= 0.05 ~ "High-High",
    TRUE ~ "Not significant")) -> us_cty_phe

# Map it!
ggplot() +
  geom_sf(data  = us_cty_phe, 
          aes(fill = cluster_type)) +
  scale_fill_manual(values = c("High-High" = "#FF0000", 
                               "Low-Low" = "#0000FF",   
                               "Low-High" = "#66CCFF",   
                               "High-Low" = "#FF99CC",  
                               "Not significant" = "grey90"))


# Select the HH and LL clusters
us_cty_phe |> 
  filter(cluster_type %in% c("High-High", "Low-Low")) -> cluster_map

# Map it!
ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 0) +
  geom_sf(data = us_cty_phe,
          fill = NA,
          color = "black") +
  geom_sf(data = cluster_map,
          aes(fill = cluster_type),
          alpha = 0.8,
          color = NA) +
  scale_fill_manual(values = c("#FF0000", "#0000FF")) +
  labs(title = "Cluster Analysis of  Expenditures on Preventative Health Program",
       subtitle = "U.S. Counties",
       fill = "Clusters",
       caption = "Author: Tobechi Dimkpa \n Source:Final 2025  \n Date:April 20, 2025") +
  theme_void() +
  theme(plot.title = element_text(size = 13),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 11,
                                     face = "italic"),
        legend.position = "bottom",
        plot.caption = element_text(colour = "grey10",
                                    size = 8,
                                    vjust = 35,
                                    hjust = .95)) + 
  annotation_north_arrow(height = unit(.8, "cm"),
                         width = unit(.6, "cm"),
                         pad_x = unit(0.6, "in"), 
                         pad_y = unit(0, "in"),
                         style = north_arrow_minimal) +
  annotation_scale(style = "ticks",
                   pad_x = unit(1, "in"),
                   pad_y = unit(.05, "in"),
                   unit_category = "imperial",
                   width_hint = .2) 


##SECTION 2
#Create a scatter plot of expenditure on preventative health programs and poverty.
ggplot(us_cty_phe, 
       aes(x = PHlth_Exp, y = PCT_POV)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm", color = "red",  linewidth = 0.5, se = FALSE) +  # Regression line
  theme_minimal()+
  labs(title = "Relationship between Poverty and Expenditures on Preventative Health Programs",
                        subtitle = "US Counties",
                        x = "Expenditures",,
                        y = "% below Poverty Line")
us_cty_phe |> 
  ggplot(aes(x = PCT_POV, y = PHlth_Exp)) +
  geom_point(color = "grey50") +
  geom_smooth(method = "loess",
              color = "darkred",
              linetype = "dashed",
              linewidth = .7,
              se = FALSE) +
  theme_minimal() +
  labs(title = "Relationship between Poverty and Expenditures on Preventative Health Programs",
       subtitle = "US Counties",
       x = "% below Poverty Line",
       y = "Expenditures")
#Bivariate map
#### Bivariate Map ----
# Classify our data
bi_us <- bi_class(us_cty_phe, 
                   x = PHlth_Exp, 
                   y = PCT_POV, 
                   style = "quantile",
                   dim = 3)

# Map it!
ggplot () +
  geom_sf(data = bi_us, 
          aes(fill = bi_class), 
          color = NA, 
          linewidth = 0.1, 
          show.legend = FALSE)

# Preview bivariate color palette  
bi_pal(pal = "GrPink", dim = 3)
?bi_pal

# Create map 
ggplot () +
  geom_sf(data = bi_us, aes(fill = bi_class), 
          color = NA, 
          show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  geom_sf(data = us_cty_phe,
          color = "black",
          fill = NA) +
  labs(title = "Expenditure in Preventative Health Programs and Poverty",
       subtitle = "United States Counties",
       caption ="Author: Tobechi Dimkpa \n Source: Final 2025 \n Date:April 21 2025") +
  theme_void() + 
  theme(plot.title = element_text(size = 13),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 11,
                                     face = "italic"),
        legend.title = element_text(size = 12),
        legend.position = c(0.2, 0.68),
        legend.key.size = unit(.4, 'cm'),
        plot.caption = element_text(colour = "grey50",
                                    size = 8,
                                    vjust = 11,
                                    hjust = .95))+
  annotation_scale(pad_x = unit(1.1, "in"),
                   pad_y = unit(.05, "in"),
                   unit_category = "imperial") +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         height = unit(.8, "cm"),
                         width = unit(1, "cm"),
                         pad_x = unit(0.8, "in"), 
                         pad_y = unit(0.05, "in"),
                         style = north_arrow_fancy_orienteering) -> map2

# Create bivariate legend 
bi_legend(pal = "GrPink",
          dim = 3,
          xlab = "Expenditure",
          ylab =  "% Poor",
          size = 8) -> legend 

# Add it as an inset with {cowplot}
map2 +
  inset_element(legend, 
                left = 0.90, bottom = 0.35, 
                right = 1.05, top = 1.00) 

##SECTION 3
#### Multiple Regression ----
# Run the model
mreg <- lm(PHlth_Exp ~ PCT_RUR + PCT_POV + PCT_BA  + PCT_WH,
           data = us_cty_phe) 

# Display model results
summary(mreg)
#Residuals:
#Min      1Q  Median      3Q     Max 
#-202767  -33070   -4815   21124  691711 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -26200.97    6919.22  -3.787 0.000156 ***
#  PCT_RUR        112.57      44.34   2.539 0.011172 *  
#  PCT_POV       2544.15     248.75  10.228  < 2e-16 ***
 # PCT_BA        8165.32     200.27  40.772  < 2e-16 ***
 # PCT_WH         305.50      58.06   5.262 1.52e-07 ***
 # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 61290 on 3103 degrees of freedom
#Multiple R-squared:  0.4673,	Adjusted R-squared:  0.4666 
#F-statistic: 680.4 on 4 and 3103 DF,  p-value: < 2.2e-16

#Question 3: 
#### Geographically-Weighted Regression ----

# Fixed Bandwidth 
class(us_cty_phe)
data <- as_Spatial(us_cty_phe)


fixed_bw <- gwr.sel(PHlth_Exp ~ PCT_RUR + PCT_POV + PCT_BA  + PCT_WH,
                    data = data, 
                    adapt= FALSE, 
                    gweight = gwr.Gauss,
                    verbose = TRUE)


fbw_gwr <- gwr(PHlth_Exp ~ PCT_RUR + PCT_POV + PCT_BA  + PCT_WH,
               data = data, 
               bandwidth = fixed_bw,
               gweight= gwr.Gauss,
               hatmatrix=TRUE, 
               se.fit=TRUE)

print(fbw_gwr)

# What is in this object?
names(fbw_gwr)

# Grab results 
results <- fbw_gwr$SDF

class(results)

# Turn it back to sf object
results |> 
  st_as_sf() -> results.sf

class(results.sf)

#### Mapping Results ----

# Local R2
# Setting our breaks 
r2breaks <- classIntervals(results.sf$localR2, n = 4, style = "fisher")$brks

ggplot() +
  geom_sf(data = results.sf,
          aes(fill = localR2),
          color = NA) +
  scale_fill_fermenter(
    direction = 1,
    palette = "BuPu",
    breaks = r2breaks,
    guide = guide_colorbar(
      barwidth = 10,
      barheight = 1.5,
      ticks.colour = "white",
      ticks.linewidth = 0.5,
      label.theme = element_text(angle = 45, hjust = 1)  # <<< rotate the labels
    )) +
  labs(
    title = "Local R-Square",
    fill = "R2",
    caption = "Author: Tobechi Dimkpa \n Sources: Final 2025 \n Date: April 21 2025"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 18),
    legend.position = "bottom",
    legend.key.size = unit(.6, 'cm'),
    legend.text = element_text(size = 8),
    plot.caption = element_text(colour = "grey50", size = 8, vjust = 0, hjust = .9)
  )-> localr2

# Export as jpg and submit as part of Assignment 10
ggsave(localr2,
       filename = "localr2_plot.jpg",
       width = 4.5,
       height = 6)


# Percent 65yrs and over 
p65breaks <- classIntervals(results.sf$PCT_POV, n = 4, style = "jenks")$brks

ggplot() +
  geom_sf(data = results.sf,
          aes(fill = PCT_POV),
          color = NA) +
  scale_fill_fermenter(direction = 1,
                       palette = "BuPu",
                       breaks = p65breaks,
                       #labels = c("11.5", "15.3", "20.5", "24.6", "29.5"),
                       guide = guide_colorbar(barwidth = 10, 
                                              barheight = 1, 
                                              ticks.colour = "white", 
                                              ticks.linewidth = 0.5,
                                              label.theme = element_text(angle = 45, hjust = 1))) + 
  labs(fill = "Coefficient") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.size = unit(.3, 'cm')) -> a1



# Determining statistical significance
results.sf |> 
  mutate(tval_65 = PCT_POV / PCT_POV_se,
         t_65_cat = cut(tval_65,
                        breaks = c(-Inf, -2, 2, Inf),
                        labels = c("Significant", "Non-significant", "Significant"))) -> results.sf

# Mapping statistical significance
ggplot() +
  geom_sf(data = results.sf,
          aes(fill = t_65_cat),
          color = NA) +
  scale_fill_manual(values = c("#f88a7f", "#00a1d5")) + 
  theme_void() +
  labs(fill = "",
       caption = "Author: Tobechi Dimkpa \n Sources: Final 2025 \n Date: April 21 2025") +
  theme_void() +
  theme(plot.title = element_text(size = 18),
        legend.position = "bottom",
        legend.key.size = unit(.6, 'cm'),
        plot.caption = element_text(colour = "grey50",
                                    size = 8,
                                    vjust = 0,
                                    hjust = .9)) -> a2



a1 + a2  +
  plot_annotation(title = "Effect of % Poverty")  &
  theme(text = element_text(size = 14)) 


#Section4
exp <- st_transform(us_cty_phe, crs = 4326)

#### Add a tile layer -----

# Get list of map providers 
names(providers)

# Base tiles
leaflet() |> 
  addProviderTiles("CartoDB", group = "CartoDB")

# You can add multiple base maps
leaflet() |> 
  addProviderTiles("CartoDB", group = "CartoDB") |> 
  addProviderTiles("CartoDB.DarkMatterNoLabels", group = "CartoDB - Black") 
# Control layer
leaflet() |>  
  addProviderTiles("CartoDB", group = "CartoDB") |> 
  addProviderTiles("CartoDB.DarkMatterNoLabels", group = "CartoDB - Black") |> 
  addLayersControl(baseGroups = c("CartoDB", "CartoDB - Black")) # adds control layer


# Specify location of interest
leaflet() |>  
  addProviderTiles("CartoDB", group = "CartoDB") |> 
  addProviderTiles("CartoDB.DarkMatterNoLabels", group = "CartoDB - Black") |> 
  setView(lng = -98.5795, lat = 39.8283, zoom = 4) |>  # Coordinates of the geographic center of the contiguous US
  addLayersControl(baseGroups = c("CartoDB", "CartoDB - Black"))

# Assign it to an object
leaflet() |>  
  addProviderTiles("CartoDB", group = "CartoDB") |> 
  addProviderTiles("CartoDB.DarkMatterNoLabels", group = "CartoDB - Black") |> 
  setView(lng = -98.5795, lat = 39.8283, zoom = 4)|>  # Coordinates of NYC
  addLayersControl(baseGroups = c("CartoDB", "CartoDB - Black")) -> base


#### Add choropleth layer ----

# Select color palette 
display.brewer.all(type = "seq")

# Continuous Color palette  function
pal_death <- colorNumeric("Reds", 
                          domain = exp$PHlth_Exp,
                          reverse = FALSE)
# Add the layer 
base |> 
  addPolygons(data = exp,
              color = "black",
              weight = 0.5,   # line width 
              fillColor = ~pal_death(PHlth_Exp),  # adds color palette function
              fillOpacity = 0.7,
              label = ~PHlth_Exp) |> 
  addLegend(pal = pal_death,
            values = exp$PHlth_Exp,
            title = "Expenditure",
            position = "bottomright") 
# A binned color palette
breaks <- classIntervals(exp$PHlth_Exp, n = 7, style = "jenks")$brks

pal_death2 <- colorBin(palette = "Reds",             
                       domain = exp$PHlth_Exp, 
                       bins = breaks,
                       reverse = FALSE)

base |> 
  addPolygons(data = exp,
              color = "black",
              weight = 0.5,
              fillColor = ~pal_death2(PHlth_Exp),
              fillOpacity = 0.7,
              label = ~NAME) |> 
  addLegend(pal = pal_death2,
            values = exp$PHlth_Exp,
            title = "Expenditure",
            position = "bottomright") 


# Add pop-up
base |> 
  addPolygons(data = exp,
              color = "black",
              weight = 0.5,
              fillColor = ~pal_death2(PHlth_Exp),
              fillOpacity = 0.7,
              label = ~NAME,
              group = "Expenditure for PHP",   # adding a layer name 
              popup = ~paste(
                NAME, 
                PHlth_Exp,
                PCT_RUR,
                PCT_POV, 
                PCT_BA, 
                PCT_WH )) |> 
  addLegend(pal = pal_death2,
            values = exp$PHlth_Exp,
            title = "Expenditure",
            position = "bottomright") 
base |> 
  addPolygons(data = exp,
              color = "black",
              weight = 0.5,
              fillColor = ~pal_death2(PHlth_Exp),
              fillOpacity = 0.7,
              label = ~NAME,
              group = "Expenditure on PHP",   
              popup = ~paste(
                "<div style='font-family: Roboto, sans-serif; font-size: 14px; line-height: 1.4;'>",
                "<div style='font-size: 16px; font-weight: bold; color: #d9534f;'> 
                <b>County:</b> ", NAME, "</div>",
                "<b>Expenditures on Preventative Health Program:</b> <span style='color: #03a5fc;'>", PHlth_Exp, " $</span><br>",
                "<b>Percent of the population who reside in rural places:</b> <span style='color: #03a5fc;'>", PCT_RUR, "%</span><br>",
                "<b>Percent of the population that falls under the poverty lines:</b> <span style='color: #03a5fc;'>", PCT_POV, "%</span><br>",
                "<b>Percent of the population with a BA degree:</b> <span style='color: #03a5fc;'>", PCT_BA, "%</span><br>",
                "<b> Percent of the population who are White:</b> <span style='color: #03a5fc;'>", PCT_WH, "%</span><br>",
                "</div>"
              )) |> 
  addLegend(pal = pal_death2,
            values = exp$PHlth_Exp,
            title = "Expenditure",
            position = "bottomright")
# Add the layer 
base |> 
  addPolygons(data = exp,
              color = "black",
              weight = 0.5,
              fillColor = ~pal_death2(PHlth_Exp),
              fillOpacity = 0.7,
              label = ~NAME,
              group = "Expenditure on PHP",   
              popup = ~paste(
                "<div style='font-family: Roboto, sans-serif; font-size: 14px; line-height: 1.4;'>",
                "<div style='font-size: 16px; font-weight: bold; color: #d9534f;'> 
                <b>County:</b> ", NAME, "</div>",
                "<b>Expenditures on Preventative Health Program:</b> <span style='color: #03a5fc;'>", PHlth_Exp, " $</span><br>",
                "<b>Percent of the population who reside in rural places:</b> <span style='color: #03a5fc;'>", PCT_RUR, "%</span><br>",
                "<b>Percent of the population that falls under the poverty lines:</b> <span style='color: #03a5fc;'>", PCT_POV, "%</span><br>",
                "<b>Percent of the population with a BA degree:</b> <span style='color: #03a5fc;'>", PCT_BA, "%</span><br>",
                "<b> Percent of the population who are White:</b> <span style='color: #03a5fc;'>", PCT_WH, "%</span><br>",
                "</div>"
              )) |> 
  addLegend(pal = pal_death2,
            values = exp$PHlth_Exp,
            title = "Expenditure",
            position = "bottomright") -> basemap_rate

#### Add the clusters ----


pal_cluster <- colorFactor(palette = c("darkred","darkblue"),
                           domain = exp$LISA_CL)

basemap_rate |> 
  addPolygons(data = exp,
              color = ~pal_cluster(LISA_CL),
              weight = 2,
              fillColor = NA,
              group = "Clusters") |> 
  addLayersControl(baseGroups = c("CartoDB", "CartoDB - Black"),
                   overlayGroups = c("Expenditures", "Clusters"), 
                   options = layersControlOptions(collapsed = FALSE)) -> basemap_rate_clusters


basemap_rate_clusters |> 
  hideGroup(c("Clusters")) |>  # Hide or deactivate layers
  showGroup("Expenditures") -> web_map
saveWidget(web_map,
           file = "expenditures_map.html")


# Create the map
leaflet() |>  
  addProviderTiles("CartoDB", group = "CartoDB") |> 
  addProviderTiles("CartoDB.DarkMatterNoLabels", group = "CartoDB - Black") |> 
  setView(lng = -98.5795, lat = 39.8283, zoom = 4) |>  
  
  # Expenditure Layer
  addPolygons(data = exp,
              color = "black",
              weight = 0.5,
              fillColor = ~pal_death2(PHlth_Exp),
              fillOpacity = 0.7,
              label = ~NAME,
              group = "Expenditures",
              popup = ~paste(
                "<div style='font-family: Roboto, sans-serif; font-size: 14px;'>",
                "<b>County:</b> ", NAME, "<br>",
                "<b>Expenditures on Preventative Health Program:</b> <span style='color:#03a5fc;'>", PHlth_Exp, " $</span><br>",
                "<b>% Rural:</b> ", PCT_RUR, "%<br>",
                "<b>% Poverty:</b> ", PCT_POV, "%<br>",
                "<b>% BA Degree:</b> ", PCT_BA, "%<br>",
                "<b>% White:</b> ", PCT_WH, "%</div>"
              )) |> 
  addLegend(pal = pal_death2,
            values = exp$PHlth_Exp,
            title = "Expenditure",
            position = "bottomright",
            group = "Expenditures") |> 
  
  # Cluster Layer
  addPolygons(data = exp,
              color = ~pal_cluster(LISA_CL),
              weight = 2,
              fillColor = NA,
              group = "Clusters") |> 
  
  # Layers control
  addLayersControl(baseGroups = c("CartoDB", "CartoDB - Black"),
                   overlayGroups = c("Expenditures", "Clusters"),
                   options = layersControlOptions(collapsed = FALSE)) |> 
  
  # Show and hide groups
  hideGroup("Clusters") |>
  showGroup("Expenditures") -> web_map

# Save the map
saveWidget(web_map, file = "expenditures_map3.html")