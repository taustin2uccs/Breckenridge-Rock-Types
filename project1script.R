install.packages("ggplot")
install.packages("viridisLite")
library(ggspatial)
library(sf)
library(dplyr)
library(tidyverse)
library(reshape2)
library(geojsonsf)
library(tmap)
library(ggplot2)
library(viridis)
library(cartogram)
library(pals)
library(corrplot)
library(viridisLite)

#Load Files
geo<-st_read("data2/GIS_Data/geol_poly.shp")
arc<-st_read("data2/GIS_Data/GEOL_ARC.SHP")
shear<-st_read("data2/GIS_Data/SHEAR.SHP")
struct<-st_read("data2/GIS_Data/STRUCT.SHP")


#Make Percentage Column
rock_table <- geo %>%
  count(DESCRIPTIO) %>%
  mutate(Percentage = round(n / sum(n) * 100, 2)) %>%
  arrange(desc(Percentage))

# View the table
print(rock_table)

# Reorder factor levels by percentage
rock_table$DESCRIPTIO <- factor(rock_table$DESCRIPTIO, levels = rock_table$DESCRIPTIO)

# Plot
ggplot(rock_table, aes(x = DESCRIPTIO, y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Rock Type Distribution",
       x = "Rock Type",
       y = "Percentage of Area") +
  theme_minimal()

geo_filtered <- geo %>%
  filter(DESCRIPTIO %in% c("Qfp", "Qop", "Qop2", "Qop1", "Qtp", "Qtp3", "Qtp2", "Qpt1", "Qob", "Qfb", "Qtb", "Qto"))

# Define your groups
pine <- c("Qfp", "Qop", "Qop2", "Qop1", "Qtp", "Qtp3", "Qtp2", "Qtp1")
bull  <- c("Qob", "Qfb", "Qtb")
old <-("Qto")

# Create a new column for group
rock_table <- geo %>%
  count(DESCRIPTIO) %>%
  mutate(Percentage = round(n / sum(n) * 100, 2),
         Group = case_when(
           DESCRIPTIO %in% pine ~ "Pinedale Glaciation Deposits",
           DESCRIPTIO %in% bull  ~ "Bull Lake Glaciation Deposits",
           DESCRIPTIO %in% old ~ "Older Till",
           TRUE                        ~ "Other Rock Types"
         )) %>%
  arrange(desc(Percentage))

# Reorder factor levels for plotting
rock_table$DESCRIPTIO <- factor(rock_table$DESCRIPTIO, levels = rock_table$DESCRIPTIO)
str(rock_table)

# Bar Plot With Style
perc_bar<- ggplot(rock_table, aes(x = DESCRIPTIO, y = Percentage, fill = Group)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("Pinedale Glaciation Deposits" = "forestgreen",
                               "Bull Lake Glaciation Deposits" = "blue",
                               "Older Till" = "steelblue",
                               "Other Rock Types" = "darkred")) +
  labs(title = "Rock Type Distribution by Group",
       x = "Rock Type",
       y = "Percentage of Area",
       fill = "Rock Group") +
  theme_minimal()
print(perc_bar)

#Export Bar plot
pdf("plots/barplot_of_rock_types_breck.pdf", width=7, height=10)
print(perc_bar)
dev.off()

library(RColorBrewer)
display.brewer.all()
#Map Glacial Deposits
glac_depo <- tm_shape(geo_filtered) +
  tm_polygons("DESCRIPTIO", palette = "Set3", title = "Filtered Geology") +
  tm_shape(shear) +
  tm_lines(col = "blue", lwd = 1.5, title.col = "Shear Zones") +
  tm_shape(struct) +
  tm_lines(col = "green", lwd = 1, title.col = "Structures") +
  tm_compass(type = "8star", position = c("left", "bottom"), size = 2) +
  tm_scale_bar(position = c("right", "bottom"), text.size = 0.8) +
  tm_layout(
    legend.outside = TRUE,
    frame = FALSE
  )
glac_depo

#Export Glacial Deposits Map
pdf("plots/glacial_deposit_types.pdf", width=7, height=10)
print(glac_depo)
dev.off()

#Add Group Column To Geo Filtered
geo_filtered$Group <- case_when(
  geo_filtered$DESCRIPTIO %in% pine ~ "Pinedale Glaciation Deposits",
  geo_filtered$DESCRIPTIO %in% bull ~ "Bull Lake Glaciation Deposits",
  geo_filtered$DESCRIPTIO %in% old ~ "Older Till",
  TRUE ~ "Other Rock Types"
)

#Map with Style
tm_shape(geo_filtered) +
  tm_fill("Group", palette = c("Pinedale Glaciation Deposits" = "forestgreen",
                               "Bull Lake Glaciation Deposits" = "steelblue",
                               "Older Till" = "blue",
                               "Other Rock Types" = "firebrick"),
          title = "Rock Groups") +
  tm_shape(shear) +
  tm_lines(col = "blue", lwd = 1.5, title.col = "Shear Zones") +
  tm_shape(struct) +
  tm_lines(col = "green", lwd = 1, title.col = "Structures") +
  tm_compass(type = "8star", position = c("left", "top")) +  
  tm_scale_bar(position = c("right", "bottom")) +            
  tm_layout(
    title = "Geological Features of Breckenridge",
    title.position = c("center", "top"),
    title.size = 2,
    outer.margins = c(0.1, 0, 0, 0),  # top, right, bottom, left
    legend.outside = TRUE,
    frame = FALSE
  )

# Plot with ggplot
glac<-ggplot() +
  geom_sf(data = geo_filtered, aes(fill = Group), color = NA) +
  geom_sf(data = shear, color = "blue", size = 0.75) +
  geom_sf(data = struct, color = "green", size = 0.5) +
  scale_fill_manual(values = c("Pinedale Glaciation Deposits" = "forestgreen",
                               "Bull Lake Glaciation Deposits" = "steelblue",
                               "Older Till" = "blue",
                               "Other Rock Types" = "firebrick")) +
  labs(title = "Geological Features of Breckenridge",
       fill = "Rock Group") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, margin = margin(b = 20)),
    legend.position = "right"
  )
glac

#Export as Pdf
pdf("plots/glacial_deposits_of_breck.pdf",width=10,height=7)
print(glac)
dev.off()

# Plot with ggplot2 + ggspatial
fan_glac<-ggplot() +
  geom_sf(data = geo_filtered, aes(fill = Group), color = NA) +
  geom_sf(data = shear, color = "blue", size = 0.75) +
  geom_sf(data = struct, color = "green", size = 0.5) +
  scale_fill_manual(values = c("Pinedale Glaciation Deposits" = "forestgreen",
                               "Bull Lake Glaciation Deposits" = "blue",
                               "Older Till" = "steelblue",
                               "Other Rock Types" = "firebrick")) +
  labs(title = "Geological Features of Breckenridge",
       fill = "Rock Group") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, margin = margin(b = 20)),
    legend.position = "right"
  ) +
  annotation_scale(location = "bl", width_hint = 0.5) +  # bottom left
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering())

#Export as PDF
pdf("plots/scale_bar_north_arrow_glacier_deposit_map.pdf", width=10, height=12)
print(fan_glac)
dev.off()

#Make A New Column
geo <- geo %>%
  mutate(Group = case_when(
    DESCRIPTIO %in% pine ~ "Pinedale Glaciation Deposits",
    DESCRIPTIO %in% bull ~ "Bull Lake Glaciation Deposits",
    DESCRIPTIO %in% old ~ "Older Till",
    TRUE ~ "Other Rock Types"
  ))

#Rough Rock Map
tm <- tm_shape(geo) +
  tm_polygons(col = "DESCRIPTIO", palette = "Set3", title = "Geology") +
  tm_shape(arc) +
  tm_lines(col = "black", lwd = 1, title.col = "Geologic Arcs") +
  tm_shape(shear) +
  tm_lines(col = "blue", lwd = 1.5, title.col = "Shear Zones") +
  tm_shape(struct) +
  tm_lines(col = "darkgreen", lwd = 1, title.col = "Structures") +
  tm_layout(title = "Geological Features of Breckenridge",
            legend.outside = TRUE,
            frame = FALSE)
tm

#Round Out Rocks
tm <- tm_shape(geo_filtered) +
  tm_fill("Group", palette = c("Pinedale Glaciation Deposits" = "forestgreen",
                               "Bull Lake Glaciation Deposits" = "blue",
                               "Older Till" = "steelblue",
                               "Other Rock Types" = "firebrick"),
          title = "Rock Groups") +
  tm_shape(arc) +
  tm_lines(col = "grey", lwd = 1, title.col = "Geologic Arcs") +
  tm_shape(shear) +
  tm_lines(col = "blue", lwd = 1.5, title.col = "Shear Zones") +
  tm_shape(struct) +
  tm_lines(col = "darkgreen", lwd = 1, title.col = "Structures") +
  tm_compass(type = "8star", position = c("left", "top"), size = 2) +
  tm_scale_bar(position = c("right", "bottom"), text.size = 0.8) +
  tm_layout(title = "Geological Features of Breckenridge",
            title.position = c("center", "top"),
            title.size = 2,
            outer.margins = c(0.2, 0, 0, 0),  # adds space above the map
            legend.outside = TRUE,
            frame = FALSE)
tm

geo$Group <- case_when(
  DESCRIPTIO %in% pine ~ "Pinedale Glaciation Deposits",
  DESCRIPTIO %in% c(bull, "Qto") ~ "Bull Lake Glaciation Deposits",
  TRUE ~ "Other Rock Types"
)

# Colors For Pinedale group
pine_colors <- setNames(viridisLite::viridis(length(pine)), pine)
# For Bull Lake group
bull_colors <- setNames(viridisLite::cividis(length(bull)), bull)
old_colors<- setNames(viridisLite::cividis(length(old)),old)
# For Other group (warm colors)
library(pals)
other_types <- setdiff(unique(geo$DESCRIPTIO), c(pine, bull))
other_colors <- setNames(warmcool(length(other_types)), other_types)
# Combine all
rock_colors <- c(pine_colors, bull_colors, other_colors)

#Plot Take One
plot1<-ggplot(geo, aes(fill = DESCRIPTIO)) +
  geom_sf(color = NA) +
  scale_fill_manual(values = rock_colors, name = "Rock Type") +
  labs(title = "Rock Types of the Breckenridge Quadrangle") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, margin = margin(b = 20)),
    legend.position = "right"
  ) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering())
plot1

#Plot Attempt to Org Legend
plot2 <- ggplot(geo, aes(fill = DESCRIPTIO)) +
  geom_sf(color = NA) +
  scale_fill_manual(
    values = rock_colors,
    name = "Rock Type",
    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      ncol = 5,              # vertical legend by column #
      byrow = FALSE,
      override.aes = list(size = 5)  # optional: make legend swatches larger
    )
  ) +
  labs(title = "Rock Types of the Breckenridge Quadrangle") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, margin = margin(b = 20)),
    legend.position = "right"
  ) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering())
plot2

#Export
pdf("plots/rock_types_map.pdf", height=8,width=12)
print(plot2)
dev.off()


















































