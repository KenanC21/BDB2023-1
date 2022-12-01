library(tidyverse)
library(png)

# This code is based off of that made by Coach Matthew Edwards of the University of Virginia

mypng <- readPNG("/Users/michaelegle/Downloads/ISU_logo.png")
isu_wordmark <- readPNG("/Users/michaelegle/Desktop/iowa_state_wordmark.png")
cyclones_wordmark <- readPNG("/Users/michaelegle/Desktop/cyclones_wordmark.png")

football_field <- ggplot() +
  geom_rect(aes(xmin = -4, xmax = 124, ymin = -4, ymax = 57.33), fill = "#669933", colour = "#FFFFFF", size = .5) + ##These First few geom-rect's set up the field space
  geom_rect(aes(xmin = 0, xmax = 120, ymin = 0, ymax = 53.33), fill = "#669933", colour = "#FFFFFF", size = .5) +
  geom_rect(aes(xmin = 10, xmax = 15, ymin = 0, ymax = 53.33), fill = "#76b03f", colour = "#FFFFFF", size = 0.5) + # Add the alternating grass colors that the turf team uses
  geom_rect(aes(xmin = 25, xmax = 35, ymin = 0, ymax = 53.33), fill = "#76b03f", colour = "#FFFFFF", size = 0.5) +
  geom_rect(aes(xmin = 45, xmax = 55, ymin = 0, ymax = 53.33), fill = "#76b03f", colour = "#FFFFFF", size = 0.5) +
  geom_rect(aes(xmin = 65, xmax = 75, ymin = 0, ymax = 53.33), fill = "#76b03f", colour = "#FFFFFF", size = 0.5) +
  geom_rect(aes(xmin = 85, xmax = 95, ymin = 0, ymax = 53.33), fill = "#76b03f", colour = "#FFFFFF", size = 0.5) +
  geom_rect(aes(xmin = 105, xmax = 110, ymin = 0, ymax = 53.33), fill = "#76b03f", colour = "#FFFFFF", size = 0.5) +
  geom_rect(aes(xmin = 35, xmax = 85, ymin = -2, ymax = 0), fill = "#FFFFFF", colour = "#FFFFFF") + ##These next few set up the boxes around the field
  geom_rect(aes(xmin = 35, xmax = 85, ymin = 53.33, ymax = 55.33), fill = "#FFFFFF", colour = "#FFFFFF") +
  geom_rect(aes(xmin = -2, xmax = 35, ymin = -2, ymax = 0), fill = "#FFFFFF") +
  geom_rect(aes(xmin = -2, xmax = 0, ymin = -2, ymax = 55.33), fill = "#FFFFFF") +
  geom_rect(aes(xmin = -2, xmax = 35, ymin = 53.33, ymax = 55.33), fill = "#FFFFFF") +
  geom_rect(aes(xmin = 120, xmax = 122, ymin = -2, ymax = 55.33), fill = "#FFFFFF") +
  geom_rect(aes(xmin = 85, xmax = 122, ymin = -2, ymax = 0), fill = "#FFFFFF") +
  geom_rect(aes(xmin = 85, xmax = 122, ymin = 53.33, ymax = 55.33), fill = "#FFFFFF") +
  geom_segment(aes(x = 10, xend = 10, y = 0, yend = 53.33), colour = "#FFFFFF") + ##These are the yardmarkers
  geom_segment(aes(x = 15, xend = 15, y = 0, yend = 53.33), colour = "#FFFFFF") +
  geom_segment(aes(x = 20, xend = 20, y = 0, yend = 53.33), colour = "#FFFFFF") + 
  geom_segment(aes(x = 25, xend = 25, y = 0, yend = 53.33), colour = "#FFFFFF") +
  geom_segment(aes(x = 30, xend = 30, y = 0, yend = 53.33), colour = "#FFFFFF") +
  geom_segment(aes(x = 35, xend = 35, y = 0, yend = 53.33), colour = "#FFFFFF") +
  geom_segment(aes(x = 40, xend = 40, y = 0, yend = 53.33), colour = "#FFFFFF") +
  geom_segment(aes(x = 45, xend = 45, y = 0, yend = 53.33), colour = "#FFFFFF") +
  geom_segment(aes(x = 50, xend = 50, y = 0, yend = 53.33), colour = "#FFFFFF") +
  geom_segment(aes(x = 55, xend = 55, y = 0, yend = 53.33), colour = "#FFFFFF") +
  geom_segment(aes(x = 60, xend = 60, y = 0, yend = 53.33), colour = "#FFFFFF") +
  geom_segment(aes(x = 65, xend = 65, y = 0, yend = 53.33), colour = "#FFFFFF") +
  geom_segment(aes(x = 70, xend = 70, y = 0, yend = 53.33), colour = "#FFFFFF") +
  geom_segment(aes(x = 75, xend = 75, y = 0, yend = 53.33), colour = "#FFFFFF") +
  geom_segment(aes(x = 80, xend = 80, y = 0, yend = 53.33), colour = "#FFFFFF") +
  geom_segment(aes(x = 85, xend = 85, y = 0, yend = 53.33), colour = "#FFFFFF") +
  geom_segment(aes(x = 90, xend = 90, y = 0, yend = 53.33), colour = "#FFFFFF") +
  geom_segment(aes(x = 95, xend = 95, y = 0, yend = 53.33), colour = "#FFFFFF") +
  geom_segment(aes(x = 100, xend = 100, y = 0, yend = 53.33), colour = "#FFFFFF") +
  geom_segment(aes(x = 105, xend = 105, y = 0, yend = 53.33), colour = "#FFFFFF") +
  geom_segment(aes(x = 110, xend = 110, y = 0, yend = 53.33), colour = "#FFFFFF") +
  geom_segment(aes(x = 122, xend = 122, y = -2, yend = 55.33), colour = "#FFFFFF") +
  geom_segment(aes(x = -2, xend = -2, y = -2, yend = 55.33), colour = "#FFFFFF") +
  geom_segment(aes(x = -2, xend = 122, y = -2, yend = -2), colour = "#FFFFFF") +
  geom_segment(aes(x = -2, xend = 122, y = 55.33, yend = 55.33), colour = "#FFFFFF") +
  geom_segment(aes(x = 10, xend = 110, y = 20, yend = 20), colour = "#FFFFFF", linetype = "dashed") + ##These are the hashes
  geom_segment(aes(x = 10, xend = 110, y = 33.33, yend = 33.33), colour = "#FFFFFF", linetype = "dashed") +
  geom_segment(aes(x = 10, xend = 110, y = .5, yend = .5), colour = "#FFFFFF", linetype = "dashed") +
  geom_segment(aes(x = 10, xend = 110, y = 52.83, yend = 52.83), colour = "#FFFFFF", linetype = "dashed") +
  annotate(geom ="text", x = 20.6, y = 7.5, label = "10", colour = "#FFFFFF", fontface = 2, size = 8, family = "NCAA Iowa St Cyclone") + ##Put in the numbers for the yard lines
  annotate(geom ="text", x = 30, y = 7.5, label = "20", colour = "#FFFFFF", fontface = 2, size = 8, family = "NCAA Iowa St Cyclone") +
  annotate(geom ="text", x = 40, y = 7.5, label = "30", colour = "#FFFFFF", fontface = 2, size = 8, family = "NCAA Iowa St Cyclone") +
  annotate(geom ="text", x = 50, y = 7.5, label = "40", colour = "#FFFFFF", fontface = 2, size = 8, family = "NCAA Iowa St Cyclone") +
  annotate(geom ="text", x = 60, y = 7.5, label = "50", colour = "#FFFFFF", fontface = 2, size = 8, family = "NCAA Iowa St Cyclone") +
  annotate(geom ="text", x = 70, y = 7.5, label = "40", colour = "#FFFFFF", fontface = 2, size = 8, family = "NCAA Iowa St Cyclone") +
  annotate(geom ="text", x = 80, y = 7.5, label = "30", colour = "#FFFFFF", fontface = 2, size = 8, family = "NCAA Iowa St Cyclone") +
  annotate(geom ="text", x = 90, y = 7.5, label = "20", colour = "#FFFFFF", fontface = 2, size = 8, family = "NCAA Iowa St Cyclone") +
  annotate(geom ="text", x = 100.2, y = 7.5, label = "10", colour = "#FFFFFF", fontface = 2, size = 8, family = "NCAA Iowa St Cyclone") +
  annotate(geom ="text", x = 19.4, y = 45.83, label = "10", colour = "#FFFFFF", fontface = 2, angle = 180, size = 8, family = "NCAA Iowa St Cyclone") + ## Put the numbers on the other side, angle is flipped so the numbers are upside down
  annotate(geom ="text", x = 30, y = 45.83, label = "20", colour = "#FFFFFF", fontface = 2, angle = 180, size = 8, family = "NCAA Iowa St Cyclone") +
  annotate(geom ="text", x = 40, y = 45.83, label = "30", colour = "#FFFFFF", fontface = 2, angle = 180, size = 8, family = "NCAA Iowa St Cyclone") +
  annotate(geom ="text", x = 50, y = 45.83, label = "40", colour = "#FFFFFF", fontface = 2, angle = 180, size = 8, family = "NCAA Iowa St Cyclone") +
  annotate(geom ="text", x = 60, y = 45.83, label = "50", colour = "#FFFFFF", fontface = 2, angle = 180, size = 8, family = "NCAA Iowa St Cyclone") +
  annotate(geom ="text", x = 70, y = 45.83, label = "40", colour = "#FFFFFF", fontface = 2, angle = 180, size = 8, family = "NCAA Iowa St Cyclone") +
  annotate(geom ="text", x = 80, y = 45.83, label = "30", colour = "#FFFFFF", fontface = 2, angle = 180, size = 8, family = "NCAA Iowa St Cyclone") +
  annotate(geom ="text", x = 90, y = 45.83, label = "20", colour = "#FFFFFF", fontface = 2, angle = 180, size = 8, family = "NCAA Iowa St Cyclone") +
  annotate(geom ="text", x = 99.8, y = 45.83, label = "10", colour = "#FFFFFF", fontface = 2, angle = 180, size = 8, family = "NCAA Iowa St Cyclone") +
  annotation_raster(mypng, ymin = 18, ymax = 35.5, xmin = 52, xmax = 68) + ##This is where you add the Midfield Logo
  annotation_raster(isu_wordmark, ymin = 9, ymax = 41, xmin = -10, xmax = 20) +
  annotation_raster(cyclones_wordmark, ymin = 10, ymax = 40, xmin = 130, xmax = 100) +
  geom_segment(aes(x = 13, xend = 13, y = 26, yend = 27.33), colour = "#FFFFFF") + ##Add the extra pt conversion mark
  geom_segment(aes(x = 107, xend = 107, y = 26, yend = 27.33), colour = "#FFFFFF") + #+
  theme(rect = element_blank(), ##This removes all of the exterior lines from a typical ggplot
        line = element_blank(),
        text = element_blank()) +
  coord_fixed()

football_field

ggsave("field_jack_trice.png", width = 12, height = 5.5)

