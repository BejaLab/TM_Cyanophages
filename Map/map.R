suppressMessages(library(dplyr))
library(tidyr)
library(maps)
library(ggplot2)
library(ggthemes)

#from the anvi'o summary MAGs-SUMMARY/bins_across_samples/relative_abundance.txt
df.abundance <- read.csv("./relative_abundance.txt", sep = "\t")

df.tara_metadata <- read.csv("../data/TARA_metadata.csv")[ ,c("dataset", "Latitude_Start", "Longitude_Start", "Station", "fraction")]

abundance <- gather(df.abundance,dataset,rel_abundance, -bins)

abundance <- inner_join(abundance, df.tara_metadata)

abundance <- abundance %>% filter(fraction != "GIRUS")

gg <- ggplot()

wrld <- map_data("world")

xlims = c(-155, 70)
ylims = c(-50, 50)

p <- ggplot()
p <- p + theme(panel.background = element_rect(fill =NA),
               panel.border = element_rect(colour = "#000000",
                                           size = 1,
                                           linetype = "solid",
                                           fill = NA),
               axis.title = element_blank(),
               axis.ticks.x = element_blank(),
               axis.text.x = element_blank(),
               axis.text.y = element_text(),
               axis.ticks.y = element_line(),
               legend.position="bottom",
               legend.background = element_rect(fill="white", colour = "black"),
               legend.key = element_rect(fill=NA))

#Draws the map and assigns background color for continents
p <-p + geom_polygon( data=wrld, aes(x=long, y=lat, group = group), colour="#4d4d4d", fill="#4d4d4d")#,colour="black", fill="black" )

#Plots negative stations
neg_map <- p + geom_point( data=abundance %>% filter(rel_abundance == 0),
                           shape = 21,
#                           colour = "#a7a7a7",
#                           fill = "#a7a7a7",
                           colour="black",fill="black",
                           size = 0.5,
                           aes(x=Longitude_Start, y=Latitude_Start)
)

#Add positive stations sized by rel_abundance

p <- neg_map + geom_point( data=abundance %>%
                             filter(rel_abundance > 0), 
                     shape=21, 
                     colour="#b21616", fill="#e84646",
                     aes(x=Longitude_Start, y=Latitude_Start, size=rel_abundance)
                     )
# create facetted plot by fraction
p <- p + facet_wrap(~fraction) + coord_quickmap() + theme_map()

p <- p + coord_fixed(xlim = xlims, ylim = ylims)

p
