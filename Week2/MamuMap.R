rm(list=ls())

library(magick)
library(ggnewscale)
library(tigris)
library(raster)
library(tidyverse)
library(ggsn)
library(cowplot)


#Data frame describing sampling sites

site = structure(list(siteName = c("Cape Lookout", "Silt 1211", "Clevage", 
                                  "Bayview Alder", "Pollard Cedar", "Canal 410", "Grass 1", "Louie II", 
                                  "Canal Thinning", "Grass 2", "Sugar Maple", "Buckhorn", "Burnt Ridge", 
                                  "Boulder Rubble", "Bucktooth", "Randall Salado", "Lower Louie", 
                                  "Head East", "Bear Paw", "Green Bean", "Bullrun Head", "Alsea Rock", 
                                  "Silt 1208", "Little Rowell", "Chetco River Fuels 1", "Chetco River Fuels 2", 
                                  "West Coon 2", "West Coon 3"), siteNumber = c("1", "3", "6", 
                                                                                "7", "18", "19", "23", "25", "28", "29", "30", "32", "39", "45", 
                                                                                "47", "52", "53", "55", "56", "57", "59", "62", "63", "65", "Siskiyou", 
                                                                                "Siskiyou", "Siskiyou", "Siskiyou"), treatment = c("Control", 
                                                                                                                                   "Playback", "Control", "Playback", "Control", "Playback", "Playback", 
                                                                                                                                   "Playback", "Playback", "Control", "Playback", "Control", "Control", 
                                                                                                                                   "Playback", "Playback", "Control", "Control", "Playback", "Playback", 
                                                                                                                                   "Control", "Control", "Playback", "Control", "Control", "Playback", 
                                                                                                                                   "Control", "Control", "Playback"), easting = c(422549, 416755, 
                                                                                                                                                                                  415810, 419751, 436684, 424842, 425110, 437687, 427288, 426457, 
                                                                                                                                                                                  423047, 428363, 441927, 442337, 431877, 436807, 435770, 449862, 
                                                                                                                                                                                  438203, 434352, 439578, 440900, 443051, 452532, 403925, 403722, 
                                                                                                                                                                                  413041, 412289), northing = c(5021137, 4930836, 4880251, 4923762, 
                                                                                                                                                                                                                5008731, 4909944, 4900603, 4998453, 4910527, 4900605, 4864021, 
                                                                                                                                                                                                                4898909, 5006527, 5012220, 4904570, 4932619, 4911155, 5018587, 
                                                                                                                                                                                                                4910724, 4896705, 4925386, 4910599, 4933250, 4982813, 4660983, 
                                                                                                                                                                                                                4657202, 4666519, 4665587)), class = c("tbl_df", "tbl", "data.frame"
                                                                                                                                                                                                                ), row.names = c(NA, -28L)) %>% mutate(years = ifelse(siteNumber %in% c('Siskiyou', '65'), '2016', '2016-2017'))


ggplot(site, aes(x=easting, y=northing))+
  geom_point()

#Creating a spatial "shapefile" object
siteSpatial = SpatialPointsDataFrame(coords=cbind(site$easting, site$northing),
                                     proj4string=CRS('+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'),
                                     data=site)


#Creating a spatial study area object
studyArea = as(extent(siteSpatial)+20000, 'SpatialPolygons')
crs(studyArea) = crs(siteSpatial)

#Download state boundaries from US census bureau and subset Oregon
USA = states()
oregon = USA[USA$NAME=='Oregon',]
oregon = as(oregon, 'Spatial')
oregon = spTransform(oregon, crs(siteSpatial))

# writeOGR(oregon, "C:/Users/Jonathon/Documents/Marbled Murrelets/GIS", "Oregon", 
#          driver = "ESRI Shapefile", overwrite_layer=T)

plot(oregon)
plot(studyArea, add=T)
plot(siteSpatial, add=T)


#Might be nice to see elevation
#This is going to download elevation data to your working directory
usaElev = raster::getData("alt", country='USA', mask=T)


tmp = spTransform(oregon, crs(usaElev[[1]]))
tmp = crop(usaElev[[1]], tmp)
oregonElev = projectRaster(tmp, crs = crs(siteSpatial))

plot(oregonElev)
plot(oregon, add=T)


#Apparently the crop function crops the raster by the extent of the
#shapefile object, but I want it to represent the boundaries of
#Oregon specifically, so I'm going to mask values not inside
#the boundary

tmp = mask(oregonElev, oregon)
plot(tmp)
plot(oregon, add=T)
oregonElev = tmp

#Might look a little better if we can pull out aspect and slope

oregonAspect = terrain(oregonElev, 'Aspect')
oregonSlope = terrain(oregonElev, 'slope')

#The hillShade() function uses aspect and slope to create a more visually
#intuitive topographic image

oregonHillshade = hillShade(oregonAspect, oregonSlope)

plot(oregonHillshade,
     col=gray(1:100/100),
     legend=F)
plot(oregonElev, col=bpy.colors(200), add=T, alpha=0.5)

#Kinda nifty.  Let's zoom in on the plot area
plot(studyArea)
plot(oregonHillshade,
     col=gray(1:100/100),
     legend=F, add=T)
plot(oregonElev, col=bpy.colors(200), add=T, alpha=0.5)
plot(studyArea, add=T, border='red')
plot(siteSpatial, col='yellow', add=T)



#For right now, the other variable I'm interested in is the distribution of
#old forest which is potentially MAMU habitat.  This needs to be brought in from
#a file I provided you.

oregonOgsi80 = raster("C:/Users/Jonathon/Documents/Marbled Murrelets/R for Data Science/MapMaking/ogsi80_utm.tif")

#White inidcates forest at least 80 years old
plot(oregonOgsi80)

#I want to crop it to the study area
saOgsi80 = crop(oregonOgsi80, studyArea)
saOgsi80[is.na(saOgsi80)] = 0

saOgsi80 #See some details about the raster

#And this is a 30 m cell raster (note the resolution) which means there are a ton of cells.
#Since I just need this for visualization, I am going to aggregate those many
#small cells into fewer large cells.
saOgsi80 = aggregate(saOgsi80, fact=5, fun=mean)

saOgsi80
#So now each cell has a 150 m resolution.
#We aggregated 5-by-5 groups of cells and calculated the mean value of
#them as the value for the new cell.

#Now we use as.data.frame() to convert the raster data into a data frame
#that we can plot with ggplot

saOgsi80 = as.data.frame(saOgsi80, xy=T) %>% 
  filter(!is.na(ogsi80_utm)) %>% 
  filter(ogsi80_utm > 0.5) %>% #Getting rid of cells comprised of < 50% old forest
  mutate(ogsi80_utm = 1)


#Cropping the hillshade and elevation rasters to the study area and then
#turning them into data frames as well
saHillshade = as.data.frame(crop(oregonHillshade, studyArea), xy=T)
saElev = as.data.frame(mask(crop(oregonElev, studyArea), oregon), xy=T) %>% 
  filter(!is.na(USA1_msk_alt))
saOregon = crop(oregon, studyArea)

#This fortify() function turns shapefiles into a dataframe that can be
#plotted in ggplot with the geom_polygon() function
oregonDf = fortify(oregon)
studyAreaDf = fortify(studyArea)
saOregonDf = fortify(saOregon)

#Creating a simple map showing the location of the study area
(zoomedOut = ggplot()+
  geom_polygon(data=oregonDf, aes(x=long, y=lat, group=group), fill='#CCCCCC', color='black')+
  geom_polygon(data=studyAreaDf, aes(x=long, y=lat, group=group), fill=NA, color='red')+
  coord_fixed(ratio=1)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank())+
  theme(plot.margin=unit(c(0,0,0,0),'cm'))+
  theme(panel.border=element_blank()))

#And this is a more detailed map of the study area
(zoomedIn = ggplot()+
  coord_fixed(ratio=1)+
  geom_polygon(data=studyAreaDf, aes(x=long, y=lat, group=group), fill=NA, color='red', size=1)+
  geom_polygon(data=saOregonDf, aes(x=long, y=lat, group=group), fill='#CCCCCC', color='black')+
  #geom_raster(data=saElev, aes(x=x, y=y, fill=USA1_msk_alt), alpha=1)+
  #scale_fill_gradient(low='#333333', high='white', name='Elevation (m)',
  #                    guide=guide_legend(label.position='bottom', title.position='top',
  #                                       direction='horizontal'))+
  #new_scale_fill()+
  geom_raster(data=saOgsi80, aes(x=x, y=y, fill=as.factor(ogsi80_utm)), alpha=1)+
  scale_fill_manual(values='#009900', name='> 80 year old forest', label='')+
  new_scale_fill()+
  geom_point(data=site, aes(x=easting, y=northing, fill=treatment), size=2, alpha=1, shape=21)+
  scale_fill_manual(values=c('aquamarine2', '#FF9933'), name='Treatment')+
  #geom_point(data=site, aes(x=easting, y=northing, shape=years), size=2, fill=adjustcolor('yellow', alpha.f=0.5))+
  #scale_shape_manual(values=c(21,24), name='Years sampled')+
  theme_bw()+
  theme(panel.grid=element_blank())+
  theme(axis.text.x = element_text(angle=45, hjust=1, size=8),
        axis.text.y = element_text(size=8, angle=90, hjust=0.5))+
  theme(axis.title=element_blank())+
  theme(plot.margin=unit(c(0,0,0,0),'cm'))+
  scale_y_continuous(position='left', expand=c(0,0))+
  theme(panel.border=element_blank())+
  new_scale_fill()+
  geom_polygon(data=studyAreaDf, aes(x=long, y=lat, group=group), fill=NA, color='red', size=1))


#Now I want to bring in a couple of pictures I want to combine with the maps in the
#same figure.  The magick and cowplot packages allow this nifty-ness

img1 = image_read("C:/Users/Jonathon/Documents/Marbled Murrelets/R for Data Science/MapMaking/IMG_0155 x CA site Buckhorn.jpg")
img2 = image_read("C:/Users/Jonathon/Documents/Marbled Murrelets/R for Data Science/MapMaking/Rossiter_Stephen_20170708_gap.jpg")

#plot_grid() is one of the cowplot functions I use endlessly.  It gives you tons
#of flexibility for combining two plots together.  Here I am aggregating
#the broad scale Oregon plot with the legend

a = plot_grid(get_legend(zoomedIn+theme(legend.background=element_blank())+theme(legend.text=element_text(size=8), legend.title=element_text(size=8))+
                           theme(legend.spacing.y=unit(0.1, 'cm'))+
                           theme(legend.key.size=unit(0.25, 'cm'))),
              zoomedOut+theme(plot.margin=grid::unit(c(0,0,0,0), 'cm')),
              nrow=1, scale=c(1,1), rel_widths=c(1,1))


b = ggdraw()+draw_image(img1)
c = ggdraw()+draw_image(img2)

d = plot_grid(b, NULL, c, a, ncol=1, rel_heights=c(1,0.05,1,1), labels=c('B', '', 'C', ''), label_colour='white')
e = plot_grid(zoomedIn+theme(legend.position='none')+theme(plot.margin=grid::unit(c(0.05,0,0,0), 'cm')),
              d, nrow=1, labels=c('A', ''))

ggsave2('TEST.jpg', plot=e, width=3.5, height=4, units='in', dpi=300)






