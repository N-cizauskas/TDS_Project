# step 1: import packages
library(devtools)
# install arcdiagram
#devtools::install_github('gastonstat/arcdiagram')
# load arcdiagram
library(arcdiagram)
library(igraph)

# step 2: import data
final_femaletrain_2203 <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/final_femaletrain_2203.rds")
final_maletrain_2203 <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/final_maletrain_2203.rds")
graph <- read.graph("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DIVYA/trial_G.gml", format="gml")


# step 3: get graph features
# get edgelist
edgelist = get.edgelist(graph)
# get vertex labels
vlabels = get.vertex.attribute(graph, "label")
# get vertex groups
vgroups = get.vertex.attribute(graph, "group")
# get vertex fill color
vfill = get.vertex.attribute(graph, "fill")
# get vertex border color
vborders = get.vertex.attribute(graph, "border")
# get vertex degree
degrees = degree(graph)
# get edges value
values = get.edge.attribute(graph, "value")