#### skript has no own code, made only to play
# 10.2.23


if(!require('mapview')) {
  install.packages('mapview')
  library('mapview')
}


mapview()

## simple features ====================================================
library(sf)

# sf
mapview(breweries)
mapview(franconia)

# sfc
mapview(st_geometry(breweries)) # no popup

# sfg / XY - taken from ?sf::st_point
outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
pts = list(outer, hole1, hole2)
(pl1 = st_polygon(pts))
mapview(pl1)

## raster ==============================================================
if (interactive()) {
  library(plainview)
  
  mapview(plainview::poppendorf[[5]])
}

## spatial objects =====================================================
mapview(leaflet::gadmCHE)
mapview(leaflet::atlStorms2005)


## styling options & legends ===========================================
mapview(franconia, color = "white", col.regions = "red")
mapview(franconia, color = "magenta", col.regions = "white")

mapview(breweries, zcol = "founded")
mapview(breweries, zcol = "founded", at = seq(1400, 2200, 200), legend = TRUE)
mapview(franconia, zcol = "district", legend = TRUE)

clrs <- sf.colors
mapview(franconia, zcol = "district", col.regions = clrs, legend = TRUE)

### multiple layers ====================================================
mapview(franconia) + breweries
mapview(list(breweries, franconia))
mapview(franconia) + mapview(breweries) + trails

mapview(franconia, zcol = "district") + mapview(breweries, zcol = "village")
mapview(list(franconia, breweries),
        zcol = list("district", NULL),
        legend = list(TRUE, FALSE))


### burst ==============================================================
mapview(franconia, burst = TRUE)
mapview(franconia, burst = TRUE, hide = TRUE)
mapview(franconia, zcol = "district", burst = TRUE)


### ceci constitue la fin du pipe ======================================
library(poorman)
library(sf)

franconia %>%
  sf::st_union() %>%
  mapview()

franconia %>%
  group_by(district) %>%
  summarize() %>%
  mapview(zcol = "district")

franconia %>%
  group_by(district) %>%
  summarize() %>%
  mutate(area = st_area(.) / 1e6) %>%
  mapview(zcol = "area")

franconia %>%
  mutate(area = sf::st_area(.)) %>%
  mapview(zcol = "area", legend = TRUE)

breweries %>%
  st_intersection(franconia) %>%
  mapview(zcol = "district")

franconia %>%
  mutate(count = lengths(st_contains(., breweries))) %>%
  mapview(zcol = "count")

franconia %>%
  mutate(count = lengths(st_contains(., breweries)),
         density = count / st_area(.)) %>%
  mapview(zcol = "density")# }




library(sf)
library(mapview)
dat <- structure(list(Blong = c(-75.58333, -76.08333, -81.08333, -94.25, 
                                -75.41667, -99.41667, -77.41667, -116.08333, -89.58333, -77.58333
), Blat = c(37.58333, 40.58333, 42.75, 41.91667, 38.25, 28.25, 
            38.91667, 43.58333, 44.25, 38.91667), Elong = c(-65.91667, -75.75, 
                                                            -80.58333, -95.41667, -73.58333, -89.41667, -77.58333, -116.41667, 
                                                            -96.41667, -77.41667), Elat = c(45.91667, 40.58333, 42.75, 29.75, 
                                                                                            45.58333, 48.25, 38.75, 43.58333, 34.08333, 38.91667), Flyway = structure(c(2L, 
                                                                                                                                                                        2L, 2L, 1L, 2L, 2L, 2L, 3L, 2L, 2L), .Label = c("Central", "Eastern", 
                                                                                                                                                                                                                        "West"), class = "factor")), .Names = c("Blong", "Blat", "Elong", 
                                                                                                                                                                                                                                                                "Elat", "Flyway"), row.names = c(NA, -10L), class = c("tbl_df", 
                                                                                                                                                                                                                                                                                                                      "tbl", "data.frame"))
b = dat[, c("Blong", "Blat")]
names(b) = c("long", "lat")
e = dat[, c("Elong", "Elat")]
names(e) = c("long", "lat")

dat$geometry = do.call(
  "c", 
  lapply(seq(nrow(b)), function(i) {
    st_sfc(
      st_linestring(
        as.matrix(
          rbind(b[i, ], e[i, ])
        )
      ),
      crs = 4326
    )
  }))

dat_sf = st_as_sf(dat)

mapview(dat_sf, zcol = "Flyway")


# Load library
library(sf)

# Create points data
multipoints <- st_multipoint(matrix(c(10, 10, 15, 20, 30, 30), nrow = 3, byrow = TRUE), dim = "XY")
points <- st_cast(st_geometry(multipoints), "POINT") 

# Number of total linestrings to be created
n <- length(points) - 1

# Build linestrings
linestrings <- lapply(X = 1:n, FUN = function(x) {
  
  pair <- st_combine(c(points[x], points[x + 1]))
  line <- st_cast(pair, "LINESTRING")
  return(line)
  
})

# One MULTILINESTRING object with all the LINESTRINGS
multilinetring <- st_multilinestring(do.call("rbind", linestrings))

# Plot
plot(multipoints, pch = 19, cex = 2)
plot(multilinetring[[1]], col = "orange", lwd = 2, add = TRUE)
plot(multilinetring[[2]], col = "green", lwd = 2, add = TRUE)

mapview(multilinetring)

