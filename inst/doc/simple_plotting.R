## ------------------------------------------------------------------------
library(c14bazAAR)
library(ggplot2)
library(magrittr)

## ------------------------------------------------------------------------
aDRAC <- get_aDRAC()

## ------------------------------------------------------------------------
Batalimo <- aDRAC %>%
  dplyr::filter(site == "Batalimo")

## ---- include = FALSE----------------------------------------------------
Batalimo_calibrated <- Batalimo %>%
  calibrate(choices = "calprobdistr")

## ---- eval = FALSE-------------------------------------------------------
#  Batalimo_calibrated <- Batalimo %>%
#    calibrate(choices = "calprobdistr")

## ---- echo=FALSE---------------------------------------------------------
Batalimo_calibrated

## ------------------------------------------------------------------------
Batalimo_cal_dens <- Batalimo_calibrated %>% tidyr::unnest()

## ------------------------------------------------------------------------
Batalimo_cal_dens %>%
  ggplot() +
  # a special geom for ridgeplots is provided by the ggridges package
  ggridges::geom_ridgeline(
    # the relevant variables that have to be mapped for this geom are 
    # x (the time -- here the calibrated age transformed to calBC), 
    # y (the individual lab number of the dates) and
    # height (the probability for each year and date) 
    aes(x = -calage + 1950, y = labnr, height = density),
    # ridgeplots lack a scientifically clear y axis for each 
    # distribution plot and we can adjust the scaling to our needs
    scale = 300
  ) +
  xlab("age calBC/calAD") +
  ylab("dates")

## ------------------------------------------------------------------------
load(system.file('data/intcal13.rda', package = 'Bchron'))

## ---- include = FALSE----------------------------------------------------
Batalimo_calibrated <- Batalimo %>%
  calibrate(choices = "calrange")

## ---- eval = FALSE-------------------------------------------------------
#  Batalimo_calibrated <- Batalimo %>%
#    calibrate(choices = "calrange")

## ------------------------------------------------------------------------
Batalimo_calibrated$calrange[1:3]

## ------------------------------------------------------------------------
Batalimo_cal_range <- Batalimo_calibrated %>% tidyr::unnest()

## ---- warning=FALSE------------------------------------------------------
ggplot() +
  # line plot of the intcal curve
  geom_line(
    data = intcal13,
    # again we transform the age information from BP to BC
    mapping = aes(x = -V1 + 1950, y = -V2 + 1950)
  ) +
  # the errorbars are plotted on top of the curve
  geom_errorbarh(
    data = Batalimo_cal_range,
    mapping = aes(y = -c14age + 1950, xmin = -to + 1950, xmax = -from + 1950)
  ) +
  # we define the age range manually -- typically the calcurve
  # is arranged to go from the top left to the bottom right corner
  xlim(-1000, 2000) +
  ylim(2000, -1000) +
  xlab("age calBC/calAD") +
  ylab("uncalibrated age BC/AD")

## ------------------------------------------------------------------------
aDRAC_sf <- aDRAC %>% as.sf()

## ---- echo=FALSE---------------------------------------------------------
aDRAC_sf %>% dplyr::select(data.labnr, data.c14age, data.c14std, geom)

## ------------------------------------------------------------------------
Moga_spatial <- aDRAC_sf %>%
  dplyr::filter(grepl("Moga 2008", data.shortref)) %>%
  dplyr::group_by(data.site) %>%
  dplyr::summarise()

## ------------------------------------------------------------------------
Moga_spatial %>% mapview::mapview()

## ------------------------------------------------------------------------
countries <- rnaturalearth::ne_countries() %>% sf::st_as_sf()

## ---- warning=FALSE------------------------------------------------------
ggplot() +
  # geom_sf is a special geom to handle spatial data in the sf format
  geom_sf(data = countries) +
  # the explicit mapping of variables is not necessary here, as geom_sf 
  # automatically finds the *geom* column in the input table
  geom_sf_text(data = countries, mapping = aes(label = formal_en), size = 2) +
  geom_sf(data = Moga_spatial) +
  # with geom_sf comes coord_sf to manage the underlying coordinate grid
  coord_sf(xlim = c(10, 30), ylim = c(0, 15))

