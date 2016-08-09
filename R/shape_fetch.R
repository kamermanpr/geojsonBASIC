#' Get shapefiles from Natural Earth.
#'
#' Import shapefiles from \url{http://www.naturalearthdata.com}
#' and extract the contents in preperation for converting the shapefile to
#' GeoJSON using \code{geojson_convert}.
#'
#' @param ne.type Set the type of map (see \url{http://www.naturalearthdata.com/downloads/}
#' for details). Supports: 'physical' and 'cultural' (default value is
#' 'physical').
#'
#' @param ne.filename Set the map layer (see \url{http://www.naturalearthdata.com/downloads/}
#' for details). Default value is 'ne_110m_land'.
#'
#' @param ne.scale Set the map scale (see \url{http://www.naturalearthdata.com/downloads/}
#' for details). Supports: '10m' (1:10m), '50m' (1:50m), and '110m' (1:110m)
#' (default value is '110m'). Please note that '10m' file size is large.
#'
#' @param dest Set the path of the destination folder the downloaded zip file
#' will be extracted into (default path is './data/natural-earth').
#'
#' @export
shape_fetch <- function(ne.type = 'physical',
                        ne.filename = 'ne_110m_land',
                        ne.scale = '110m',
                        dest = './data/natural-earth') {
    # Make download url
    ne.url <- paste0('http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/',
                     ne.scale,
                     '/', ne.type,
                     '/', ne.filename, '.zip')

    if(!file.exists(dest)) {
        temp_shp <- tempfile(fileext = '.zip') # create temp file
        download.file(ne.url, temp_shp) # download into temp file
        unzip(temp_shp, exdir = dest) # unzip temp file into destination
    }
}
