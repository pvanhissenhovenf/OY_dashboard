#OY mapping all orgs
#PVH
#August 15, 2017
setwd("/Users/patricia/Desktop/r/OY")
install.packages("splancs")
install.packages("pander")
install.packages("xlsx")
#Import packages
library(rgdal)
library(splancs)
library(dplyr)
library(magrittr)
library(pander)
library(readxl)


#TUTORIAL 
# save copied link as a character vector
geojson_comarea_url <- "https://data.cityofchicago.org/api/geospatial/cauq-8yn6?method=export&format=GeoJSON"

# transform vector into spatial dataframe
comarea606 <- readOGR( dsn = geojson_comarea_url
                       , layer = "OGRGeoJSON"
                       , stringsAsFactors = FALSE
)
#plot polygons
plot(comarea606)

#Importing neighborhood data
geojson_nhood_url <- "https://data.cityofchicago.org/api/geospatial/bbvz-uum9?method=export&format=GeoJSON"

# create spatial dataframe for neighborhoods
nhood606 <- readOGR( dsn = geojson_nhood_url
                     , layer = "OGRGeoJSON"
                     , stringsAsFactors = FALSE)
#plot neighborhoods
plot(nhood606)


#importing excel into R and viewing data
org_loc_df <- read.csv( file = "/Users/patricia/Desktop/r/OY/taxonomy100.csv")
View(org_loc_df)

#Create placeholder for community area and neighborhood
# Create 'Community_Area' variable inside the data frame
# and assign it the string "Replace Me"
dim(org_loc_df) #479 12
org_loc_df$Community_Area <- "Replace Me"
org_loc_df$Neighborhood <- "Replace Me"
dim(org_loc_df) # 479 14

#Tutorial get matrix of spatial polygon dataframe
get_poly_matrix_coord <- function( spatial_poly_df ) {
  # start counter
  i <- 1
  # create empty list
  empty_list <- list()
  # start while loop
  while( nrow( spatial_poly_df ) >= i ) {
    # fill the empty list with one set of coordinates
    empty_list[[i]] <- spatial_poly_df@polygons[[i]]@Polygons[[1]]@coords
    # add 1 to the counter
    i <- i + 1
  } # end of while loop
  return( empty_list )
} # end of function

class(comarea606@polygons)# this object is a list
comarea606@polygons #this shows all 77 lists
comarea606@polygons[[77]]#this gives you slots 
comarea606@polygons[[77]]@Polygons[[1]]@coords #this yields all of the coordinate pairs for each community area

#Get the coordinate points of each polygon in comarea606
com_area_polygons <- get_poly_matrix_coord( comarea606 )
com_area_polygons
#Naming the polygons
names(com_area_polygons)<-comarea606$community
#names(nhood_polygons)<-nhood606$sec_neigh

#Run through the entire matrix and assign Community Area names to each coordinate pair
get_CA_names <- function( a.data.frame, a.list.of.matrices, a.spatial.df ) {
  # ensure necessary packages are imported
  require( splancs )
  require( dplyr )
  # start your counter
  i <- 1
  
  # start your while loop
  while( i <= length( a.list.of.matrices )  ) {
    # 1. df with only long and lat
    df_lon_lat <- select( a.data.frame
                          , Longitude # double check
                          , Latitude # double check
    )
    # rename long as x and lat as y
    colnames(df_lon_lat)[1] <- "x"
    colnames(df_lon_lat)[2]  <- "y"
    
    # 2. add in.shape to dataframe
    df_lon_lat$in.shape <- 1:nrow( df_lon_lat) %in%
      inpip( df_lon_lat
             , a.list.of.matrices[[i]]
      )
    #### THIS IS WHERE YOU HAVE TO FILTER BASED ON WHETHER
    #### df_lon_lat$in.shape has any TRUE values
    #### if yes, procceed with steps 3-6
    #### if no, tell the counter to increase by 1
    #### and restart the function
    if( any( df_lon_lat$in.shape ) == FALSE ) {
      # add one to counter
      i <- i + 1
    } else{
      # 3. give me the elements that are TRUE
      only_true <- df_lon_lat[ df_lon_lat$in.shape == TRUE, ]
      
      # 4. filter the orgs data frame by the row names within step 3
      # 5. and assign the Community Area polygon name
      a.data.frame[ as.numeric( row.names( only_true ) ), ]$Community_Area <- a.spatial.df$community[i]
      
      # 6. repeat until all community areas are given a name
      i <- i + 1
    } # end of else statement
    
  } # end of while loop
  # return a new data frame
  return( a.data.frame )
} # end of the function
org_loc_df <- get_CA_names( a.data.frame = org_loc_df
                            , a.list.of.matrices = com_area_polygons
                            , a.spatial.df = comarea606 
)

View(org_loc_df)
#FOR NEIGHBORHOODS
#What is the class of nhood606
class(nhood606)

#Where are the lists of coordinates for every neighborhood in the nhood spatial polygon dataframe
nhood606@polygons
nhood606@polygons[[77]]@Polygons[[1]]@coords

#use get_poly_matrix_coord to find the lists of coordinates in each neighborhood
nhood_polygons<-get_poly_matrix_coord(nhood606)
nhood_polygons


#naming the polygons 
names(nhood_polygons)<-nhood606$sec_neigh
org_loc_df <- get_CA_names( a.data.frame = org_loc_df
                            , a.list.of.matrices = nhood_polygons
                            , a.spatial.df = nhood606)
View(org_loc_df)

#write CSV in R
write.csv(org_loc_df, file = "/Users/patricia/Desktop/r/OY/tax_full.csv")