##############################################################
#Formatting Excel sheets sent from meta-analysis respondents##
##############################################################

#Written by Aleah Querns
#4/2024

#################################################################################
#Step 1: make character vector of study names in the same order as studies in the list and rows in the matrix 
            # PRODUCT NAME: vector_studies_noxl
#Step 2: put study matrices into an array (matrix dimension * matrix dimension * population)
            
#Step 3: Avg over years if necessary (for most of these, this seems to have been done pre-processing)

#Step 4: Put arrays into a list called "matrices_emailed"

#Step 5: make another matrix. Matrix will include lat/longs in columns (as well as population ID?), and study name as row
              # * consider making array of 1*2*N (N being max number of populations) with dimensions (study, location, population)?



##################################################################################
#                                                                                #
#                                 Packages                                       #
#                                                                                #
##################################################################################

library(dplyr)
library(readxl)
library(abind)

######################################################################################################
########################################################################################################
#                                           Step 1:                                                    #
# make character vector of study names in the same order as studies in the list and rows in the matrix #
########################################################################################################

vector_studies <-list.files( path="F:/Sensitivity meta analysis/sen-across-space/Raw data") #list of studies, not including path, including ".xlsx"

vector_studies_noxl<- gsub(".xlsx", "", vector_studies) # *********This is the character vector of study names for Allison*************

vector_studies_WITHPATH <-list.files("F:/Sensitivity meta analysis/sen-across-space/Raw data", full.names = TRUE) #this vector is useful for reading in files with for() loop


         # Now let's get vectors of pop names #
populations<- data.frame(replicate(length(vector_studies_noxl), 1:37)) # make a dataframe to hold population names; 
                               #adjust dimensions based off the study with the largest number of populations
colnames(populations) <- vector_studies_noxl
populations[,] <- "NA" #make sure everything is NAs so we don't have weirdo names for studies without max number of names


for(i in seq_along(vector_studies_noxl)){
  #read in data into unique dataframe names
  # assign(paste(vector_studies_noxl[[i]], "env", sep="_"),data.frame(read_xlsx(vector_studies_WITHPATH[i], sheet="latlong")))
  #also make dummy so we can get those pop names
dummy<-data.frame(read_xlsx(vector_studies_WITHPATH[i], sheet="latlong"))
  for( z in 1:length(dummy$population)){
    populations[z,i]<-dummy$population[z]

  }
}





########################################################################################################
#                                           Step 2-4:                                                  #
#            put study matrices into an array (matrix dimension * matrix dimension * population)       #
#                              put into list named matrices_emailed                                    #
########################################################################################################

#note that all matrices are all already avg across years so step 3 is skipped

matrices_emailed <- list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18) #specify list dimensions equal to number of studies

for( i in 1:length(populations)){ #for each of the included studies
VectorPops<-populations[i] %>% filter(populations[i] != "NA") #get populations names (NOTE: population names MUST match sheet names exactly)
for ( z in 1:nrow(VectorPops)){ #read in each sheet
    assign(paste("item1"),
           data.matrix(read_xlsx(paste(vector_studies_WITHPATH[i]),
                                 sheet = VectorPops[z,], col_names=F))) #read in individual sheets one by one, assign the name "item1"
 
   if (z==1){
    item2<- item1 #if on the first sheet, we make the first sheet "item2" to bind to next sheet
  } else{
    item2 <- abind(item1, item2, along=3) #if not on first sheet, bind new sheet to old sheet(s)
  }
  
}
#after binding together all sheets in a study into one array, add array to list
matrices_emailed[[i]] <- item2

}

#add study names to matrices
names(matrices_emailed) <- vector_studies_noxl


dimnames(matrices_emailed[[1]])

#assign populations to third dimension in each study specific array
for (t in 1:length(populations)){
  VectorPops<-populations[t] %>% filter(populations[t] != "NA")
  for( p in 1:nrow(VectorPops)){
    
    dimnames(matrices_emailed[[t]]) <- list(as.numeric(1:nrow(matrices_emailed[[t]])),
                                            as.numeric(1:ncol(matrices_emailed[[t]])),
                                            VectorPops[1:nrow(VectorPops),])
    
  }}


#######################################################################################################
#                       Step 5: make another matrix for environment                                   #      
#######################################################################################################

sum(populations != "NA") #count number of populations to figure out how many rows we need in dataframe

for(i in seq_along(vector_studies_noxl)){
  study_i <- vector_studies_noxl[i]
  # make dummy so we can get those pop latlongs
  dummy<-data.frame(read_xlsx(vector_studies_WITHPATH[i], sheet="latlong"))
  #bind study names  
  all<-cbind(study_i, dummy[, c("population", "lat", "long")])
    #if on first study, move along
    if (i==1){
      dummy2<-all
    } else { #on next study, bind to previous study
      dummy2 <- rbind(all, dummy2)
    }
  
}

colnames (dummy2) <- c("study", "population", "lat", "long")

env_emailed <-dummy2

###############################################################################
##                                                                           ##
##                                  Save Products                            ##
###############################################################################

save(matrices_emailed, file="PRODUCTS/matrices_emailed.RData") # List with matrices (each object is array with study name, third dimension in array is populations)

save(vector_studies_noxl, file="PRODUCTS/study_character_vector.RData") # character vector of study names

save(populations, file="PRODUCTS/populations_emailed.RData") #dataframe of populations within each study

save(env_emailed, file="PRODUCTS/env_emailed.RData") #dataframe of populations within each study
