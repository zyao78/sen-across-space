setwd("~/Desktop/Meta-analysis")
library("Rcompadre")
# compadre <- cdb_fetch("compadre")
# compadre_mp <- compadre[which(compadre$NumberPopulations>1),]
# 
warning("discard knight 2003, because you have knight 2009 from the email campaign with more data")
warning("discard Menges et al. 2004, because populations differ in fire frequency? should check this paper")
# str(compadre_mp)
warning("Remove Iler study. We got them from email, but were uploaded to COMPADRE after")
# 
# for (i in 1:dim(compadre_mp)[1]){
#   compadre_mp[i,]@data$mat[[1]]@matA
#   
# }



# here is Thomas' code
# load in the compadre files
compadre <- cdb_fetch('compadre')

# Filter compadre data to keep matrices that have >1 population, have a latitude and longitude coordinate, "Individual" in the "MatrixComposite" column, "Unmanipulated" in the "MatrixTreatment" column, and no "Algae" in the "OrganismType" column
compadre_mp <- compadre[complete.cases(compadre@data[, c("Lat", "Lon")]) & 
                          compadre@data$MatrixComposite %in% "Individual" & 
                          compadre@data$MatrixTreatment %in% "Unmanipulated" & 
                          compadre@data$NumberPopulations > 1 & 
                          compadre@data$OrganismType != "Algae", ]

# filter results to keep one example from each unique publication 
unique_compadre_mp <- compadre_mp[
  which(!duplicated(paste(compadre_mp$Authors, compadre_mp$Journal,compadre_mp$YearPublication)))]

#view results
unique_compadre_mp

# transform data into a visible data frame 
unique_compadre_mp_df <- as.data.frame(unique_compadre_mp)

# Subset the data frame to include only the desired columns
subset_df <- unique_compadre_mp_df[, c("Authors", "Journal", "YearPublication", "DOI_ISBN")]

# Export the subsetted data frame as a CSV file
write.csv(subset_df, file = "unique_compadre_mp.csv", row.names = FALSE)


###### Sarah's code

# stages in matrices
stages <-  matrixClass(unique_compadre_mp)
stages[[1]]$MatrixClassAuthor # one of the mats
# length of matrix for each study
lengths <- unique_compadre_mp$MatrixDimension # number of dimensions for each matrix
# make summary table
dims_mat <- matrix(data = NA, nrow = nrow(unique_compadre_mp_df), ncol = max(lengths)+3, dimnames = list(c(1:nrow(unique_compadre_mp_df)), c(paste0("sizeclass", 1:max(lengths)), "num_pops_in_study", "num_dims", "species")))
for (i in 1:nrow(unique_compadre_mp_df)) {
  dims_mat[i,1:length(stages[[i]]$MatrixClassAuthor)] <- stages[[i]]$MatrixClassAuthor # add dimension names to matrix
  dims_mat[i, ncol(dims_mat)-2] <- unique_compadre_mp_df$NumberPopulations[i] # add number of species in study
  dims_mat[i, ncol(dims_mat)-1] <- lengths[i] # add number of dimensions to matrix (second to last col)
  dims_mat[i, ncol(dims_mat)] <- unique_compadre_mp_df$SpeciesAccepted[i] # add species to mat (last col)
}

# filter studies based on manuscript filtering/email campaign 
screening_res <- readxl::read_xlsx("03_initial screening results 17Nov23.xlsx")

# re format compadre df to match screening results
n <- 2 # remove authors after the nth ;
auth <- vapply(
    strsplit(unique_compadre_mp_df$Authors, '; '), # split by ; (and a white space to clean everything up)
  function(x) 
    paste(x[seq.int(n)], collapse=';'), character(1L)) # bring nth authors back
auth <- stringr::str_replace(auth, "; ", ";") # remove any extra spaces after the ;
auth <- stringr::str_remove(auth, ";NA") # get rid of ;NA

# deal with 3+ authors
auth[lengths(strsplit(unique_compadre_mp_df$Authors, '; ')) >2] <- 
  paste0(
    stringr::str_remove(auth[lengths(strsplit(unique_compadre_mp_df$Authors, '; ')) >2], "\\;[^;]*$"), # remove second author so that it can be replaced with 'et al'
    " et al") # add et al for when there are 3+ authors
# deal with 2 authors
auth[lengths(strsplit(unique_compadre_mp_df$Authors, '; ')) == 2] <- 
  stringr::str_replace(auth[lengths(strsplit(unique_compadre_mp_df$Authors, '; ')) == 2], ";", " & ")
# add publication year
auth_compadre <- paste(auth, unique_compadre_mp_df$YearPublication)

# find accepted studies from literature search and see if they are in COMPADRE
# studies in our list but *not* in compadre
setdiff(screening_res$`Study (PDF Title)`, auth_compadre)
# find studies *not* in literature search but are in COMPADRE
setdiff(auth_compadre, screening_res$`Study (PDF Title)`)

# hi Sarah, anther thing. can you add into the code a ‘catch’ that asks whether the data that we got via email is not in compadre (and vice versa, though we have already checked that once)?
# # studies in our database but *not* in compadre
setdiff(screening_res[screening_res$`matrices included in database and in correct format (Y/N)` == "Y",]$`Study (PDF Title)`,
        auth_compadre)
# # find studies *not* in database search but are in COMPADRE
# setdiff(auth_compadre, 
#         screening_res[screening_res$`matrices included in database and in correct format (Y/N)` == "Y",]$`Study (PDF Title)`)
# studies in both our dataset *and* compadre
overlap <- intersect(screening_res[screening_res$`matrices included in database and in correct format (Y/N)` == "Y",]$`Study (PDF Title)`,
          auth_compadre)
if (length(overlap) > 0) { warning("Be careful, there are matrices in compadre that are also in our database. Need to remove one to prevent duplicates")}

  