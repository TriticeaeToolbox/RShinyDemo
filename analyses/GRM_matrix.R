#################
# Estimate the genomic relationship matrix
# better to use the dosage matrix

GRM_matix <- function(marker_data) {
  SNPs  <- as.data.frame(marker_data) # change to data frame to use some commands
  rownames(SNPs) <- SNPs$Marker # naming the row of the data
  SNPs  <-  SNPs[,-1]

  ########impute the SNPs data and estimate the GRM
  SNPs_t  <- t(SNPs) # transpose the SNP data to make geno x marker format

  # remove SNP marker if the NA values is >20%

  SNps_NA <-  c() # the number of markers with more than 20% missing data in the population
  for(i in 1:ncol(SNPs_t)){
    if(length(which(is.na(SNPs_t[,i]))) > 0.2 *nrow(SNPs_t)){
      SNps_NA = c(SNps_NA, i)
    }
  }


  # removing the markers with NA values > 20%

  if(length(SNps_NA) == 0){
    SNPs_t1 <- SNPs_t
  }else{
    SNPs_t1 = SNPs_t[,-SNps_NA]
  }


  # filter based on MAF
  SNPs_t1_MAF <- maf_filter(M =SNPs_t1, thresh = 0.05)

  #####
  # Estimate the GRM - for the Allegro genotyping protocol
  # imputing the markers using the mean values
  missing_av <-  apply(X = SNPs_t1_MAF, MARGIN = 2, FUN = mean, na.rm = T) # impute the data based on the average value
  for(i in 1:ncol(SNPs_t1_MAF)){
    SNPs_t1_MAF[is.na(SNPs_t1_MAF[,i]), i] <- missing_av[i]
  }

  GRM <-  kinship(M = SNPs_t1_MAF, type = "add") # Estimating Kinship with the additive model
  return(GRM)

}
