library(sommer)
source("./utils/cleanTraitName.R")

#Stage wise genomic prediction with spatial inforamtion
stage1 <- function(Traits, data){
  print("====> RUN STAGE 1")
  print(Traits)
  print(as_tibble(data))

  BLUEL_sp <- list()
  env <- unique(data$studyName)
  print(env)

  for(i in env){
    print(sprintf("--> Environment: %s", i))
    data1 <- droplevels(data[data$studyName == i ,])
    data1$germplasmName <- as.factor(data1$germplasmName)
    for(Trait in Traits){
      print(sprintf("----> Trait: %s", Trait))
      print("Observations:")
      print(data1[Trait])

      print("...... starting mmer")
      ans <-  mmer(
                paste0(Trait,"~germplasmName -1"),
                random =~ spl2Da(rowNumber,colNumber),
                rcov =~ vsr(units),
                data = data1
              )
      print("...... finished mmer")

      ans$Beta$Env <- i
      ans$Beta$Effect <-  gsub(pattern = "germplasmName",replacement = "", x = ans$Beta$Effect)
      W <- diag(solve(ans$VarBeta))
      ans$Beta$W <- W
      colnames(ans$Beta)[2] <- "germplasmName"
      BLUEL_sp[[Trait]][[i]] <- ans$Beta
    }
  }

  return(BLUEL_sp)
}




