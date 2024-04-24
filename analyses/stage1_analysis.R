library(sommer)

#Stage wise genomic prediction with spatial inforamtion
stage1 <- function(Traits, data) {
  BLUEL_sp <- list()
  env <- unique(data$studyName)

  withProgress(message = "Calculating BLUE Estimates...", value = 0, min = 0, max = length(env), {
    for(i in env) {
      print(sprintf("--> ENV: %s", i))
      incProgress(amount = 1, message = i)

      data1 <- droplevels(data[data$studyName == i ,])
      TraitN = colnames(data1[Traits])[colSums(is.na(data1[Traits])) < 25]
      data1$germplasmName <- as.factor(data1$germplasmName)
      for(Trait in TraitN) {
        print(sprintf("----> TRAIT: %s", Trait))
        incProgress(amount = 0, message = i, detail = Trait)

        try(eval(parse(text = paste("ans <- mmer(",Trait,"~germplasmName -1,
                   random=~  spl2Da(rowNumber,colNumber) ,
                   rcov=~vsr(units),
                   data= data1)"))), silent = TRUE)

        ans$Beta$Env <- i
        ans$Beta$Effect <-  gsub(pattern = "germplasmName",replacement = "", x = ans$Beta$Effect)
        W <- diag(solve(ans$VarBeta))
        ans$Beta$W <- W
        colnames(ans$Beta)[2] <- "germplasmName"
        BLUEL_sp[[Trait]][[i]] <- ans$Beta
        # # to be comparable to 1/(se^2) = 1/PEV = 1/Ci = 1/[(X'X)inv]
        # vcov_sp[[Trait]][[i]] <- solve(ans$VarBeta)
        # output <- list(BLUE = BLUEL_sp, vcov = vcov_sp)
      }
    }
  })

  return(BLUEL_sp)
}
