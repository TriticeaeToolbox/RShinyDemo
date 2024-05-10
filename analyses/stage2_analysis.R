
#Traits is the list of trait names to be analyzed
# data is the BLUE mean obtained from the first stage analysis
stage2 <- function(data,GRM){
  TraitN <- names(data)
  
  #Combine the mean of the Traits across environment
  BLUE_Trait <- list()
  for(Trait in TraitN){
    BLUE1 <- do.call(rbind,data[[Trait]])
    BLUE_Trait[[Trait]] <- BLUE1
  }

  #The genomic prediction section
  GEBV_value <- list()

  withProgress(message = "Calculating GEBVs...", value = 0, min = 0, max = length(TraitN), {
    for(Trait in TraitN) {
      incProgress(amount = 1, detail = Trait)

      BLUEsel <- BLUE_Trait[[Trait]]
      idg <- BLUEsel$germplasmName %in% rownames(GRM)
      BLUEsel1 <- BLUEsel[idg,]
      BLUEsel1$germplasmName <- as.factor(BLUEsel1$germplasmName)
      BLUEsel1$Env <- as.factor(BLUEsel1$Env)
      ans2 <- mmer(fixed = Estimate  ~ Env,
                   random = ~ vsr(germplasmName, Gu = GRM) + vsr(usr(Env), germplasmName, Gu = GRM),weights = W,
                   data = BLUEsel1)

      BLUP1 <- as.data.frame(ans2$U$`u:germplasmName`$Estimate + ans2$Beta[1,"Estimate"])
      head(BLUP1)
      colnames(BLUP1) <- "GEBVs"
      BLUP1$germplasmName <- rownames(BLUP1)
      BLUP1 <- BLUP1[,c(2,1)]
      rownames(BLUP1) <- NULL

      BLUP1 <- cbind(Trait =  Trait, BLUP1)
      env <- unique(BLUEsel1$Env)

      BLUPgxe <- list()
      BLUPgxecomb <- list()
      ssgxe <- c()
      for(i in env){
        ss <- names(ans2$U)[which(str_detect(pattern = names(ans2$U),
                      string = paste(c(i,"germplasmName"),collapse = ":")))]
        ssgxe <- c(ssgxe,ss)
      }

      for(s in ssgxe){
        Blup1gxe <- as.data.frame(ans2$U[[s]]$Estimate + ans2$Beta[1,"Estimate"])
        colnames(Blup1gxe)[1] <- "GEBV"
        Blup1gxe$germplasmName <- rownames(Blup1gxe)
        rownames(Blup1gxe) <- NULL
        Blup1gxe <- Blup1gxe[,c(2,1)]
        Blup1gxe <- cbind(Trial = s, Trait = Trait, Blup1gxe)
        BLUPgxe[[s]] <- Blup1gxe
      }

      BLUPgxecomb <- do.call(rbind, BLUPgxe)
      rownames(BLUPgxecomb) <- NULL
      GEBV_value[[Trait]] <- list(GEBV_G = BLUP1, GEBV_GxE = BLUPgxecomb)
    }

  })

  return(GEBV_value)
}

