NON_TRAIT_COLUMNS = c("studyDbId", "studyName", "programName", "year", "locationName", "observationUnitDbId", "observationUnitName", "germplasmDbId", "germplasmName", "rowNumber", "colNumber", "plot", "rep", "block")

getTraitNames = function(data) {
	cols = colnames(data)
	traits = cols[which(!cols %in% NON_TRAIT_COLUMNS)]
	return(traits)
}