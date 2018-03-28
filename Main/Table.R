countries <- list.files("../Data/map/")
countries <- countries[countries %in% c("AfricanCountries", "World", "Senegal", "Bangladesh", "Cambodia", "Indonesia", "Philippines", "Angola", "Cote_dIvoire", "Burundi", "Comoros") == F]


var.table <- NULL
for(countryname in countries){
	load(paste0("Fitted/", countryname, "-yearly.rda"))
	# beta <- model.rw2$fit$summary.fixed[2, 1]
	# tvec <- (model.rw2$newdata[!is.na(model.rw2$newdata[, "logit.est"]), "time.fix"])
	# var.fx <- beta^2 * mean((tvec-mean(tvec))^2)

	# out$variance.rw2[1, 1] <- var.fx
	# out$variance.rw2[, 2] <- out$variance.rw2[, 1] / sum(out$variance.rw2[, 1])

	print(out$variance.rw2) 
	var.table <- rbind(var.table, c(out$variance.rw2[,1], out$variance.rw2[,2]))
}
colnames(var.table) <- c(rownames(out$variance.rw2), paste0("Prop: ", rownames(out$variance.rw2)))
rownames(var.table) <- countries[1:dim(var.table)[1]]
write.csv(var.table, "Tables/variance_proportion.csv")