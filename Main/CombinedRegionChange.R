##
## Pull the information from everyone's single country script together
##
## Current have: Bangladesh, BurkinaFaso, Cambodia, Egypt, Ethiopia, Ghana, Indonesia, Malawi, Nigeria, Philippines, Uganda, Zambia, Zimbabwe
## 
getBmat <- function(countryname){
	if(countryname %in% c("Bangladesh", "Ghana")){
		return(NULL)
	}else if(countryname == "Cambodia"){
		all_possible_region <- c("banteay mean chey", "kampong cham", "kampong chhnang", "kampong speu", "kampong thom", "kandal",
                "kratie", "phnom penh","prey veng","pursat","siem reap","svay rieng","takeo","otdar mean chey",
                "battambang & krong pailin", "kampot & krong kep","krong preah sihanouk & kaoh kong", "preah vihear & steung treng", "mondol kiri & rattanak kiri")
		final_region <- c("banteay mean chey", "kampong cham", "kampong chhnang", "kampong speu", "kampong thom", "kandal",
		                "kratie", "phnom penh","prey veng","pursat","siem reap","svay rieng","takeo","otdar mean chey",
		                "battambang & krong pailin", "kampot & krong kep","krong preah sihanouk & kaoh kong", "preah vihear & steung treng", "mondol kiri & rattanak kiri")
		Bmat <- matrix(0, length(all_possible_region), length(final_region))
		colnames(Bmat) <- final_region
		rownames(Bmat) <- all_possible_region
		Bmat[1, 1] <- 1
		Bmat[2, 2] <- 1
		Bmat[3, 3] <- 1
		Bmat[4, 4] <- 1
		Bmat[5, 5] <- 1
		Bmat[6, 6] <- 1
		Bmat[7, 7] <- 1
		Bmat[8, 8] <- 1
		Bmat[9, 9] <- 1
		Bmat[10, 10] <- 1
		Bmat[11, 11] <- 1
		Bmat[12, 12] <- 1
		Bmat[13, 13] <- 1
		Bmat[14, 14] <- 1
		Bmat[15, 15] <- 1
		Bmat[16, 16] <- 1
		Bmat[17, 17] <- 1
		Bmat[18, 18] <- 1
		Bmat[19, 19] <- 1
		return(Bmat)
	}else if(countryname == "Ethiopia"){
		# all_possible_region <- c("addis abeba", "afar", "amhara", "ben-gumz", "dire dawa", "gambela", "harari", "oromiya", "snnp", "somali", "tigray")
		# final_region <- c("addis abeba", "afar", "amhara", "ben-gumz", "dire dawa", "gambela", "harari", "oromiya", "snnp", "somali", "tigray")
		# Bmat <- matrix(0, length(all_possible_region), length(final_region))
		# colnames(Bmat) <- final_region
		# rownames(Bmat) <- all_possible_region
		# Bmat[1, 1] <- 1
		# Bmat[2, 2] <- 1
		# Bmat[3, 3] <- 1
		# Bmat[4, 4] <- 1
		# Bmat[5, 5] <- 1
		# Bmat[6, 6] <- 1
		# Bmat[7, 7] <- 1
		# Bmat[8, 8] <- 1
		# Bmat[9, 9] <- 1
		# Bmat[10, 10] <- 1
		# Bmat[11, 11] <- 1
		return(NULL)

	}else if(countryname == "Zambia"){
		all_possible_region <- c("central", "copperbelt", "eastern", "luapula", "lusaka", "northern", "north-western", "southern", "western")
		final_region <- c("central", "copperbelt", "eastern", "luapula", "lusaka", "northern", "north-western", "southern", "western")
		Bmat <- matrix(0, length(all_possible_region), length(final_region))
		colnames(Bmat) <- final_region
		rownames(Bmat) <- all_possible_region
		Bmat[1, 1] <- 1
		Bmat[2, 2] <- 1
		Bmat[3, 3] <- 1
		Bmat[4, 4] <- 1
		Bmat[5, 5] <- 1
		Bmat[6, 6] <- 1
		Bmat[7, 7] <- 1
		Bmat[8, 8] <- 1
		Bmat[9, 9] <- 1
		return(Bmat)

	}else if(countryname == "Burkina Faso"){
		return(NULL)
	}else if(countryname == "Rwanda"){
		# all_possible_region <- c( "kigali city", "south", "west", "north", "east", "city of kigali", "kigali ngali", "gitarama", "butare", "gikongoro", "cyangugu", "kibuye", "gisenyi", "ruhengeri", "byumba", "umutara", "kibungo", "kigali ville (pvk)", "kigali rurale", "kigali", "northwest", "southwest", "central, south", "northeast", "ville de kigali", "sud", "ouest", "nord", "est")
		# final_region <- c("West", "South", "North", "Kigali", "East")
		# Bmat <- matrix(0, length(all_possible_region), length(final_region))
		# colnames(Bmat) <- final_region
		# rownames(Bmat) <- all_possible_region
		# Bmat[1, 4] <- 1
		# Bmat[2, 2] <- 1
		# Bmat[3, 1] <- 1
		# Bmat[4, 3] <- 1
		# Bmat[5, 5] <- 1
		# Bmat[6, 4] <- 1
		# Bmat[7, 4] <- 1
		# Bmat[8, 2] <- 1
		# Bmat[9, 2] <- 1
		# Bmat[10, 2] <- 1
		# Bmat[11, 1] <- 1
		# Bmat[12, 1] <- 1
		# Bmat[13, 1] <- 1
		# Bmat[14, 3] <- 1
		# Bmat[15, 3] <- 1
		# Bmat[16, 5] <- 1
		# Bmat[17, 5] <- 1
		# Bmat[18, 4] <- 1
		# Bmat[19, 4] <- 1
		# Bmat[20, 4] <- 1
		# Bmat[21, 1] <- 1
		# Bmat[22, 1] <- 1
		# Bmat[23, 2] <- 1
		# Bmat[24, 5] <- 1
		# Bmat[25, 4] <- 1
		# Bmat[26, 2] <- 1
		# Bmat[27, 1] <- 1
		# Bmat[28, 3] <- 1
		# Bmat[29, 5] <- 1
		Bmat <- NULL
		return(Bmat)
	}else if(countryname == "Egypt"){
		return(NULL)
	}else if(countryname == "Indonesia"){
		return(NULL)
	}else if(countryname == "Malawi"){
		all_possible_region <- c("northern region", "central region", "southern region", "north", "central", "south", "northern", "southern")
		final_region <- c("northern region", "central region", "southern region")
		Bmat <- matrix(0, length(all_possible_region), length(final_region))
		colnames(Bmat) <- final_region
		rownames(Bmat) <- all_possible_region
		Bmat[1, 1] <- 1
		Bmat[2, 2] <- 1
		Bmat[3, 3] <- 1
		Bmat[4, 1] <- 1
		Bmat[5, 2] <- 1
		Bmat[6, 3] <- 1
		Bmat[7, 1] <- 1
		Bmat[8, 3] <- 1
		return(Bmat)
	}else if(countryname == "Nigeria"){
		return(NULL)
	}else if(countryname == "Philippines"){
		return(NULL)		
	}else if(countryname == "Uganda"){
		all_possible_region <- c( "kampala", "central 1", "central 2", "east central", "eastern", "north", "karamoja", "west-nile", "western", "southwest", "west nile", "central", "northern", "east", "west", "south west")
		final_region <- c("Northern", "Western", "Central", "Eastern")
		Bmat <- matrix(0, length(all_possible_region), length(final_region))
		colnames(Bmat) <- final_region
		rownames(Bmat) <- all_possible_region
		Bmat[1, 3] <- 1
		Bmat[2, 3] <- 1
		Bmat[3, 3] <- 1
		Bmat[4, 4] <- 1
		Bmat[5, 4] <- 1
		Bmat[6, 1] <- 1
		Bmat[7, 1] <- 1
		Bmat[8, 1] <- 1
		Bmat[9, 2] <- 1
		Bmat[10, 2] <- 1
		Bmat[11, 1] <- 1
		Bmat[12, 3] <- 1
		Bmat[13, 1] <- 1
		Bmat[14, 4] <- 1
		Bmat[15, 2] <- 1
		Bmat[16, 2] <- 1
		return(Bmat)
	}else if(countryname == "Zimbabwe"){
		all_possible_region <- c("manicaland", "mashonaland central", "mashonaland east", "mashonaland west", "matabeleland north", "matabeleland south", "midlands", "masvingo", "harare/chitungwiza", "bulawayo", "matebeleland north", "matebeleland south", "harare")
		# harare/chitungwiza -> harare
		final_region <- c("manicaland", "mashonaland central", "mashonaland east", "mashonaland west", "matabeleland north", "matabeleland south", "midlands", "masvingo", "harare", "bulawayo")
		Bmat <- matrix(0, length(all_possible_region), length(final_region))
		colnames(Bmat) <- final_region
		rownames(Bmat) <- all_possible_region
		Bmat[1, 1] <- 1
		Bmat[2, 2] <- 1
		Bmat[3, 3] <- 1
		Bmat[4, 4] <- 1
		Bmat[5, 5] <- 1
		Bmat[6, 6] <- 1
		Bmat[7, 7] <- 1
		Bmat[8, 8] <- 1
		Bmat[9, 9] <- 1
		Bmat[10, 10] <- 1
		Bmat[11, 5] <- 1
		Bmat[12, 6] <- 1
		Bmat[13, 9] <- 1
		return(Bmat)
	}else if(countryname == "Cameroon"){
		return(NULL)
	}else if(countryname == "Mali"){
		return(NULL)
	}else if(countryname == "Morocco"){
		return(NULL)
	}else if(countryname == "Niger"){
		return(NULL)
	}


}