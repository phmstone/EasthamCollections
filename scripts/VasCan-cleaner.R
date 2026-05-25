# Script to read in the VasCan table and clean the data


############################################################
# 0. Load VasCan
############################################################

VasCan <- read_tsv("vascan-Canada.tsv")

VasCan <- read_tsv("../VASCAN/VASCAN-BC-checklist.tsv")

# use this one
# made it by ticking everything from mainland canada (e.g. not greenland, st pierre et miquelon) and downloading whole checklist
# including absent/extirpated/ephemeral species
VasCan <- read_tsv("../VASCAN/VASCAN-Canada-checklist.tsv")

############################################################
# 1. Clean VasCan
############################################################

# Something strange happened with the special character text encoding
# Make a function to fix them individually

fix_chars <- function(x) {
  if (!is.character(x)) return(x)
  replacements <- c(
    "Ã©"="é","Ã¨"="è","Ãª"="ê","Ã«"="ë",
    "Ã "="à","Ã¢"="â","Ã®"="î","Ã¯"="ï",
    "Ã´"="ô","Ã¹"="ù","Ã»"="û","Ã§"="ç",
    "Ã‰"="É","Ãˆ"="È","ÃŠ"="Ê","Ã‹"="Ë",
    "Ã€"="À","Ã‚"="Â","ÃŽ"="Î","Ã�"="Ï",
    "Ã”"="Ô","Ã™"="Ù","Ã‡"="Ç",
    "Ã—"="× ",
    "â€“"="–","â€”"="—",
    "â€˜"="‘","â€™"="’",
    "â€œ"="“","â€�"="”",
    "ÃÂ©"="é","ÃÂª"="ê","ÃÂ¨"="è","Ãâ€”"="×"
  )
  for (i in seq_along(replacements)) {
    x <- gsub(names(replacements)[i], replacements[i], x, fixed = TRUE)
  }
  x
}

# actually fix the df
VasCan <- VasCan %>%
  mutate(across(where(is.character), fix_chars))

############################################################
# 3. Get the species
############################################################

# I am only interested in the species rank so slice those into a different dataframe

VasCanSpecies <- VasCan %>% 
  filter(Rank == "Species")

############################################################
# 3. Save the species data
############################################################

saveRDS(VasCanSpecies, "VasCanSpecies.rds")

