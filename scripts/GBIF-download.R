# ==========================================================
# Download GBIF preserved specimens collected by
# John William Eastham (all known name variants)
# ==========================================================

# load libraries
library(rgbif)
library(tidyverse)



# GBIF credentials
user <- "" # Your GBIF account username
pwd  <- "" # Your GBIF account password
email <- "" # Your GBIF account email address

# ----------------------------------------------------------
# 1. Define collector name variants
# ----------------------------------------------------------

collector_variants <- c(
  "J.W. Eastham",
  "J. W. Eastham",
  "J. Eastham",
  "JW Eastham",
  "J W Eastham",
  "Eastham, J.W.",
  "Eastham,J.W.",
  "Eastham, JW",
  "Eastham, J. W.",
  "Eastham J. W.",
  "Eastham J.W.",
  "Eastham JW",
  "John Eastham",
  "John W Eastham",
  "John W. Eastham",
  "John William Eastham",
  "Eastham, John William",
  "Eastham, John W.",
  "Eastham"
)

# ----------------------------------------------------------
# 2. Create GBIF download query
# ----------------------------------------------------------

download_key <- occ_download(
  pred("basisOfRecord", "PRESERVED_SPECIMEN"),
  pred_in("recordedBy", collector_variants),
  format = "SIMPLE_CSV",
  user = user,
  pwd = pwd,
  email = email
)

cat("GBIF download submitted. Download key:", download_key, "\n")

# ----------------------------------------------------------
# 3. Wait for GBIF processing
# ----------------------------------------------------------

cat("Waiting for GBIF to finish processing...\n")

repeat {
  meta <- occ_download_meta(download_key)
  cat(Sys.time(), "- Current status:", meta$status, "\n")
  
  if (meta$status == "SUCCEEDED") {
    cat("Download is ready!\n")
    break
  } else if (meta$status %in% c("KILLED", "CANCELLED")) {
    stop("GBIF download failed or was cancelled.")
  } else {
    Sys.sleep(60)  # Wait 1 minute before checking again
  }
}


# When status = SUCCEEDED run next section

# ----------------------------------------------------------
# 4. Download dataset
# ----------------------------------------------------------

download_file <- occ_download_get(download_key)
gbif_data <- occ_download_import(download_file)
cat("Total records downloaded (before filtering):", nrow(gbif_data), "\n")

# ----------------------------------------------------------
# 5. Extra filtering for Eastham
# ----------------------------------------------------------

eastham_records <- gbif_data %>%
  filter(str_detect(recordedBy, regex("eastham", ignore_case = TRUE))) %>%
  filter(kingdom %in% c("Plantae", "Fungi"))

cat("Records after filtering for Plantae and Fungi:", nrow(eastham_records), "\n")

# ----------------------------------------------------------
# 6. Save results
# ----------------------------------------------------------

saveRDS(eastham_records, "jw_eastham_gbif_specimens.rds")
write.csv(eastham_records, "jw_eastham_gbif_specimens.csv", row.names = FALSE)
cat("Saved to 'jw_eastham_gbif_specimens.rds' and 'jw_eastham_gbif_specimens.csv'\n")

# ----------------------------------------------------------
# 7. Quick summary
# ----------------------------------------------------------

cat("Kingdom summary:\n")
print(table(eastham_records$kingdom))

cat("Institution summary:\n")
print(table(eastham_records$institutionCode))


# ----------------------------------------------------------
# 8. Download all historical North American GBIF data
# ----------------------------------------------------------

# Specimens collected before Eastham's last specimen was collected
# Still downloading for plants and fungi

# download_key <- occ_download(
# 
#   pred_in("kingdomKey", c(6,5)),
# 
#   pred_in("country", c("CA","US","GL")),
# 
#   pred("basisOfRecord","PRESERVED_SPECIMEN"),
# 
#   pred("hasCoordinate", TRUE),
# 
#   pred("eventDate","1800-01-01,1970-12-31"),
# 
#   format = "SIMPLE_CSV",
# 
#   user = user,
#   pwd = pwd,
#   email = email
# )
# 
# cat("Download key:", download_key, "\n")
# 
# # wait for GBIF to prepare data
# occ_download_wait(download_key)
# 
# # download the dataset
# download_file <- occ_download_get(download_key)
# 
# # import
# gbif_all <- occ_download_import(download_file)
# 
# # save the data as an RDS
# saveRDS(gbif_all, "gbif_NA_all_records.rds")

gbif_all <- readRDS("gbif_NA_all_records.rds")



