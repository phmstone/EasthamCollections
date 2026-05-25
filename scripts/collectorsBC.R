# ==========================================================
# Download GBIF preserved specimens collected in BC
# during the years that Eastham lived in BC
# ==========================================================

# load libraries
library(rgbif)
library(tidyverse)



# GBIF credentials
user <- "" # Your GBIF account username
pwd  <- "" # Your GBIF account password
email <- "" # Your GBIF account email address


# ----------------------------------------------------------
# 1. Construct download enquiry
# ----------------------------------------------------------

# only want vascular plants because that's what Eastham collected
trach_key <- name_backbone(name = "Tracheophyta", rank = "phylum")$usageKey

download_key <- occ_download(
  pred("basisOfRecord", "PRESERVED_SPECIMEN"), # only want herbarium specimens
  pred("taxonKey", trach_key), # vascular plants
  # include multiple spellings of BC
  pred_or(
    pred("stateProvince", "British Columbia"),
    pred("stateProvince", "BC"),
    pred("stateProvince", "B.C."),
    pred("stateProvince", "British Columbia (Prov.)"),
    pred("stateProvince", "British Columbia / Colombie Britanique"),
    pred("stateProvince", "B C"),
    pred("stateProvince", "Brit. Columbia")),
  pred_gte("year", 1914), # dates that eastham was in BC
  pred_lte("year", 1968),
  format = "SIMPLE_CSV",
  user = user,
  pwd = pwd,
  email = email
)


cat("GBIF download submitted. Download key:", download_key, "\n")


# ----------------------------------------------------------
# 2. Wait for GBIF processing
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
# 3. Download dataset
# ----------------------------------------------------------

download_file <- occ_download_get(download_key)
gbif_data <- occ_download_import(download_file)
cat("Total records downloaded (before filtering):", nrow(gbif_data), "\n")


# ----------------------------------------------------------
# 4. Overview of collectors in BC during Eastham's active years
# ----------------------------------------------------------

# all vascular plant collections from that era in BC
BC_records <- gbif_data

# overview of the collectors
BC_collectors <- BC_records %>% 
  count(recordedBy)

# split up collectors grouped together so counts are individual
BC_collectors_individual <- BC_collectors %>%
  mutate(
    recordedBy = str_replace_all(recordedBy, "\\s*&\\s*|\\s+and\\s+", ";"),
    recordedBy = str_replace_all(recordedBy, ";+", ";")
  ) %>%
  separate_rows(recordedBy, sep = ";") %>%
  mutate(recordedBy = str_trim(recordedBy))





# ----------------------------------------------------------
# 5. Name mapping for the top collectors in BC
# ----------------------------------------------------------


name_map <- bind_rows(
  
  # --------------------------------------------------
  # John W. Eastham
  # --------------------------------------------------
  tribble(
    ~raw, ~clean,
    "John W. Eastham",      "John W. Eastham",
    "John Eastham",         "John W. Eastham",
    "Eastham, J.W.",        "John W. Eastham",
    "Eastham, J. W.",       "John W. Eastham",
    "J. W. Eastham",        "John W. Eastham",
    "J.W. Eastham",         "John W. Eastham",
    "J. Eastham",           "John W. Eastham",
    "Eastham, John William", "John W. Eastham"
  ),
  
  # --------------------------------------------------
  # Vladimir J. Krajina
  # --------------------------------------------------
  tribble(
    ~raw, ~clean,
    "Vladimir J. Krajina", "Vladimir J. Krajina",
    "Vladimir Krajina", "Vladimir J. Krajina",
    "V. J. Krajina", "Vladimir J. Krajina",
    "VJ Krajina", "Vladimir J. Krajina",
    "V.J. Krajina", "Vladimir J. Krajina",
    "Krajina, VJ", "Vladimir J. Krajina",
    "Krajina, V.", "Vladimir J. Krajina"
  ),
  
  # --------------------------------------------------
  # Vernon C. Brink
  # --------------------------------------------------
  tribble(
    ~raw, ~clean,
    "Vernon C. Brink", "Vernon C. Brink",
    "V. C. Brink", "Vernon C. Brink",
    "Brink, V.C.", "Vernon C. Brink"
  ),
  
  # --------------------------------------------------
  # Adam F. Szczawinski
  # --------------------------------------------------
  tribble(
    ~raw, ~clean,
    "Adam F. Szczawinski",    "Adam F. Szczawinski",
    "Adam Szczawinski",       "Adam F. Szczawinski",
    "T. M. C. Taylor, A. F. Szczawinski,", "Adam F. Szczawinski",
    "A. F. Szczawinski",     "Adam F. Szczawinski",
    "Szczawinski, Adam F.",  "Adam F. Szczawinski",
    "Szczawinski, AF",       "Adam F. Szczawinski",
    "A.F. Szczawinski",      "Adam F. Szczawinski",
    "A. Szczawinski",        "Adam F. Szczawinski"
  ),
  
  # --------------------------------------------------
  # Thomas M.C. / Roy L. Taylor (separated correctly)
  # --------------------------------------------------
  tribble(
    ~raw, ~clean,
    
    # Thomas M.C. Taylor
    "Thomas M.C. Taylor", "Thomas M. C. Taylor",
    "Thomas Taylor", "Thomas M. C. Taylor",
    "Taylor, TMC", "Thomas M. C. Taylor",
    "Taylor, Thomas M.C.", "Thomas M. C. Taylor",
    "W. Taylor", "Thomas M. C. Taylor",
    "T. M. C. Taylor, A. F. Szczawinski,", "Thomas M. C. Taylor",
  ),
  
  # --------------------------------------------------
  # Fred Fodor
  # --------------------------------------------------
  tribble(
    ~raw, ~clean,
    "Fred Fodor", "Fred Fodor",
    "F. Fodor", "Fred Fodor",
    "Fodor, F.", "Fred Fodor",
    "F Fodor", "Fred Fodor"
  ),
  
  # --------------------------------------------------
  # William C. McCalla
  # --------------------------------------------------
  tribble(
    ~raw, ~clean,
    "William C. McCalla", "William C. McCalla",
    "McCalla, William C.", "William C. McCalla",
    "W. McCalla", "William C. McCalla",
    "W. C. McCalla", "William C. McCalla",
    "McCalla, W. C.", "William C. McCalla",
    "McCalla, W.C.", "William C. McCalla",
    "W.C. McCalla", "William C. McCalla"
  ),
  
  # --------------------------------------------------
  # J. A. Calder (all variants merged)
  # --------------------------------------------------
  tribble(
    ~raw, ~clean,
    "J. A. Calder", "J. A. Calder",
    "Calder, J. A.", "J. A. Calder",
    "Calder, J.A.", "J. A. Calder",
    "J.A. Calder", "J. A. Calder",
    "James A. Calder", "J. A. Calder",
    "James Calder", "J. A. Calder",
    "J. A. Calder, D. B. O. Savile, J. M. Ferguson", "J. A. Calder",
    "J. A. Calder, D. B. O. Savile, R. L. Taylor", "J. A. Calder",
    "J. A. Calder, J. A. Parmelee, R. L. Taylor",  "J. A. Calder",
    "J. A. Calder, R. L. Taylor",  "J. A. Calder",
    "J. A. Calder, D. B. O. Savile", "J. A. Calder",
    "Calder", "J. A. Calder"
  ),
  
  # --------------------------------------------------
  # John W. Thompson
  # --------------------------------------------------
  tribble(
    ~raw, ~clean,
    "J. W. Thompson",           "John W. Thompson",
    "J. William Thompson, Emily M. Thompson", "John W. Thompson",
    "Thompson, J. William",     "John W. Thompson",
    "J. William Thompson",       "John W. Thompson",
    "Thompson, J.W.",            "John W. Thompson"
  ),
  
  # --------------------------------------------------
  # Emily M. Thompson
  # --------------------------------------------------
  tribble(
    ~raw, ~clean,
    "J. William Thompson, Emily M. Thompson", "Emily M. Thompson",
    "E. M. Thompson",             "Emily M. Thompson",
    "Thompson, Emily M.",         "Emily M. Thompson",
    "Emily M. Thompson",          "Emily M. Thompson",
    "Thompson, E.M.",             "Emily M. Thompson"
  ),
  
  # --------------------------------------------------
  # Freek Vrugtman
  # --------------------------------------------------
  tribble(
    ~raw, ~clean,
    "Freek Vrugtman",                    "Freek Vrugtman",
    "F. Vrugtman",                        "Freek Vrugtman",
    "K. Beamish, F. Vrugtman",            "Freek Vrugtman",
    "Vrugtman, Freek",                     "Freek Vrugtman",
    "Fred Vrugtman",                       "Freek Vrugtman",
    "K. I. Beamish, F. Vrugtman",         "Freek Vrugtman",
    "Vrugtman, F.",                        "Freek Vrugtman",
    "K. Beamish, F. Vrugtman, K. Sperrings","Freek Vrugtman"
  ),
  
  # --------------------------------------------------
  # Marc Bell
  # --------------------------------------------------
  tribble(
    ~raw, ~clean,
    "Marc Bell",                             "Marc Bell",
    "M. Bell",                               "Marc Bell",
    "Bell, M.",                              "Marc Bell",
    "T. M. C. Taylor, A. F. Szczawinski, M. Bell", "Marc Bell",
    "T. M. C. Taylor, A. Szczawinski, M. Bell",   "Marc Bell"
  ),
  
  # --------------------------------------------------
  # Roy L. Taylor
  # --------------------------------------------------
  tribble(
    ~raw, ~clean,
    "Roy L. Taylor",                             "Roy L. Taylor",
    "Taylor, R. L.",                               "Roy L. Taylor",
    "Taylor, R.L.",                              "Roy L. Taylor",
    "R. Taylor",                               "Roy L. Taylor",
    "R. L. Taylor",                             "Roy L. Taylor",
    "R.L. Taylor",                             "Roy L. Taylor",
    "Taylor, Roy L.",                             "Roy L. Taylor",
    "R. L. Taylor, D. H. Ferguson",            "Roy L. Taylor",
    "Roy Taylor",                               "Roy L. Taylor"
  ),
  
  # --------------------------------------------------
  # John Davidson (with rule: keep J.F. separate)
  # --------------------------------------------------
  tribble(
    ~raw, ~clean,
    "John Davidson", "John Davidson",
    "Davidson, J.", "John Davidson",
    "Davidson, J", "John Davidson",
    "J. G.N. Davidson", "John Davidson",
    "J. G. N. Davidson", "John Davidson",
    "John G.N. Davidson", "John Davidson",
    "Jack Davidson", "John Davidson",
    
    # KEEP SEPARATE
    "John F. Davidson", "J. F. Davidson",
    "J. F. Davidson", "J. F. Davidson"
  )
)



# ----------------------------------------------------------
# 5. Count out the top collectors with proper name mapping
# ----------------------------------------------------------

# Make a named vector for recoding
lookup <- setNames(name_map$clean, name_map$raw)

BC_collectors_mapped <- BC_collectors_individual %>%
  mutate(
    recordedBy_clean = recode(recordedBy, !!!lookup, .default = recordedBy)
  )

# Count properly after mapping
top_collectors <- BC_collectors_mapped %>%
  group_by(recordedBy_clean) %>%
  summarise(total_n = sum(n, na.rm = TRUE), .groups = "drop") %>%  # sum the old counts
  arrange(desc(total_n))


# peel off the top ten collectors
top_ten_collectors <- top_collectors %>% 
  slice_head(n = 10) %>% 
  rename("Collector" = "recordedBy_clean") %>% 
  rename("Number of specimens" = "total_n")


# save the top ten collectors
write_csv(top_ten_collectors, "topCollectorsBC.csv")


# ----------------------------------------------------------
# 6. looking into most prolific collectors
# ----------------------------------------------------------

# where are Calder's specimens stored
calder_institutions <- BC_records %>%
  filter(str_detect(recordedBy, "Calder")) %>%  # keep rows with "Calder"
  count(institutionCode, sort = TRUE)           # count occurrences per institutionCode


# where are Eastham's specimens stored
Eastham_institutions <- BC_records %>%
  filter(str_detect(recordedBy, "Eastham")) %>%  # not foolproof because of his son, Mrs Eastham, etc.
  count(institutionCode, sort = TRUE)           # count occurrences per institutionCode





