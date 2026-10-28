# =============================================================================
# WHO SEARO — Daily Flood Early Warning Scan (R)
# =============================================================================
# Pulls Google Flood Hub (gauges, significant events, urban flash floods) + GDACS,
# builds a short triage brief, and lists met/hydro links only for WATCH/ALERT countries.
#
# HOW TO RUN (RStudio):
#   1. Open this file in RStudio
#   2. Click "Source" (or Ctrl+Shift+S)
# =============================================================================

# ---- CONFIG (edit these) ----------------------------------------------------

FLOODS_API_KEY <- Sys.getenv("FLOODS_API_KEY", unset = "")

OUT_DIR         <- "outputs"
BOUNDARY_CACHE  <- "boundary_cache"
DO_ADMIN1       <- TRUE   # set FALSE to skip Admin-1 screening
DO_GDACS        <- TRUE
DO_SIG_EVENTS   <- TRUE
DO_FLASH_FLOODS <- TRUE   # urban flash floods (next ~24h)
DO_OPENMETEO    <- TRUE
LOCAL_TZ        <- "Asia/Kolkata"  # Indian Standard Time (IST, UTC+5:30)

# WHO office networks often need a corporate proxy to reach Google APIs.
# Leave blank if you already have HTTPS_PROXY / https_proxy set in Windows,
# or paste your WHO proxy, e.g. "http://proxy.who.int:8080"
HTTPS_PROXY_URL <- Sys.getenv("HTTPS_PROXY", unset = Sys.getenv("https_proxy", unset = ""))
HTTP_TIMEOUT_SEC <- 120
# -----------------------------------------------------------------------------

options(timeout = 300)

# ---- Packages (auto-install on first run) -----------------------------------
need <- c(
  "httr2", "jsonlite", "dplyr", "tidyr", "readr",
  "stringr", "sf", "tibble", "officer", "flextable",
  "tidyverse", "magrittr", "maps", "gt"
)
missing <- need[!vapply(need, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing)) {
  message("Installing packages: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(sf)
  library(tibble)
  library(officer)
  library(flextable)
  library(tidyverse)
  library(magrittr)
  library(maps)
  library(gt)
})

# ---- Constants --------------------------------------------------------------
FLOOD_API    <- "https://floodforecasting.googleapis.com/v1"
GDACS_SEARCH <- "https://www.gdacs.org/gdacsapi/api/events/geteventlist/SEARCH"
NE_ADMIN1_URL <- paste0(
  "https://naciscdn.org/naturalearth/10m/cultural/",
  "ne_10m_admin_1_states_provinces.zip"
)

FLOOD_HUB_COUNTRIES <- tibble(
  country     = c("Bangladesh", "India", "Myanmar", "Nepal", "Sri Lanka", "Thailand"),
  region_code = c("BD", "IN", "MM", "NP", "LK", "TH")
)

SEARO_ISO2 <- c("BD", "BT", "KP", "IN", "MV", "MM", "NP", "LK", "TH", "TL")
SEARO_ISO2_NAMES <- c(
  BD = "Bangladesh", BT = "Bhutan", KP = "DPR Korea", IN = "India",
  MV = "Maldives", MM = "Myanmar", NP = "Nepal", LK = "Sri Lanka",
  TH = "Thailand", TL = "Timor-Leste"
)
SEARO_NAME_HINTS <- c(
  "bangladesh", "bhutan", "india", "maldives", "myanmar", "burma",
  "nepal", "sri lanka", "thailand", "timor", "dprk", "korea, dem"
)

SEVERITY_MAP <- c(
  UNKNOWN = "Unknown",
  NO_FLOODING = "Normal",
  ABOVE_NORMAL = "Warning",
  SEVERE = "Danger",
  EXTREME = "Extreme"
)
ESCALATION_LEVELS <- c("Danger", "Extreme")

MET_VERIFY_LINKS <- c(
  "Bangladesh"  = "https://www.bmd.gov.bd/ / https://www.ffwc.gov.bd/",
  "Bhutan"      = "https://www.nchm.gov.bt/",
  "India"       = "https://mausam.imd.gov.in/ / https://ffs.india-water.gov.in/",
  "Maldives"    = "https://www.meteorology.gov.mv/",
  "Myanmar"     = "https://www.moezala.gov.mm/",
  "Nepal"       = "https://www.dhm.gov.np/",
  "Sri Lanka"   = "https://www.meteo.gov.lk/ / https://www.irrigation.gov.lk/",
  "Thailand"    = "https://www.tmd.go.th/ / https://www.thaiwater.net/",
  "Timor-Leste" = "National met/hydro bulletin (check MoH / NDMD channels)",
  "DPR Korea"   = "Limited open sources — rely on GDACS + Flood Hub only"
)


cities_db <- list(
  # ==========================================
  # India (State level mapping)
  # ==========================================
  # Uttar Pradesh
  "Noida" = list(lat = 28.5355, lon = 77.3910, name = "Noida", state = "Uttar Pradesh", country = "India"),
  "Ghaziabad" = list(lat = 28.6692, lon = 77.4538, name = "Ghaziabad", state = "Uttar Pradesh", country = "India"),
  "Meerut" = list(lat = 28.9845, lon = 77.7064, name = "Meerut", state = "Uttar Pradesh", country = "India"),
  "Prayagraj" = list(lat = 25.4358, lon = 81.8463, name = "Prayagraj", state = "Uttar Pradesh", country = "India"),
  "Bareilly" = list(lat = 28.3670, lon = 79.4304, name = "Bareilly", state = "Uttar Pradesh", country = "India"),
  "Aligarh" = list(lat = 27.8974, lon = 78.0880, name = "Aligarh", state = "Uttar Pradesh", country = "India"),
  "Moradabad" = list(lat = 28.8386, lon = 78.7733, name = "Moradabad", state = "Uttar Pradesh", country = "India"),
  "Gorakhpur" = list(lat = 26.7606, lon = 83.3732, name = "Gorakhpur", state = "Uttar Pradesh", country = "India"),
  "Mathura" = list(lat = 27.4924, lon = 77.6737, name = "Mathura", state = "Uttar Pradesh", country = "India"),
  
  # Maharashtra
  "Thane" = list(lat = 19.2183, lon = 72.9781, name = "Thane", state = "Maharashtra", country = "India"),
  "Navi Mumbai" = list(lat = 19.0330, lon = 73.0297, name = "Navi Mumbai", state = "Maharashtra", country = "India"),
  "Solapur" = list(lat = 17.6599, lon = 75.9064, name = "Solapur", state = "Maharashtra", country = "India"),
  "Amravati" = list(lat = 20.9374, lon = 77.7796, name = "Amravati", state = "Maharashtra", country = "India"),
  "Nanded" = list(lat = 19.1383, lon = 77.3210, name = "Nanded", state = "Maharashtra", country = "India"),
  
  # Karnataka
  "Belgaum" = list(lat = 15.8497, lon = 74.4977, name = "Belgaum", state = "Karnataka", country = "India"),
  "Davangere" = list(lat = 14.4644, lon = 75.9218, name = "Davangere", state = "Karnataka", country = "India"),
  "Bellary" = list(lat = 15.1394, lon = 76.9214, name = "Bellary", state = "Karnataka", country = "India"),
  "Shimoga" = list(lat = 13.9299, lon = 75.5681, name = "Shimoga", state = "Karnataka", country = "India"),
  "Gulbarga" = list(lat = 17.3297, lon = 76.8343, name = "Gulbarga", state = "Karnataka", country = "India"),
  
  # West Bengal
  "Howrah" = list(lat = 22.5958, lon = 88.2636, name = "Howrah", state = "West Bengal", country = "India"),
  "Durgapur" = list(lat = 23.5204, lon = 87.3119, name = "Durgapur", state = "West Bengal", country = "India"),
  "Bardhaman" = list(lat = 23.2324, lon = 87.8615, name = "Bardhaman", state = "West Bengal", country = "India"),
  "Darjeeling" = list(lat = 27.0410, lon = 88.2663, name = "Darjeeling", state = "West Bengal", country = "India"),
  "Kharagpur" = list(lat = 22.3460, lon = 87.2320, name = "Kharagpur", state = "West Bengal", country = "India"),
  
  # Tamil Nadu
  "Salem" = list(lat = 11.6643, lon = 78.1460, name = "Salem", state = "Tamil Nadu", country = "India"),
  "Tirunelveli" = list(lat = 8.7139, lon = 77.7567, name = "Tirunelveli", state = "Tamil Nadu", country = "India"),
  "Vellore" = list(lat = 12.9165, lon = 79.1325, name = "Vellore", state = "Tamil Nadu", country = "India"),
  "Erode" = list(lat = 11.3410, lon = 77.7172, name = "Erode", state = "Tamil Nadu", country = "India"),
  "Thanjavur" = list(lat = 10.7870, lon = 79.1378, name = "Thanjavur", state = "Tamil Nadu", country = "India"),
  
  # Telangana
  "Karimnagar" = list(lat = 18.4386, lon = 79.1288, name = "Karimnagar", state = "Telangana", country = "India"),
  "Khammam" = list(lat = 17.2473, lon = 80.1514, name = "Khammam", state = "Telangana", country = "India"),
  "Ramagundam" = list(lat = 18.7557, lon = 79.4738, name = "Ramagundam", state = "Telangana", country = "India"),
  "Mahbubnagar" = list(lat = 16.7488, lon = 78.0035, name = "Mahbubnagar", state = "Telangana", country = "India"),
  
  # Gujarat
  "Gandhinagar" = list(lat = 23.2156, lon = 72.6369, name = "Gandhinagar", state = "Gujarat", country = "India"),
  "Bhavnagar" = list(lat = 21.7645, lon = 72.1519, name = "Bhavnagar", state = "Gujarat", country = "India"),
  "Jamnagar" = list(lat = 22.4707, lon = 70.0577, name = "Jamnagar", state = "Gujarat", country = "India"),
  "Junagadh" = list(lat = 21.5222, lon = 70.4579, name = "Junagadh", state = "Gujarat", country = "India"),
  "Anand" = list(lat = 22.5645, lon = 72.9289, name = "Anand", state = "Gujarat", country = "India"),
  
  # Rajasthan
  "Kota" = list(lat = 25.2138, lon = 75.8648, name = "Kota", state = "Rajasthan", country = "India"),
  "Bikaner" = list(lat = 28.0229, lon = 73.3119, name = "Bikaner", state = "Rajasthan", country = "India"),
  "Ajmer" = list(lat = 26.4499, lon = 74.6399, name = "Ajmer", state = "Rajasthan", country = "India"),
  "Bhilwara" = list(lat = 25.3407, lon = 74.6313, name = "Bhilwara", state = "Rajasthan", country = "India"),
  "Alwar" = list(lat = 27.5530, lon = 76.6346, name = "Alwar", state = "Rajasthan", country = "India"),
  
  # Kerala
  "Kozhikode" = list(lat = 11.2588, lon = 75.7804, name = "Kozhikode", state = "Kerala", country = "India"),
  "Thrissur" = list(lat = 10.5276, lon = 76.2144, name = "Thrissur", state = "Kerala", country = "India"),
  "Kollam" = list(lat = 8.8932, lon = 76.6141, name = "Kollam", state = "Kerala", country = "India"),
  "Kannur" = list(lat = 11.8745, lon = 75.3704, name = "Kannur", state = "Kerala", country = "India"),
  "Alappuzha" = list(lat = 9.4981, lon = 76.3388, name = "Alappuzha", state = "Kerala", country = "India"),
  "Palakkad" = list(lat = 10.7867, lon = 76.6548, name = "Palakkad", state = "Kerala", country = "India"),
  
  # Madhya Pradesh
  "Sagar" = list(lat = 23.8388, lon = 78.7378, name = "Sagar", state = "Madhya Pradesh", country = "India"),
  "Dewas" = list(lat = 22.9676, lon = 76.0534, name = "Dewas", state = "Madhya Pradesh", country = "India"),
  "Satna" = list(lat = 24.6005, lon = 80.8322, name = "Satna", state = "Madhya Pradesh", country = "India"),
  "Ratlam" = list(lat = 23.3315, lon = 75.0367, name = "Ratlam", state = "Madhya Pradesh", country = "India"),
  "Rewa" = list(lat = 24.5362, lon = 81.3037, name = "Rewa", state = "Madhya Pradesh", country = "India"),
  
  # Andhra Pradesh
  "Guntur" = list(lat = 16.3067, lon = 80.4365, name = "Guntur", state = "Andhra Pradesh", country = "India"),
  "Nellore" = list(lat = 14.4426, lon = 79.9865, name = "Nellore", state = "Andhra Pradesh", country = "India"),
  "Rajahmundry" = list(lat = 17.0005, lon = 81.8040, name = "Rajahmundry", state = "Andhra Pradesh", country = "India"),
  "Kakinada" = list(lat = 16.9891, lon = 82.2475, name = "Kakinada", state = "Andhra Pradesh", country = "India"),
  "Kadapa" = list(lat = 14.4673, lon = 78.8242, name = "Kadapa", state = "Andhra Pradesh", country = "India"),
  "Anantapur" = list(lat = 14.6819, lon = 77.6006, name = "Anantapur", state = "Andhra Pradesh", country = "India"),
  
  # Bihar
  "Bhagalpur" = list(lat = 25.2425, lon = 86.9842, name = "Bhagalpur", state = "Bihar", country = "India"),
  "Darbhanga" = list(lat = 26.1542, lon = 85.8918, name = "Darbhanga", state = "Bihar", country = "India"),
  "Purnia" = list(lat = 25.7771, lon = 87.4753, name = "Purnia", state = "Bihar", country = "India"),
  "Bihar Sharif" = list(lat = 25.1982, lon = 85.5149, name = "Bihar Sharif", state = "Bihar", country = "India"),
  "Arrah" = list(lat = 25.5560, lon = 84.6603, name = "Arrah", state = "Bihar", country = "India"),
  
  # Jammu and Kashmir
  "Anantnag" = list(lat = 33.7311, lon = 75.1522, name = "Anantnag", state = "Jammu and Kashmir", country = "India"),
  "Baramulla" = list(lat = 34.2045, lon = 74.3436, name = "Baramulla", state = "Jammu and Kashmir", country = "India"),
  "Udhampur" = list(lat = 32.9254, lon = 75.1416, name = "Udhampur", state = "Jammu and Kashmir", country = "India"),
  "Kathua" = list(lat = 32.3688, lon = 75.5162, name = "Kathua", state = "Jammu and Kashmir", country = "India"),
  
  # Assam
  "Dibrugarh" = list(lat = 27.4728, lon = 94.9120, name = "Dibrugarh", state = "Assam", country = "India"),
  "Silchar" = list(lat = 24.8333, lon = 92.7789, name = "Silchar", state = "Assam", country = "India"),
  "Jorhat" = list(lat = 26.7509, lon = 94.2037, name = "Jorhat", state = "Assam", country = "India"),
  "Nagaon" = list(lat = 26.3464, lon = 92.6840, name = "Nagaon", state = "Assam", country = "India"),
  "Tezpur" = list(lat = 26.6528, lon = 92.7926, name = "Tezpur", state = "Assam", country = "India"),
  
  # Odisha
  "Puri" = list(lat = 19.8135, lon = 85.8312, name = "Puri", state = "Odisha", country = "India"),
  "Berhampur" = list(lat = 19.3150, lon = 84.7941, name = "Berhampur", state = "Odisha", country = "India"),
  "Sambalpur" = list(lat = 21.4669, lon = 83.9812, name = "Sambalpur", state = "Odisha", country = "India"),
  "Balasore" = list(lat = 21.4934, lon = 86.9135, name = "Balasore", state = "Odisha", country = "India"),
  
  # Jharkhand
  "Bokaro Steel City" = list(lat = 23.6693, lon = 86.1511, name = "Bokaro Steel City", state = "Jharkhand", country = "India"),
  "Hazaribagh" = list(lat = 23.9961, lon = 85.3647, name = "Hazaribagh", state = "Jharkhand", country = "India"),
  "Deoghar" = list(lat = 24.4826, lon = 86.7001, name = "Deoghar", state = "Jharkhand", country = "India"),
  "Giridih" = list(lat = 24.1860, lon = 86.3090, name = "Giridih", state = "Jharkhand", country = "India"),
  
  # Uttarakhand
  "Roorkee" = list(lat = 29.8543, lon = 77.8880, name = "Roorkee", state = "Uttarakhand", country = "India"),
  "Haldwani" = list(lat = 29.2183, lon = 79.5130, name = "Haldwani", state = "Uttarakhand", country = "India"),
  "Rishikesh" = list(lat = 30.0869, lon = 78.2676, name = "Rishikesh", state = "Uttarakhand", country = "India"),
  "Rudrapur" = list(lat = 28.9800, lon = 79.4000, name = "Rudrapur", state = "Uttarakhand", country = "India"),
  
  # Himachal Pradesh
  "Solan" = list(lat = 30.9045, lon = 77.0967, name = "Solan", state = "Himachal Pradesh", country = "India"),
  "Mandi" = list(lat = 31.7087, lon = 76.9320, name = "Mandi", state = "Himachal Pradesh", country = "India"),
  "Kullu" = list(lat = 31.9579, lon = 77.1095, name = "Kullu", state = "Himachal Pradesh", country = "India"),
  "Kasauli" = list(lat = 30.9013, lon = 76.9649, name = "Kasauli", state = "Himachal Pradesh", country = "India"),
  
  # Chhattisgarh
  "Durg" = list(lat = 21.1904, lon = 81.2849, name = "Durg", state = "Chhattisgarh", country = "India"),
  "Bhilai" = list(lat = 21.2144, lon = 81.3805, name = "Bhilai", state = "Chhattisgarh", country = "India"),
  "Korba" = list(lat = 22.3595, lon = 82.7501, name = "Korba", state = "Chhattisgarh", country = "India"),
  "Rajnandgaon" = list(lat = 21.0974, lon = 81.0387, name = "Rajnandgaon", state = "Chhattisgarh", country = "India"),
  "Jagdalpur" = list(lat = 19.0748, lon = 82.0090, name = "Jagdalpur", state = "Chhattisgarh", country = "India"),
  
  # Punjab
  "Jalandhar" = list(lat = 31.3260, lon = 75.5762, name = "Jalandhar", state = "Punjab", country = "India"),
  "Patiala" = list(lat = 30.3398, lon = 76.3869, name = "Patiala", state = "Punjab", country = "India"),
  "Bathinda" = list(lat = 30.2110, lon = 74.9455, name = "Bathinda", state = "Punjab", country = "India"),
  "Mohali" = list(lat = 30.7046, lon = 76.7179, name = "Mohali", state = "Punjab", country = "India"),
  "Pathankot" = list(lat = 32.2643, lon = 75.6525, name = "Pathankot", state = "Punjab", country = "India"),
  
  # Haryana
  "Faridabad" = list(lat = 28.4089, lon = 77.3178, name = "Faridabad", state = "Haryana", country = "India"),
  "Panipat" = list(lat = 29.3909, lon = 76.9635, name = "Panipat", state = "Haryana", country = "India"),
  "Ambala" = list(lat = 30.3782, lon = 76.7767, name = "Ambala", state = "Haryana", country = "India"),
  "Rohtak" = list(lat = 28.8955, lon = 76.6066, name = "Rohtak", state = "Haryana", country = "India"),
  "Karnal" = list(lat = 29.6857, lon = 76.9905, name = "Karnal", state = "Haryana", country = "India"),
  "Sonipat" = list(lat = 28.9931, lon = 77.0151, name = "Sonipat", state = "Haryana", country = "India"),
  
  # Union Territories & Goa
  "Kargil" = list(lat = 34.5539, lon = 76.1349, name = "Kargil", state = "Ladakh", country = "India"),
  "Diskit" = list(lat = 34.5714, lon = 77.5583, name = "Diskit", state = "Ladakh", country = "India"),
  "Margao" = list(lat = 15.2832, lon = 73.9862, name = "Margao", state = "Goa", country = "India"),
  "Mapusa" = list(lat = 15.5937, lon = 73.8142, name = "Mapusa", state = "Goa", country = "India"),
  "Karaikal" = list(lat = 10.9254, lon = 79.8380, name = "Karaikal", state = "Puducherry", country = "India"),
  "Mahe" = list(lat = 11.7002, lon = 75.5340, name = "Mahe", state = "Puducherry", country = "India"),
  "Diglipur" = list(lat = 13.2667, lon = 92.9833, name = "Diglipur", state = "Andaman and Nicobar Islands", country = "India"),
  "Agatti" = list(lat = 10.8533, lon = 72.1932, name = "Agatti", state = "Lakshadweep", country = "India"),
  "Minicoy" = list(lat = 8.2833, lon = 73.0489, name = "Minicoy", state = "Lakshadweep", country = "India"),
  
  # North-Eastern States
  "Churachandpur" = list(lat = 24.3333, lon = 93.6667, name = "Churachandpur", state = "Manipur", country = "India"),
  "Lunglei" = list(lat = 22.8878, lon = 92.7380, name = "Lunglei", state = "Mizoram", country = "India"),
  "Dimapur" = list(lat = 25.9068, lon = 93.7271, name = "Dimapur", state = "Nagaland", country = "India"),
  "Dharmanagar" = list(lat = 24.3833, lon = 92.1667, name = "Dharmanagar", state = "Tripura", country = "India"),
  "Tura" = list(lat = 25.5144, lon = 90.2023, name = "Tura", state = "Meghalaya", country = "India"),
  "Naharlagun" = list(lat = 27.1064, lon = 93.6934, name = "Naharlagun", state = "Arunachal Pradesh", country = "India"),
  "Namchi" = list(lat = 27.1667, lon = 88.3500, name = "Namchi", state = "Sikkim", country = "India"),
  
  # ==========================================
  # Thailand
  # ==========================================
  "Nonthaburi" = list(lat = 13.8591, lon = 100.5217, name = "Nonthaburi", state = "Nonthaburi", country = "Thailand"),
  "Samut Prakan" = list(lat = 13.5991, lon = 100.5968, name = "Samut Prakan", state = "Samut Prakan", country = "Thailand"),
  "Rangsit" = list(lat = 13.9877, lon = 100.6153, name = "Rangsit", state = "Pathum Thani", country = "Thailand"),
  "Fang" = list(lat = 19.9169, lon = 99.2142, name = "Fang", state = "Chiang Mai", country = "Thailand"),
  "Mae Sai" = list(lat = 20.4335, lon = 99.8762, name = "Mae Sai", state = "Chiang Rai", country = "Thailand"),
  "Patong" = list(lat = 7.8961, lon = 98.2982, name = "Patong", state = "Phuket", country = "Thailand"),
  "Songkhla" = list(lat = 7.1756, lon = 100.6143, name = "Songkhla", state = "Songkhla", country = "Thailand"),
  "Ko Samui" = list(lat = 9.5120, lon = 100.0136, name = "Ko Samui", state = "Surat Thani", country = "Thailand"),
  "Ban Chang" = list(lat = 12.7269, lon = 101.0583, name = "Ban Chang", state = "Rayong", country = "Thailand"),
  "Sangkhla Buri" = list(lat = 15.1558, lon = 98.4525, name = "Sangkhla Buri", state = "Kanchanaburi", country = "Thailand"),
  "Cha-am" = list(lat = 12.7998, lon = 99.9678, name = "Cha-am", state = "Phetchaburi", country = "Thailand"),
  "Prachuap Khiri Khan" = list(lat = 11.8124, lon = 99.7972, name = "Prachuap Khiri Khan", state = "Prachuap Khiri Khan", country = "Thailand"),
  "Pak Chong" = list(lat = 14.7083, lon = 101.4161, name = "Pak Chong", state = "Nakhon Ratchasima", country = "Thailand"),
  "Chum Phae" = list(lat = 16.5442, lon = 102.0994, name = "Chum Phae", state = "Khon Kaen", country = "Thailand"),
  "Warin Chamrap" = list(lat = 15.1925, lon = 104.8586, name = "Warin Chamrap", state = "Ubon Ratchathani", country = "Thailand"),
  "Pai" = list(lat = 19.3589, lon = 98.4369, name = "Pai", state = "Mae Hong Son", country = "Thailand"),
  "Chiang Khan" = list(lat = 17.8953, lon = 101.6542, name = "Chiang Khan", state = "Loei", country = "Thailand"),
  "Ao Nang" = list(lat = 8.0333, lon = 98.8250, name = "Ao Nang", state = "Krabi", country = "Thailand"),
  "Khao Lak" = list(lat = 8.6539, lon = 98.2514, name = "Khao Lak", state = "Phang Nga", country = "Thailand"),
  "Thung Song" = list(lat = 8.1644, lon = 99.6803, name = "Thung Song", state = "Nakhon Si Thammarat", country = "Thailand"),
  "Kantang" = list(lat = 7.4061, lon = 99.5156, name = "Kantang", state = "Trang", country = "Thailand"),
  "Betong" = list(lat = 5.7744, lon = 101.0717, name = "Betong", state = "Yala", country = "Thailand"),
  "Su-ngai Kolok" = list(lat = 6.0278, lon = 101.9667, name = "Su-ngai Kolok", state = "Narathiwat", country = "Thailand"),
  
  # ==========================================
  # Bhutan (Reduced to Dzongkhag Hubs)
  # ==========================================
  "Paro" = list(lat = 27.4287, lon = 89.4164, name = "Paro", state = "Paro", country = "Bhutan"),
  "Punakha" = list(lat = 27.5819, lon = 89.8624, name = "Punakha", state = "Punakha", country = "Bhutan"),
  "Chukha" = list(lat = 27.0656, lon = 89.5786, name = "Chukha", state = "Chukha", country = "Bhutan"),
  "Jakar" = list(lat = 27.5492, lon = 90.7533, name = "Jakar", state = "Bumthang", country = "Bhutan"),
  "Trongsa" = list(lat = 27.5003, lon = 90.5074, name = "Trongsa", state = "Trongsa", country = "Bhutan"),
  "Mongar" = list(lat = 27.2747, lon = 91.2396, name = "Mongar", state = "Mongar", country = "Bhutan"),
  "Trashigang" = list(lat = 27.3331, lon = 91.5542, name = "Trashigang", state = "Trashigang", country = "Bhutan"),
  "Samdrup Jongkhar" = list(lat = 26.8007, lon = 91.5052, name = "Samdrup Jongkhar", state = "Samdrup Jongkhar", country = "Bhutan"),
  "Gasa" = list(lat = 27.9037, lon = 89.7269, name = "Gasa", state = "Gasa", country = "Bhutan"),
  
  # ==========================================
  # Bangladesh
  # ==========================================
  "Gazipur" = list(lat = 23.9999, lon = 90.4203, name = "Gazipur", state = "Dhaka Division", country = "Bangladesh"),
  "Narayanganj" = list(lat = 23.6238, lon = 90.5000, name = "Narayanganj", state = "Dhaka Division", country = "Bangladesh"),
  "Cumilla" = list(lat = 23.4682, lon = 91.1788, name = "Cumilla", state = "Chattogram Division", country = "Bangladesh"),
  "Teknaf" = list(lat = 20.8654, lon = 92.2982, name = "Teknaf", state = "Chattogram Division", country = "Bangladesh"),
  "Sreemangal" = list(lat = 24.3065, lon = 91.7296, name = "Sreemangal", state = "Sylhet Division", country = "Bangladesh"),
  "Bogura" = list(lat = 24.8465, lon = 89.3777, name = "Bogura", state = "Rajshahi Division", country = "Bangladesh"),
  "Pabna" = list(lat = 24.0064, lon = 89.2372, name = "Pabna", state = "Rajshahi Division", country = "Bangladesh"),
  "Jashore" = list(lat = 23.1664, lon = 89.2081, name = "Jashore", state = "Khulna Division", country = "Bangladesh"),
  "Mongla" = list(lat = 22.4833, lon = 89.6000, name = "Mongla", state = "Khulna Division", country = "Bangladesh"),
  "Patuakhali" = list(lat = 22.3596, lon = 90.3299, name = "Patuakhali", state = "Barisal Division", country = "Bangladesh"),
  "Dinajpur" = list(lat = 25.6217, lon = 88.6355, name = "Dinajpur", state = "Rangpur Division", country = "Bangladesh"),
  "Jamalpur" = list(lat = 24.9375, lon = 89.9378, name = "Jamalpur", state = "Mymensingh Division", country = "Bangladesh"),
  
  # ==========================================
  # Maldives 
  # ==========================================
  "Hulhumale" = list(lat = 4.2135, lon = 73.5414, name = "Hulhumalé", state = "Kaafu Atoll", country = "Maldives"),
  "Hithadhoo" = list(lat = -0.6050, lon = 73.0889, name = "Hithadhoo", state = "Addu Atoll", country = "Maldives"),
  "Hanimaadhoo" = list(lat = 6.7600, lon = 73.1750, name = "Hanimaadhoo", state = "Haa Dhaalu Atoll", country = "Maldives"),
  "Eydhafushi" = list(lat = 5.1042, lon = 73.0700, name = "Eydhafushi", state = "Baa Atoll", country = "Maldives"),
  "Naifaru" = list(lat = 5.4444, lon = 73.3656, name = "Naifaru", state = "Lhaviyani Atoll", country = "Maldives"),
  "Mahibadhoo" = list(lat = 3.7572, lon = 72.9686, name = "Mahibadhoo", state = "Alif Dhaal Atoll", country = "Maldives"),
  "Fonadhoo" = list(lat = 1.8328, lon = 73.5028, name = "Fonadhoo", state = "Laamu Atoll", country = "Maldives"),
  "Thinadhoo (GDh)" = list(lat = 0.5317, lon = 72.9983, name = "Thinadhoo", state = "Gaafu Dhaalu Atoll", country = "Maldives"),
  "Fuvahmulah" = list(lat = -0.2986, lon = 73.4242, name = "Fuvahmulah", state = "Gnaviyani Atoll", country = "Maldives"),
  
  # ==========================================
  # Myanmar
  # ==========================================
  "Pathein" = list(lat = 16.7754, lon = 94.7381, name = "Pathein", state = "Ayeyarwady Region", country = "Myanmar"),
  "Bago" = list(lat = 17.3367, lon = 96.4797, name = "Bago", state = "Bago Region", country = "Myanmar"),
  "Hakha" = list(lat = 22.6428, lon = 93.6096, name = "Hakha", state = "Chin State", country = "Myanmar"),
  "Myitkyina" = list(lat = 25.3833, lon = 97.4000, name = "Myitkyina", state = "Kachin State", country = "Myanmar"),
  "Loikaw" = list(lat = 19.6742, lon = 97.2094, name = "Loikaw", state = "Kayah State", country = "Myanmar"),
  "Hpa-an" = list(lat = 16.8900, lon = 97.6300, name = "Hpa-an", state = "Kayin State", country = "Myanmar"),
  "Magway" = list(lat = 20.1491, lon = 94.9214, name = "Magway", state = "Magway Region", country = "Myanmar"),
  "Mandalay" = list(lat = 21.9588, lon = 96.0891, name = "Mandalay", state = "Mandalay Region", country = "Myanmar"),
  "Mawlamyine" = list(lat = 16.4905, lon = 97.6283, name = "Mawlamyine", state = "Mon State", country = "Myanmar"),
  "Naypyidaw" = list(lat = 19.7633, lon = 96.0785, name = "Naypyidaw", state = "Naypyidaw Union Territory", country = "Myanmar"),
  "Sittwe" = list(lat = 20.1489, lon = 92.8758, name = "Sittwe", state = "Rakhine State", country = "Myanmar"),
  "Monywa" = list(lat = 22.1100, lon = 95.1400, name = "Monywa", state = "Sagaing Region", country = "Myanmar"),
  "Taunggyi" = list(lat = 20.7892, lon = 97.0378, name = "Taunggyi", state = "Shan State", country = "Myanmar"),
  "Dawei" = list(lat = 14.0800, lon = 98.1900, name = "Dawei", state = "Tanintharyi Region", country = "Myanmar"),
  "Yangon" = list(lat = 16.8409, lon = 96.1735, name = "Yangon", state = "Yangon Region", country = "Myanmar"),
  
  # ==========================================
  # Timor-Leste 
  # ==========================================
  "Metinaro" = list(lat = -8.5297, lon = 125.7486, name = "Metinaro", state = "Dili Municipality", country = "Timor-Leste"),
  "Venilale" = list(lat = -8.6433, lon = 126.3789, name = "Venilale", state = "Baucau Municipality", country = "Timor-Leste"),
  "Maubisse" = list(lat = -8.8383, lon = 125.5969, name = "Maubisse", state = "Ainaro Municipality", country = "Timor-Leste"),
  "Lospalos" = list(lat = -8.5186, lon = 126.9958, name = "Lospalos", state = "Lautém Municipality", country = "Timor-Leste"),
  "Viqueque" = list(lat = -8.8575, lon = 126.3647, name = "Viqueque", state = "Viqueque Municipality", country = "Timor-Leste"),
  "Manatuto" = list(lat = -8.5114, lon = 126.0131, name = "Manatuto", state = "Manatuto Municipality", country = "Timor-Leste"),
  "Same" = list(lat = -9.0044, lon = 125.6464, name = "Same", state = "Manufahi Municipality", country = "Timor-Leste"),
  "Gleno" = list(lat = -8.7242, lon = 125.4358, name = "Gleno", state = "Ermera Municipality", country = "Timor-Leste"),
  "Maliana" = list(lat = -8.9917, lon = 125.2197, name = "Maliana", state = "Bobonaro Municipality", country = "Timor-Leste"),
  "Pante Macassar" = list(lat = -9.2025, lon = 124.3842, name = "Pante Macassar", state = "Special Administrative Region of Oecusse-Ambeno", country = "Timor-Leste"),
  
  # ==========================================
  # Nepal
  # ==========================================
  "Lalitpur" = list(lat = 27.6644, lon = 85.3188, name = "Lalitpur", state = "Bagmati Province", country = "Nepal"),
  "Bharatpur" = list(lat = 27.6833, lon = 84.4333, name = "Bharatpur", state = "Bagmati Province", country = "Nepal"),
  "Besishahar" = list(lat = 28.2333, lon = 84.3833, name = "Besishahar", state = "Gandaki Province", country = "Nepal"),
  "Itahari" = list(lat = 26.6667, lon = 87.2833, name = "Itahari", state = "Koshi Province", country = "Nepal"),
  "Siddharthanagar" = list(lat = 27.5058, lon = 83.4503, name = "Siddharthanagar", state = "Lumbini Province", country = "Nepal"),
  "Birendranagar" = list(lat = 28.6019, lon = 81.6339, name = "Birendranagar", state = "Karnali Province", country = "Nepal"),
  "Bhimdatta" = list(lat = 28.9667, lon = 80.1833, name = "Bhimdatta", state = "Sudurpashchim Province", country = "Nepal"),
  "Birgunj" = list(lat = 27.0104, lon = 84.8774, name = "Birgunj", state = "Madhesh Province", country = "Nepal"),
  
  # ==========================================
  # Sri Lanka
  # ==========================================
  "Sri Jayawardenepura Kotte" = list(lat = 6.9016, lon = 79.9077, name = "Sri Jayawardenepura Kotte", state = "Western Province", country = "Sri Lanka"),
  "Negombo" = list(lat = 7.2008, lon = 79.8736, name = "Negombo", state = "Western Province", country = "Sri Lanka"),
  "Vavuniya" = list(lat = 8.7542, lon = 80.4982, name = "Vavuniya", state = "Northern Province", country = "Sri Lanka"),
  "Kalmunai" = list(lat = 7.4111, lon = 81.8217, name = "Kalmunai", state = "Eastern Province", country = "Sri Lanka"),
  "Dambulla" = list(lat = 7.8731, lon = 80.6517, name = "Dambulla", state = "Central Province", country = "Sri Lanka"),
  "Polonnaruwa" = list(lat = 7.9403, lon = 81.0188, name = "Polonnaruwa", state = "North Central Province", country = "Sri Lanka"),
  "Matara" = list(lat = 5.9549, lon = 80.5550, name = "Matara", state = "Southern Province", country = "Sri Lanka"),
  "Puttalam" = list(lat = 8.0362, lon = 79.8283, name = "Puttalam", state = "North Western Province", country = "Sri Lanka"),
  "Kegalle" = list(lat = 7.2513, lon = 80.3464, name = "Kegalle", state = "Sabaragamuwa Province", country = "Sri Lanka"),
  "Bandarawela" = list(lat = 6.8333, lon = 80.9833, name = "Bandarawela", state = "Uva Province", country = "Sri Lanka"),
  
  # ==========================================
  # DPR Korea 
  # ==========================================
  "Kaesong" = list(lat = 37.9708, lon = 126.5544, name = "Kaesong", state = "Kaesong Special City", country = "DPR Korea"),
  "Nampo" = list(lat = 38.7375, lon = 125.4078, name = "Nampo", state = "Nampo Special City", country = "DPR Korea"),
  "Rason" = list(lat = 42.2475, lon = 130.3014, name = "Rason", state = "Rason Special City", country = "DPR Korea"),
  "Hungnam" = list(lat = 39.8317, lon = 127.6186, name = "Hungnam", state = "South Hamgyong Province", country = "DPR Korea"),
  "Kimchaek" = list(lat = 40.6681, lon = 129.1956, name = "Kimchaek", state = "North Hamgyong Province", country = "DPR Korea"),
  "Kusong" = list(lat = 39.9806, lon = 125.2444, name = "Kusong", state = "North Pyongan Province", country = "DPR Korea"),
  "Pyongsong" = list(lat = 39.2472, lon = 125.8703, name = "Pyongsong", state = "South Pyongan Province", country = "DPR Korea"),
  "Sariwon" = list(lat = 38.5078, lon = 125.7544, name = "Sariwon", state = "North Hwanghae Province", country = "DPR Korea"),
  "Haeju" = list(lat = 38.0406, lon = 125.7147, name = "Haeju", state = "South Hwanghae Province", country = "DPR Korea"),
  "Kanggye" = list(lat = 40.9694, lon = 126.5853, name = "Kanggye", state = "Chagang Province", country = "DPR Korea"),
  "Hyesan" = list(lat = 41.4017, lon = 128.1778, name = "Hyesan", state = "Ryanggang Province", country = "DPR Korea")
)


# ---- Helpers ----------------------------------------------------------------
get_api_key <- function() {
  key <- trimws(FLOODS_API_KEY)
  if (!nzchar(key)) {
    stop(
      "Missing Flood Hub API key.\n",
      "Set environment variable FLOODS_API_KEY, or paste the key into\n",
      "FLOODS_API_KEY at the top of this script.",
      call. = FALSE
    )
  }
  key
}

apply_proxy <- function(req) {
  proxy <- trimws(HTTPS_PROXY_URL)
  if (!nzchar(proxy)) return(req)
  # Also set env so curl sees it (helps some Windows builds)
  Sys.setenv(HTTPS_PROXY = proxy, https_proxy = proxy, HTTP_PROXY = proxy, http_proxy = proxy)
  req_proxy(req, proxy)
}

flood_req <- function(url) {
  request(url) |>
    apply_proxy() |>
    req_timeout(HTTP_TIMEOUT_SEC) |>
    req_retry(max_tries = 3, backoff = ~2) |>
    req_error(is_error = \(r) FALSE)
}

post_json <- function(url, body) {
  req <- flood_req(url)
  
  if (length(body) == 0L) {
    req <- httr2::req_body_raw(
      req,
      body = "{}",
      type = "application/json"
    )
  } else {
    req <- httr2::req_body_json(req, body)
  }
  
  resp <- httr2::req_perform(req)
  data <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  
  if (!is.null(data$error)) {
    stop(
      jsonlite::toJSON(data$error, auto_unbox = TRUE),
      call. = FALSE
    )
  }
  
  data
}

get_json <- function(url, query = list()) {
  req <- flood_req(url)
  if (length(query)) req <- req_url_query(req, !!!query)
  resp <- req_perform(req)
  resp_body_json(resp, simplifyVector = FALSE)
}

network_help_message <- function(err_msg = "") {
  paste0(
    "Cannot reach Flood Hub API (floodforecasting.googleapis.com).\n",
    "This is almost always a WHO network / proxy / firewall issue — not a bad API key.\n\n",
    "Try on your colleague's PC:\n",
    "  1. Open a browser and visit https://floodforecasting.googleapis.com/ — if it hangs, the network is blocking Google APIs.\n",
    "  2. Connect to WHO VPN (or try off-VPN / mobile hotspot) and re-run.\n",
    "  3. Set the corporate proxy in CONFIG, e.g.\n",
    "       HTTPS_PROXY_URL <- \"http://proxy.who.int:8080\"\n",
    "     (ask IT for the exact WHO proxy if unsure), then Source again.\n",
    "  4. Or set a Windows user env var HTTPS_PROXY to that same URL and restart RStudio.\n\n",
    if (nzchar(err_msg)) paste0("Technical detail: ", err_msg, "\n") else ""
  )
}

check_flood_api_reachability <- function(api_key) {
  message("  Checking Flood Hub API connectivity...")
  url <- paste0(FLOOD_API, "/floodStatus:searchLatestFloodStatusByArea?key=", api_key)
  # Tiny probe: one country, pageSize 1 — fails fast if network is blocked
  ok <- tryCatch({
    post_json(url, list(
      regionCode = "BD",
      pageSize = 1L,
      includeNonQualityVerified = TRUE
    ))
    TRUE
  }, error = function(e) {
    message(network_help_message(conditionMessage(e)))
    FALSE
  })
  if (!ok) {
    stop("Flood Hub unreachable — fix network/proxy, then re-run.", call. = FALSE)
  }
  message("    Flood Hub reachable.")
}

map_severity <- function(x) {
  if (is.null(x) || !length(x) || !nzchar(as.character(x[[1]]))) {
    return("Unknown")
  }
  mapped <- unname(SEVERITY_MAP[as.character(x[[1]])])
  if (length(mapped) == 0 || is.na(mapped)) "Unknown" else mapped
}

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

today_local <- function() as.Date(Sys.time(), tz = LOCAL_TZ)
stamp_local <- function(fmt = "%Y-%m-%d %H:%M IST") {
  format(Sys.time(), tz = LOCAL_TZ, format = fmt)
}

# ---- Flood Hub --------------------------------------------------------------
fetch_flood_statuses <- function(api_key, region_code) {
  statuses <- list()
  body <- list(
    regionCode = region_code,
    pageSize = 10000L,
    includeNonQualityVerified = TRUE
  )
  url <- paste0(FLOOD_API, "/floodStatus:searchLatestFloodStatusByArea?key=", api_key)
  repeat {
    data <- post_json(url, body)
    batch <- data$floodStatuses %||% list()
    statuses <- c(statuses, batch)
    token <- data$nextPageToken
    if (is.null(token) || !nzchar(token)) break
    body$pageToken <- token
  }
  statuses
}

summarize_country_gauges <- function(country, region_code, statuses) {
  counts <- setNames(as.list(rep(0L, length(SEVERITY_MAP))), unname(SEVERITY_MAP))
  hot <- list()
  for (s in statuses) {
    sev <- map_severity(s$severity)
    counts[[sev]] <- counts[[sev]] + 1L
    if (sev %in% ESCALATION_LEVELS) {
      loc <- s$gaugeLocation %||% list()
      hot[[length(hot) + 1]] <- tibble(
        country = country,
        region_code = region_code,
        gauge_id = as.character(s$gaugeId %||% s$name %||% ""),
        severity = sev,
        latitude = loc$latitude %||% NA_real_,
        longitude = loc$longitude %||% NA_real_
      )
    }
  }
  list(
    summary = tibble(
      country = country,
      region_code = region_code,
      total_gauges = length(statuses),
      Unknown = counts$Unknown,
      Normal = counts$Normal,
      Warning = counts$Warning,
      Danger = counts$Danger,
      Extreme = counts$Extreme
    ),
    hot_gauges = if (length(hot)) bind_rows(hot) else tibble()
  )
}

fetch_significant_events <- function(api_key) {
  events <- list()
  body <- list()
  url <- paste0(FLOOD_API, "/significantEvents:search?key=", api_key)
  repeat {
    data <- post_json(url, body)
    events <- c(events, data$significantEvents %||% list())
    token <- data$nextPageToken
    if (is.null(token) || !nzchar(token)) break
    body$pageToken <- token
  }
  events
}

event_touches_searo <- function(event) {
  blob <- tolower(jsonlite::toJSON(event, auto_unbox = TRUE))
  if (any(vapply(SEARO_NAME_HINTS, grepl, blob, fixed = TRUE, FUN.VALUE = logical(1)))) {
    return(TRUE)
  }
  candidates <- character()
  for (key in c("country", "regionCode", "countryCode")) {
    if (!is.null(event[[key]]) && nzchar(as.character(event[[key]]))) {
      candidates <- c(candidates, as.character(event[[key]]))
    }
  }
  loc <- event$location
  if (is.list(loc)) {
    for (key in c("country", "countryCode", "regionCode")) {
      if (!is.null(loc[[key]]) && nzchar(as.character(loc[[key]]))) {
        candidates <- c(candidates, as.character(loc[[key]]))
      }
    }
  }
  any(toupper(candidates) %in% SEARO_ISO2)
}

# Urban flash floods — ~24h forecast at ~20km (Flood Hub flashFloods:search)
fetch_flash_floods <- function(api_key, country_codes = SEARO_ISO2) {
  events <- list()
  body <- list(countryCodes = unname(as.character(country_codes)), pageSize = 10000L)
  url <- paste0(FLOOD_API, "/flashFloods:search?key=", api_key)
  repeat {
    data <- post_json(url, body)
    events <- c(events, data$flashFloodEvents %||% list())
    token <- data$nextPageToken
    if (is.null(token) || !nzchar(token)) break
    body$pageToken <- token
  }
  events
}

normalize_flash_floods <- function(events) {
  empty <- tibble(
    country = character(), iso2 = character(),
    forecast_issue = character(), period_hours = integer(),
    highly_likely = logical(), likely = logical()
  )
  if (!length(events)) return(empty)

  rows <- list()
  for (e in events) {
    codes <- toupper(as.character(unlist(e$affectedCountryCodes %||% list())))
    codes <- codes[codes %in% SEARO_ISO2]
    if (!length(codes)) next
    issue <- as.character(e$forecastIssueTime %||% "")
    period <- as.integer(e$forecastPeriodHours %||% NA_integer_)
    highly <- nzchar(as.character(e$highlyLikelyAffectedPolygonId %||% ""))
    likely <- nzchar(as.character(e$likelyAffectedPolygonId %||% ""))
    for (code in codes) {
      rows[[length(rows) + 1]] <- tibble(
        country = unname(SEARO_ISO2_NAMES[[code]] %||% code),
        iso2 = code,
        forecast_issue = issue,
        period_hours = period,
        highly_likely = highly,
        likely = likely
      )
    }
  }
  if (!length(rows)) return(empty)
  bind_rows(rows) |>
    arrange(desc(highly_likely), desc(likely), country)
}

summarize_flash_by_country <- function(flash_df) {
  if (!nrow(flash_df)) {
    return(tibble(
      country = character(), iso2 = character(), events = integer(),
      highly_likely_events = integer(), likely_events = integer(),
      period_hours = integer(), latest_issue = character()
    ))
  }
  flash_df |>
    group_by(country, iso2) |>
    summarise(
      events = n(),
      highly_likely_events = sum(highly_likely, na.rm = TRUE),
      likely_events = sum(likely, na.rm = TRUE),
      period_hours = {
        x <- period_hours[!is.na(period_hours)]
        if (!length(x)) NA_integer_ else as.integer(max(x))
      },
      latest_issue = {
        x <- forecast_issue[nzchar(forecast_issue)]
        if (!length(x)) "" else max(x)
      },
      .groups = "drop"
    ) |>
    arrange(desc(highly_likely_events), desc(events), country)
}

# ---- GDACS ------------------------------------------------------------------
fetch_gdacs_events <- function(event_types = "FL;TC", days_back = 7) {
  today <- today_local()
  features <- list()
  page <- 1L
  repeat {
    data <- get_json(GDACS_SEARCH, query = list(
      eventlist = event_types,
      fromdate = as.character(today - days_back),
      todate = as.character(today),
      alertlevel = "green;orange;red",
      pagenumber = page
    ))
    batch <- data$features %||% list()
    if (!length(batch)) break
    features <- c(features, batch)
    if (length(batch) < 100 || page > 20) break
    page <- page + 1L
  }
  features
}

gdacs_touches_searo <- function(feature) {
  p <- feature$properties %||% list()
  country <- as.character(p$country %||% p$countrylist %||% "")
  iso3 <- as.character(p$iso3 %||% p$iso3country %||% "")
  name <- as.character(p$name %||% "")
  blob <- tolower(paste(country, iso3, name))
  if (any(vapply(SEARO_NAME_HINTS, grepl, blob, fixed = TRUE, FUN.VALUE = logical(1)))) {
    return(TRUE)
  }
  FALSE
}

normalize_gdacs <- function(features) {
  if (!length(features)) {
    return(tibble(
      event_type = character(), event_id = character(), name = character(),
      country = character(), alert_level = character(),
      from_date = character(), to_date = character(), url = character()
    ))
  }
  bind_rows(lapply(features, function(f) {
    p <- f$properties %||% list()
    tibble(
      event_type = as.character(p$eventtype %||% p$eventType %||% NA),
      event_id = as.character(p$eventid %||% p$eventId %||% NA),
      name = as.character(p$name %||% p$eventname %||% NA),
      country = as.character(p$country %||% p$countrylist %||% NA),
      alert_level = tolower(as.character(p$alertlevel %||% p$episodealertlevel %||% "")),
      from_date = as.character(p$fromdate %||% p$fromDate %||% NA),
      to_date = as.character(p$todate %||% p$toDate %||% NA),
      url = as.character(p$url %||% p$link %||% "")
    )
  }))
}

# ---- OpenmeteoAPI ------------------------------------------------------------------

# WMO Weather Code Mapping
get_weather_description <- function(code) {
  code_str <- as.character(code)
  desc <- switch(
    code_str,
    "0" = "Clear Sky", "1" = "Mainly Clear", "2" = "Partly Cloudy", "3" = "Overcast",
    "45" = "Fog", "48" = "Rime Fog",
    "51" = "Light Drizzle", "53" = "Moderate Drizzle", "55" = "Dense Drizzle",
    "56" = "Light Freezing Drizzle", "57" = "Dense Freezing Drizzle",
    "61" = "Light Rain", "63" = "Moderate Rain", "65" = "Heavy Rain",
    "66" = "Light Freezing Rain", "67" = "Heavy Freezing Rain",
    "71" = "Light Snow", "73" = "Moderate Snow", "75" = "Heavy Snow", "77" = "Snow Grains",
    "80" = "Light Rain Showers", "81" = "Moderate Rain Showers", "82" = "Heavy Rain Showers",
    "85" = "Light Snow Showers", "86" = "Heavy Snow Showers",
    "95" = "Thunderstorm", "96" = "Thunderstorm with Light Hail", "99" = "Thunderstorm with Heavy Hail",
    "Unknown"
  )
  return(desc)
}

# Weather Alert System
get_weather_alert <- function(
    country, temp_max, temp_min, precip,
    wind_speed = NA_real_,
    weather_code = NA_integer_,
    rain_chance = NA_real_,
    season = NA_character_,
    fallback_rain = c(yellow = 30, orange = 70, red = 150),
    date = NA,
    # Retained for compatibility with existing calls
    is_terai = FALSE,
    consecutive_hot_days = NA_integer_
) {
  # Guard against NA or empty country input
  country_clean <- if (is.null(country) || length(country) == 0L || is.na(country)) {
    ""
  } else {
    trimws(tolower(as.character(country)))
  }
  
  season_clean <- if (is.null(season) || length(season) == 0L || is.na(season)) {
    NA_character_
  } else {
    trimws(tolower(as.character(season)))
  }
  
  weather_code <- suppressWarnings(as.integer(weather_code))
  
  # Daily rainfall totals in mm
  rain_thresholds <- switch(
    country_clean,
    "bangladesh" = c(yellow = 23,   orange = 44,    red = 89),
    "bhutan"     = c(yellow = 31,   orange = 71,    red = 151),
    "india"      = c(yellow = 64.5, orange = 115.6, red = 204.5),
    "thailand"   = c(yellow = 10.1, orange = 35.1,  red = 90.1),
    "nepal"      = c(yellow = 30,   orange = 70,    red = 140),
    "sri lanka"  = c(yellow = 74.5, orange = 75,    red = 100),
    fallback_rain
  )
  
  rain_source <- switch(
    country_clean,
    "bangladesh" = "BMD rainfall categories",
    "bhutan"     = "NCHM rainfall categories",
    "india"      = "IMD rainfall categories",
    "thailand"   = "TMD rainfall categories",
    "nepal"      = "DHM 24-hour warning level; lower levels are custom",
    "sri lanka"  = "DMC ADRC SL NBRO warning categories",
    "Custom dashboard rainfall thresholds"
  )
  
  if (all(is.na(c(temp_max, temp_min, precip, wind_speed, weather_code)))) {
    return(list(
      level = NA_character_,
      text = "No forecast data",
      reason = NA_character_,
      rain_source = rain_source
    ))
  }
  
  level <- "Green"
  reasons <- character(0)
  ranks <- c(Green = 0, Yellow = 1, Orange = 2, Red = 3)
  
  # Keep reasons only at the highest triggered level
  raise_level <- function(new_level, new_reason) {
    if (ranks[[new_level]] > ranks[[level]]) {
      level <<- new_level
      reasons <<- new_reason
    } else if (ranks[[new_level]] == ranks[[level]]) {
      reasons <<- unique(c(reasons, new_reason))
    }
  }
  
  # Approximate fixed season calendar for Thailand
  get_thailand_season <- function(d) {
    parsed_date <- tryCatch(as.Date(d), error = function(e) as.Date(NA))
    if (is.na(parsed_date)) return(NA_character_)
    
    month_day <- format(parsed_date, "%m-%d")
    
    if (month_day >= "02-15" && month_day < "05-15") {
      "summer"
    } else if (month_day >= "05-15" && month_day < "10-15") {
      "rainy"
    } else {
      "winter"
    }
  }
  
  if (country_clean == "thailand" && is.na(season_clean)) {
    season_clean <- get_thailand_season(date)
  }
  
  # Daily rainfall thresholds
  if (!is.na(precip)) {
    if (precip >= rain_thresholds[["red"]]) {
      raise_level("Red", "Extreme rainfall")
    } else if (precip >= rain_thresholds[["orange"]]) {
      raise_level("Orange", "Very heavy rainfall")
    } else if (precip >= rain_thresholds[["yellow"]]) {
      raise_level("Yellow", "Heavy rainfall")
    }
  }
  
  # Bangladesh: maximum-temperature categories
  if (country_clean == "bangladesh" && !is.na(temp_max)) {
    if (temp_max >= 42) {
      raise_level("Red", "Very severe heatwave temperature")
    } else if (temp_max >= 40) {
      raise_level("Orange", "Severe heatwave temperature")
    } else if (temp_max >= 38) {
      raise_level("Yellow", "Moderate heatwave temperature")
    } else if (temp_max >= 36) {
      raise_level("Yellow", "Mild heatwave temperature")
    }
  }
  
  # Thailand: summer maximum and winter minimum temperatures
  if (country_clean == "thailand") {
    if (!is.na(season_clean) && season_clean == "summer" && !is.na(temp_max)) {
      if (temp_max >= 40) {
        raise_level("Red", "Very hot")
      } else if (temp_max >= 35) {
        raise_level("Yellow", "Hot")
      }
    }
    
    if (!is.na(season_clean) && season_clean == "winter" && !is.na(temp_min)) {
      if (temp_min < 8) {
        raise_level("Red", "Very cold")
      } else if (temp_min < 16) {
        raise_level("Orange", "Cold")
      } else if (temp_min < 23) {
        raise_level("Yellow", "Cool")
      }
    }
  }
  
  # India temperature indicators
  if (country_clean == "india" && !is.na(temp_max)) {
    if (temp_max >= 47) {
      raise_level("Red", "Maximum temperature ≥47°C")
    } else if (temp_max >= 45) {
      raise_level("Orange", "Maximum temperature ≥45°C")
    }
  }
  
  # Sri Lanka: heat index / temp indicators
  if (country_clean == "sri lanka" && !is.na(temp_max)) {
    if (temp_max >= 46) {
      raise_level("Red", "Extreme caution")
    } else if (temp_max >= 39) {
      raise_level("Orange", "Caution")
    }
  }
  
  # Generic heat indicators for other countries
  if (!(country_clean %in% c("bangladesh", "thailand", "india", "sri lanka")) && !is.na(temp_max)) {
    if (temp_max > 42) {
      raise_level("Red", "Very hot")
    } else if (temp_max > 38) {
      raise_level("Orange", "High temperature")
    } else if (temp_max > 35) {
      raise_level("Yellow", "Hot")
    }
  }
  
  # Weather-code indicators
  if (!is.na(weather_code)) {
    if (weather_code %in% c(96L, 99L)) {
      raise_level("Red", "Thunderstorm with hail")
    } else if (weather_code %in% c(65L, 67L, 75L, 82L, 86L, 95L)) {
      description <- switch(
        as.character(weather_code),
        "65" = "Heavy rain",
        "67" = "Heavy freezing rain",
        "75" = "Heavy snow",
        "82" = "Heavy rain showers",
        "86" = "Heavy snow showers",
        "95" = "Thunderstorm"
      )
      raise_level("Orange", description)
    } else if (weather_code %in% c(56L, 57L, 63L, 66L, 73L, 81L, 85L)) {
      description <- switch(
        as.character(weather_code),
        "56" = "Light freezing drizzle",
        "57" = "Dense freezing drizzle",
        "63" = "Moderate rain",
        "66" = "Light freezing rain",
        "73" = "Moderate snow",
        "81" = "Moderate rain showers",
        "85" = "Light snow showers"
      )
      raise_level("Yellow", description)
    }
  }
  
  # Rain probability
  if (!is.na(rain_chance) && !is.na(precip) && rain_chance > 70 && precip > 5) {
    raise_level("Yellow", "Rain likely")
  }
  
  # Wind speed indicators (km/h)
  if (!is.na(wind_speed)) {
    if (wind_speed > 60) {
      raise_level("Red", "Very strong wind")
    } else if (wind_speed > 50) {
      raise_level("Orange", "Strong wind")
    } else if (wind_speed > 40) {
      raise_level("Yellow", "Elevated wind")
    }
  }
  
  # Additional Red rules: prevent redundant "Daily rainfall above 100 mm" if "Extreme rainfall" is already recorded
  if (!is.na(precip)) {
    if (precip > 100 && !("Extreme rainfall" %in% reasons)) {
      raise_level("Red", "Daily rainfall above 100 mm")
    }
    
    if (precip > 50 && !is.na(wind_speed) && wind_speed > 60) {
      raise_level("Red", "Rainfall above 50 mm with wind above 60 km/h")
    }
  }
  
  alert_text <- c(
    Green = "Normal",
    Yellow = "Advisory",
    Orange = "Watch",
    Red = "Warning"
  )[[level]]
  
  list(
    level = level,
    text = alert_text,
    reason = if (length(reasons)) {
      paste(reasons, collapse = "; ")
    } else {
      "No threshold reached"
    },
    rain_source = rain_source
  )
}

# Fetch all weather data
fetch_city_weather <- function(city_info) {
  tryCatch({
    response <- httr2::request(
      "https://api.open-meteo.com/v1/forecast"
    ) %>%
      httr2::req_url_query(
        latitude = city_info$lat,
        longitude = city_info$lon,
        current = paste(
          "temperature_2m", "relative_humidity_2m",
          "apparent_temperature", "precipitation",
          "weather_code", "pressure_msl",
          "wind_speed_10m", "is_day",
          sep = ","
        ),
        hourly = paste(
          "temperature_2m", "relative_humidity_2m",
          "weather_code", "precipitation_probability",
          "precipitation", "wind_speed_10m",
          sep = ","
        ),
        daily = paste(
          "weather_code", "temperature_2m_max",
          "temperature_2m_min", "precipitation_sum",
          "precipitation_probability_max",
          "wind_speed_10m_max", "sunrise", "sunset",
          sep = ","
        ),
        timezone = "auto",
        wind_speed_unit = "kmh",
        forecast_days = 7
      ) %>%
      httr2::req_timeout(30) %>%
      httr2::req_retry(max_tries = 3) %>%
      httr2::req_perform()
    
    httr2::resp_body_json(response)
    
  }, error = function(e) {
    message(
      "Error fetching ", city_info$name, ": ",
      conditionMessage(e)
    )
    NULL
  })
}


get_daily_forecast <- function(cities_db, delay = 0.1) {
  
  all_weather_data <- list()
  
  # 1. Fetch weather for each city
  for (city_name in names(cities_db)) {
    
    cat("Fetching:", city_name, "\n")
    
    city_info <- cities_db[[city_name]]
    weather <- fetch_city_weather(city_info)
    
    if (!is.null(weather)) {
      all_weather_data[[city_name]] <- list(
        city = if (is.null(city_info$name)) city_name else city_info$name,
        state = city_info$state,
        country = city_info$country,
        lat = city_info$lat,
        lon = city_info$lon,
        weather = weather
      )
    }
    
    Sys.sleep(delay)
  }
  
  # 2. Create daily forecast rows with coordinates
  daily_forecast_df <- dplyr::bind_rows(
    lapply(all_weather_data, function(city_data) {
      
      daily <- city_data$weather$daily
      
      if (is.null(daily) || is.null(daily$time)) {
        return(NULL)
      }
      
      dates <- as.Date(unlist(daily$time))
      
      dplyr::bind_rows(lapply(seq_along(dates), function(i) {
        
        max_temp     <- unlist(daily$temperature_2m_max)[i]
        min_temp     <- unlist(daily$temperature_2m_min)[i]
        precip       <- unlist(daily$precipitation_sum)[i]
        rain_chance  <- unlist(daily$precipitation_probability_max)[i]
        weather_code <- unlist(daily$weather_code)[i]
        wind_speed   <- unlist(daily$wind_speed_10m_max)[i]
        
        alert <- get_weather_alert(
          country = city_data$country,
          temp_max = max_temp,
          temp_min = min_temp,
          precip = precip,
          wind_speed = wind_speed,
          weather_code = weather_code,
          rain_chance = rain_chance,
          date = dates[i]
        )
        
        data.frame(
          Date = dates[i],
          Day_of_Week = weekdays(dates[i]),
          City = city_data$city,
          State = city_data$state,
          Country = city_data$country,
          Alert_Level = alert$text,
          Alert_Color = alert$level,
          Alert_Reason = alert$reason,
          Weather_Condition = get_weather_description(weather_code),
          Weather_Code = weather_code,
          Max_Temp_C = round(max_temp, 1),
          Min_Temp_C = round(min_temp, 1),
          Precipitation_mm = round(precip, 1),
          Windspeed = round(wind_speed, 1),
          Rain_Chance_Percent = rain_chance,
          Sunrise = unlist(daily$sunrise)[i],
          Sunset = unlist(daily$sunset)[i],
          lat = city_data$lat,
          lon = city_data$lon
        )
      }))
    })
  )
  
  # Return early if no forecasts are available
  if (nrow(daily_forecast_df) == 0L) {
    return(daily_forecast_df)
  }
  
  # 3. Sort by date, alert severity, and city
  daily_forecast_df <- daily_forecast_df |>
    dplyr::mutate(
      Alert_Order = match(
        Alert_Level,
        c("Warning", "Watch", "Advisory", "Normal")
      )
    ) |>
    dplyr::arrange(Date, Alert_Order, City) |>
    dplyr::select(-Alert_Order)
  
  rownames(daily_forecast_df) <- NULL
  
  return(daily_forecast_df)
}

daily_forecast_df <- get_daily_forecast(cities_db)


# ---- Admin-1 (Natural Earth) ------------------------------------------------
load_natural_earth_admin1 <- function(cache_dir) {
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  zip_path <- file.path(cache_dir, "ne_10m_admin_1_states_provinces.zip")
  if (!file.exists(zip_path)) {
    message("  Downloading Natural Earth Admin-1 (first run only)...")
    utils::download.file(NE_ADMIN1_URL, zip_path, mode = "wb", quiet = TRUE)
  } else {
    message("  Using cached Natural Earth Admin-1")
  }
  # GDAL virtual zip path works on Windows/macOS/Linux with sf
  gdf <- st_read(
    paste0("/vsizip/", gsub("\\\\", "/", normalizePath(zip_path, winslash = "/", mustWork = TRUE))),
    quiet = TRUE
  )
  need_cols <- c("iso_a2", "name")
  if (!all(need_cols %in% names(gdf))) {
    stop("Natural Earth Admin-1 missing expected columns: iso_a2, name", call. = FALSE)
  }
  gdf
}

hot_gauge_sf <- function(statuses) {
  rows <- list()
  for (s in statuses) {
    sev <- map_severity(s$severity)
    if (!sev %in% ESCALATION_LEVELS) next
    loc <- s$gaugeLocation %||% list()
    if (is.null(loc$latitude) || is.null(loc$longitude)) next
    rows[[length(rows) + 1]] <- tibble(
      severity = sev,
      longitude = as.numeric(loc$longitude),
      latitude = as.numeric(loc$latitude)
    )
  }
  if (!length(rows)) return(NULL)
  pts <- bind_rows(rows)
  st_as_sf(pts, coords = c("longitude", "latitude"), crs = 4326)
}

admin1_hotspots_natural_earth <- function(cache_dir, country_df, raw_by_region) {
  ne <- tryCatch(load_natural_earth_admin1(cache_dir), error = function(e) {
    message("  Natural Earth Admin-1 load failed: ", conditionMessage(e))
    NULL
  })
  if (is.null(ne)) return(tibble())

  frames <- list()
  for (i in seq_len(nrow(country_df))) {
    rc <- country_df$region_code[[i]]
    country <- country_df$country[[i]]
    country_gdf <- ne[ne$iso_a2 == rc, ]
    if (!nrow(country_gdf)) {
      message("  No Natural Earth Admin-1 polygons for ", rc)
      next
    }
    pts <- hot_gauge_sf(raw_by_region[[rc]] %||% list())
    if (is.null(pts) || !nrow(pts)) next

    country_gdf <- st_make_valid(country_gdf)
    joined <- st_join(pts, country_gdf["name"], join = st_within, left = FALSE)
    if (!nrow(joined)) next

    counts <- as.data.frame(joined) |>
      count(name, severity, name = "n") |>
      tidyr::pivot_wider(names_from = severity, values_from = n, values_fill = 0)

    if (!"Danger" %in% names(counts)) counts$Danger <- 0L
    if (!"Extreme" %in% names(counts)) counts$Extreme <- 0L

    frames[[length(frames) + 1]] <- tibble(
      country = country,
      region_code = rc,
      ADM1 = counts$name,
      Danger = as.integer(counts$Danger),
      Extreme = as.integer(counts$Extreme),
      hot_gauges = as.integer(counts$Danger + counts$Extreme)
    )
  }

  if (!length(frames)) return(tibble())
  bind_rows(frames) |>
    arrange(desc(hot_gauges), desc(Extreme), desc(Danger))
}

# ---- Day-over-day + triage --------------------------------------------------
load_previous_country_summary <- function(out_root, today) {
  prev_path <- file.path(out_root, as.character(today - 1), "country_summary.csv")
  if (file.exists(prev_path)) return(readr::read_csv(prev_path, show_col_types = FALSE))
  if (!dir.exists(out_root)) return(NULL)
  dirs <- list.dirs(out_root, full.names = FALSE, recursive = FALSE)
  dirs <- sort(dirs[dirs < as.character(today)], decreasing = TRUE)
  for (d in dirs) {
    candidate <- file.path(out_root, d, "country_summary.csv")
    if (file.exists(candidate)) return(readr::read_csv(candidate, show_col_types = FALSE))
  }
  NULL
}

add_day_over_day <- function(today_df, prev_df) {
  today_df$new_danger <- 0L
  today_df$new_extreme <- 0L
  today_df$cleared_hot <- 0L
  if (is.null(prev_df) || !nrow(prev_df)) return(today_df)

  for (i in seq_len(nrow(today_df))) {
    rc <- today_df$region_code[[i]]
    p <- prev_df[prev_df$region_code == rc, , drop = FALSE]
    if (!nrow(p)) {
      today_df$new_danger[[i]] <- today_df$Danger[[i]]
      today_df$new_extreme[[i]] <- today_df$Extreme[[i]]
      next
    }
    today_df$new_danger[[i]] <- max(0L, today_df$Danger[[i]] - as.integer(p$Danger[[1]]))
    today_df$new_extreme[[i]] <- max(0L, today_df$Extreme[[i]] - as.integer(p$Extreme[[1]]))
    prev_hot <- as.integer(p$Danger[[1]]) + as.integer(p$Extreme[[1]])
    today_hot <- today_df$Danger[[i]] + today_df$Extreme[[i]]
    today_df$cleared_hot[[i]] <- max(0L, prev_hot - today_hot)
  }
  today_df
}

assign_tier <- function(row, gdacs_hot_countries, flash_hot_countries = character()) {
  country <- as.character(row[["country"]])
  extreme <- as.integer(row[["Extreme"]])
  danger <- as.integer(row[["Danger"]])
  new_hot <- as.integer(row[["new_danger"]]) + as.integer(row[["new_extreme"]])
  gdacs_hit <- any(grepl(tolower(country), tolower(gdacs_hot_countries), fixed = TRUE))
  flash_hit <- country %in% flash_hot_countries

  if (extreme > 0 || (danger > 0 && gdacs_hit) || (new_hot > 0 && danger >= 3)) return("ALERT")
  if (danger > 0 || gdacs_hit || new_hot > 0 || flash_hit) return("WATCH")
  "STABLE"
}

# Soft row fills + strong text for triage tiers (WHO brief-friendly)
TIER_BG <- c(
  ALERT  = "#FFCDD2",
  WATCH  = "#FFE0B2",
  STABLE = "#C8E6C9"
)
TIER_FG <- c(
  ALERT  = "#B71C1C",
  WATCH  = "#E65100",
  STABLE = "#1B5E20"
)

# Match measles sitrep Quarto/docx theme (Office Aptos)
BRIEF_FONT <- "Aptos"
BRIEF_FONT_DISPLAY <- "Aptos Display"

fp_brief <- function(size = 11, bold = FALSE, italic = FALSE, color = "black",
                     font = BRIEF_FONT) {
  fp_text(
    font.family = font,
    font.size = size,
    bold = bold,
    italic = italic,
    color = color
  )
}

add_title <- function(doc, text) {
  body_add_fpar(
    doc,
    fpar(ftext(text, prop = fp_brief(16, bold = TRUE, color = "#0F4761", font = BRIEF_FONT_DISPLAY)))
  )
}

add_heading <- function(doc, text) {
  body_add_fpar(
    doc,
    fpar(ftext(text, prop = fp_brief(13, bold = TRUE, color = "#0F4761", font = BRIEF_FONT_DISPLAY)))
  )
}

add_body <- function(doc, text, size = 11, bold = FALSE, italic = FALSE, color = "black") {
  body_add_fpar(doc, fpar(ftext(text, prop = fp_brief(size, bold, italic, color))))
}

add_footnote <- function(doc, lines) {
  for (line in lines) {
    doc <- add_body(doc, line, size = 8, italic = TRUE, color = "#555555")
  }
  doc
}

TIER_FOOTNOTES <- c(
  "ALERT: ≥1 Extreme gauge; or Danger gauges plus a GDACS orange/red flood/cyclone for that country; or newly elevated hot gauges with ≥3 Danger gauges.",
  "WATCH: ≥1 Danger gauge, a GDACS orange/red hit, urban flash-flood signal (next ~24h), and/or newly elevated hot gauges — without meeting ALERT criteria.",
  "STABLE: No Danger/Extreme gauges, no GDACS orange/red, and no urban flash-flood signal for that country."
)

HOT_GAUGE_FOOTNOTES <- c(
  "Hot gauges: Flood Hub stations currently at Danger (severe) or Extreme severity. Admin-1 units are listed only if they contain ≥1 hot gauge; use as a geographic filter and verify with gauges + national met/hydro before acting."
)

FLASH_FLOOD_FOOTNOTES <- c(
  "Urban flash floods: Flood Hub model of flash-flood likelihood in urban areas over the next ~24 hours (~20 km). Highly likely = stronger polygon signal. Decision support only — verify with national met/hydro."
)

df_to_md <- function(df) {
  if (!nrow(df)) return("_None_")
  # simple markdown table without knitr dependency
  cols <- names(df)
  header <- paste0("| ", paste(cols, collapse = " | "), " |")
  sep <- paste0("| ", paste(rep("---", length(cols)), collapse = " | "), " |")
  rows <- apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  paste(c(header, sep, rows), collapse = "\n")
}

# Prefer one GDACS row per event (report page) for cleaner briefs
gdacs_for_brief <- function(gdacs_df) {
  if (!nrow(gdacs_df)) return(gdacs_df)
  gshow <- gdacs_df
  gshow$`_ord` <- match(gshow$alert_level, c("red", "orange", "green"), nomatch = 9)
  urls <- as.character(gshow$url)
  urls[is.na(urls)] <- ""
  # Prefer human-readable report URLs when duplicates exist
  gshow$`_url_pref` <- ifelse(
    grepl("report\\.aspx", urls, ignore.case = TRUE), 0L,
    ifelse(grepl("geteventdata", urls, ignore.case = TRUE), 1L, 2L)
  )
  gshow <- gshow[order(gshow$`_ord`, gshow$`_url_pref`), ]
  key <- paste(gshow$event_type, gshow$event_id, gshow$country, sep = "|")
  gshow <- gshow[!duplicated(key), ]
  gshow$`_ord` <- NULL
  gshow$`_url_pref` <- NULL
  gshow
}

ft_table <- function(df) {
  if (!nrow(df)) return(NULL)
  flextable(df) |>
    font(fontname = BRIEF_FONT, part = "all") |>
    fontsize(size = 9, part = "all") |>
    bold(part = "header") |>
    align(align = "center", part = "header") |>
    autofit() |>
    set_table_properties(layout = "autofit", width = 1)
}

ft_country_table <- function(df) {
  ft <- ft_table(df)
  if (is.null(ft) || !"tier" %in% names(df)) return(ft)

  for (tier in names(TIER_BG)) {
    rows <- which(df$tier == tier)
    if (!length(rows)) next
    ft <- bg(ft, i = rows, bg = TIER_BG[[tier]])
    ft <- color(ft, i = rows, j = "tier", color = TIER_FG[[tier]])
    ft <- bold(ft, i = rows, j = "tier")
  }
  ft
}

write_brief_docx <- function(
    path,
    run_date,
    country_df,
    gdacs_df,
    sig_events,
    admin1_df,
    flash_df = tibble::tibble(),
    weather_map = NULL,
    daily_forecast_df = NULL
) {
  generated <- stamp_local()

  doc <- read_docx()
  doc <- add_title(doc, paste0("WHO SEARO Daily Flood Early Warning Brief — ", run_date))
  doc <- add_body(doc, paste0("Generated ", generated), size = 10, italic = TRUE, color = "#555555")
  doc <- add_body(doc, "")
  doc <- add_heading(doc, "Verdict (triage)")

  for (tier in c("ALERT", "WATCH", "STABLE")) {
    subset <- country_df[country_df$tier == tier, , drop = FALSE]
    if (!nrow(subset)) next
    doc <- add_body(
      doc,
      paste0(tier, ": ", paste(subset$country, collapse = ", ")),
      bold = TRUE,
      color = TIER_FG[[tier]]
    )
  }

  show_cols <- intersect(
    c("country", "tier", "total_gauges", "Warning", "Danger", "Extreme",
      "new_danger", "new_extreme", "cleared_hot"),
    names(country_df)
  )
  doc <- add_body(doc, "")
  doc <- add_heading(doc, "Flood Hub — country gauge summary")
  ft <- ft_country_table(country_df[show_cols])
  if (!is.null(ft)) doc <- body_add_flextable(doc, ft)
  doc <- add_footnote(doc, TIER_FOOTNOTES)

  doc <- add_body(doc, "")
  doc <- add_heading(doc, "Urban flash floods — next ~24 hours (SEARO)")
  flash_sum <- summarize_flash_by_country(flash_df)
  if (!nrow(flash_sum)) {
    doc <- add_body(doc, "No SEARO urban flash-flood events returned.")
  } else {
    ft <- ft_table(flash_sum)
    if (!is.null(ft)) doc <- body_add_flextable(doc, ft)
  }
  doc <- add_footnote(doc, FLASH_FLOOD_FOOTNOTES)

  doc <- add_body(doc, "")
  doc <- add_heading(doc, "GDACS — SEARO floods & cyclones (last 7 days)")

  gshow <- gdacs_for_brief(gdacs_df)
  if (!nrow(gshow)) {
    doc <- add_body(doc, "No SEARO flood/cyclone events returned.")
  } else {
    cols <- intersect(
      c("alert_level", "event_type", "country", "name", "from_date", "url"),
      names(gshow)
    )
    ft <- ft_table(utils::head(gshow[cols], 20))
    if (!is.null(ft)) doc <- body_add_flextable(doc, ft)
  }

  doc <- add_body(doc, "")
  doc <- add_heading(doc, "Flood Hub significant events (SEARO filter)")
  if (!length(sig_events)) {
    doc <- add_body(doc, "None touching SEARO right now.")
  } else {
    for (e in utils::head(sig_events, 10)) {
      txt <- substr(jsonlite::toJSON(e, auto_unbox = TRUE), 1, 200)
      doc <- add_body(doc, paste0("- ", txt, "..."), size = 9)
    }
  }

  if (!is.null(admin1_df) && nrow(admin1_df)) {
    doc <- add_body(doc, "")
    doc <- add_heading(doc, "Admin-1 screen (Danger/Extreme gauges only)")
    ft <- ft_table(utils::head(admin1_df, 25))
    if (!is.null(ft)) doc <- body_add_flextable(doc, ft)
    doc <- add_footnote(doc, HOT_GAUGE_FOOTNOTES)
  }
  
  # OpenMeteo Forecasting
  doc <- add_body(doc, "")
  doc <- add_heading(doc, "OpenMeteo Forecasting")
  
  if (!is.null(daily_forecast_df) && nrow(daily_forecast_df) > 0L) {
    
    n_cities <- daily_forecast_df |>
      dplyr::distinct(Country, State, City) |>
      nrow()
    
    n_warning <- sum(
      daily_forecast_df$Alert_Level == "Warning",
      na.rm = TRUE
    )
    
    n_watch <- sum(
      daily_forecast_df$Alert_Level == "Watch",
      na.rm = TRUE
    )
    
    dates <- as.Date(daily_forecast_df$Date)
    dates <- dates[!is.na(dates)]
    
    date_text <- if (length(dates) > 0L) {
      min_date <- format(min(dates), "%d %B %Y")
      max_date <- format(max(dates), "%d %B %Y")
      paste0(" from ", min_date, " to ", max_date)
    } else {
      ""
    }
    
    doc <- add_body(
      doc,
      paste0(
        "Forecast data collected for ", n_cities, " locations",
        date_text, ". ",
        "Across this forecast period, ",
        n_warning, " Warning and ",
        n_watch, " Watch alerts were identified ",
        "(counts represent location–date forecasts)."
      )
    )
    
    # Larger gap before Warning and Watch reasons
    doc <- add_body(doc, "")
    
    for (alert_level in c("Warning", "Watch")) {
      
      reasons <- daily_forecast_df |>
        dplyr::filter(Alert_Level == alert_level) |>
        dplyr::pull(Alert_Reason)
      
      # 1. Filter out NAs and placeholder text
      reasons <- reasons[!is.na(reasons) & nzchar(trimws(reasons))]
      reasons <- reasons[reasons != "No threshold reached"]
      
      if (length(reasons) > 0L) {
        # 2. Split on semicolons (and handle any legacy 'with' if older data exists)
        # Protect phrases like 'with hail' or 'with wind' before splitting on legacy separators
        reasons <- gsub("(?i)with (hail|wind)", "WITH_\\1", reasons, perl = TRUE)
        reasons <- unlist(strsplit(reasons, "\\s*;\\s*|\\s+with\\s+", perl = TRUE))
        reasons <- gsub("WITH_", "with ", reasons, fixed = TRUE)
        
        # 3. Trim extra spaces
        reasons <- trimws(reasons)
        reasons <- reasons[nzchar(reasons)]
        
        # 4. Enforce case-insensitive deduplication while preserving Title/proper case
        # Matches strings that differ only by upper/lower case
        unique_indices <- !duplicated(tolower(reasons))
        reasons <- sort(reasons[unique_indices])
      }
      
      if (length(reasons) > 0L) {
        doc <- add_body(
          doc,
          paste0(
            alert_level, " reasons: ",
            paste(reasons, collapse = "; "),
            "."
          )
        )
      }
    }
  }
  
  # Weather map
  if (!is.null(weather_map)) {
    
    # Start on a new page
    doc <- officer::body_add_break(doc)
    
    # Keep the caption with the following image
    doc <- officer::body_add_fpar(
      doc,
      value = officer::fpar(
        officer::ftext(
          "Daily forecast alerts by location",
          prop = officer::fp_text(bold = TRUE)
        ),
        fp_p = officer::fp_par(
          keep_with_next = TRUE
        )
      )
    )
    
    map_file <- tempfile(fileext = ".png")
    on.exit(unlink(map_file), add = TRUE)
    
    ggplot2::ggsave(
      filename = map_file,
      plot = weather_map,
      width = 10,
      height = 8,
      units = "in",
      dpi = 300,
      bg = "white"
    )
    
    doc <- officer::body_add_img(
      doc,
      src = map_file,
      width = 6,
      height = 4.8,
      style = "Normal"
    )
  }
  
  # Forecast summary tables
  # Combine unique reasons from Warning and Watch rows
  combine_alert_reasons <- function(reasons, levels) {
    reasons <- as.character(
      reasons[!is.na(levels) & levels %in% c("Warning", "Watch")]
    )
    
    reasons <- trimws(reasons)
    reasons <- unique(reasons[!is.na(reasons) & nzchar(reasons)])
    
    if (length(reasons) == 0L) {
      return("—")
    }
    
    paste(reasons, collapse = "; ")
  }
  
  if (!is.null(daily_forecast_df) && nrow(daily_forecast_df) > 0L) {
    
     country_alerts <- daily_forecast_df |>
        dplyr::group_by(Country) |>
        dplyr::summarise(
          Warning = sum(Alert_Level == "Warning", na.rm = TRUE),
         Watch   = sum(Alert_Level == "Watch", na.rm = TRUE),
         Alert_Reasons = paste(
           unique(na.omit(Alert_Reason[Alert_Reason != "" & Alert_Level %in% c("Warning", "Watch")])),
            collapse = "; "
          ),
          .groups = "drop"
        ) |>
        dplyr::arrange(dplyr::desc(Warning), dplyr::desc(Watch))
    
    doc <- add_body(doc, "")
    doc <- add_body(doc, "Forecast alerts by country", bold = TRUE)
    
    ft <- ft_table(country_alerts)
    
      if (!is.null(ft)) {
        doc <- flextable::body_add_flextable(doc, ft)
     }
    }
    
    state_alerts <- daily_forecast_df |>
      dplyr::group_by(Country, State) |>
      dplyr::summarise(
        Warning = sum(Alert_Level == "Warning", na.rm = TRUE),
        Watch = sum(Alert_Level == "Watch", na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(Total = Warning + Watch) |>
      dplyr::filter(Total > 0) |>
      dplyr::arrange(
        dplyr::desc(Warning),
        dplyr::desc(Total),
        Country,
        State
      ) |>
      dplyr::select(Country, State, Warning, Watch, Total)
    
    doc <- add_body(doc, "")
    doc <- add_body(
      doc,
      "Forecast alerts by state/province",
      bold = TRUE
    )
    
    if (nrow(state_alerts) == 0L) {
      doc <- add_body(doc, "No Warning or Watch alerts.")
    } else {
      ft <- ft_table(state_alerts)
      
      if (!is.null(ft)) {
        doc <- flextable::body_add_flextable(doc, ft)
      }
    }
    
  } else {
    doc <- add_body(doc, "No Open-Meteo forecast data available.")
  }
  
  doc <- add_body(
    doc,
    paste(
      "Source: Open-Meteo.",
      "Alert counts represent location–date forecast rows across all forecast dates.",
      "Forecast alerts use WMO weather codes and country-specific thresholds for",
      "Bangladesh, Bhutan, India, Thailand, Nepal, and Sri Lanka, informed by",
      "national meteorological and disaster-management agencies.",
      "Custom rules apply where unavailable.",
      "The highest triggered severity determines the forecast alert level:",
      "Normal, Advisory, Watch, or Warning.",
      "These indicators are not official agency warnings."
    ),
    size = 9,
    italic = TRUE,
    color = "#555555"
  )
  
  concern <- country_df$country[country_df$tier %in% c("ALERT", "WATCH")]
  doc <- add_body(doc, "")
  doc <- add_heading(doc, "Met / hydro verification checklist")
  if (!length(concern)) {
    doc <- add_body(doc, "No countries on WATCH/ALERT — routine scan only.")
  } else {
    for (cname in concern) {
      link <- MET_VERIFY_LINKS[[cname]] %||% "National met/hydro bulletin"
      doc <- add_body(doc, paste0("[ ] ", cname, " — ", link))
    }
  }

  doc <- add_body(doc, "")
  doc <- add_body(
    doc,
    paste(
      "Sources: Google Flood Forecasting API / Flood Hub (gauges, significant events, urban flash floods);",
      "GDACS; national NMHS/hydro. Decision support only — not an official WHO warning."
    ),
    size = 9,
    italic = TRUE,
    color = "#555555"
  )

  print(doc, target = path)
  invisible(path)
}

build_brief <- function(run_date, country_df, gdacs_df, sig_events, admin1_df, flash_df = tibble()) {
  lines <- c(
    paste0("# WHO SEARO Daily Flood Early Warning Brief — ", run_date),
    "",
    paste0("_Generated ", stamp_local(), "_"),
    "",
    "## Verdict (triage)",
    ""
  )

  for (tier in c("ALERT", "WATCH", "STABLE")) {
    subset <- country_df[country_df$tier == tier, , drop = FALSE]
    if (nrow(subset)) {
      lines <- c(lines, paste0("- **", tier, ":** ", paste(subset$country, collapse = ", ")))
    }
  }

  show_cols <- intersect(
    c("country", "tier", "total_gauges", "Warning", "Danger", "Extreme",
      "new_danger", "new_extreme", "cleared_hot"),
    names(country_df)
  )
  lines <- c(lines, "", "## Flood Hub — country gauge summary", "", df_to_md(country_df[show_cols]))

  flash_sum <- summarize_flash_by_country(flash_df)
  lines <- c(lines, "", "## Urban flash floods — next ~24 hours (SEARO)", "")
  if (!nrow(flash_sum)) {
    lines <- c(lines, "_No SEARO urban flash-flood events returned._")
  } else {
    lines <- c(lines, df_to_md(flash_sum))
  }

  lines <- c(lines, "", "## GDACS — SEARO floods & cyclones (last 7 days)", "")
  gshow <- gdacs_for_brief(gdacs_df)
  if (!nrow(gshow)) {
    lines <- c(lines, "_No SEARO flood/cyclone events returned._")
  } else {
    cols <- intersect(c("alert_level", "event_type", "country", "name", "from_date", "url"), names(gshow))
    lines <- c(lines, df_to_md(utils::head(gshow[cols], 20)))
  }

  lines <- c(lines, "", "## Flood Hub significant events (SEARO filter)", "")
  if (!length(sig_events)) {
    lines <- c(lines, "_None touching SEARO right now._")
  } else {
    for (e in utils::head(sig_events, 15)) {
      txt <- substr(jsonlite::toJSON(e, auto_unbox = TRUE), 1, 240)
      lines <- c(lines, paste0("- `", txt, "...`"))
    }
  }

  if (!is.null(admin1_df) && nrow(admin1_df)) {
    lines <- c(
      lines, "",
      "## Admin-1 screen (Danger/Extreme gauges only)", "",
      "_Use as a filter — verify with gauges + met before acting._", "",
      df_to_md(utils::head(admin1_df, 25))
    )
  }

  concern <- country_df$country[country_df$tier %in% c("ALERT", "WATCH")]
  lines <- c(lines, "", "## Met / hydro verification checklist", "")
  if (!length(concern)) {
    lines <- c(lines, "_No countries on WATCH/ALERT — routine scan only._")
  } else {
    lines <- c(lines, "Verify only these flagged countries today:")
    for (c in concern) {
      link <- MET_VERIFY_LINKS[[c]] %||% "National met/hydro bulletin"
      lines <- c(lines, paste0("- [ ] **", c, "** — ", link))
    }
  }

  lines <- c(
    lines, "",
    "---",
    paste(
      "_Sources: Google Flood Forecasting API / Flood Hub (gauges, significant events, urban flash floods);",
      "GDACS; national NMHS/hydro. Decision support only — not an official WHO warning._"
    )
  )
  paste(lines, collapse = "\n")
}


# ---- Openmeteo --------------------------------------------------------------

### Map #####
#testing
#daily_forecast_df <- daily_forecast_df %>%
#  mutate(
#    Alert_Color = if_else(
#      Country == "Timor-Leste" & Alert_Level == "Normal",
#      "Red",
#      Alert_Color
#    ),
#    Alert_Level = if_else(
#      Country == "Timor-Leste" & Alert_Level == "Normal",
#      "Warning",
#      Alert_Level
#    )
#  )

create_weather_map <- function(daily_forecast_df, ncol = 3) {
  
  # Prepare alert data
  alert_data <- daily_forecast_df |>
    dplyr::mutate(Date = as.Date(Date)) |>
    dplyr::filter(
      is.finite(lat),
      is.finite(lon),
      !is.na(Date),
      Alert_Level %in% c("Warning", "Watch")
    )
  
  if (nrow(alert_data) == 0L) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate(
          "text",
          x = 0, y = 0,
          label = "No Warning or Watch alerts with valid coordinates",
          size = 4
        ) +
        ggplot2::theme_void()
    )
  }
  
  # Separate Timor-Leste
  timor_alerts <- alert_data |>
    dplyr::filter(Country == "Timor-Leste")
  
  main_alerts <- alert_data |>
    dplyr::filter(is.na(Country) | Country != "Timor-Leste")
  
  # World boundaries
  world_map <- ggplot2::map_data("world")
  
  # Main map extent
  extent_data <- if (nrow(main_alerts) > 0L) {
    main_alerts
  } else {
    alert_data
  }
  
  lon_limits <- range(extent_data$lon) + c(-4, 4)
  lat_limits <- range(extent_data$lat) + c(-4, 3)
  
  map_width  <- diff(lon_limits)
  map_height <- diff(lat_limits)
  
  inset_width  <- map_width * 0.30
  inset_height <- map_height * 0.23
  
  # Horizontally centred, near the upper edge
  inset_xmin <- mean(lon_limits) - inset_width / 2
  inset_xmax <- mean(lon_limits) + inset_width / 2
  inset_ymax <- lat_limits[2] - map_height * 0.025
  inset_ymin <- inset_ymax - inset_height
  
  # Geographic extent inside the inset
  timor_lon <- c(124, 128)
  timor_lat <- c(-10, -7.5)
  
  inset_x <- function(x) {
    inset_xmin +
      (x - timor_lon[1]) / diff(timor_lon) *
      (inset_xmax - inset_xmin)
  }
  
  inset_y <- function(y) {
    inset_ymin +
      (y - timor_lat[1]) / diff(timor_lat) *
      (inset_ymax - inset_ymin)
  }
  
  # Only dates with Timor-Leste alerts receive an inset
  inset_dates <- timor_alerts |>
    dplyr::distinct(Date)
  
  inset_boxes <- inset_dates |>
    dplyr::mutate(
      xmin = inset_xmin,
      xmax = inset_xmax,
      ymin = inset_ymin,
      ymax = inset_ymax
    )
  
  timor_boundary <- world_map |>
    dplyr::filter(region %in% c("Timor-Leste", "East Timor")) |>
    dplyr::mutate(
      inset_long = inset_x(long),
      inset_lat = inset_y(lat)
    )
  
  inset_boundary <- merge(
    timor_boundary,
    inset_dates,
    by = NULL
  )
  
  inset_alerts <- timor_alerts |>
    dplyr::mutate(
      inset_long = inset_x(lon),
      inset_lat = inset_y(lat)
    )
  
  inset_labels <- inset_dates |>
    dplyr::mutate(
      x = (inset_xmin + inset_xmax) / 2,
      y = inset_ymax - (inset_ymax - inset_ymin) * 0.08,
      label = "Timor-Leste"
    )
  
  # Create map
  ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = world_map,
      ggplot2::aes(x = long, y = lat, group = group),
      fill = "#F2F2F2",
      colour = "#AAAAAA",
      linewidth = 0.2
    ) +
    
    # Main map: Watch
    ggplot2::geom_point(
      data = dplyr::filter(main_alerts, Alert_Level == "Watch"),
      ggplot2::aes(x = lon, y = lat, fill = Alert_Level),
      shape = 21,
      size = 1.5,
      alpha = 0.45,
      colour = "white",
      stroke = 0.3
    ) +
    
    # Main map: Warning
    ggplot2::geom_point(
      data = dplyr::filter(main_alerts, Alert_Level == "Warning"),
      ggplot2::aes(x = lon, y = lat, fill = Alert_Level),
      shape = 21,
      size = 2,
      alpha = 0.9,
      colour = "white",
      stroke = 0.3
    ) +
    
    # Inset background and frame
    ggplot2::geom_rect(
      data = inset_boxes,
      ggplot2::aes(
        xmin = xmin, xmax = xmax,
        ymin = ymin, ymax = ymax
      ),
      fill = "white",
      colour = "#777777",
      linewidth = 0.35
    ) +
    
    # Inset boundary
    ggplot2::geom_polygon(
      data = inset_boundary,
      ggplot2::aes(
        x = inset_long, y = inset_lat,
        group = group
      ),
      fill = "#F2F2F2",
      colour = "#AAAAAA",
      linewidth = 0.2
    ) +
    
    # Inset: Watch
    ggplot2::geom_point(
      data = dplyr::filter(inset_alerts, Alert_Level == "Watch"),
      ggplot2::aes(
        x = inset_long, y = inset_lat,
        fill = Alert_Level
      ),
      shape = 21,
      size = 0.8,
      alpha = 0.45,
      colour = "white",
      stroke = 0.15,
      show.legend = FALSE
    ) +
    
    # Inset: Warning
    ggplot2::geom_point(
      data = dplyr::filter(inset_alerts, Alert_Level == "Warning"),
      ggplot2::aes(
        x = inset_long, y = inset_lat,
        fill = Alert_Level
      ),
      shape = 21,
      size = 1.2,
      alpha = 0.9,
      colour = "white",
      stroke = 0.15,
      show.legend = FALSE
    ) +
    
    ggplot2::geom_text(
      data = inset_labels,
      ggplot2::aes(x = x, y = y, label = label),
      size = 2.1,
      fontface = "bold"
    ) +
    
    ggplot2::scale_fill_manual(
      name = "Alert level",
      values = c(
        "Watch" = "#F28E2B",
        "Warning" = "#D73027"
      )
    ) +
    
    ggplot2::coord_quickmap(
      xlim = lon_limits,
      ylim = lat_limits,
      expand = FALSE
    ) +
    
    ggplot2::facet_wrap(
      ~ Date,
      ncol = ncol,
      labeller = ggplot2::labeller(
        Date = function(x) format(as.Date(x), "%d %b %Y")
      )
    ) +
    
    ggplot2::labs(
      title = "Daily forecast alerts by location",
      x = NULL,
      y = NULL
    ) +
    
    ggplot2::theme_minimal(base_size = 11) +
    
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 11),
      plot.title = ggplot2::element_text(face = "bold"),
      panel.spacing = grid::unit(1, "lines")
    )
}

#table
openmeteo_warning_table <- function(daily_forecast_df) {
  
  # Include all forecast dates, even dates without warnings
  forecast_dates <- sort(unique(as.Date(daily_forecast_df$Date)))
  forecast_dates <- forecast_dates[!is.na(forecast_dates)]
  date_columns <- as.character(forecast_dates)
  
  daily_forecast_df |>
    dplyr::mutate(Date = as.Date(Date)) |>
    dplyr::filter(Alert_Level == "Warning", !is.na(Date)) |>
    dplyr::group_by(Country, Date) |>
    dplyr::summarise(
      Details = paste(
        unique(paste0(City, " - ", Alert_Reason)),
        collapse = ", "
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      Date = factor(as.character(Date), levels = date_columns)
    ) |>
    tidyr::pivot_wider(
      names_from = Date,
      values_from = Details,
      names_expand = TRUE,
      values_fill = ""
    ) |>
    dplyr::arrange(Country) |>
    dplyr::select(Country, dplyr::all_of(date_columns))
}


#run map
weather_map <- create_weather_map(daily_forecast_df)

# ---- Main -------------------------------------------------------------------
api_key <- get_api_key()
run_date <- today_local()
day_dir <- file.path(OUT_DIR, as.character(run_date))
dir.create(day_dir, showWarnings = FALSE, recursive = TRUE)

message("SEARO daily flood EWS — ", run_date)
message("Output: ", normalizePath(day_dir, mustWork = FALSE))
if (nzchar(trimws(HTTPS_PROXY_URL))) {
  message("Using HTTPS proxy: ", HTTPS_PROXY_URL)
}

check_flood_api_reachability(api_key)

summaries <- list()
hot_rows <- list()
raw_by_region <- list()

for (i in seq_len(nrow(FLOOD_HUB_COUNTRIES))) {
  country <- FLOOD_HUB_COUNTRIES$country[[i]]
  rc <- FLOOD_HUB_COUNTRIES$region_code[[i]]
  message("  Flood Hub: ", country, " (", rc, ")...")
  statuses <- tryCatch(
    fetch_flood_statuses(api_key, rc),
    error = function(e) {
      message("    FAILED: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(statuses)) next
  raw_by_region[[rc]] <- statuses
  res <- summarize_country_gauges(country, rc, statuses)
  summaries[[length(summaries) + 1]] <- res$summary
  if (nrow(res$hot_gauges)) hot_rows[[length(hot_rows) + 1]] <- res$hot_gauges
  message(
    "    gauges=", res$summary$total_gauges,
    " Danger=", res$summary$Danger,
    " Extreme=", res$summary$Extreme
  )
}

if (!length(summaries)) {
  stop(
    paste0(
      "No Flood Hub country data retrieved — aborting.\n",
      network_help_message()
    ),
    call. = FALSE
  )
}
country_df <- bind_rows(summaries)
prev_df <- load_previous_country_summary(OUT_DIR, run_date)
country_df <- add_day_over_day(country_df, prev_df)

gdacs_df <- tibble()
gdacs_hot_countries <- character()
if (DO_GDACS) {
  message("  GDACS: floods + tropical cyclones...")
  gdacs_result <- tryCatch({
    features <- fetch_gdacs_events()
    searo <- Filter(gdacs_touches_searo, features)
    out <- normalize_gdacs(searo)
    message("    SEARO events: ", nrow(out))
    if (nrow(out)) {
      readr::write_csv(out, file.path(day_dir, "gdacs_searo.csv"))
      hot <- out[out$alert_level %in% c("orange", "red"), , drop = FALSE]
      list(df = out, hot_countries = unique(na.omit(as.character(hot$country))))
    } else {
      list(df = out, hot_countries = character())
    }
  }, error = function(e) {
    message("    GDACS failed (continuing): ", conditionMessage(e))
    list(df = tibble(), hot_countries = character())
  })
  gdacs_df <- gdacs_result$df
  gdacs_hot_countries <- gdacs_result$hot_countries
}

flash_df <- tibble()
flash_hot_countries <- character()
if (DO_FLASH_FLOODS) {
  message("  Flood Hub: urban flash floods (next ~24h)...")
  flash_df <- tryCatch({
    events <- fetch_flash_floods(api_key, SEARO_ISO2)
    out <- normalize_flash_floods(events)
    message("    SEARO flash-flood country-rows: ", nrow(out))
    if (nrow(out)) {
      readr::write_csv(out, file.path(day_dir, "flash_floods_searo.csv"))
      readr::write_csv(
        summarize_flash_by_country(out),
        file.path(day_dir, "flash_floods_country_summary.csv")
      )
    }
    out
  }, error = function(e) {
    message("    Flash floods failed (continuing): ", conditionMessage(e))
    tibble()
  })
  if (nrow(flash_df)) {
    flash_hot_countries <- unique(as.character(flash_df$country))
  }
}

country_df$tier <- vapply(
  seq_len(nrow(country_df)),
  function(i) {
    assign_tier(country_df[i, , drop = FALSE], gdacs_hot_countries, flash_hot_countries)
  },
  character(1)
)

sig_events <- list()
if (DO_SIG_EVENTS) {
  message("  Flood Hub: significant events...")
  sig_events <- tryCatch({
    all_events <- fetch_significant_events(api_key)
    searo <- Filter(event_touches_searo, all_events)
    message("    SEARO significant events: ", length(searo))
    writeLines(
      jsonlite::toJSON(searo, auto_unbox = TRUE, pretty = TRUE),
      file.path(day_dir, "significant_events_searo.json")
    )
    searo
  }, error = function(e) {
    message("    Significant events failed (continuing): ", conditionMessage(e))
    list()
  })
}

admin1_df <- tibble()
if (!DO_ADMIN1) {
  message("  Admin-1 screening skipped (DO_ADMIN1 <- FALSE).")
} else {
  message("  Admin-1 spatial screen (Natural Earth)...")
  admin1_df <- tryCatch(
    admin1_hotspots_natural_earth(BOUNDARY_CACHE, country_df, raw_by_region),
    error = function(e) {
      message("    Admin-1 failed: ", conditionMessage(e))
      tibble()
    }
  )
  if (nrow(admin1_df)) {
    readr::write_csv(admin1_df, file.path(day_dir, "admin1_hotspots.csv"))
    message("    Admin-1 units with Danger/Extreme: ", nrow(admin1_df))
  } else {
    message("    No Admin-1 hotspots (or boundaries unavailable).")
  }
}

readr::write_csv(country_df, file.path(day_dir, "country_summary.csv"))
if (length(hot_rows)) {
  readr::write_csv(bind_rows(hot_rows), file.path(day_dir, "hot_gauges.csv"))
}

# Open-Meteo warning details
openmeteo_warning_df <- tibble::tibble()

if (DO_OPENMETEO) {
  
  message("  Open-Meteo: forecast warnings...")
  
  openmeteo_warning_df <- tryCatch({
    
    out <- openmeteo_warning_table(daily_forecast_df)
    
    message("    Countries with warnings: ", nrow(out))
    
    readr::write_csv(
      out,
      file.path(day_dir, "openmeteo_warnings.csv")
    )
    
    out
    
  }, error = function(e) {
    
    message(
      "    Open-Meteo warning table failed (continuing): ",
      conditionMessage(e)
    )
    
    tibble::tibble()
  })
}

brief <- build_brief(run_date, country_df, gdacs_df, sig_events, admin1_df, flash_df)
run_stamp <- stamp_local("%Y-%m-%d_%H%MIST")
brief_docx <- file.path(day_dir, paste0("daily_brief_", run_stamp, ".docx"))
write_brief_docx(
  path = brief_docx,
  run_date = run_date,
  country_df = country_df,
  gdacs_df = gdacs_df,
  sig_events = sig_events,
  admin1_df = admin1_df,
  flash_df = flash_df,
  weather_map = weather_map,
  daily_forecast_df = daily_forecast_df
)

message("\n", strrep("=", 60))
cat(brief, "\n")
message(strrep("=", 60))
message("\nSaved Word brief → ", normalizePath(brief_docx))
