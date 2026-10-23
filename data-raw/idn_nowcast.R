# Builds data/idn_nowcast.rda from FRED CSV files downloaded on 2026-10-23.
# Each file was downloaded from https://fred.stlouisfed.org/graph/fredgraph.csv?id=<ID>.
read_fred <- function(id) {
  x <- utils::read.csv(file.path("data-raw", paste0(id, ".csv")))
  names(x) <- c("date", "value"); x$date <- as.Date(x$date); x$value <- suppressWarnings(as.numeric(x$value))
  x[!is.na(x$value), ]
}
m_ids <- c(ekspor = "XTEXVA01IDM667S", impor = "XTIMVA01IDM667S", cadev = "TRESEGIDM052N", saham = "SPASTT01IDM661N",
           kurs = "CCUSMA02IDM618N", ikk = "IDNCSCICP02STSAM", bunga = "IR3TIB01IDM156N", reer = "RBIDBIS",
           batubara = "PCOALAUUSDM", cpo = "PPOILUSDM", nikel = "PNICKUSDM", karet = "PRUBBUSDM",
           brent = "POILBREUSDM", ekspor_cn = "XTEXVA01CNM667S", ip_as = "INDPRO")
q_ids <- c(pdb = "NGDPRSAXDCIDQ", konsumsi = "NCPRSAXDCIDQ", pmtb = "NFIRSAXDCIDQ", ekspor_q = "NXRSAXDCIDQ",
           impor_q = "NMRSAXDCIDQ")
months <- seq(as.Date("2000-01-01"), as.Date("2026-09-01"), by = "month")
monthly <- data.frame(date = months)
for (k in names(m_ids)) { x <- read_fred(m_ids[[k]]); monthly[[k]] <- x$value[match(months, x$date)] }
quarters <- seq(as.Date("2000-01-01"), as.Date("2026-04-01"), by = "quarter")
quarterly <- data.frame(date = quarters)
for (k in names(q_ids)) { x <- read_fred(q_ids[[k]]); quarterly[[k]] <- x$value[match(quarters, x$date)] / 1e6 }
idn_nowcast <- nowcast_data(
  monthly, quarterly, target = "pdb", components = c("konsumsi", "pmtb", "ekspor_q", "impor_q"),
  transform = c(ikk = "level", bunga = "diff"),
  release_lag = c(ekspor = 15, impor = 15, cadev = 7, saham = 1, kurs = 1, ikk = 8, bunga = 1, reer = 15,
                  batubara = 3, cpo = 3, nikel = 3, karet = 3, brent = 3, ekspor_cn = 7, ip_as = 16),
  target_lag = 37,
  labels = c(ekspor = "Exports of goods (USD, SA)", impor = "Imports of goods (USD, SA)",
             cadev = "Reserves excluding gold", saham = "Share price index", kurs = "Rupiah per US dollar",
             ikk = "Consumer confidence (net balance)", bunga = "Interbank rate, 3 months",
             reer = "Real effective exchange rate (BIS)", batubara = "Coal price (Australia)",
             cpo = "Palm oil price", nikel = "Nickel price", karet = "Rubber price", brent = "Brent oil price",
             ekspor_cn = "Exports of China (SA)", ip_as = "US industrial production"),
  groups = c(ekspor = "trade", impor = "trade", cadev = "finance", saham = "finance", kurs = "finance",
             ikk = "survey", bunga = "finance", reer = "finance", batubara = "commodity", cpo = "commodity",
             nikel = "commodity", karet = "commodity", brent = "commodity", ekspor_cn = "global", ip_as = "global"))
attr(idn_nowcast, "fred_ids") <- c(m_ids, q_ids)
usethis::use_data(idn_nowcast, overwrite = TRUE, compress = "xz")
