test_that("Read yaml configuration", {

  pop_map <- data.table::fread(test_path("testdata", "demographic_mapping_n24.csv"),
                               , colClasses = "character")
  pop_init <- data.table::fread(test_path("testdata", "population_init_n24.csv"))
  vac <- data.table::fread(test_path("testdata", "vaccination_n24.csv"))
  m_wd_d <- read.csv(test_path("testdata", "m_weekday_day_n24.csv"), header = F)
  m_wd_n <- read.csv(test_path("testdata", "m_weekday_night_n24.csv"), header = F)
  m_we_d <- read.csv(test_path("testdata", "m_weekend_day_n24.csv"), header = F)
  m_we_n <- read.csv(test_path("testdata", "m_weekend_night_n24.csv"), header = F)
  start_date <- "2023-09-04"
  sim_length <- 100L
  nsim <- 20L
  ts <- rep(0.7, 24)
  ts[1] <- 0.6
  tv <- rep(0.35, 24)
  tv[c(2, 5, 8, 11, 14, 17, 20, 23)] <- 0.7
  ve <- rep(0.5, 24)
  dv <- rep(50L, 24)
  dp <- rep(10L, 24)
  da <- rep(10L, 24)
  ds <- rep(2L, 24)
  dh <- rep(3L, 24)
  dr <- rep(50L, 24)
  pea <- rep(0.5, 24)
  psr <- rep(0.5, 24)
  phr <- rep(0.7, 24)
  de <- list()
  de[["dist"]] <- "lognormal"
  de[["mu"]] <- 1.9
  de[["sd"]] <- 1.23

  true_val <- list(pop_map = pop_map,
                   pop_init = pop_init,
                   vac = vac,
                   m_wd_d = m_wd_d,
                   m_wd_n = m_wd_n,
                   m_we_d = m_we_d,
                   m_we_n = m_we_n,
                   start_date = start_date,
                   sim_length = sim_length,
                   nsim = nsim,
                   ts = ts,
                   tv = tv,
                   ve = ve,
                   dv = dv,
                   de = de,
                   dp = dp,
                   da = da,
                   ds = ds,
                   dh = dh,
                   dr = dr,
                   pea = pea,
                   psr = psr,
                   phr = phr)

  val <- parse_config("testdata/sample_config.yaml")

  expect_identical(true_val, val)

})
