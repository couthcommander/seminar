set.seed(35)

nr <- 10000
dat <- data.frame(id = seq(nr))

samp <- function(x, ...) sample(x, nr, replace=TRUE, ...)

yesno_vars <- c('prev.surg')
binary_vars <- c('tn.resident','diabetes','on.insulin','dose.req')

dat$age <- samp(40:65)
dat$height <- round(rnorm(nr, 66, 6))
dat$weight <- round(rnorm(nr, 180, 40))
dat$exercise <- samp(c('None', 'Some', 'Moderate', 'Lots'))

dat$enroll.date <- as.Date(samp(365), origin='2010-01-01')
dat$surgery.date <- as.Date(samp(365*2), origin='2011-01-01')
dat$followup.date <- as.Date(dat$surgery.date + round(rnorm(nr, 60, 10)))

for(i in yesno_vars) {
  dat[[i]] <- samp(c('Yes', 'No'))
}
for(i in binary_vars) {
  dat[[i]] <- samp(0:1)
}

write.csv(dat, file='demo_data.csv', row.names=FALSE)

hosp <- do.call(rbind, lapply(seq(nrow(dat)), FUN=function(x) {
  nr <- sample(1:4, 1)
  init <- as.numeric(dat[x,'surgery.date'])
  admit.time <- as.POSIXct(rnorm(nr, init-86400, 3600*8), tz='UTC', origin='2011-01-01')
  discharge.time <- as.POSIXct(rnorm(nr, init+86400*2, 3600*8), tz='UTC', origin='2011-01-01')
  dose1.time <- dose2.time <- NA
  if(dat[x,'dose.req']) {
    dose1.time <- rnorm(nr, init+86400, 3600*2)
    dose2.time <- rnorm(nr, dose1.time+43200, 3600*2)
    dose1.time <- as.POSIXct(dose1.time, tz='UTC', origin='2011-01-01')
    dose2.time <- as.POSIXct(dose2.time, tz='UTC', origin='2011-01-01')
  }
  data.frame(id=x, admit.time, dose1.time, dose2.time, discharge.time)
}))

write.csv(hosp, file='hosp_data.csv', row.names=FALSE)
