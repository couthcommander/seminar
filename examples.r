library(Hmisc)

# platform-independent file path
work.dir <- file.path('~', 'code', 'rcode', 'seminar')
setwd(work.dir)

# simulated datasets, forgive unrealistic values
dat <- read.csv(file.path(work.dir, 'demo_data.csv'), stringsAsFactors=FALSE)
hosp <- read.csv(file.path(work.dir, 'hosp_data.csv'), stringsAsFactors=FALSE)

## what's the best way to assign a new column?
x <- data.frame(id=seq(1000000))
s <- sample(nrow(x))
# dollar sign
x$s1 <- s
# single bracket with column
x[,'s2'] <- s
# single bracket
x['s3'] <- s
# double bracket
x[['s4']] <- s
# assign default (NA), then value
x$s <- NA
x$s <- s

## convert TRUE/FALSE to 1/0
x <- sample(TRUE:FALSE, 10, replace=TRUE)
as.numeric(x)
x+0
x*1
mode(x) <- 'numeric'

dat$prev.surg <- (dat$prev.surg == 'Yes')+0
dat$bmi <- 703 * dat$weight / dat$height^2
dat$obese <- (dat$bmi >= 30)+0

## converting strings to dates
# assign one at a time
dat$enroll.date <- as.Date(dat$enroll.date, format = '%Y-%m-%d')
dat$surgery.date <- as.Date(dat$surgery.date, format = '%Y-%m-%d')
dat$followup.date <- as.Date(dat$followup.date, format = '%Y-%m-%d')

# name date columns
date.vars <- c('enroll.date', 'surgery.date', 'followup.date')
# Hmisc::Cs doesn't require quotes
date.vars <- Cs(enroll.date, surgery.date, followup.date)
# rely on good column names
date.vars <- grep('date$', names(dat), value=TRUE)

fakedat <- dat[,date.vars]
# loop over date variables
for(i in date.vars) {
  fakedat[,i] <- as.Date(dat[,i], format = '%Y-%m-%d')
}
# cannot re-assign all at once
tryCatch(dat[,date.vars] <- as.Date(dat[,date.vars], format = '%Y-%m-%d'), error = function(e) e)
# lapply works
dat[,date.vars] <- lapply(dat[,date.vars], as.Date, format = '%Y-%m-%d')

## converting strings to date-times
# good idea to specify timezone - default relies on locale so results may vary on user/computer/location
# UTC will avoid DST problems
time.vars <- grep('time$', names(hosp), value=TRUE)
fakehosp <- hosp[,time.vars]
for(i in time.vars) {
  fakehosp[,i] <- as.POSIXct(fakehosp[,i], format = '%Y-%m-%d %H:%M', tz = 'UTC')
}

# consider using function if called from multiple locations in code
dtCT <- function(x, format = '%Y-%m-%d %H:%M', tz = 'UTC', ...) as.POSIXct(x, format = format, tz = tz, ...)
hosp[,time.vars] <- lapply(hosp[,time.vars], dtCT)

# check out library(lubridate)

## too many tests
# when is an ifelse statement too long?  what are the alternatives?
age.grp <- ifelse(dat$age < 46, 1, ifelse(dat$age < 51, 2, ifelse(dat$age < 56, 3, ifelse(dat$age < 61, 4, 5))))
# solve arithmetically
age.grp2 <- floor((dat$age-36)/5)
# almost worked, convert 0s to 1s
age.grp2[age.grp2 == 0] <- 1
# specify break-points with cut - good for creating factors
age.grp3 <- cut(dat$age, breaks = c(40, 46, 51, 56, 61, 66), labels = FALSE, right = FALSE)
# just want integer result, use findInterval
dat$age.grp <- findInterval(dat$age, c(46, 51, 56, 61)) + 1 # seq(46, 61, by=5)

# what about more complicated tests?
r <- numeric(nrow(dat))
for(i in seq_along(r)) {
  tot <- sum(dat[i, c('prev.surg', 'diabetes', 'obese')])
  e <- dat[i, 'exercise']
  # this is the worst
  if(e == 'None') {
    if(dat$prev.surg[i] && dat$diabetes[i] && dat$obese[i]) {
      r[i] <- 10
    } else if((!dat$prev.surg[i] && dat$diabetes[i] && dat$obese[i]) || (dat$prev.surg[i] && !dat$diabetes[i] && dat$obese[i]) || (dat$prev.surg[i] && dat$diabetes[i] && !dat$obese[i])) {
      r[i] <- 9
    } else if((!dat$prev.surg[i] && !dat$diabetes[i] && dat$obese[i]) || (!dat$prev.surg[i] && dat$diabetes[i] && !dat$obese[i]) || (dat$prev.surg[i] && !dat$diabetes[i] && !dat$obese[i])) {
      r[i] <- 8
    } else {
      r[i] <- 5
    }
  # better
  } else if(e == 'Some') {
    if(tot == 3) {
      r[i] <- 9
    } else if(tot == 2) {
      r[i] <- 7
    } else if(tot == 1) {
      r[i] <- 5
    } else {
      r[i] <- 3
    }
  # reasonable
  } else if(e == 'Moderate') {
    r[i] <- ifelse(tot == 3, 8, ifelse(tot == 2, 6, ifelse(tot == 1, 3, 1)))
  # less intuitive, but less code
  } else if(e == 'Lots') {
    r[i] <- c(0,2,5,7)[match(tot, 0:3)]
  }
}

# vectorized solution
tots <- rowSums(dat[, c('prev.surg', 'diabetes', 'obese')])
dat$risk <- ifelse(dat$exercise == 'None', ifelse(tots == 3, 10, ifelse(tots == 2, 9, ifelse(tots == 1, 8, 5))),
  ifelse(dat$exercise == 'Some', ifelse(tots == 3, 9, ifelse(tots == 2, 7, ifelse(tots == 1, 5, 3))),
    ifelse(dat$exercise == 'Moderate', c(1,3,6,8)[match(tots, 0:3)],
      c(0,2,5,7)[match(tots, 0:3)]
    )
  )
)

## merging datasets
# merge on single value
alldat <- merge(dat, hosp)
alldat <- cbind(dat[match(hosp$id, dat$id),], hosp[,-match('id', names(hosp))])
# merge works well

# merge on multiple values
dat$key <- sample(LETTERS, nrow(dat), replace = TRUE)
hosp$key <- sample(LETTERS, nrow(hosp), replace = TRUE)

alldat <- merge(dat, hosp)
dkey <- with(dat, paste(id, key, sep = '|'))
hkey <- with(hosp, paste(id, key, sep = '|'))
alldat <- cbind(dat[match(hkey, dkey),], hosp[,-match(c('id','key'), names(hosp))])
alldat <- alldat[!is.na(alldat$id),]
# custom merge using match is faster, though more complicated (write a function to generalize!)

## finish transformations, create intermediate dataset
# easier to pick up from here
# compressed storage
save(dat, hosp, alldat, file = file.path(work.dir, 'demo_data.RData'))
# load(file.path(work.dir, 'demo_data.RData'))

## validation
# write functions for different form validations -- modularity
# shorter functions are easier to maintain and test

checkDemographics <- function(x) {
  ## what should be vectorized?
  # the way to not do validation
  errors <- character()
  errors <- NULL
  for(i in seq(nrow(x))) {
    if(is.na(x$enroll.date[i])) errors <- c(errors, 'no enroll date')
    if(is.na(x$surgery.date[i])) errors <- c(errors, 'no surgery date')
    if(is.na(x$followup.date[i])) errors <- c(errors, 'no followup date')
    if(x$enroll.date[i] > x$surgery.date[i]) errors <- c(errors, 'must be enrolled before surgery')
    if(x$surgery.date[i] > x$followup.date[i])  errors <- c(errors, 'must have surgery before follow-up')
    if(x$height[i] < 48 || x$height[i] > 84) errors <- c(errors, 'height is too short or tall')
    if(x$on.insulin[i] && !x$diabetes[i]) errors <- c(errors, 'on insulin, but not diabetic')
  }
  # does information in one row inform another row?
  # it would be easy to check if there are no errors and adjust output
  errors <- character(7)
  # ?sprintf, use %s for variable place-holder
  errors[1] <- sprintf("no enroll date for rows [%s]", paste(which(is.na(x$enroll.date)), collapse=','))
  errors[2] <- sprintf("no surgery date for rows [%s]", paste(which(is.na(x$surgery.date)), collapse=','))
  errors[3] <- sprintf("no followup date for rows [%s]", paste(which(is.na(x$followup.date)), collapse=','))
  errors[4] <- sprintf("must be enrolled before surgery for rows [%s]", paste(which(x$enroll.date > x$surgery.date), collapse=','))
  errors[5] <- sprintf("must have surgery before follow-up for rows [%s]", paste(which(x$surgery.date > x$followup.date), collapse=','))
  errors[6] <- sprintf("height is too short or tall for rows [%s]", paste(which(x$height < 48 | x$height > 84), collapse=','))
  errors[7] <- sprintf("on insulin, but not diabetic [%s]", paste(which(x$on.insulin & !x$diabetes), collapse=','))
  # I prefer a data.frame approach - cleaner and easy to check
  errorList <- c(
    miss.enroll = 'no enroll date',
    miss.surgery = 'no surgery date',
    miss.followup = 'no followup date',
    late.enroll = 'must be enrolled before surgery',
    late.surgery = 'must have surgery before follow-up',
    bad.height = 'height is too short or tall',
    bad.insulin = 'on insulin, but not diabetic'
  )
  errors <- data.frame(matrix(NA, nrow=nrow(x), ncol=length(errorList)))
  names(errors) <- names(errorList)
  errors$miss.enroll <- is.na(x$enroll.date)
  errors$miss.surgery <- is.na(x$surgery.date)
  errors$miss.followup <- is.na(x$followup.date)
  # DRY applies here, especially if we checked more than 3 columns for missing
  errors$late.enroll <- x$enroll.date > x$surgery.date
  errors$late.surgery <- x$surgery.date > x$followup.date
  errors$bad.height <- x$height < 48 | x$height > 84
  errors$bad.insulin <- x$on.insulin & !x$diabetes
  errorMsg <- lapply(names(which(colSums(errors, na.rm=TRUE) > 0)), FUN=function(i) {
    sprintf("%s for rows [%s]", errorList[i], paste(which(errors[,i]), collapse=','))
  })
  errorMsg
}

checkHospitalizationById <- function(x, errorList) {
  uid <- unique(x$id)
  if(length(uid) > 1) stop("checkHospitalizationById shouldn't receive multiple ids")
  errors <- matrix("", nrow=1, ncol=length(errorList))
  dimnames(errors) <- list(uid, names(errorList))
  errors[,'bad.dose'] <- paste(which(as.numeric(x$dose2.time - x$dose1.time, unit = 'hours') < 8), collapse=',')
  errors[,'bad.admit'] <- paste(which(as.numeric(x$dose1.time - x$admit.time, unit = 'hours') < 2), collapse=',')
  errors[,'need.dose'] <- paste(which(x$dose.req & (is.na(x$dose1.time) | is.na(x$dose2.time))), collapse=',')
  errors[,'extra.dose'] <- paste(which(!x$dose.req & (!is.na(x$dose1.time) | !is.na(x$dose2.time))), collapse=',')
  errors
}

checkHospitalization <- function(x) {
  errorList <- c(
    bad.dose = 'time between doses is too short',
    bad.admit = 'first dose given too early',
    need.dose = 'dose required but missing',
    extra.dose = 'dose given but not required'
  )
  uids <- unique(x$id)
  # don't use subset
  z <- lapply(uids, FUN=function(i) checkHospitalizationById(subset(x, id == i), errorList))
  # predefine list size, easier to debug
  zz <- vector('list', length(uids))
  for(i in seq_along(uids)) {
    zz[[i]] <- checkHospitalizationById(x[x$id == uids[i],], errorList)
  }
  zzz <- do.call(rbind, zz)
  errorMsg <- lapply(colnames(zzz), FUN=function(i) {
    badix <- which(nchar(zzz[,i]) > 0)
    if(length(badix)) {
      sprintf("%s for id [%s] at rows [%s]", errorList[i], rownames(zzz)[badix], zzz[badix,i])
    }
  })
  errorMsg
}

dem.errors <- checkDemographics(dat)
hosp.errors <- checkHospitalization(alldat)
all.errors <- unlist(c(dem.errors, hosp.errors))
# write out errors
write(all.errors, file = file.path(work.dir, 'validation_errors.txt'))

## bonus code
page(hosp, "print")
# ?page

prettyprint <- function(x, labels) {
  x <- data.frame(x)
  sz <- ncol(x)
  if(!missing(labels)) names(x)[seq_along(labels)] <- labels
  size <- sapply(seq(sz), FUN=function(i) max(nchar(c(names(x)[i], as.character(x[[i]]))))) + 2
  newrow <- sprintf("+%s+", paste(sapply(size, FUN=function(i) paste(rep('-',i), collapse='')), collapse='+'))
  rowPr <- sapply(size, FUN=function(i) sprintf("%%%ss ", i-1))
  header <- sprintf("|%s|", paste(sapply(seq(sz), FUN=function(j) sprintf(rowPr[j], names(x)[j])), collapse='|'))
  content <- sprintf("|%s|", apply(x, MARGIN=1, FUN=function(i) paste(sapply(seq(sz), FUN=function(j) sprintf(rowPr[j], i[j])), collapse='|')))
  cat(paste(c(newrow, header, newrow, content, newrow), collapse='\n'), "\n")
}

q_tab <- function(vars,df) {
  for(idx in vars) {
    prettyprint(table(df[idx],exclude=NULL), idx)
  }
}

prettyprint(head(dat))
q_tab(c('age.grp','risk'), dat)
