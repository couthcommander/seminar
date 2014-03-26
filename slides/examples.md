# The R Seminar
# You Wish You'd Had Ten Years Ago

[GitHub: R Seminar](https://github.com/couthcommander/seminar)

---

The objective of this presentation is to save you future time

## Code Efficiency
1. How long does it take to write?
2. How long does it take to modify?
3. How long does it take to run?

This talk is specifically tailored to the R programming language, but you could certainly extend the principles to other languages.

* It's a good thing if other people can read and understand your code
    * It's even better if you can understand your code when you revisit it three years later
* It's a good thing if your code runs start to finish without intervention
    * It's even better if you can specify at what point to start or finish
* It's a good thing if you can optimize code so that it runs in 1 minute instead of 10 minutes
    * It's even better if you didn't spend 4 hours (pre-optimization)

---

## Key Concepts
1. Don't repeat yourself - don't copy and paste
2. Use modular design - keep it simple or comment
3. Learn to vectorize and predefine the length of vectors

Inspiration for this talk came from examining an R script used for data validation.
This is fairly normal task, and it provided several examples of things worth thinking about when writing such a script.
The data sets provided were simulated (and probably aren't very realistic).

In a perfect world, this data would be accompanied by a data dictionary or meta-data that would perform the validation.

---

## Starting Point

Before validating your data, transform your data.
This may including adding, editing, or removing columns, as well as merging multiple data sets.
Most transformation operations can be vectorized.

Transformations should create intermediate data sets.

1. These can be saved in compressed formats
2. These provide checkpoints that can be utilized instead of re-running entire scripts

---

## Assign a new column

    !r
    x <- data.frame(id=seq(1000000))
    s <- sample(nrow(x))

    # dollar sign
    x$s1 <- s

    # single bracket with comma
    x[,'s2'] <- s

    # single bracket
    x['s3'] <- s

    # double bracket
    x[['s4']] <- s

    # assign default (NA), then value
    x$s <- NA
    x$s <- s

### read more

    !r
    ?Extract

---

## Convert TRUE/FALSE to 1/0

    !r
    x <- sample(TRUE:FALSE, 10, replace=TRUE)
    as.numeric(x)
    x+0
    x*1
    mode(x) <- 'numeric'

Some optimizations just don't matter.

It's good to know there's more than one way to do it.

Choose your preference weighing how easy it is to type or read -- then be consistent.

---

## Converting strings to dates

    !r
    # This violates **don't repeat yourself**
    dat$enroll.date <- as.Date(dat$enroll.date, format = '%Y-%m-%d')
    dat$surgery.date <- as.Date(dat$surgery.date, format = '%Y-%m-%d')
    dat$followup.date <- as.Date(dat$followup.date, format = '%Y-%m-%d')

    # name date columns
    date.vars <- c('enroll.date', 'surgery.date', 'followup.date')

    # Hmisc::Cs doesn't require quotes
    date.vars <- Cs(enroll.date, surgery.date, followup.date)

    # rely on good column names
    date.vars <- grep('date$', names(dat), value=TRUE)

    # loop over date variables
    for(i in date.vars) {
      dat[,i] <- as.Date(dat[,i], format = '%Y-%m-%d')
    }

    # lapply works
    dat[,date.vars] <- 
      lapply(dat[,date.vars], as.Date, format = '%Y-%m-%d')

---

## Converting strings to date-times
### Specify the timezone -- default relies on locale so results may vary on user/computer/location
### UTC will avoid daylight savings problems

    !r
    time.vars <- grep('time$', names(hosp), value=TRUE)
    for(i in time.vars) {
      hosp[,i] <-
        as.POSIXct(hosp[,i], format = '%Y-%m-%d %H:%M', tz = 'UTC')
    }

### Consider using function if called from multiple locations in code

    !r
    dtCT <- function(x, format = '%Y-%m-%d %H:%M', tz = 'UTC', ...) {
      as.POSIXct(x, format = format, tz = tz, ...)
    }
    hosp[,time.vars] <- lapply(hosp[,time.vars], dtCT)

### Check out [lubridate](https://github.com/hadley/lubridate)

---

## Too many tests
### When is an ifelse statement too long and what are alternatives?

    !r
    age.grp <- ifelse(dat$age < 46, 1,
      ifelse(dat$age < 51, 2,
        ifelse(dat$age < 56, 3,
          ifelse(dat$age < 61, 4, 5)
        )
      )
    )

    # solve arithmetically
    age.grp2 <- floor((dat$age-36)/5)
    # almost worked, convert 0s to 1s
    age.grp2[age.grp2 == 0] <- 1

    # specify break-points with cut - good for creating factors
    age.grp3 <- cut(dat$age, breaks = c(40, 46, 51, 56, 61, 66),
      labels = FALSE, right = FALSE
    )

    # just want integer result, use findInterval
    dat$age.grp <- findInterval(dat$age, c(46, 51, 56, 61)) + 1
    # or seq(46, 61, by=5)

---

## What about more complicated tests?

    !r
    r <- numeric(nrow(dat))
    for(i in seq_along(r)) {
      tot <- sum(dat[i, c('prev.surg', 'diabetes', 'obese')])
      e <- dat[i, 'exercise']
      # this is the worst
      if(e == 'None') {
        if(dat$prev.surg[i] && dat$diabetes[i] && dat$obese[i]) {
          r[i] <- 10
        } else if((!dat$prev.surg[i] && dat$diabetes[i] && dat$obese[i])
          || (dat$prev.surg[i] && !dat$diabetes[i] && dat$obese[i])
          || (dat$prev.surg[i] && dat$diabetes[i] && !dat$obese[i])) {
            r[i] <- 9
        } else if((!dat$prev.surg[i] && !dat$diabetes[i] && dat$obese[i])
          || (!dat$prev.surg[i] && dat$diabetes[i] && !dat$obese[i])
          || (dat$prev.surg[i] && !dat$diabetes[i] && !dat$obese[i])) {
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
        r[i] <- ifelse(tot == 3, 8,
          ifelse(tot == 2, 6,
            ifelse(tot == 1, 3, 1)
          )
        )
      # less intuitive, but less code
      } else if(e == 'Lots') {
        r[i] <- c(0,2,5,7)[match(tot, 0:3)]
      }
    }

---

## Vectorized solution is best

    !r
    tots <- rowSums(dat[, c('prev.surg', 'diabetes', 'obese')])
    dat$risk <- ifelse(dat$exercise == 'None',
      ifelse(tots == 3, 10,
        ifelse(tots == 2, 9,
          ifelse(tots == 1, 8, 5)
        )
      ),
      ifelse(dat$exercise == 'Some',
        ifelse(tots == 3, 9,
          ifelse(tots == 2, 7,
            ifelse(tots == 1, 5, 3)
          )
        ),
        ifelse(dat$exercise == 'Moderate', c(1,3,6,8)[match(tots, 0:3)],
          c(0,2,5,7)[match(tots, 0:3)]
        )
      )
    )

---

## Merging datasets
### Merge on single value

    !r
    alldat <- merge(dat, hosp)
    alldat <- cbind(dat[match(hosp$id, dat$id),],
      hosp[,-match('id', names(hosp))]
    )

### Merge on multiple values

    !r
    dat$key <- sample(LETTERS, nrow(dat), replace = TRUE)
    hosp$key <- sample(LETTERS, nrow(hosp), replace = TRUE)

    alldat <- merge(dat, hosp)
    dkey <- with(dat, paste(id, key, sep = '|'))
    hkey <- with(hosp, paste(id, key, sep = '|'))
    alldat <- cbind(dat[match(hkey, dkey),],
      hosp[,-match(c('id','key'), names(hosp))]
    )
    alldat <- alldat[!is.na(alldat$id),]

### A custom merge using match can be much faster

---

## Intermediate data set

### Convenient starting point and compressed

    !r
    # platform-independent path to working directory
    work.dir <- file.path('path','to','your','data')

    # save for later
    save(dat, hosp, alldat, file = file.path(work.dir, 'demo_data.RData'))

    # pick up where you left off
    load(file.path(work.dir, 'demo_data.RData'))

---

## Validation
* Modularity - write functions for different form validations
* Shorter functions are easier to maintain and test

### Goal
* Generate text file with errors by creating a vector of character strings

### Examine by ID or all IDs
* Does information in one row inform another?
* Determine tasks that can be vectorized

---

## How not to validate

    !r
    errors <- character()
    errors <- NULL
    for(i in seq(nrow(x))) {
      if(is.na(x$enroll.date[i]))
        errors <- c(errors, 'no enroll date')
      if(is.na(x$surgery.date[i]))
        errors <- c(errors, 'no surgery date')
      if(is.na(x$followup.date[i]))
        errors <- c(errors, 'no followup date')
      if(x$enroll.date[i] > x$surgery.date[i])
        errors <- c(errors, 'must be enrolled before surgery')
      if(x$surgery.date[i] > x$followup.date[i])
        errors <- c(errors, 'must have surgery before follow-up')
      if(x$height[i] < 48 || x$height[i] > 84)
        errors <- c(errors, 'height is too short or tall')
      if(x$on.insulin[i] && !x$diabetes[i])
        errors <- c(errors, 'on insulin, but not diabetic')
    }

---

## Pre-define errors

    !r
    errors <- character(7)
    # ?sprintf
    # use %s for variable place-holder
    errors[1] <- sprintf("no enroll date for rows [%s]",
      paste(which(is.na(x$enroll.date)), collapse=','))
    errors[2] <- sprintf("no surgery date for rows [%s]",
      paste(which(is.na(x$surgery.date)), collapse=','))
    errors[3] <- sprintf("no followup date for rows [%s]",
      paste(which(is.na(x$followup.date)), collapse=','))
    errors[4] <- sprintf("must be enrolled before surgery for rows [%s]",
      paste(which(x$enroll.date > x$surgery.date), collapse=','))
    errors[5] <- sprintf("surgery required before follow-up for rows [%s]",
      paste(which(x$surgery.date > x$followup.date), collapse=','))
    errors[6] <- sprintf("height is too short or tall for rows [%s]",
      paste(which(x$height < 48 | x$height > 84), collapse=','))
    errors[7] <- sprintf("on insulin, but not diabetic [%s]",
      paste(which(x$on.insulin & !x$diabetes), collapse=','))

---

## data.frame approach

    !r
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
    # DRY applies here
    errors$miss.enroll <- is.na(x$enroll.date)
    errors$miss.surgery <- is.na(x$surgery.date)
    errors$miss.followup <- is.na(x$followup.date)
    errors$late.enroll <- x$enroll.date > x$surgery.date
    errors$late.surgery <- x$surgery.date > x$followup.date
    errors$bad.height <- x$height < 48 | x$height > 84
    errors$bad.insulin <- x$on.insulin & !x$diabetes
    errorMsg <- lapply(names(which(colSums(errors, na.rm=TRUE) > 0)),
      FUN=function(i) {
        sprintf("%s for rows [%s]",
          errorList[i], paste(which(errors[,i]), collapse=',')
        )
      }
    )

---

## Some things must be done one ID at a time

    !r
    checkHospitalization <- function(x) {
      errorL <- c(
        bad.dose = 'time between doses is too short',
        bad.admit = 'first dose given too early',
        need.dose = 'dose required but missing',
        extra.dose = 'dose given but not required'
      )
      uids <- unique(x$id)
      # don't use subset
      z <- lapply(uids, FUN=function(i) {
        checkHospitalizationById(subset(x, id == i), errorL)
      })
      # predefine list size, easier to debug
      zz <- vector('list', length(uids))
      for(i in seq_along(uids)) {
        zz[[i]] <- checkHospitalizationById(x[x$id == uids[i],], errorL)
      }
      zzz <- do.call(rbind, zz)
      errorMsg <- lapply(colnames(zzz), FUN=function(i) {
        badix <- which(nchar(zzz[,i]) > 0)
        if(length(badix)) {
          sprintf("%s for id [%s] at rows [%s]",
            errorL[i], rownames(zzz)[badix], zzz[badix,i]
          )
        }
      })
      errorMsg
    }

---

## Putting it together

    !r
    dem.errors <- checkDemographics(dat)
    # when testing, work on a smaller subset
    hosp.errors <- checkHospitalization(alldat[1:1000,])
    all.errors <- unlist(c(dem.errors, hosp.errors))

    # write out errors
    write(all.errors, file = file.path(work.dir, 'validation_errors.txt'))

### Check out [data.table](http://datatable.r-forge.r-project.org/) and [dplyr](https://github.com/hadley/dplyr)
