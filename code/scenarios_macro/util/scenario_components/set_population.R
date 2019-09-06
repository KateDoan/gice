## Read the data in
singpop2017 <- read.csv(file.path(datDir, "singpop_2017.csv"))
expLifeYears <- (read.csv(file.path(datDir, "life_expectancy_never_smoker.csv")))[,-1]
deathRateN <- (read.csv(file.path(datDir, "death_rate_never_smoker.csv")))[,-1]
expLifeYears <- expLifeYears[1:numAge,1:numYear]
deathRateN <- deathRateN[1:numAge,1:numYear]

totalPop2017 <- singpop2017$total_pop_2017[1:numAge]
datPropNArray <- readRDS(file.path(smallMatDir, "propNArray.rds"))
datPropN2017 <- c(1, rep(0.985,6), unlist(unname(datPropNArray[1:(numAge-7),9])))
datNumN2017 <- totalPop2017 * datPropN2017

datPropCArray <- readRDS(file.path(smallMatDir, "propCArray.rds"))
datPropC2017 <- c(0, rep(0.01,6), unlist(unname(datPropCArray[1:(numAge-7),9])))
datNumC2017 <- totalPop2017 * datPropC2017

datPropQArray <- readRDS(file.path(smallMatDir, "propQArray.rds"))
datPropQ2017 <- c(0, rep(0.005,6),unlist(unname(datPropQArray[1:(numAge-7),9])))
datNumQ2017 <- totalPop2017 * datPropQ2017

# A vector to represent the number of persons in the youngest age over the years
# Population growth factor
fpop <- readRDS(file.path(fpopDir, "fpop.rds"))
numStartAge_2017 <- singpop2017$total_pop_2017[1]
numStartAges <- numStartAge_2017 * c(1, fpop)