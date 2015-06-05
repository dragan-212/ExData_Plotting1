plot1 <- function(outdev = "screen") {
# 
# Program reads in Electric power consumption data set and plots a histogram
# Usage: Plot1() defauts to screen output only
#        Plot("file") plots to screen and file, Plot1.png
#

    # Get two days worth of data from 2007-02-01 to 2007-02-02 
    firstline <- 66638
    maxrows <- 2880

    inputfile <- "household_power_consumption.txt"

    # initialize df for read.table()
    df <- data.frame()
    # capture header here
    h <- data.frame()   

    h <- read.table(inputfile, sep = ";" , nrows = 1, na.strings = "?", 
                    stringsAsFactors = FALSE)
    df <- read.table(inputfile, sep = ";" , skip = (firstline -1), nrows = maxrows, 
                    na.strings = "?", stringsAsFactors = FALSE)

    # name headers using data captured in h
    colnames(df) <- unlist(h)

    # change type to data.table  
    dt <- data.table(df)

    # Concatenate Date and Time into one variable & delete the original Date & Time columns
    dt <- dt[, DateTime:=paste(Date, Time)] 
    dt <- dt[, c("Date", "Time") := NULL]

    # Now re-order, adding DateTime to first column
    setcolorder(dt, c("DateTime", "Global_active_power", "Global_reactive_power","Voltage",
                "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

    # Ready to plot, check where first
    if (tolower(outdev) == "file") {
        # plot to file
        png(filename = "plot1.png", width = 480, height = 480, units = "px")
        do_plot()
        dev.off()
    }
    else {
        # plot to screen
        do_plot()
    }
}  

# function that does the plotting
do_plot <- function() {
    hist(dt$Global_active_power, col = "Red", main = "Global Active Power",
        xlab = "Global Active Power(kilowatts)")
}
