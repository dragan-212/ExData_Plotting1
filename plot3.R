plot3 <- function(outdev = "screen") {
# 
# Program reads in Electric power consumption data set and plots a three-line graph
# Usage: Plot3() defauts to screen output only
#        Plot3("file") plots to screen and file, Plot1.png
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
    setcolorder(dt, c("DateTime", "Global_active_power", "Global_reactive_power", "Voltage", 
                "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

    # Ready to plot, check where first
    if (tolower(outdev) == "file") {
        # plot to file
        png(filename = "plot3.png", width = 480, height = 480, units = "px")
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
    plot(strptime(dt$DateTime, "%d/%m/%Y %H:%M:%S"), dt$Sub_metering_1, xlab = "", 
                ylab = "Energy sub metering", type = "n", mar = c(5,5,2,2))
    lines(strptime(dt$DateTime, "%d/%m/%Y %H:%M:%S"), dt$Sub_metering_1, col = "Black")
    lines(strptime(dt$DateTime, "%d/%m/%Y %H:%M:%S"), dt$Sub_metering_2, col = "Red")
    lines(strptime(dt$DateTime, "%d/%m/%Y %H:%M:%S"), dt$Sub_metering_3, col = "Blue")

    # construct headings for legend & place in top right corner
    clab1 <- names(dt)[6]
    clab2 <- names(dt)[7]
    clab3 <- names(dt)[8]
    legend('topright', c(clab1, clab2, clab3), col = c("black", "red", "blue"),
            lty=c(1,1,1), cex = 0.75)
}
