#Plot 1
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Calculate sum of emissions grouped by year
total_emissions_by_year <- sapply(split(NEI$Emissions, NEI$year), sum)
observed_years <- c(1999, 2002, 2005, 2008)

png("plot1.png", width=720, height=480)
# Basic plotting with y-axis adjusted into range [0, value-large-enough]
plot(observed_years, total_emissions_by_year, bty="n",
     pch=20, main="Total PM2.5 Emissions by Year",
     xlab="Year", ylab="Total PM2.5 Emissions",
     yaxs="i", ylim=c(0, max(total_emissions_by_year) * 1.1),
     xaxt="n")
# Adjust x-axis to observed years
axis(1, at=observed_years)
# add linear fit for trend prediction
linear_fit <- lm(total_emissions_by_year ~ observed_years)
abline(linear_fit$coefficients[1], linear_fit$coefficients[2], lty="dashed", col="grey")
dev.off()


#Plot 2
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Subset NEI on Baltimore City, calculate sum of emissions grouped by year
baltimore_subset <- NEI[NEI$fips == "24510",]
baltimore_emissions_by_year <- sapply(split(baltimore_subset$Emissions, baltimore_subset$year), sum)
observed_years <- c(1999, 2002, 2005, 2008)

png("plot2.png", width=720, height=480)
# Basic plotting with y-axis adjusted into range [0, value-large-enough]
plot(observed_years, baltimore_emissions_by_year, bty="n",
     pch=20, main="Baltimore City PM2.5 Emissions by Year",
     xlab="Year", ylab="Baltimore City PM2.5 Emissions",
     yaxs="i", ylim=c(0, max(baltimore_emissions_by_year) * 1.1),
     xaxt="n")
# Adjust x-axis to observed years
axis(1, at=observed_years)
# add linear fit for trend prediction
linear_fit <- lm(baltimore_emissions_by_year ~ observed_years)
abline(linear_fit$coefficients[1], linear_fit$coefficients[2], lty="dashed", col="grey")
dev.off()


#Plot 3
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Subset NEI on Baltimore City, calculate sum of emissions grouped by year
# and type.
baltimore_subset <- NEI[NEI$fips == "24510",]
baltimore_emissions_by_year_and_type <- aggregate(Emissions ~ type + year,
                                                 data=baltimore_subset, FUN=sum)

# Use ggplot2 and grid library for plotting and margin adjusting.
library(ggplot2)
library(grid)
png("plot3.png", width=720, height=480)
g <- ggplot(baltimore_emissions_by_year_and_type, aes(year, Emissions))
g + geom_point(aes(color=type)) +
    facet_grid(.~type) +
    geom_smooth(method="lm", se=FALSE, lty="dashed") +
    labs(title="Baltimore City Total PM2.5 Emissions by Type and Year") +
    theme_bw() +
    theme(legend.position="none", panel.margin=unit(.4, "cm")) +
    labs(y="Total PM2.5 Emissions") +
    scale_x_continuous("Year", breaks=c(1999, 2002, 2005, 2008))
dev.off()


#Plot 4
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Subset NEI on coal combustion-related, calculate sum of emissions grouped by year
SCC_coal <- SCC[grepl("Coal$", SCC$SCC.Level.Three), "SCC"]
coal_subset <- NEI[NEI$SCC %in% SCC_coal,]
coal_emissions_by_year <- sapply(split(coal_subset$Emissions, coal_subset$year), sum)
observed_years <- c(1999, 2002, 2005, 2008)

png("plot4.png", width=720, height=480)
# Basic plotting with y-axis adjusted into range [0, value-large-enough]
plot(observed_years, coal_emissions_by_year, bty="n",
     pch=20, main="Coal Combustion-Related PM2.5 Emissions by Year",
     xlab="Year", ylab="Coal Combustion-related PM2.5 Emissions",
     yaxs="i", ylim=c(0, max(coal_emissions_by_year) * 1.1),
     xaxt="n")
points(observed_years, coal_emissions_by_year, type="l")
# Adjust x-axis to observed years
axis(1, at=observed_years)
# add linear fit for trend prediction
linear_fit <- lm(coal_emissions_by_year ~ observed_years)
abline(linear_fit$coefficients[1], linear_fit$coefficients[2], lty="dashed", col="grey")
dev.off()


#Plot 5
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Subset NEI on motor vehicles and Baltimore, calculate sum of emissions grouped by year
SCC_motor <- SCC[SCC$SCC.Level.One=="Mobile Sources", "SCC"]
motor_subset <- NEI[NEI$fips == "24510" & NEI$SCC %in% SCC_motor,]
motor_emissions_by_year <- sapply(split(motor_subset$Emissions, motor_subset$year), sum)
observed_years <- c(1999, 2002, 2005, 2008)

png("plot5.png", width=720, height=480)
# Basic plotting with y-axis adjusted into range [0, value-large-enough]
plot(observed_years, motor_emissions_by_year, bty="n",
     pch=20, main="Baltimore Motor Vehicles PM2.5 Emissions by Year",
     xlab="Year", ylab="Baltimore Motor Vehicles PM2.5 Emissions",
     yaxs="i", ylim=c(0, max(motor_emissions_by_year) * 1.1),
     xaxt="n")
points(observed_years, motor_emissions_by_year, type="l")
# Adjust x-axis to observed years
axis(1, at=observed_years)
# add linear fit for trend prediction
linear_fit <- lm(motor_emissions_by_year ~ observed_years)
abline(linear_fit$coefficients[1], linear_fit$coefficients[2], lty="dashed", col="grey")
dev.off()


#Plot 6
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Subset NEI on motor vehicles and, calculate sum of emissions grouped by year
SCC_motor <- SCC[SCC$SCC.Level.One=="Mobile Sources", "SCC"]
motor_subset <- NEI[NEI$fips %in% c("06037", "24510") & NEI$SCC %in% SCC_motor,]
motor_emissions_by_year_and_city <- aggregate(Emissions ~ fips + year,
                                             data=motor_subset, FUN=sum)
# Use ggplot2 library for plotting
library(ggplot2)
png("plot6.png", width=720, height=480)
g <- ggplot(motor_emissions_by_year_and_city, aes(year, Emissions))
g + geom_line(aes(color=fips)) +
    geom_point(aes(color=fips)) +
    labs(title="Motor Vehicles Total PM2.5 Emissions Comparison by Year") +
    theme_bw() +
    labs(y="Motor Vehicles PM2.5 Emissions") +
    scale_x_continuous("Year", breaks=c(1999, 2002, 2005, 2008))
dev.off()