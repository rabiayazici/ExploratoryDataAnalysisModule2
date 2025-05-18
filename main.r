# Read the data
NEI <- readRDS("summarySCC_PM25.rds")

# Aggregate data by year
total_emissions <- aggregate(Emissions ~ year, data = NEI, sum)

# Plot
png("plot1.png")
plot(total_emissions$year, total_emissions$Emissions, type = "o", 
     col = "blue", xlab = "Year", ylab = "Total Emissions (tons)", 
     main = "Total PM2.5 Emissions in the US (1999-2008)")
dev.off()
# Subset data for Baltimore City
baltimore_data <- subset(NEI, fips == "24510")

# Aggregate data by year
baltimore_emissions <- aggregate(Emissions ~ year, data = baltimore_data, sum)

# Plot
png("plot2.png")
plot(baltimore_emissions$year, baltimore_emissions$Emissions, type = "o", 
     col = "red", xlab = "Year", ylab = "Total Emissions (tons)", 
     main = "Total PM2.5 Emissions in Baltimore City (1999-2008)")
dev.off()
library(ggplot2)

# Aggregate emissions by year and type
baltimore_type_emissions <- aggregate(Emissions ~ year + type, data = baltimore_data, sum)

# Plot
png("plot3.png")
ggplot(baltimore_type_emissions, aes(x = year, y = Emissions, color = type)) +
  geom_line() +
  labs(x = "Year", y = "Emissions (tons)", title = "PM2.5 Emissions by Source Type in Baltimore City") +
  theme_minimal()
dev.off()

library(ggplot2)

# Aggregate emissions by year and type
baltimore_type_emissions <- aggregate(Emissions ~ year + type, data = baltimore_data, sum)

# Plot
png("plot3.png")
ggplot(baltimore_type_emissions, aes(x = year, y = Emissions, color = type)) +
  geom_line() +
  labs(x = "Year", y = "Emissions (tons)", title = "PM2.5 Emissions by Source Type in Baltimore City") +
  theme_minimal()
dev.off()
# Read the SCC data
SCC <- readRDS("Source_Classification_Code.rds")

# Filter coal combustion sources from SCC
coal_sources <- subset(SCC, grepl("coal", SCC$EI.Sector, ignore.case = TRUE))

# Merge with NEI data to get coal-related emissions
coal_data <- merge(NEI, coal_sources, by = "SCC")

# Aggregate emissions by year
coal_emissions <- aggregate(Emissions ~ year, data = coal_data, sum)

# Plot
png("plot4.png")
plot(coal_emissions$year, coal_emissions$Emissions, type = "o", 
     col = "green", xlab = "Year", ylab = "Emissions (tons)", 
     main = "Coal Combustion PM2.5 Emissions (1999-2008)")
dev.off()
# Filter motor vehicle sources
vehicle_sources <- subset(SCC, grepl("motor", SCC$EI.Sector, ignore.case = TRUE))

# Merge with NEI data for vehicle-related emissions
vehicle_data <- merge(baltimore_data, vehicle_sources, by = "SCC")

# Aggregate emissions by year
vehicle_emissions <- aggregate(Emissions ~ year, data = vehicle_data, sum)

# Plot
png("plot5.png")
plot(vehicle_emissions$year, vehicle_emissions$Emissions, type = "o", 
     col = "purple", xlab = "Year", ylab = "Emissions (tons)", 
     main = "Motor Vehicle PM2.5 Emissions in Baltimore City (1999-2008)")
dev.off()
# Subset data for Los Angeles County
la_data <- subset(NEI, fips == "06037")

# Merge both cities' data with motor vehicle sources
vehicle_data_baltimore <- merge(baltimore_data, vehicle_sources, by = "SCC")
vehicle_data_la <- merge(la_data, vehicle_sources, by = "SCC")

# Combine both datasets
combined_vehicle_data <- rbind(vehicle_data_baltimore, vehicle_data_la)

# Aggregate emissions by year and fips
combined_vehicle_emissions <- aggregate(Emissions ~ year + fips, data = combined_vehicle_data, sum)

# Plot comparison
png("plot6.png")
ggplot(combined_vehicle_emissions, aes(x = year, y = Emissions, color = fips)) +
  geom_line() +
  labs(x = "Year", y = "Emissions (tons)", title = "Motor Vehicle Emissions: Baltimore vs. Los Angeles") +
  scale_color_manual(values = c("red", "blue"), labels = c("Baltimore", "Los Angeles")) +
  theme_minimal()
dev.off()
