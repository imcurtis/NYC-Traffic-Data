#reading data
CollisionData<- read.csv("NYPD_Motor_Vehicle_Collisions (1).csv")

#Omitting blanks
FullData=na.omit(CollisionData)

#calling map
library(ggmap)
nyc <- get_map(location = 'new york city', zoom = 11)
nyc <- ggmap(nyc)

#Heatmap
library(MASS)
library(RgoogleMaps)
library(RColorBrewer)

#Keeping lat and lon data
rawdata <- data.frame(as.numeric(FullData$LONGITUDE), as.numeric(FullData$LATITUDE))
names(rawdata) <- c("lon", "lat")
data <- as.matrix(rawdata)

# Rotate the lat-lon coordinates
theta = pi/-2.0
m = matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), nrow=2)
data <- as.matrix(data) %*% m

#reproduce map
par(bg='black')
plot(data, cex=0.1, col="white", pch=16)

# Create heatmap with kde2d
k <- kde2d(FullData[,5], FullData[,6], n=100)

# Intensity going from green to red
cols <- rev(colorRampPalette(brewer.pal(8, 'RdYlGn'))(100))
par(bg='white')
image(k, col=cols, xaxt='n', yaxt='n')
points(FullData, cex=0.1, pch=16)

# Mapping via RgoogleMaps
# Find map center and get map
center <- rev(sapply(rawdata, mean))
map <- GetMap(center=center, zoom=11)

# Translate original data
coords <- LatLon2XY.centered(map, rawdata$lat, rawdata$lon, 11)
coords <- data.frame(coords)

# Rerun heatmap
k2 <- kde2d(coords$newX, coords$newY, n=100)

# Create exponential transparency vector and add
alpha <- seq.int(0.5, 0.95, length.out=100)
alpha <- exp(alpha^6-1)
cols2 <- AddAlpha(cols, alpha)

#Plot
PlotOnStaticMap(map)
image(k2, col=cols2, add=T)
points(coords$newX, coords$newY, pch=16, cex=0.01)
