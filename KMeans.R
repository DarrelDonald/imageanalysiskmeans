library(jpeg)
url <- "https://www.seriouseats.com/recipes/images/2012/02/20120227-tonkotsu-ramen-broth-pork-fat-26.jpg"

downloadedFile <- download.file(url, "Ramen.jpg")
img <- readJPEG("Ramen.jpg")

imgDm <- dim(img)

imgRGB <- data.frame(
  x = rep(1:imgDm[2], each = imgDm[1]),
  y = rep(imgDm[1]:1, imgDm[2]),
  R = as.vector(img[,,1]),
  G = as.vector(img[,,2]),
  B = as.vector(img[,,3])
)

library(ggplot2)

plotTheme <- function() {
  theme(
    plot.title = element_text(
      size = 20,
      face = "bold",
      vjust = 1.5)
  )
}

ggplot(data = imgRGB, aes(x = x, y = y)) + 
  geom_point(colour = rgb(imgRGB[c("R", "G", "B")])) +
  labs(title = "Original Image: Ramen") +
  plotTheme()

kMeans <- kmeans(imgRGB[, c("R", "G", "B")], centers = 2)
kColours <- rgb(kMeans$centers[kMeans$cluster,])
ggplot(data = imgRGB, aes(x = x, y = y)) + 
  geom_point(colour = kColours) +
  labs(title = paste("k-Means Clustering of", 2, "Colors")) 
plotTheme()

kMeans <- kmeans(imgRGB[, c("R", "G", "B")], centers = 3)
kColours <- rgb(kMeans$centers[kMeans$cluster,])
ggplot(data = imgRGB, aes(x = x, y = y)) + 
  geom_point(colour = kColours) +
  labs(title = paste("k-Means Clustering of", 3, "Colors")) 
plotTheme()

kMeans <- kmeans(imgRGB[, c("R", "G", "B")], centers = 4)
kColours <- rgb(kMeans$centers[kMeans$cluster,])
ggplot(data = imgRGB, aes(x = x, y = y)) + 
  geom_point(colour = kColours) +
  labs(title = paste("k-Means Clustering of", 4, "Colors")) 
plotTheme()

kMeans <- kmeans(imgRGB[, c("R", "G", "B")], centers = 5)
kColours <- rgb(kMeans$centers[kMeans$cluster,])
ggplot(data = imgRGB, aes(x = x, y = y)) + 
  geom_point(colour = kColours) +
  labs(title = paste("k-Means Clustering of", 5, "Colors")) 
plotTheme()

kMeans <- kmeans(imgRGB[, c("R", "G", "B")], centers = 6)
kColours <- rgb(kMeans$centers[kMeans$cluster,])
ggplot(data = imgRGB, aes(x = x, y = y)) + 
  geom_point(colour = kColours) +
  labs(title = paste("k-Means Clustering of", 6, "Colors")) 
plotTheme()