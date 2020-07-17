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

allK <- c(2, 3, 4, 5, 6, 7, 8, 9)

for (k in allK) {
  kMeans <- kmeans(imgRGB[, c("R", "G", "B")], centers = k)
  kColours <- rgb(kMeans$centers[kMeans$cluster,])
  ggplot(data = imgRGB, aes(x = x, y = y)) + 
    geom_point(colour = kColours) +
    labs(title = paste("k-Means Clustering of", k, "Colors")) 
    plotTheme()
}