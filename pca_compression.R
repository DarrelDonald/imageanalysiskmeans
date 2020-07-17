library(jpeg)
url <- "https://www.seriouseats.com/recipes/images/2012/02/20120227-tonkotsu-ramen-broth-pork-fat-26.jpg"

downloadedFile <- download.file(url, "Ramen.jpg", mode = 'wb')
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

r <- img[,,1]
g <- img[,,2]
b <- img[,,3]

img.r.pca <- prcomp(r, center = FALSE)
img.g.pca <- prcomp(g, center = FALSE)
img.b.pca <- prcomp(b, center = FALSE)

summary(img.r.pca)
summary(img.g.pca)
summary(img.b.pca)

rgb.pca <- list(img.r.pca, img.g.pca, img.b.pca)

for (i in seq.int(3, round(nrow(img) - 10), length.out = 5)) {
  pca.img <- sapply(rgb.pca, function(j) {
    compressed.img <- j$x[,1:i] %*% t(j$rotation[,1:i])
  }, simplify = 'array')
  writeJPEG(pca.img, paste('compressed/pca', round(i,0), '.jpg', sep = ''))
}