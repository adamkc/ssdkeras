img_address <- "greenhousetest3.jpg"
image_matrices <- list(
  image_array_resize(image_load(img_address), img_height, img_width)
)
X = array (
  data = do.call(rbind, map(image_matrices, as.vector)),
  dim = c(length(image_matrices), dim(image_matrices[[1]]))
)
y_pred <- model$predict(X)

y_pred_decoded = decode_y2(y_pred,
                           confidence_thresh = 0.00001,
                           iou_threshold = 0.15,
                           top_k = 5L,
                           input_coords = 'centroids',
                           normalize_coords = FALSE,
                           img_height = NULL,
                           img_width = NULL,
                           n_classes = n_classes,
                           oneOfEach = FALSE)

  img <- magick::image_read(img_address) %>%
    magick::image_scale("250x250!")

plot.new()
rasterImage(img, 0, 0, 1, 1)

if (dim(y_pred_decoded[[1]])[1] > 0) {
  predBoxes <- cbind(y_pred_decoded[[1]][, 3:4, drop = FALSE] / img_width, 1 - y_pred_decoded[[1]][, 5:6, drop = FALSE] / img_height)
  rect(xleft = predBoxes[, 1], xright = predBoxes[, 2], ybottom = predBoxes[, 3], ytop = predBoxes[, 4], border = "red")
  captions <- str_c(classes[y_pred_decoded[[1]][, 1]], " ", format(round(y_pred_decoded[[1]][, 2], 2), nsmall = 2))
  text(x = predBoxes[, 1], y = predBoxes[, 3], labels = captions, adj = c(-0.1, -0.3), col = "white", cex = 1.0)
}
