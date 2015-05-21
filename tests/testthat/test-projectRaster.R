library(raster)
context("projectRaster")

set.seed(100)
r <- raster()
values(r) <- matrix(rnorm(nrow(r) * ncol(r)), nrow = nrow(r), ncol = ncol(r))

test_that("projectRaster gives consistent results", {

  projected <- projectRaster(crop(r, extent(-180, 180, -85, 85)),
    crs = CRS("+init=epsg:3857"))

  expect_identical(digest::digest(extent(projected), "sha1"), "dc1e03264f08cec4cff21438c059c9107afd995b")
  expect_identical(digest::digest(values(projected), "sha1"), "6d32e314e261932d2ef79e2102e166d7947cf5c9")
  expect_identical(digest::digest(projected, "sha1"), "224d0913b4557ec8479cd0a97941abddc10cd2c0")
})
