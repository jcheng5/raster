library(raster)
context("projectRaster")

set.seed(100)
r <- raster()
values(r) <- matrix(rnorm(nrow(r) * ncol(r)), nrow = nrow(r), ncol = ncol(r))

# smallRaster <- raster(nrows = 3, ncols = 3, xmn = 0, xmx = 1, ymn = 0, ymx = 1)
# values(smallRaster) <- t(matrix(1:9, 3, 3))

test_that("projectRaster gives consistent results", {

  projected <- projectRaster(crop(r, extent(-180, 180, -85, 85)),
    crs = CRS("+init=epsg:3857"))

  expect_identical(digest::digest(extent(projected), "sha1"), "dc1e03264f08cec4cff21438c059c9107afd995b")
  expect_identical(digest::digest(values(projected), "sha1"), "6d32e314e261932d2ef79e2102e166d7947cf5c9")
})

test_that("fourCellsFromXY gives consistent results", {
	expect_identical(fourCellsFromXY(raster(), cbind(-181, 0)), matrix(NA_integer_, 1, 4))
	expect_identical(fourCellsFromXY(raster(), cbind(0, 91)), matrix(NA_integer_, 1, 4))
	expect_identical(fourCellsFromXY(raster(), cbind(181, 0)), matrix(NA_integer_, 1, 4))
	expect_identical(fourCellsFromXY(raster(), cbind(0, -91)), matrix(NA_integer_, 1, 4))
	
	expect_identical(
		fourCellsFromXY(raster(), cbind(0.5, 0.5), duplicates = FALSE),
		matrix(c(32221L, 32581L, 32582L, 32222L), 1, 4)
	)
	
	expect_identical(
		fourCellsFromXY(raster(), cbind(0.5, 0.5), duplicates = TRUE),
		matrix(32221L, 1, 4)
	)
})