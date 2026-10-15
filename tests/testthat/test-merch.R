test_that("taper: tip is zero, DBH reproduced at breast height, regions resolve", {
  expect_equal(kozak_taper(14, 75, 75, "RS"), 0)
  expect_equal(kozak_taper(35, 20, 1.37, "SM", units = "metric"), 35)
  expect_true(all(diff(kozak_taper(14, 75, seq(5, 70, 5), "RS")) < 0))
  expect_equal(kozak_taper(14, 75, 30, "RS", region = "GL"), kozak_taper(14, 75, 30, "RS"))
  expect_gt(suppressMessages(kozak_taper(20, 120, 40, "DF", region = "PNW")), 0)
})

test_that("legacy GreenTimbMerch interfaces keep their published values", {
  # values from the original GreenTimbMerch functions
  expect_equal(KozakTreeVol("ob", "WP", 32.5, 21), 0.886728, tolerance = 1e-5)
  expect_equal(KozakTreeVol("ib", "RO", 40, 18, stump = .5, topD = 25.3), 0.683547, tolerance = 1e-5)
  expect_equal(KozakTaper("ib", "RS", 8, 28, 16, TRUE), 15.8908, tolerance = 1e-4)
  a <- Merchandize.ASG(1, 1, 1, "RS", 30, 14, "ASG", FALSE)
  expect_equal(a$Saw.BF.ASG, 78.268, tolerance = 1e-3)
  expect_equal(MerchHT(1, 1, 1, "SM", 52, 14, 2, 7)$Saw.BF.MH, 138.6174, tolerance = 1e-3)
})

test_that("operator-precedence bug is fixed: NC outside bark uses the ob fit", {
  expect_false(isTRUE(all.equal(KozakTaper("ob", "NC", 5, 30, 18), KozakTaper("ib", "NC", 5, 30, 18))))
})

test_that("merchandize_trees returns per-acre products", {
  tl <- data.frame(stand = "A", fvs = c("RS", "SM", "WP"), dbh = c(12, 16, 20),
                   ht = c(60, 70, 85), tpa = c(40, 20, 10))
  m <- merchandize_trees(tl)
  expect_equal(nrow(m$trees), 3)
  expect_true(all(m$trees$total_cuft >= m$trees$merch_cuft))
  expect_gt(m$per_acre$saw_bf, 0)
})

test_that("Kozak_merchR: PNW default unchanged in shape, NE uses species taper, no negative Scribner", {
  x <- Kozak_merchR(data.frame(dbh = c(14.1, 18), tht = c(58, 80)))
  expect_true(all(x[, c("junk", "pulp", "cns", "ssl", "lsl", "psl")] >= 0))
  y <- Kozak_merchR(data.frame(dbh = 14, tht = 70, spp = "RS"), region = "NE")
  expect_gt(y$vol, 0)
  expect_error(Kozak_merchR(data.frame(dbh = 14, tht = 70), region = "NE"))
})
