Sys.setenv(LANGUAGE = "en") # Force locale

## Make conversion functions
BP_to_AD <- convert("BP", "AD")
AD_to_BP <- convert("AD", "BP")

## Convert years
expect_identical(BP_to_AD(0), 1950)
expect_identical(AD_to_BP(1950), 0)
