## Define time scales
BP <- era("BP")
AD <- era("AD")

## Make conversion functions
BP_to_AD <- convert(BP, AD)
AD_to_BP <- convert(AD, BP)

## Convert years
BP_to_AD(0)
AD_to_BP(1950)
