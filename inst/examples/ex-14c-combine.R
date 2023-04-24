## Replicate Ward and Wilson (1978), p. 28
polach1972 <- data.frame(
  samples = c("ANU-7", "ANU-7", "ANU-7", "W-1571", "ANU-5",
              "C-800", "L-698D", "FSU-3", "Tx-44"),
  ages = c(14550, 15000, 13700, 14650, 11700, 10860, 11840, 11245, 10700),
  errors = c(270, 600, 300, 500, 260, 410, 100, 450, 210)
)

c14_combine(
  ages = polach1972$ages,
  errors = polach1972$errors,
  groups = polach1972$samples
)
