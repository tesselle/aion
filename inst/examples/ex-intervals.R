## Create time intervals
int <- intervals(
  start = c(625, 700, 1200, 1225, 1250, 500, 1000, 1200,
            1325, 1375, 1200, 1300, 1375, 1275, 1325),
  end = c(750, 825, 1250, 1275, 1325, 700, 1300, 1325,
          1400, 1500, 1300, 1375, 1500, 1325, 1425),
  calendar = CE()
)

## Plot intervals
plot(int) # Default calendar
