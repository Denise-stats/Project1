##ReadMe

An example of the initialized data:

        V2    V3   V4  control  drug
      97.7 105.8 109.1 104.20000 0.01
      88.7 100.4  97.9  95.66667 0.05
      60.6  64.5  71.2  65.43333 0.10
      21.7  25.3  28.0  25.00000 0.50
      36.7  43.7  45.4  41.93333 1.00
      40.7  40.6  45.8  42.36667 5.00



* solve_x_log.r     ----  input % control, and generate the corresponding concentration of log(drug).
* area_xy_log.r ---- input % control and the concentration of drug. generate the area, with log(drug).
* area2_x_log.r ---- input the concentration of drug. generate the area above the minimum value of y, with log(drug).
* main.r       ---- the main function.

**Note that `area2_x_log.r` is a function computing the area4. And the `d.area` is calculated via `area4-area2`.**

More details are contained in the corresponding files.

