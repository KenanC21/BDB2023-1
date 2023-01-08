# Good Rush, Bad Blocking: A Spatial Analysis of the Trenches
## By [Kenan Clarke](https://twitter.com/kdclarke_21) and [Michael Egle](https://twitter.com/deceptivespeed_)

This is the repo for our Big Data Bowl project, everything is fully reproducible with the data provided by the NFL. You can run some of create_week_blocking_assignments.py and create_one_week_field_control.R files but they take a bit of time and the results are already pre-compiled in the data folder.


## File Structure

- `blocking_assignments.py`: Creates the class structure that is used in `create_week_blocking_assignments.py`

- `create_week_blocking_assignments.py`: Runs through a week of tracking data and generates the blocking assignment for each player at each frame on a play

- `field_control.R`: Functions for calculating field control for players. Called by `create_one_week_field_control.R`

- `create_one_week_field_control.R`: Gets field control numbers for every player in a week based on tracking data.

- `data_aggregation.R`: Aggregates the blocking and field control data to compile results at the individual player level

- `eda.R`: Generic EDA for the project, not necessary to reproduce the project but possibly helpful for learning more

- `influence_time_throw_viz.R`: Creates the visualization for the pass result vs offense field ownership at point of throw plot

- `viz.R`: All remaining visualizations are created in here


## Contact

For any questions regarding this project, please feel free to reach out to Michael Egle or Kenan Clarke on Twitter (linked above)
