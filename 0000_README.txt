This file includes information about the scripts and data included in this repository.

=====================================================
DATA

To protect the privacy of individuals with police contact, we have opted to include data without individual identifiers in this repository.
Because we use latitude and longitude in the matching procedure, this means we provide post-match data.
However, we do include code that documents how we move from the raw, identifiable data, to the data that are provided.

Files provided:
1. hist_rolls.rds: This includes individual-level voter history information from the voter files.
2. matches_hills.rds: This includes the outcomes of the matching procedure for the main specification.
3. real_pre_match_hills_anon.rds: This includes the individual-level characteristics used for matching, with latitude, longitude, and other identifiers removed.
4. month_matches.rds: This includes the outcomes of the matching procedure for windows ranging from 1 to 23 months.
5. matches_hills_no_prior.rds: This includes the outcomes of the matching procedure for the match where prior turnout is excluded.

=====================================================
SCRIPTS. Should be run in numerical order.

00_initialize.R: This loads the packages and sets up the workspace. DATA NOT PROVIDED
01_process_raw_vf.R: This reads in the voter file data and geocodes it. DATA NOT PROVIDED
02_download_hills_stops.R: This reads the police stop data, cleans it, and merges it with the voter file data. Produces Table A1. DATA NOT PROVIDED
03_genmatch_weights.R: This constructs the weights for the matching excercise. DATA NOT PROVIDED
04_match.R: This matches the treated and control voters. Produces Table 1. DATA NOT PROVIDED
05_month_matches.R: This matches treated and control voters inside other windows ranging from 1--23 months around the election. DATA NOT PROVIDED
06_overall_regressions.R: This runs the primary regression models. Produces Tables 2, A2--A6, and Figures 2, A1. DATA PROVIDED
07_window_regressions.R: This runs the regressions for different windows around election day. Produces Tables A8--A11 and Figure 3. DATA PROVIDED
08_robust_no_prior_match/01_gen_no_turnout.R: This constructs the weights for the matching excercise, where prior turnout is excluded. DATA NOT PROVIDED
08_robust_no_prior_match/02_match_no_turnout.R: This matches the treated and control voters, without prior turnout. DATA NOT PROVIDED
08_robust_no_prior_match/03_reg_no_prior.R: This runs the regression for the setup where prior turnout is excluded from the match. DATA PROVIDED
09_rob_other/01_no_matching.R: This runs the regression for the models where no matching is done. DATA PROVIDED
09_rob_other/02_alt_reg_table.R: This creates Table A7. DATA PROVIDED
09_rob_other/03_plot: This creates Figure 1. DATA PROVIDED


A11