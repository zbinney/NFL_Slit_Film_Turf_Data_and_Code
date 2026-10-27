# NFL Slit Film Turf Analysis Data and Code
Data and code for the paper "Association of Slit-Film and Other Artificial Turf with Lower Extremity Injuries Among National Football League Players, 2018-23"

The main analysis code file is `Turf_Manuscript_0625.R`.

The main data file is `model_data_final.csv`. It contains data on the surface for each NFL regular season game from 2018-23 and a count of lower extremity injuries in each game, among other variables. A data dictionary is forthcoming, but the analysis code file will give you an idea of what certain variables mean.

The raw injury data used to populate the `n_injs` and `n_injs_footankle` columns in `model_data_final.csv` is commercially protected. For access consideration, please email Dr. Binney at zbinney at emory dot edu. We expect to be able to provide access for fuller replication attempts but cannot offer the data publicly.

For all users, `model_data_final.csv` may be used to replicate and verify our analysis. We also encourage anyone to verify our counts of publicly-reported lower extremity injuries using historical NFL injury reports as well as by reviewing our code for errors. Other non-injury game data can also be verified using public sources.

Lines 8-149 of `Turf_Manuscript_0625.R` import raw game and injury data and join and manipulate the files to create `model_data_final.csv`. However, this code will not run properly without the raw injury data file.

Lines 153-484 conduct our analysis. Lines 156-172, used to create Table 1 in the manuscript, will not run without the raw injury data file; the rest will.

To replicate our analysis we recommend loading the necessary packages in lines 4-5; importing `model_data_final.csv`; and running the analysis code from lines 173 onwards. If you also wish to verify and replicate the code used to create `model_data_final.csv`, please reach out to zbinney at emory dot edu for the necessary raw injury data file. All reasonable requests will be accepted.

