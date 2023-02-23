# Trees_in_Agriculture

## Files list  

### Housekeeping files
* `.gitignore`
* `README.md`: readme file for this github repository. any git user of this repository should update
* `Trees_in_Agriculture_R_Project.RProj`: R project created by Lisa McCullough, she uses to commit from RStudio to this repository in git

### TIA Policy Analysis Project
#### Raw data /Files for input
* `TIA_Policy_raw.xlsx`: data drawn from Sprenkle-Hyppolite expert elicitation (specficially part 1: Trees in Croplands)
* `TIA_Policy.csv` : csv version of TIA_Policy_raw.xlsx
#### Files for analysis
* `tia_policy_analysis.Rmd`: cleans and analyzes TIA_Policy.csv, creating wide and long data, and making an interactive app for making a custom histogram that plots reasons trees are in fields and on boundaries of fields of different (user-selected) crops. However, you cannot publish the app on Shiny Apps privately without a subscription, so this interactive idea was tabled until we are ready to publicly communicate data.
* `tia_policy_analysis_not_shiny.Rmd`: the working file for making histograms of reasons trees in fields/on boundaries. Essentially same as TIA_policy_analysis.Rmd except its output is static (not an interactive app). Latest updates go in this file, usually
* `tia1_policy_grapher.Rmd`: imports tia1_policy_long.csv and uses it to plot histograms of experts' recommended policy actions to increase tree density in agricultural landscapes
#### Output files
##### output by `tia_policy_analysis_not_shiny.Rmd`
* `tia1_long_data` folder: contains files of long form data subsetted by crop and tree location on boundaries/in fields
* `tia1_policy_wide.csv`: clean, wide version of TIA_Policy.csv
* `tia1_policy_long.csv`: clean, long version of TIA_Policy.csv
* `tia_policy_analysis_not_shiny.html`: a knit html document of tia_policy_analysis_not_shiny.Rmd. Beware that knitting is done manually. To ensure up-to-date html document, open tia_policy_analysis_not_shiny.Rmd and re-knit it. Advantage of HTML document is it has the niftiest formatting for online viewing/sharing experience
* `tia_policy_analysis_not_shiny.docx`: a knit Word document of tia_policy_analysis_not_shiny.Rmd. Beware that knitting is done manually. To ensure up-to-date Word document, open tia_policy_analysis_not_shiny.Rmd and re-knit it. Advantage of Word document is you can copy and paste images of graphs from it.
##### output by `tia1_policy_grapher.Rmd`
* `tia1_policy_grapher.html`: a knit html document of tia1_policy_grapher.Rmd. Beware that knitting is done manually. To ensure up-to-date html document, open tia1_policy_grapher.Rmd and re-knit it. Advantage of HTML document is it has the niftiest formatting for online viewing/sharing experience
* `tia1_policy_grapher.docx`: a knit Word document of tia1_policy_grapher.Rmd. Beware that knitting is done manually. To ensure up-to-date Word document, open tia1_policy_grapher.Rmd and re-knit it. Advantage of Word document is you can copy and paste images of graphs from it.

### Coffee/Cacao Project
#### Raw data /Files for input
* `Agroforestry Data Dec 2021_MERGED_METANALYSES_coffee_cacao_subset.xlsx` : Created by Vivian from some wizardry in Excel. See outlook emails from July/August 2022 for how she adapted it from Agroforestry Database.
#### Files for analysis
* None at the moment
#### Output files
* None at the moment

### TIA ANOVA Project
#### Raw data /Files for input
* `TIA1ANOVASept22.xlsx`: Uploaded by Lisa; sent by Starry S-H in email with description in mid-September 2022.
* `anovaData11.csv`: csv version of the corresponding sheet in TIA1ANOVASept22.xlsx
#### Files for analysis
* `tia_anova_analysis.Rmd`: imports anovaData11.csv and eventually will do anova and manova on some cool stuff
#### Output files
##### output by `tia_anova_analysis.Rmd`
* `tia_anova_analysis.html`: a knit html document of tia_anova_analysis.Rmd. Beware that knitting is done manually. To ensure up-to-date html document, open tia_anova_analysis.Rmd and re-knit it. Advantage of HTML document is it has the niftiest formatting for online viewing/sharing experience
