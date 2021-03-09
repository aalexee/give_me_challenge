# give_me_challenge
Here you can find the data and replication code for my ["Give Me a Challenge or Give Me a Raise"](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3449468) paper, upcoming in *Experimental Economics*.

- Folder "ztree" contains the treatment file for ztree

- Folder "raw_data" contains the .xls files generated by zTree, the files are in a format YYMMDD_HHMM.xls

- Folder "data" contains the processed data used for analysis

"data_demog.csv" contains the subject-level data from the demographic survey, it has the following variables:

-- session - session date and time in the format "M_DD_YY_HH"
-- id - unique subject id (the key variable)
age - age in years
gender - subjects' gender
race - subjects' racial or ethnic background
marital - subject's marital status
major - subject's major 
gpa - Grade Point Average
year - year in program (Freshman, Sophomore, etc.)
hh_size - the number of people in a subject's household
hh_inc - the total income of a subject's household
par_inc - the total income of a subject's parents

- Folder "r_code" contains the R code used to analyze the data
"graph_settings.R" is an auxiliary code that sets up how graphs look like
"master_reduced_form.R" performs the reduced-form analysis (Sections 5.1 and 5.2 of the paper)
"master_structural.R" performs the structural analysis (Section 5.3)