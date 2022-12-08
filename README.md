# ACR_shorebird_data_management
Code to clean ACR's Tomales Bay shorebird monitoring data for analysis or annual summary



Basic data cleaning workflow involves running the code in the following files:  
- 1. sbird_data_prep_1_read_clean.R - Read data in from Access db (this must be saved locally in data_files) and do basic cleaning of column names, dates, convert to long format.  
- 2. raptors_from_shorebirds.R - Extract all raptor data. (requires sbird_data which is a product of sbird_data_prep_1_read_clean.R)  
- 3. sbird_data_prep_2_interpolate_missing_surveys.R - OPTIONAL. Can interpolate data from missed surveys  
- 4. sbird_data_prep_3_split_lumped_species.R - Split birds identified to species groups (e.g. PEEP) into appropriate species level.  
- 5. sbird_data_prep_4_group_similar_species.R - lump similar species which cannot be reliably identified to species level  

After running these scripts, the resulting   
- data_files/rds/shorebirds_for_analysis  
or  
- data_files/csv/sbirds4analysis.csv  

is ready for analysis or creating an annual summary. 