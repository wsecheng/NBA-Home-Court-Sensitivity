import pandas as pd
import os

year_box_score_dir = '../data/box_scores/daily/2008/'

for filename in os.listdir(year_box_score_dir):
    file = pd.read_csv(year_box_score_dir + filename)
    file['date'] = filename.split("_")[0]
    file.to_csv(year_box_score_dir+ filename, index=False)

combined_files = pd.concat([pd.read_csv(year_box_score_dir + f) for f in os.listdir(year_box_score_dir)])
combined_files.to_csv('../data/box_scores/final/final_box_scores_2008.csv', index=False)
