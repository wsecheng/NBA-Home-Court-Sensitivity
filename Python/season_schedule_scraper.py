from basketball_reference_web_scraper import client

# enums from module needed for writing to csv file
from basketball_reference_web_scraper.data import OutputType
from basketball_reference_web_scraper.data import OutputWriteOption

# Get 5 years worth of game results
years = list(range(2014, 2019))

for yr in years:
    client.season_schedule(season_end_year = yr,
                           output_type = OutputType.CSV,
                           output_file_path = '../data/game_results.csv',
                           output_write_option = OutputWriteOption.APPEND_AND_WRITE)
