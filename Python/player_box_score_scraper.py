from basketball_reference_web_scraper import client

# enums from module needed for writing to csv file
from basketball_reference_web_scraper.data import OutputType
from basketball_reference_web_scraper.data import OutputWriteOption

start_season = 2008
end_season = 2009

months = [10, 11, 12, 1, 2, 3, 4]
days = list(range(1,32))
years = list(range(start_season, end_season + 1))

dates = [[d, m, y] for d in days for m in months for y in years if not (m >= 10 and y == end_season) and  not (m <= 4 and y == start_season)]

for dt in dates:
    client.player_box_scores(*dt,
                             output_type = OutputType.CSV,
                             output_file_path = '../data/box_scores/daily/2008/{}.{}.{}_box_scores.csv'.format(*dt),
                             output_write_option = OutputWriteOption.APPEND_AND_WRITE)
