from datetime import date, timedelta
import calendar

def daterange(start_date, end_date):
    for n in range(int ((end_date - start_date).days)):
        yield start_date + timedelta(n)

a = date(1990, 1, 1);
b = date(2000, 1, 1);

for single_date in daterange(a, b):
    print single_date.strftime("%Y-%m-%d")
    print calendar.day_name[single_date.weekday()]
