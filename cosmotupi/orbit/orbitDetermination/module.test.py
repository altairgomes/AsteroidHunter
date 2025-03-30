from datetime import datetime, timezone, timedelta
from main import process_asteroid

observations_apophis = [
   {"RA": 61.533666, "DEC": 16.917944, "timestamp": datetime(2004, 3, 15, 2, 35, 21, 696000, tzinfo=timezone.utc)},
   {"RA": 61.548958, "DEC": 16.920972, "timestamp": datetime(2004, 3, 15, 2, 58, 4, 320000, tzinfo=timezone.utc)},
   {"RA": 61.562083, "DEC": 16.923722, "timestamp": datetime(2004, 3, 15, 3, 17, 34, 272000, tzinfo=timezone.utc)},
   {"RA": 61.564917, "DEC": 16.925194, "timestamp": datetime(2004, 3, 15, 3, 21, 58, 848000, tzinfo=timezone.utc)},
]

observations_dz2 = [
   {"RA": 123.942792, "DEC": 19.404944, "timestamp": datetime(2023, 2, 11, 5, 30, 11, tzinfo=timezone.utc)},
   {"RA": 123.941458, "DEC": 19.405278, "timestamp": datetime(2023, 2, 11, 5, 37, 35, tzinfo=timezone.utc)},
   {"RA": 123.940125, "DEC": 19.405472, "timestamp": datetime(2023, 2, 11, 5, 45, 1, tzinfo=timezone.utc)},
   {"RA": 123.938792, "DEC": 19.405667, "timestamp": datetime(2023, 2, 11, 5, 52, 27, tzinfo=timezone.utc)},
]

process_asteroid("2023 DZ2", observations_dz2) 
process_asteroid("Apophis", observations_apophis)