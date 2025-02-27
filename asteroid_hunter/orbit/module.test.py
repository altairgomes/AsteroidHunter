from datetime import datetime, timezone
from orbit import process_asteroid


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

observations_dz2_2 = [
   # 2023 01 14
   # {"RA": 128.851879, "DEC": 18.382556, "timestamp": datetime(2023, 1, 14, 9, 13, 35, tzinfo=timezone.utc)},
   # {"RA": 128.850821, "DEC": 18.382778, "timestamp": datetime(2023, 1, 14, 9, 29, 56, tzinfo=timezone.utc)},
   # {"RA": 128.849775, "DEC": 18.382994, "timestamp": datetime(2023, 1, 14, 9, 46, 22, tzinfo=timezone.utc)},
   # {"RA": 128.848742, "DEC": 18.383253, "timestamp": datetime(2023, 1, 14, 10, 2, 47, tzinfo=timezone.utc)},

   # # 2023 01 28
   # {"RA": 127.06317, "DEC": 18.756111, "timestamp": datetime(2023, 1, 28, 7, 8, 52, tzinfo=timezone.utc)},

   # # 2023 02 11
   # {"RA": 123.942795, "DEC": 19.404944, "timestamp": datetime(2023, 2, 11, 5, 30, 6, tzinfo=timezone.utc)},
   # {"RA": 123.941458, "DEC": 19.405278, "timestamp": datetime(2023, 2, 11, 5, 37, 33, tzinfo=timezone.utc)},
   # {"RA": 123.940125, "DEC": 19.405472, "timestamp": datetime(2023, 2, 11, 5, 45, 1, tzinfo=timezone.utc)},
   # {"RA": 123.938792, "DEC": 19.405667, "timestamp": datetime(2023, 2, 11, 5, 52, 28, tzinfo=timezone.utc)},

   # # 2023 02 27
   # {"RA": 120.05510, "DEC": 20.139608, "timestamp": datetime(2023, 2, 27, 22, 21, 21, tzinfo=timezone.utc)},
   # {"RA": 120.05379, "DEC": 20.139722, "timestamp": datetime(2023, 2, 27, 22, 27, 43, tzinfo=timezone.utc)},
   # {"RA": 120.05241, "DEC": 20.139889, "timestamp": datetime(2023, 2, 27, 22, 34, 2, tzinfo=timezone.utc)},
   # {"RA": 120.05108, "DEC": 20.140028, "timestamp": datetime(2023, 2, 27, 22, 40, 20, tzinfo=timezone.utc)},
   # {"RA": 120.04975, "DEC": 20.140194, "timestamp": datetime(2023, 2, 27, 22, 46, 38, tzinfo=timezone.utc)},
   # {"RA": 120.04838, "DEC": 20.140278, "timestamp": datetime(2023, 2, 27, 22, 52, 58, tzinfo=timezone.utc)},
   # {"RA": 120.04700, "DEC": 20.140417, "timestamp": datetime(2023, 2, 27, 22, 59, 17, tzinfo=timezone.utc)},
   # {"RA": 120.04563, "DEC": 20.140556, "timestamp": datetime(2023, 2, 27, 23, 5, 37, tzinfo=timezone.utc)},
   # {"RA": 120.04425, "DEC": 20.140694, "timestamp": datetime(2023, 2, 27, 23, 11, 57, tzinfo=timezone.utc)},
   # {"RA": 120.04292, "DEC": 20.140778, "timestamp": datetime(2023, 2, 27, 23, 18, 17, tzinfo=timezone.utc)},
   # {"RA": 120.04155, "DEC": 20.140889, "timestamp": datetime(2023, 2, 27, 23, 24, 40, tzinfo=timezone.utc)},
   # {"RA": 120.04022, "DEC": 20.141000, "timestamp": datetime(2023, 2, 27, 23, 30, 57, tzinfo=timezone.utc)},

   # # 2023 02 28
   # {"RA": 119.87790, "DEC": 20.170200, "timestamp": datetime(2023, 2, 28, 22, 15, 38, tzinfo=timezone.utc)},
   # {"RA": 119.87780, "DEC": 20.170222, "timestamp": datetime(2023, 2, 28, 22, 16, 52, tzinfo=timezone.utc)},
   # {"RA": 119.87745, "DEC": 20.170250, "timestamp": datetime(2023, 2, 28, 22, 18, 5, tzinfo=timezone.utc)},
   # {"RA": 119.87745, "DEC": 20.170278, "timestamp": datetime(2023, 2, 28, 22, 19, 19, tzinfo=timezone.utc)},
   # {"RA": 119.87670, "DEC": 20.170333, "timestamp": datetime(2023, 2, 28, 22, 21, 46, tzinfo=timezone.utc)},
   # {"RA": 119.87640, "DEC": 20.170361, "timestamp": datetime(2023, 2, 28, 22, 22, 55, tzinfo=timezone.utc)},
   # {"RA": 119.87610, "DEC": 20.170389, "timestamp": datetime(2023, 2, 28, 22, 24, 13, tzinfo=timezone.utc)},

   # # 2023 03 01
   {"RA": 119.71556, "DEC": 20.19808, "timestamp": datetime(2023, 3, 1, 21, 46, 48, tzinfo=timezone.utc)},
   {"RA": 119.71550, "DEC": 20.19811, "timestamp": datetime(2023, 3, 1, 21, 48, 2, tzinfo=timezone.utc)},
   {"RA": 119.71540, "DEC": 20.19811, "timestamp": datetime(2023, 3, 1, 21, 49, 15, tzinfo=timezone.utc)},
   {"RA": 119.71530, "DEC": 20.19819, "timestamp": datetime(2023, 3, 1, 21, 50, 29, tzinfo=timezone.utc)},
   {"RA": 119.71530, "DEC": 20.19819, "timestamp": datetime(2023, 3, 1, 21, 51, 43, tzinfo=timezone.utc)},
   {"RA": 119.71520, "DEC": 20.19825, "timestamp": datetime(2023, 3, 1, 21, 52, 56, tzinfo=timezone.utc)},
   {"RA": 119.71520, "DEC": 20.19828, "timestamp": datetime(2023, 3, 1, 21, 54, 10, tzinfo=timezone.utc)},
   {"RA": 119.71520, "DEC": 20.19828, "timestamp": datetime(2023, 3, 1, 21, 55, 24, tzinfo=timezone.utc)},
   {"RA": 119.71520, "DEC": 20.19833, "timestamp": datetime(2023, 3, 1, 21, 56, 37, tzinfo=timezone.utc)},
   {"RA": 119.71520, "DEC": 20.19833, "timestamp": datetime(2023, 3, 1, 21, 57, 51, tzinfo=timezone.utc)},
   {"RA": 119.71520, "DEC": 20.19833, "timestamp": datetime(2023, 3, 1, 21, 59, 4, tzinfo=timezone.utc)},
   # {"RA": 119.71520, "DEC": 20.19833, "timestamp": datetime(2023, 3, 1, 22, 0, 18, tzinfo=timezone.utc)},

   # # 2023 03 16
   # {"RA": 118.90192, "DEC": 20.23466, "timestamp": datetime(2023, 3, 16, 18, 26, 33, tzinfo=timezone.utc)},
   # {"RA": 118.89783, "DEC": 20.22212, "timestamp": datetime(2023, 3, 16, 19, 6, 31, tzinfo=timezone.utc)},
   # {"RA": 118.89746, "DEC": 20.22200, "timestamp": datetime(2023, 3, 16, 19, 11, 35, tzinfo=timezone.utc)},
   # {"RA": 118.89700, "DEC": 20.22291, "timestamp": datetime(2023, 3, 16, 19, 16, 38, tzinfo=timezone.utc)},
   # {"RA": 118.90041, "DEC": 20.22606, "timestamp": datetime(2023, 3, 16, 19, 21, 31, tzinfo=timezone.utc)},
   # {"RA": 118.89636, "DEC": 20.22165, "timestamp": datetime(2023, 3, 16, 19, 21, 32, tzinfo=timezone.utc)},
   # {"RA": 118.89704, "DEC": 20.22434, "timestamp": datetime(2023, 3, 16, 19, 22, 36, tzinfo=timezone.utc)},
   # {"RA": 118.89942, "DEC": 20.22694, "timestamp": datetime(2023, 3, 16, 19, 29, 24, tzinfo=timezone.utc)},
   # {"RA": 118.89588, "DEC": 20.22303, "timestamp": datetime(2023, 3, 16, 19, 32, 51, tzinfo=timezone.utc)},
   # {"RA": 118.89477, "DEC": 20.22385, "timestamp": datetime(2023, 3, 16, 19, 43, 6, tzinfo=timezone.utc)},
   # {"RA": 118.89216, "DEC": 20.22444, "timestamp": datetime(2023, 3, 16, 19, 58, 49, tzinfo=timezone.utc)},
   # {"RA": 118.88621, "DEC": 20.23150, "timestamp": datetime(2023, 3, 16, 20, 12, 0, tzinfo=timezone.utc)},
   # {"RA": 118.88200, "DEC": 20.22300, "timestamp": datetime(2023, 3, 16, 20, 14, 42, tzinfo=timezone.utc)},
   # {"RA": 118.88061, "DEC": 20.22940, "timestamp": datetime(2023, 3, 16, 20, 56, 42, tzinfo=timezone.utc)},
   # {"RA": 118.88421, "DEC": 20.22364, "timestamp": datetime(2023, 3, 16, 20, 59, 22, tzinfo=timezone.utc)},
   # {"RA": 118.88337, "DEC": 20.22314, "timestamp": datetime(2023, 3, 16, 21, 7, 7, tzinfo=timezone.utc)},
   # {"RA": 118.88262, "DEC": 20.22275, "timestamp": datetime(2023, 3, 16, 21, 14, 51, tzinfo=timezone.utc)},
   # {"RA": 118.87811, "DEC": 20.22804, "timestamp": datetime(2023, 3, 16, 21, 15, 57, tzinfo=timezone.utc)},
   # {"RA": 118.88029, "DEC": 20.22081, "timestamp": datetime(2023, 3, 16, 21, 40, 42, tzinfo=timezone.utc)},
   # {"RA": 118.87911, "DEC": 20.22863, "timestamp": datetime(2023, 3, 16, 21, 52, 24, tzinfo=timezone.utc)},
   # {"RA": 118.87853, "DEC": 20.22836, "timestamp": datetime(2023, 3, 16, 21, 56, 55, tzinfo=timezone.utc)},
   # {"RA": 118.87800, "DEC": 20.22809, "timestamp": datetime(2023, 3, 16, 22, 1, 9, tzinfo=timezone.utc)},
   # {"RA": 118.87838, "DEC": 20.21942, "timestamp": datetime(2023, 3, 16, 22, 1, 50, tzinfo=timezone.utc)},
   # {"RA": 118.87740, "DEC": 20.22840, "timestamp": datetime(2023, 3, 16, 22, 5, 22, tzinfo=timezone.utc)},
   # {"RA": 118.88763, "DEC": 20.23831, "timestamp": datetime(2023, 3, 16, 22, 13, 30, tzinfo=timezone.utc)},
   # {"RA": 118.87730, "DEC": 20.21822, "timestamp": datetime(2023, 3, 16, 22, 17, 41, tzinfo=timezone.utc)},
   # {"RA": 118.93500, "DEC": 20.29275, "timestamp": datetime(2023, 3, 16, 23, 58, 34, tzinfo=timezone.utc)},

   # # 2023 03 17
   # {"RA": 118.93500, "DEC": 20.29278, "timestamp": datetime(2023, 3, 17, 0, 1, 19, tzinfo=timezone.utc)},
   # {"RA": 118.93500, "DEC": 20.29278, "timestamp": datetime(2023, 3, 17, 0, 4, 5, tzinfo=timezone.utc)},
   # {"RA": 118.93496, "DEC": 20.29278, "timestamp": datetime(2023, 3, 17, 0, 6, 51, tzinfo=timezone.utc)},
   # {"RA": 119.05025, "DEC": 20.17500, "timestamp": datetime(2023, 3, 17, 19, 41, 5, tzinfo=timezone.utc)},
   # {"RA": 119.05971, "DEC": 20.18135, "timestamp": datetime(2023, 3, 17, 19, 43, 55, tzinfo=timezone.utc)},
   # {"RA": 119.04888, "DEC": 20.17431, "timestamp": datetime(2023, 3, 17, 19, 53, 8, tzinfo=timezone.utc)},
   # {"RA": 119.06529, "DEC": 20.18051, "timestamp": datetime(2023, 3, 17, 20, 6, 57, tzinfo=timezone.utc)},
   # {"RA": 119.04450, "DEC": 20.17475, "timestamp": datetime(2023, 3, 17, 20, 11, 11, tzinfo=timezone.utc)},
   # {"RA": 119.04167, "DEC": 20.17311, "timestamp": datetime(2023, 3, 17, 20, 43, 17, tzinfo=timezone.utc)},
   # {"RA": 119.03575, "DEC": 20.18033, "timestamp": datetime(2023, 3, 17, 20, 45, 14, tzinfo=timezone.utc)},
   # {"RA": 119.04854, "DEC": 20.17719, "timestamp": datetime(2023, 3, 17, 21, 17, 0, tzinfo=timezone.utc)},
   # {"RA": 119.04000, "DEC": 20.17142, "timestamp": datetime(2023, 3, 17, 21, 20, 23, tzinfo=timezone.utc)},
]

# process_asteroid("Apophis", observations_apophis)
# process_asteroid("2023 DZ2", observations_dz2)
process_asteroid("2023 DZ2", observations_dz2_2)