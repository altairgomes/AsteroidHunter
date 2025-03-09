import os

astrometry_path = os.path.join(os.path.dirname(__file__), 'PRAIA_astrometry_3_0_04.dat')
header_path = os.path.join(os.path.dirname(__file__), 'PRAIA_FITS_header_extraction_3_0_04.dat')

with open(astrometry_path, 'r') as f:
    praia_astro = f.readlines()

with open(header_path, 'r') as f:
    praia_header = f.readlines()

bad_fluxes = "0 165 1"