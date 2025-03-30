import os

astrometry_path = os.path.join(os.path.dirname(__file__), 'PRAIA_astrometry_3_0_04.f')
header_path = os.path.join(os.path.dirname(__file__), 'PRAIA_FITS_header_extraction_3_0_04.f')

with open(astrometry_path, 'r') as f:
    praia_astro_source = f.readlines()

with open(header_path, 'r') as f:
    praia_header_source = f.readlines()