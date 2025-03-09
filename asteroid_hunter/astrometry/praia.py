import glob
import numpy as np
import os

def run_praia(site):
    """
    Run the PRAIA code on the respective directory with previously saved information
    :return:
    """
    if site == "PANSTARRS1":
        from asteroid_hunter.data.PANSTARRS1 import praia_astro, praia_header, bad_fluxes
    else:
        raise ValueError("Site {} not recognized".format(site))

    lista = glob.glob('*.fits')
    with open('list_fits', 'w') as f:
        for l in np.sort(lista):
            f.write(l + '\n')
    with open('bad_fluxes.pix', 'w') as f:
        f.write(bad_fluxes)

    with open("PRAIA_header.dat", "w") as f:
        f.writelines(praia_header)
    with open("PRAIA_astrometry.dat", "w") as f:
        f.writelines(praia_astro)

    os.system('rm astrometry_photometry* astrometry_reduction*')
    os.system('./PRAIA_FITS_header_extraction_3_0_04 < PRAIA_header.dat > relatorio_praia_header'.format(site))
    os.system('./PRAIA_astrometry_3_0_04 < PRAIA_astrometry.dat > relatorio_praia_astrometry'.format(site))

def compile_praia():
    from asteroid_hunter.data.PRAIA import praia_header_source, praia_astro_source
    # Verificar compilação do PRAIA
    with open("PRAIA_FITS_header_extraction_3_0_04.f", "w") as f:
        f.writelines(praia_header_source)
    with open("PRAIA_astrometry_3_0_04.f", "w") as f:
        f.writelines(praia_astro_source)

    os.system('gfortran -O3 PRAIA_FITS_header_extraction_3_0_04.f -o PRAIA_FITS_header_extraction_3_0_04')
    os.system('gfortran -O3 PRAIA_astrometry_3_0_04.f -o PRAIA_astrometry_3_0_04')

    os.system('rm PRAIA_FITS_header_extraction_3_0_04.f PRAIA_astrometry_3_0_04.f')