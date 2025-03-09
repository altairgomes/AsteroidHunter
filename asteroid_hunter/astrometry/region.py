import numpy as np

from astropy.coordinates import SkyCoord
from astropy.time import Time
from astroquery.imcce import Skybot
from astroquery.vizier import Vizier

def create_region(astrometry):
    """

    :param astrometry: An Astrometry object

    :return: None
    """

    # Search asteroids
    results = Skybot.cone_search(astrometry.center_fov, astrometry.fov/1.9, astrometry.time,
                                 find_comets=False, location=astrometry.site)
    asteroides = SkyCoord(ra=results['RA'], dec=results['DEC'])
    px, py = astrometry.coord_to_pix(asteroides)
    px = np.array(px, ndmin=1)
    py = np.array(py, ndmin=1)
    unc = results['posunc']

    columns = ['RA_ICRS', 'DE_ICRS', 'pmRA', 'pmDE']
    vquery = Vizier(timeout=600, row_limit=-1, columns=columns)
    result = vquery.query_region(astrometry.center_fov, radius=astrometry.fov/2.0, catalog='I/355/gaiadr3', cache=False)[0]
    pmra = np.nan_to_num(result['pmRA'], 0)
    pmdec = np.nan_to_num(result['pmDE'], 0)
    epoch = Time('J2016.0', scale='tdb')
    stars = SkyCoord(ra=result['RA_ICRS'], dec=result['DE_ICRS'], pm_ra_cosdec=pmra, pm_dec=pmdec, obstime=epoch)
    obs_stars = stars.apply_space_motion(new_obstime=astrometry.time)
    pxs, pys = astrometry.coord_to_pix(obs_stars)
    pxs = np.array(pxs, ndmin=1)
    pys = np.array(pys, ndmin=1)

    f = open(astrometry.name[:-4] + 'reg', 'w')
    for i in range(len(px)):
        f.write('image; circle( {}, {}, {}) # color=red width=2 text={{{}}}\n'.format(px[i], py[i], 5, results['Name'][i]))
        f.write('image; circle( {}, {}, {}) # color=red width=2 dash=1\n'.format(px[i], py[i], (
                    unc[i] / astrometry.pixel_scale).decompose().value))
    for i in range(len(pxs)):
        f.write('image; circle( {}, {}, {}) # color=green width=2\n'.format(pxs[i], pys[i], 5))
    f.close()

def create_regions():
    from asteroid_hunter.astrometry.astrometry import get_info
    astro_params = get_info()
    for astrometry in astro_params:
        create_region(astrometry)