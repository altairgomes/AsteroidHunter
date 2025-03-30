import numpy as np
import scipy.optimize as opt

from astropy.coordinates import SkyCoord
from astropy.time import Time
from io import StringIO


class Astrometry(object):

    def __init__(self, name, time, center_fov, center_proj, params_x, params_y, naxis1, naxis2, site):
        self.name = str(name)
        self.time = Time(time, format='jd', scale='utc')
        self.center_fov = SkyCoord(center_fov, unit=('hourangle', 'deg'))
        self.center_proj = SkyCoord(center_proj, unit=('hourangle', 'deg'))
        self.params_x = params_x
        self.params_y = params_y
        self.naxis1 = naxis1
        self.naxis2 = naxis2
        self.site = site

        X, Y = self.pix_to_plano([0, self.naxis1], [0, self.naxis2])
        ra, dec = self.plano_to_coord(X, Y)
        corners = SkyCoord(ra, dec)
        self.fov = corners[1].separation(corners[0])
        self.pixel_scale = self.fov / np.sqrt(self.naxis1 ** 2 + self.naxis2 ** 2)

    def coord_to_plano(self, ra, dec):
        """ Transform from celestial coordinates on tangent plane
        using gnomonic projection
        """
        ra0 = self.center_proj.ra
        dec0 = self.center_proj.dec
        X = np.cos(dec) * np.sin(ra - ra0) / (np.sin(dec) * np.sin(dec0) + np.cos(dec) * np.cos(dec0) * np.cos(ra - ra0))
        Y = (np.sin(dec) * np.cos(dec0) - np.cos(dec) * np.sin(dec0) * np.cos(ra - ra0)) / (
                    np.sin(dec) * np.sin(dec0) + np.cos(dec) * np.cos(dec0) * np.cos(ra - ra0))
        return X, Y

    def plano_to_coord(self, X, Y):
        """Transform from tangent plane on celestial coordinates
        using gnomonic projection
        """
        ra0 = self.center_proj.ra
        dec0 = self.center_proj.dec
        ra = np.arctan(X / (np.cos(dec0) - Y * np.sin(dec0))) + ra0
        dec = np.arctan(np.cos(ra - ra0) * (Y * np.cos(dec0) + np.sin(dec0)) / (np.cos(dec0) - Y * np.sin(dec0)))
        return ra, dec

    def pix_to_plano(self, x, y):
        x = np.array(x, ndmin=1) / self.naxis1
        y = np.array(y, ndmin=1) / self.naxis2
        vals = np.array([np.ones(len(x)), x, y, x ** 2, x * y, y ** 2, x ** 3, y * (x ** 2), x * (y ** 2), y ** 3])
        X = np.sum(self.params_x[:10] * vals.T, axis=1)
        Y = np.sum(self.params_y[:10] * vals.T, axis=1)
        return X, Y

    def plano_to_pix(self, X, Y):
        X = np.array(X, ndmin=1)
        Y = np.array(Y, ndmin=1)
        px = self.params_x
        py = self.params_y
        x = (py[2] * X - px[2] * Y + py[0] * px[2] - px[0] * py[2]) / (px[2] * py[1] - py[2] * px[1])
        y = (py[1] * X - px[1] * Y + py[0] * px[1] - px[0] * py[1]) / (px[1] * py[2] - py[1] * px[2])

        def sistema(vars, X1, Y1):
            xg, yg = vars
            vals = np.array([1, xg, yg, xg ** 2, xg * yg, yg ** 2, xg ** 3, yg * (xg ** 2), xg * (yg ** 2), yg ** 3])
            nX = np.sum(self.params_x[:10] * vals)
            nY = np.sum(self.params_y[:10] * vals)
            return [nX - X1, nY - Y1]

        sol = np.array([opt.fsolve(sistema, [x[j], y[j]], args=(X[j], Y[j])) for j in np.arange(len(X))])
        x_sol, y_sol = sol.T
        return x_sol * self.naxis1, y_sol * self.naxis2

    def pix_to_coord(self, x, y):
        X, Y = self.pix_to_plano(x, y)
        ra, dec = self.plano_to_coord(X, Y)
        coord = SkyCoord(ra, dec)
        return coord

    def coord_to_pix(self, coord):
        X, Y = self.coord_to_plano(coord.ra, coord.dec)
        x, y = self.plano_to_pix(X, Y)
        return x, y

def get_info():
    # Leitura dos parâmetros de todas as imagens
    with open('astrometry_reduction_Gaia_DR3', 'r') as f:
        lines = f.readlines()
    with open('header_output', 'r') as f:
        lines_header = f.readlines()
    with open('PRAIA_astrometry.dat', 'r') as f:
        lines_dat = f.readlines()
    site = lines_dat[39][:3]

    astro_params = []
    for i in range(len(lines)):
        params = Astrometry(name =lines[i][120:170].strip(),
                            time = float(lines[i][46:62]),
                            center_fov = lines[i][63:90],
                            center_proj = lines_header[i][1:28],
                            params_x = np.loadtxt(StringIO(lines[i][205:729]), unpack=True),
                            params_y = np.loadtxt(StringIO(lines[i][730:1254]), unpack=True),
                            naxis1 = int(lines[i][193:197]),
                            naxis2 = int(lines[i][199:203]),
                            site = site)
        astro_params.append(params)
    return astro_params