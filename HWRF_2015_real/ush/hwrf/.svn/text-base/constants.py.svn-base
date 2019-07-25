"""This module contains various constants, including the Earth
"radius" and unit conversions.  Unit conversion constants are given as
fractions.Fraction objects to allow the conversion to be exact."""

__all__=[''] # prevent accidental "from constants import *"

import fractions

# Metric/SI/nautical unit conversions: these conversions are exact.
# knots, nmi: http://www.nist.gov/pml/wmd/metric/length.cfm
# ft/inches:  http://physics.nist.gov/cuu/Units/outside.html
ft2m = fractions.Fraction(12*254,10000)
"""US foot to meters conversion (exact)."""

m2ft = 1/ft2m
"""Meters to US foot conversion (exact)."""

nmi2km = fractions.Fraction(1852,1000)
"Nautical miles to kilometers conversion (exact)."""

km2nmi = 1/nmi2km
"""Kilometers to nautical miles conversion (exact)."""

kts2mps = fractions.Fraction(1852,3600)
"""Knots to meters per second conversion (exact)."""

mps2kts = 1/kts2mps
"""Meters per second to knots conversion (exact)."""

# Various earth radii from the hwrfutil library constants_module:
Rpole      = 6356752.3142
"""EGM2008 Earth radius at the pole."""

Requator   = 6378137.0000
"""EGM2008 Earth radius at the equator."""

flattening = 1/298.257223563
"""EGM2008 Earth flattening ratio."""

REmean       = 6371009.0
"""Earth mean elipsoid radius from IUGG 1980"""

REauthalic   = 6371007.2
"""Earth authalic (equal surface area) radius from IUGG 1980"""

REvolume     = 6371000.8
"""Earth equal volume radius from IUGG 1980"""

RErectifying = 6367449.1
"""Earth rectivying (equal circumference) radius from IUGG 1980"""

# Compromise: mean of first three radii:
Rearth = (REmean+REauthalic+REvolume)/3
"""Average of the mean elipsoid radius, authalic radius and equal
volume radius."""
