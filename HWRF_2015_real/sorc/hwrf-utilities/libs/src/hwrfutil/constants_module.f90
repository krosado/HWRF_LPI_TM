module constants_module
implicit none
  ! The variables in this module are default values for various
  ! constants that always seem to change from program to program.  In
  ! the projection and interpolation code, a projection can provide
  ! specific values for these.  See [#] references below.

  ! Earth Elipsoid.  This is from the Earth Gravitational Model
  ! EGM2008 radius information [1].  That is a standardized,
  ! thuroughly researched, model of the Earth gravitational field.  It
  ! represents the Earth mean sea level as a geoid perturbation of an
  ! elipsoid.  The perturbations average out to zero.  These are the
  ! elipsoid specifications:
  real, parameter :: Rpole      = 6356752.3142
  real, parameter :: Requator   = 6378137.0000
  real, parameter :: flattening = 1/298.257223563
  ! Note: flattening = (Requator-Rpole)/Requator

  ! Mean earth radii.  There are many ways to compute this since the
  ! earth is not a sphere.  These means come from the International
  ! Union of Geodesy and Geophysics (IUGG) as of 1980.  See the
  ! references for details.  All three are computed from an ellipsoid,
  ! ignoring the geoid perturbations.
  real, parameter :: REmean       = 6371009.0 ! mean elipsoid radius [2]
  real, parameter :: REauthalic   = 6371007.2 ! equal surface area [2]
  real, parameter :: REvolume     = 6371000.8 ! equal volume [3]
  real, parameter :: RErectifying = 6367449.1 ! equal circumference [4]

  ! A compromise: mean of the first three means:
  real, parameter :: Rearth = (REmean+REauthalic+REvolume)/3

  ! Universal constants that people like to change:
  real, parameter :: pi      = 3.141592653589793238
  real, parameter :: ln_base = 2.718281828459045235 ! AKA: e
  real, parameter :: DEGRAD  = pi/180
  real, parameter :: RADDEG  = 180/pi

  ! References:

  ! [1] Pavlis,N.K., S.A.Holmes, S.C.Kenyon, and J.K.Factor (April
  !     13-18, 2008).  "An Earth Gravitational Model to Degree 2160:
  !     EGM2008," presented at the 2008 General Assembly of the
  !     European Geosciences Union, Vienna, Austria.

  ! [2] Moritz, H. (1980). "Geodetic Reference System 1980, by
  !     resolution of the XVII General Assembly of the IUGG in
  !     Canberra."

  ! [3] Moritz, H. (March 2000). "Geodetic Reference System 1980."
  !     Journal of Geodesy 74 (1), p. 128–133. Bibcode:
  !     2000JGeod..74..128.. doi: 10.1007/s001900050278.

  ! [4] Snyder, J.P. (1987). "Map Projections – A Working Manual." US
  !     Geological Survey Professional Paper 1395, p. 16–17.
  !     Washington D.C: United States Government Printing Office
end module constants_module
