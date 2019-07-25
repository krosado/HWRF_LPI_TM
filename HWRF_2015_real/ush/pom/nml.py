#!/usr/bin/python

#from collections import OrderedDict
from util import makenwrap

""" Written By Biju Thomas, GSO, University of Rhode Island on 6/09/14. Generating 
    necessary input files for MPI POM spin up: Please report bugs/questions/ comments
    to bijuthomas@mail.uri.edu.

    Arguments:
      title: run's title

      netcdf_file: netCDF output file (rund ID)

      mode: calculation mode
           description
           2   2-D calculation (bottom stress calculated in advave)
           3   3-D calculation (bottom stress calculated in profu,v)
           4   3-D calculation with t and s held fixed

      nadv: advection scheme
          1    Centred scheme, as originally provide in POM
          2    Smolarkiewicz iterative upstream scheme, based on
             subroutines provided by Gianmaia Sannino and Vincenzo
             Artale

      Constants for Smolarkiewicz iterative upstream scheme
      nitera: number of iterations. This should be in the range 1 - 4. 1 is
              standard upstream differencing; 3 adds 50% CPU time to POM

      sw: smoothing parameter. This should preferably be 1, but 0 < sw < 1
          gives smoother solutions with less overshoot when nitera > 1

      npg: pressure gradient scheme
      npg      pressure gradient scheme
        1     Second order scheme, as originally provide in POM
        2     Fourth order scheme using the McCalpin method (Berntsen and
               Oey, Ocean Dynamics, 2010)

      dte: external (2-D) time step (secs.) according to CFL

      isplit: <Internal (3-D) time step>/<External (2-D) time step>
              (dti/dte; dimensionless)

     time_start: date and time of start of initial run of model in format
                (i.e. UDUNITS convention)
                YYYY-MM-DD HH:MM:SS <+/->HH:MM
                where "<+/->HH:MM" is the time zone (positive eastwards
                from Coordinated Universal Time). NOTE that the
                climatological time axis (i.e. beginning of year zero,
                which does not exist in the real-world calendar) is used

     Restart
       nread_rst: index to indicate whether run to start from restart file
                 (nread_rst=0: no restart input file; nread_rst=1: restart
                 input file)
       read_rst_file: restart input file name
       write_rst: interval (days) to write a restart file
       write_rst_file: restart output file name

    days: run duration (days)

    prtd1: initial print interval (days)

    prtd2: final print interval (days)

    swtch: time to switch from prtd1 to prtd2

    nbct: surface temperature boundary condition (see initialize.f)

    ifplane: f-plane option
    ifplane  f-plane option
      0       Grid is not an f-plane, as originally provided in POM
      1       Grid is an f-plane, calculated at latitude cnorth_e

     Constant for f-plane option
     cnorth_e: latitude for Coriolis calculation when using an f-plane

    ismoth: smoothing/desmoothing option
      0      do not use 3D U V T S smoothing
      1      use 3D U V T S smoothing

    Constant for smoothing/desmoothing option
     smh: smoothing time step frequency. This has traditionally been 3.

    tnowindd: no prescribed wind after tnowindd (days)

    igeovel: initial geostrophic velocity option
    igeovel  initial geostrophic velocity option
      0       No calculation of initial geostrophic velocity
      1       Calculate initial geostrophic velocity

    nl: number of z-levels in initial condition file(s)

    ionedim: 3D (full) vs. 1D (vertical-only) physics option
    ionedim  3D (full) vs. 1D (vertical-only) physics option
      0       use full 3D physics
      1       use vertical-only 1D phsyics

    ipwave: wave-induced mixing option
    ipwave  wave-induced mixing option
      0      do not use wave-induced mixing
      1      use wave-induced mixing                                              """


class nml(object):
  def __init__(self):
    self.namelist = {'title'         :  "'MPIPOM-TC: stormenv'"        ,          
                     'netcdf_file'   :  "'stormenv'"                   ,           
                     'mode'          :  3                              ,          
                     'nadv'          :  1                              ,          
                     'nitera'        :  1                              ,           
                     'sw'            :  0.5                            ,           
                     'npg'           :  1                              ,           
                     'dte'           :  6                              ,           
                     'isplit'        :  45                             ,           
                     'time_start'    :  "'yyyy-mm-dd-hh:00:00 +00:00'" ,          
                     'nread_rst'     :  0                              ,               
                     'read_rst_file' :  "'restart.phaseN.nc'"          ,         
                     'write_rst'     :  2.0                            ,            
                     'write_rst_file':  "'restart'"                    ,           
                     'days'          :  2.0                            ,            
                     'prtd1'         :  1.0                            ,         
                     'prtd2'         :  1.0                            ,             
                     'swtch'         :  9999.                          ,             
                     'nbct'          :  3                              ,             
                     'ifplane'       :  0                              ,             
                     'cnorth_e'      :  22.4                           ,             
                     'ismoth'        :  1                              ,             
                     'smh'           :  3.                             ,             
                     'tnowindd'      :  0.                             ,             
                     'igeovel'       :  1                              ,             
                     'nl'            :  33                             ,           
                     'ionedim'       :  0                              ,           
                     'ipwave'        :  0}                            
    self.keys = ['title','netcdf_file','mode','nadv','nitera','sw','npg',  \
               'dte','isplit','time_start','nread_rst','read_rst_file',    \
               'write_rst','write_rst_file','days','prtd1','prtd2','swtch',\
               'nbct','ifplane','cnorth_e','ismoth','smh','tnowindd',      \
               'igeovel','nl','ionedim','ipwave'] 
  def __call__(self,title,netcdf_file,time_start,nread_rst,read_rst_file,  \
                    write_rst,days,prtd1,nbct,tnowindd,igeovel,nl):
     self.namelist['title']         =   title
     self.namelist['netcdf_file']   =   '"'+netcdf_file+'"'
     self.namelist['time_start']    =   time_start
     self.namelist['nread_rst']     =   nread_rst
     self.namelist['read_rst_file'] =   read_rst_file
     self.namelist['write_rst']     =   write_rst 
     self.namelist['days']          =   days
     self.namelist['prtd1']         =   prtd1
     self.namelist['nbct']          =   nbct
     self.namelist['tnowindd']      =   tnowindd
     self.namelist['igeovel']       =   igeovel
     self.namelist['nl' ]           =   nl
   
  @makenwrap
  def make(self,DEST):
    pass
