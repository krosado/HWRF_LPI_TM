%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Run HWRF/MPIPOMTC ocean diagnostics %%%
%%% Author: Richard M. Yablonsky, URI   %%%
%%% Last Update: 12/5/2014              %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear all; close all; clc;
mpth='/lfs3/projects/hur-uri/Richard.Yablonsky/MPI_POMTC_20140930/sorc/pomtc/ocean_diag/matlab' % Current directory
opth=[mpth,'/pngs']                                                                             % Path for diagnostic output
%spth='/lfs3/projects/hur-uri/Richard.Yablonsky/pytmp/MPI_POMTC_20140930'                        % Path to HWRF run $(WORKhwrf)
spth='/lfs1/projects/dtc-hurr/Ligia.Bernardet/pytmp/rocHWRF'                                    % Path to HWRF run $(WORKhwrf)
%%spth=mpth                                                                                       % For development only
snam='edouard'                                                                                  % Storm name
stid='06l'                                                                                      % Storm ID & basin identifier
sdat='2014091218'                                                                               % Storm cycle (YYYYMMDDHH)
addpath(genpath(mpth));

%%% Plot SST time series within specified radius of storm center %%%

for n=[60 100 150 200] % indicates the appropriate wind radius file to read
  plotssttimeseries(opth,spth,snam,stid,sdat,n)
end
