%Modified by Ivan Haigh from TMD toolbox functions, and further altered by Bharti Dharapuram on 20 Nov 2017

%set working directory as the path-to-FUNCTIONS folder within tide
%function [TS,conList]=tide_pred5(Model,SDtime,lat,lon,type,sc,I,amp,pha,conList)

%Providing input variables
Model = '../DATA/Model_IO_atlas'; %1/12 deg resolution, interpolated usually poor quality coastal areas from high resolution regional models
SDtime = [datenum(2016,12,16,0,0,0):datenum(0,0,0,1,0,0):datenum(2017,1,1,0,0,0)]';%time series from 16 dec 2016 to 30 dec 2017, every hour
type = 'z';
sc = [1:11]; %using all 11 consituent harmonics
I = 1; %not correcting for minor constitutents
amp=ncread('../DATA/hf.IO_2011atlas.nc', 'ha'); %input amplitude information
pha=ncread('../DATA/hf.IO_2011atlas.nc', 'hp'); %inout phase information
conList=['m2  '; 's2  '; 'n2  '; 'k2  '; 'k1  '; 'o1  '; 'p1  '; 'q1  '; 'm4  '; 'ms4 '; 'mn4 ']; %List of constituents in the model

%----------------------------------------------

%% extract constituents
cph= -i*pha*pi/180;
hc = amp.*exp(cph); % dimensions (nc,N,M)...
hc=squeeze(hc);
hc_con=hc(:,:,sc);
d0=datenum(1992,1,1); % corresponds to 48622mjd
d1=SDtime;
time=d1-d0;

%----------------------------------------------

%% Tidal prediction
TS=harp_mod(time,hc_con,conList(sc,:));

switch I
    case 0  %correct for minor constituents
        
        dh = InferMinor(hc,conList,SDtime);
        TS=TS+dh;
    case 1  %DON'T correct for minor constituents
        
end
%----------------------------------------------

%%Obtaining difference between daily maximum and minimum tidal elevation

%For my data lat=1230, lon=1513, hr=361 
[lat,lon,hr]=size(TS);
d=(hr-1)/24;

%Creating an output file
tidal_range=zeros(lat, lon, d);
tidal_min=zeros(lat, lon, 1);
tidal_max=zeros(lat, lon, 1);

%Calculating min and max over the 15 day data-set
tidal_min=min(TS,[],3);
tidal_max=max(TS,[],3);

%Running a loop to calculate the tidal range from mean and max across 24 hr blocks
for k=0:(d-1)
 sd=(24*k)+1;
 ed=sd+23;
 dat_sub=TS(:,:,sd:ed);
 tidal_range(:,:,k+1)=max(dat_sub,[],3) - min(dat_sub,[],3);
end

tidal_range_mean=mean(tidal_range, 3);
%----------------------------------------------

%%Saving the output as a netcdf file
nccreate('tide_min_dec2_11Jan18.nc', 'tidal_min', 'Dimensions', { 'x', 1230, 'y', 1513} );
ncwrite('tide_min_dec2_11Jan18.nc', 'tidal_min', tidal_min);
ncwriteatt('tide_min_dec2_11Jan18.nc', 'tidal_min', 'long_name', 'min tidal height between 1-15 Jan 2016' );
ncwriteatt('tide_min_dec2_11Jan18.nc', 'tidal_min', 'units', 'm' );

nccreate('tide_max_dec2_11Jan18.nc', 'tidal_max', 'Dimensions', { 'x', 1230, 'y', 1513} );
ncwrite('tide_max_dec2_11Jan18.nc', 'tidal_max', tidal_max);
ncwriteatt('tide_max_dec2_11Jan18.nc', 'tidal_max', 'long_name', 'max tidal height between 1-15 Jan 2016' );
ncwriteatt('tide_max_dec2_11Jan18.nc', 'tidal_max', 'units', 'm' );

nccreate('tide_dec2_11Jan18.nc', 'tidal_range_mean', 'Dimensions', { 'x', 1230, 'y', 1513} );
ncwrite('tide_dec2_11Jan18.nc', 'tidal_range_mean', tidal_range_mean);
ncwriteatt('tide_dec2_11Jan18.nc', 'tidal_range_mean', 'long_name', 'mean tidal range between 1-15 Jan 2016' );
ncwriteatt('tide_dec2_11Jan18.nc', 'tidal_range_mean', 'units', 'm' );

%----------------------------------------------
























