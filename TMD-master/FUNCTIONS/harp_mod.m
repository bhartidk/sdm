%Modified from harp.m from TMD toolbox on 22 Nov 2017
% function to predict tidal elevation fields at "time"
% using harmonic constants
% INPUT: time (days) relatively Jan 1, 1992 (48622mjd)
%        con(nc,4) - char*4 tidal constituent IDs 
%        hc(n,m,nc) - harmonic constant vector  (complex)
% OUTPUT:hhat - tidal field at time
%  
%        Nodal corrections included
%
% usage: [hhat]=harp(time,hc,con);
%
% see harp1 for making time series at ONE location
%
function [hhat_fin]=harp_mod(time,hc,con);
[n,m,nc]=size(hc);
L=length(time);
hhat_fin=zeros(n,m,L);
[nc1,dum]=size(con);
if nc1~=nc,
 fprintf('Imcompatible hc and con: check dimensions\n');
 return
 end
%
for k=1:nc
 [ispec(k),amp(k),ph(k),omega(k),alpha(k),cNum]=constit(con(k,:));
end
%
x=zeros(n,m,2*nc);
x(:,:,1:2:end)=real(hc);
x(:,:,2:2:end)=imag(hc);
%
igood=find(ispec~=-1);
con1=con(igood,:);
pu=zeros(1,nc);pf=ones(1,nc);
%
for count=1:L
 [pu1,pf1]=nodal(time(count)+48622,con1);
 pu(igood)=pu1;pf(igood)=pf1;
 hhat=zeros(n,m);
  for k=1:nc
   arg=   pf(k)*x(:,:,2*k-1)*cos(omega(k)*time(count)*86400+ph(k)+pu(k))...
          - pf(k)*x(:,:,2*k)  *sin(omega(k)*time(count)*86400+ph(k)+pu(k));
   hhat=hhat+arg;
  end
 hhat_fin(:,:,count)=hhat;
end
return
 
