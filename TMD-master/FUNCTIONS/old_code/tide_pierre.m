% Lots of times, fewer positions.
model = '/path/to/the/TPXO/model.file';
times = datenum('2000-01-01'):1/2:datenum('2000-02-01');  % a month at hourly resolution
lon = -10:1:-5;
lat = 50:1:55;
[x, y] = meshgrid(lon, lat);
xx = x(:); % make long arrays to ease looping
yy = y(:); % make long arrays to ease looping
nx = length(xx);
zeta = nan(nx, length(times));
parfor i = 1:nx
    zeta(i, :) = tmd_tide_pred(model, times, yy(i), xx(i), 'z', []);
end
tidal_range = max(zeta, [], 3) - min(zeta, [], 3);
% Make everything the right shape again.
zeta = reshape(zeta, [length(lat), length(lon), length(times)]);
% Quick plot
f = figure
subplot(121)
title('Instantaneous surface elevation (m)')
pcolor(x, y, zeta(:, :, 1))
shading flat
axis('equal', 'tight')
colorbar
subplot(122)
title('Tidal range (m)')
pcolor(x, y, tidal_range_mean)
shading flat
axis('equal', 'tight')
colorbar
saveas(f, 'test.png')
